# Extract optimal allocation (2-crop) for all 6 year × scenario combos
# Saves: opt_wheat_mm, opt_canola_mm per site × year × scenario
# Then joins with profit benefits and classifies demand

library(tidyverse)
library(purrr)
library(priceR)
library(here)

ONEDRIVE <- paste0(
  "/Users/tharakajayalath/Library/CloudStorage",
  "/OneDrive-UniversityofSaskatchewan",
  "/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData"
)
BUDGET_GRAIN <- file.path(ONEDRIVE, "CropReturn/CropReturnDarkBrown.csv")
RESULTS_DIR  <- here::here("CanDCSU6_results")
OUT_DIR      <- here::here("outputs/figures_candscu6")

YEARS     <- c(2031, 2044, 2050)
SCENARIOS <- c("OriPrcp", "RedPrcp")
Q_RIGID   <- 150
W_TOTAL   <- 300

inflate <- function(x, yrs, to = 2023)
  adjust_for_inflation(x, as.Date(paste0(yrs, "-07-01")), "CA",
                       to_date = as.Date(paste0(to, "-07-01")))

bg_grain <- read.csv(BUDGET_GRAIN, fileEncoding = "UTF-8-BOM")
bg_grain$irri_cost_fix_ac <- inflate(bg_grain$irri_cost_fix_ac, bg_grain$year)
bg_grain$irri_cost_var_ac <- inflate(bg_grain$irri_cost_var_ac, bg_grain$year)
bg_grain$price.bu         <- inflate(bg_grain$price.bu, bg_grain$year)

budget_grain <- bg_grain %>%
  group_by(crop) %>%
  summarise(across(c(irri_cost_fix_ac, irri_cost_var_ac, price.bu), mean),
            .groups = "drop")

bw <- filter(budget_grain, crop == "wheat")
bc <- filter(budget_grain, crop == "canola")

site_profit_grain <- function(df, price_bu, bu_per_tonne, irri_fix, irri_var) {
  df %>%
    mutate(
      yield_bu_ac  = Yield_tonne_per_ha * bu_per_tonne / 2.47,
      irr_level_in = Total_Irrigation_mm * 0.03937,
      irri_cost_ac = irri_var * irr_level_in + irri_fix,
      profit_ac    = yield_bu_ac * price_bu - irri_cost_ac
    ) %>%
    select(Site_ID, Max_Irrigation_mm, profit_ac)
}

extract_opt_alloc <- function(scenario, year) {
  w_path <- file.path(RESULTS_DIR, scenario,
                      sprintf("WheatCanDCSU6_%s%d.csv", scenario, year))
  c_path <- file.path(RESULTS_DIR, scenario,
                      sprintf("CanolaCanDCSU6_%s%d.csv", scenario, year))

  if (!file.exists(w_path) || !file.exists(c_path)) {
    warning("Missing files for ", scenario, " ", year); return(NULL)
  }

  wheat_prof  <- site_profit_grain(read_csv(w_path, show_col_types = FALSE),
                                   bw$price.bu, 36.74,
                                   bw$irri_cost_fix_ac, bw$irri_cost_var_ac)
  canola_prof <- site_profit_grain(read_csv(c_path, show_col_types = FALSE),
                                   bc$price.bu, 44.09,
                                   bc$irri_cost_fix_ac, bc$irri_cost_var_ac)

  sites <- intersect(unique(wheat_prof$Site_ID), unique(canola_prof$Site_ID))

  map_dfr(sites, function(sid) {
    wp <- wheat_prof  %>% filter(Site_ID == sid) %>%
          select(irr = Max_Irrigation_mm, profit = profit_ac)
    cp <- canola_prof %>% filter(Site_ID == sid) %>%
          select(irr = Max_Irrigation_mm, profit = profit_ac)

    combos <- expand.grid(w_irr = wp$irr, c_irr = cp$irr) %>%
      filter(w_irr + c_irr <= W_TOTAL) %>%
      left_join(wp %>% rename(w_irr = irr, w_prof = profit), by = "w_irr") %>%
      left_join(cp %>% rename(c_irr = irr, c_prof = profit), by = "c_irr") %>%
      mutate(joint_profit = w_prof + c_prof)

    if (nrow(combos) == 0) return(NULL)
    best <- combos %>% slice_max(joint_profit, n = 1, with_ties = FALSE)

    tibble(
      Site_ID       = sid,
      opt_wheat_mm  = best$w_irr,
      opt_canola_mm = best$c_irr,
      total_opt_mm  = best$w_irr + best$c_irr,
      scenario      = scenario,
      year          = as.character(year)
    )
  })
}

cat("Extracting optimal allocations for all 6 combos...\n")
combos_all <- expand.grid(scenario = SCENARIOS, year = YEARS,
                          stringsAsFactors = FALSE)
opt_alloc <- pmap_dfr(combos_all, extract_opt_alloc)

# Classify demand per site × year × scenario
opt_alloc <- opt_alloc %>%
  mutate(demand_class = ifelse(total_opt_mm >= (W_TOTAL - 5),
                               "Full demand (≥295mm)",
                               "Excess capacity (<295mm)"))

cat("\n=== DEMAND CLASS COUNTS ===\n")
opt_alloc %>% count(year, scenario, demand_class) %>% print(n = 30)

# Join with profit-based benefits from existing RDS
benefits <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6.rds"))

benefits_classified <- benefits %>%
  left_join(opt_alloc %>% select(Site_ID, year, scenario, demand_class, total_opt_mm),
            by = c("Site_ID", "year", "scenario"))

cat("\n=== BENEFIT (median %) BY DEMAND CLASS × YEAR × SCENARIO ===\n")
benefits_classified %>%
  group_by(year, scenario, demand_class) %>%
  summarise(n = n(), med_pct = round(median(benefit_pct, na.rm=TRUE), 1),
            .groups = "drop") %>%
  arrange(year, demand_class, scenario) %>%
  print(n = 40)

saveRDS(benefits_classified,
        file.path(OUT_DIR, "site_benefits_demand_classified.rds"))
cat("\nSaved: site_benefits_demand_classified.rds\n")
