# Site-level theory check: does n-y > m-x hold?
# Revenue only (no cost) — price × yield, no irrigation cost subtraction
# 2031, 2-crop (wheat + canola), OriPrcp vs RedPrcp

library(tidyverse)
library(purrr)
library(here)

ONEDRIVE <- paste0(
  "/Users/tharakajayalath/Library/CloudStorage",
  "/OneDrive-UniversityofSaskatchewan",
  "/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData"
)
BUDGET_GRAIN <- file.path(ONEDRIVE, "CropReturn/CropReturnDarkBrown.csv")
RESULTS_DIR  <- here::here("CanDCSU6_results")
OUT_DIR      <- here::here("outputs/figures_candscu6")

YEAR    <- 2031
Q_RIGID <- 150
W_TOTAL <- 300

# Raw prices (no inflation adjustment — relative comparison only needs consistent prices)
bg_grain <- read.csv(BUDGET_GRAIN, fileEncoding = "UTF-8-BOM")
budget_grain <- bg_grain %>%
  group_by(crop) %>%
  summarise(price.bu = mean(price.bu), .groups = "drop")

PRICE_WHEAT  <- filter(budget_grain, crop == "wheat")$price.bu
PRICE_CANOLA <- filter(budget_grain, crop == "canola")$price.bu
BU_PER_T_WHEAT  <- 36.74
BU_PER_T_CANOLA <- 44.09

# Revenue per site per irrigation level (no cost)
site_revenue <- function(df, price_bu, bu_per_tonne) {
  df %>%
    mutate(
      yield_bu_ac = Yield_tonne_per_ha * bu_per_tonne / 2.47,
      revenue_ac  = yield_bu_ac * price_bu
    ) %>%
    select(Site_ID, Max_Irrigation_mm, revenue_ac)
}

load_scenario <- function(scenario) {
  w_path <- file.path(RESULTS_DIR, scenario,
                      sprintf("WheatCanDCSU6_%s%d.csv", scenario, YEAR))
  c_path <- file.path(RESULTS_DIR, scenario,
                      sprintf("CanolaCanDCSU6_%s%d.csv", scenario, YEAR))
  list(
    wheat  = site_revenue(read_csv(w_path, show_col_types = FALSE),
                          PRICE_WHEAT, BU_PER_T_WHEAT),
    canola = site_revenue(read_csv(c_path, show_col_types = FALSE),
                          PRICE_CANOLA, BU_PER_T_CANOLA)
  )
}

compute_gain <- function(scenario) {
  dat   <- load_scenario(scenario)
  wp    <- dat$wheat
  cp    <- dat$canola
  sites <- intersect(unique(wp$Site_ID), unique(cp$Site_ID))

  map_dfr(sites, function(sid) {
    w <- wp %>% filter(Site_ID == sid) %>% select(irr = Max_Irrigation_mm, rev = revenue_ac)
    c <- cp %>% filter(Site_ID == sid) %>% select(irr = Max_Irrigation_mm, rev = revenue_ac)

    rigid_w <- w$rev[w$irr == Q_RIGID][1]
    rigid_c <- c$rev[c$irr == Q_RIGID][1]
    if (is.na(rigid_w) || is.na(rigid_c)) return(NULL)
    rigid_rev <- rigid_w + rigid_c

    combos <- expand.grid(w_irr = w$irr, c_irr = c$irr) %>%
      filter(w_irr + c_irr <= W_TOTAL) %>%
      left_join(w %>% rename(w_irr = irr, w_rev = rev), by = "w_irr") %>%
      left_join(c %>% rename(c_irr = irr, c_rev = rev), by = "c_irr") %>%
      mutate(joint_rev = w_rev + c_rev)

    if (nrow(combos) == 0) return(NULL)
    flex_rev <- max(combos$joint_rev, na.rm = TRUE)
    best     <- combos %>% slice_max(joint_rev, n = 1, with_ties = FALSE)

    tibble(
      Site_ID      = sid,
      rigid_rev    = rigid_rev,
      flex_rev     = flex_rev,
      gain         = flex_rev - rigid_rev,
      opt_wheat_mm = best$w_irr,
      opt_canola_mm = best$c_irr,
      scenario     = scenario
    )
  })
}

cat("Running OriPrcp...\n")
ori <- compute_gain("OriPrcp")
cat("Running RedPrcp...\n")
red <- compute_gain("RedPrcp")

# Join and classify
comparison <- ori %>%
  select(Site_ID, gain_ori = gain, rigid_ori = rigid_rev, flex_ori = flex_rev,
         opt_w_ori = opt_wheat_mm, opt_c_ori = opt_canola_mm) %>%
  inner_join(
    red %>% select(Site_ID, gain_red = gain, rigid_red = rigid_rev, flex_red = flex_rev,
                   opt_w_red = opt_wheat_mm, opt_c_red = opt_canola_mm),
    by = "Site_ID"
  ) %>%
  mutate(
    theory_holds = gain_red > gain_ori,
    diff         = gain_red - gain_ori
  )

cat("\n====================================================\n")
cat("  THEORY CHECK (revenue only, 2031)\n")
cat("  Theory: n-y > m-x  (RedPrcp gain > OriPrcp gain)\n")
cat("====================================================\n")
cat("Sites where theory HOLDS  (n-y > m-x):", sum(comparison$theory_holds),
    sprintf("(%.1f%%)\n", 100 * mean(comparison$theory_holds)))
cat("Sites where theory BREAKS (m-x > n-y):", sum(!comparison$theory_holds),
    sprintf("(%.1f%%)\n", 100 * mean(!comparison$theory_holds)))
cat("Total sites:", nrow(comparison), "\n\n")

cat("Median gain OriPrcp (m-x): $", round(median(comparison$gain_ori), 2), "/ac\n")
cat("Median gain RedPrcp (n-y): $", round(median(comparison$gain_red), 2), "/ac\n\n")

cat("=== Theory-BREAKING sites (top 10, m-x >> n-y) ===\n")
breaks <- comparison %>% filter(!theory_holds) %>% arrange(diff)
print(breaks %>%
        select(Site_ID, gain_ori, gain_red, diff, rigid_ori, rigid_red,
               opt_w_ori, opt_c_ori, opt_w_red, opt_c_red) %>%
        head(10), n = 10)

cat("\n=== Theory-CONFIRMING sites (top 10, n-y >> m-x) ===\n")
confirms <- comparison %>% filter(theory_holds) %>% arrange(desc(diff))
print(confirms %>%
        select(Site_ID, gain_ori, gain_red, diff, rigid_ori, rigid_red,
               opt_w_ori, opt_c_ori, opt_w_red, opt_c_red) %>%
        head(10), n = 10)

# Characterize breaking sites: what's different about them?
cat("\n=== WHAT BREAKS THE THEORY? ===\n")
cat("Breaking sites — median rigid revenue:\n")
cat("  OriPrcp rigid:", round(median(breaks$rigid_ori), 2), "\n")
cat("  RedPrcp rigid:", round(median(breaks$rigid_red), 2), "\n")
cat("Breaking sites — typical optimal allocation under OriPrcp:\n")
cat("  Wheat:", median(breaks$opt_w_ori), "mm | Canola:", median(breaks$opt_c_ori), "mm\n")
cat("Breaking sites — typical optimal allocation under RedPrcp:\n")
cat("  Wheat:", median(breaks$opt_w_red), "mm | Canola:", median(breaks$opt_c_red), "mm\n")

cat("\nConfirming sites — typical optimal allocation under OriPrcp:\n")
cat("  Wheat:", median(confirms$opt_w_ori), "mm | Canola:", median(confirms$opt_c_ori), "mm\n")
cat("Confirming sites — typical optimal allocation under RedPrcp:\n")
cat("  Wheat:", median(confirms$opt_w_red), "mm | Canola:", median(confirms$opt_c_red), "mm\n")

write_csv(comparison, file.path(OUT_DIR, "theory_check_revenue_2031.csv"))
cat("\nSaved:", file.path(OUT_DIR, "theory_check_revenue_2031.csv"), "\n")
