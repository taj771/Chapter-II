# Figure: Net benefit of flexible water allocation — 3-crop scenario
#
# Framework (per site × year × precipitation scenario):
#
#   Rigid allocation (binding per-field quota):
#     Each crop gets Q = 150 mm  →  W_total = 450 mm (3 × 150)
#     profit_rigid = profit_wheat(150) + profit_canola(150) + profit_potato(150)
#
#   Flexible allocation (optimal split within same W_total = 450 mm):
#     profit_flexible = max over (w, c, p) s.t. w + c + p ≤ 450
#                       of [profit_wheat(w) + profit_canola(c) + profit_potato(p)]
#
#   Net benefit = profit_flexible − profit_rigid  ($/acre, three-crop total)
#
# Output: boxplot across 342 sites
#   x    = climate year (2031 / 2044 / 2050)
#   fill = precipitation scenario (OriPrcp vs RedPrcp)

library(tidyverse)
library(purrr)
library(priceR)
library(patchwork)

# ── paths ─────────────────────────────────────────────────────────────────────

ONEDRIVE <- paste0(
  "/Users/tharakajayalath/Library/CloudStorage",
  "/OneDrive-UniversityofSaskatchewan",
  "/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData"
)
BUDGET_GRAIN  <- file.path(ONEDRIVE, "CropReturn/CropReturnDarkBrown.csv")
BUDGET_POTATO <- file.path(ONEDRIVE, "CropReturn/CropReturnPotato.csv")
RESULTS_DIR   <- here::here("CanDCSU6_results")
OUT_DIR       <- here::here("outputs/figures_candscu6")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

YEARS     <- c(2031, 2044, 2050)
SCENARIOS <- c("OriPrcp", "RedPrcp")

YEAR_LABS <- c(
  "2031" = "Driest\n(2031)",
  "2044" = "Average\n(2044)",
  "2050" = "Wettest\n(2050)"
)
SCEN_LABS <- c(
  "OriPrcp" = "Original precip",
  "RedPrcp" = "20% reduced precip"
)

Q_RIGID  <- 150    # mm per crop under rigid allocation
W_TOTAL  <- 450    # mm total budget (= 3 × Q_RIGID)

# ── crop budgets (inflation-adjusted to 2023 CAD) ─────────────────────────────

inflate <- function(x, yrs, to = 2023)
  adjust_for_inflation(x, as.Date(paste0(yrs, "-07-01")), "CA",
                       to_date = as.Date(paste0(to, "-07-01")))

bg_grain <- read.csv(BUDGET_GRAIN, fileEncoding = "UTF-8-BOM")
bg_grain$irri_cost_fix_ac <- inflate(bg_grain$irri_cost_fix_ac, bg_grain$year)
bg_grain$irri_cost_var_ac <- inflate(bg_grain$irri_cost_var_ac, bg_grain$year)
bg_grain$price.bu         <- inflate(bg_grain$price.bu,         bg_grain$year)

budget_grain <- bg_grain %>%
  group_by(crop) %>%
  summarise(across(c(irri_cost_fix_ac, irri_cost_var_ac, price.bu), mean),
            .groups = "drop")

bw <- filter(budget_grain, crop == "wheat")
bc <- filter(budget_grain, crop == "canola")

bg_potato <- read.csv(BUDGET_POTATO, fileEncoding = "UTF-8-BOM")
bg_potato$irri_cost_fix_ac <- inflate(bg_potato$irri_cost_fix_ac, bg_potato$year)
bg_potato$irri_cost_var_ac <- inflate(bg_potato$irri_cost_var_ac, bg_potato$year)
bg_potato$price.ton        <- inflate(bg_potato$price.ton,        bg_potato$year)

bp <- bg_potato %>%
  summarise(across(c(irri_cost_fix_ac, irri_cost_var_ac, price.ton), mean))

# ── site-level profit functions ───────────────────────────────────────────────

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

site_profit_potato <- function(df, price_ton, irri_fix, irri_var) {
  df %>%
    mutate(
      yield_ton_ac = Yield_tonne_per_ha / 2.47,
      irr_level_in = Total_Irrigation_mm * 0.03937,
      irri_cost_ac = irri_var * irr_level_in + irri_fix,
      profit_ac    = yield_ton_ac * price_ton - irri_cost_ac
    ) %>%
    select(Site_ID, Max_Irrigation_mm, profit_ac)
}

# ── compute per-site allocation benefit ───────────────────────────────────────

compute_site_benefit_3crop <- function(scenario, year) {

  w_path <- file.path(RESULTS_DIR, scenario,
                      sprintf("WheatCanDCSU6_%s%d.csv",   scenario, year))
  c_path <- file.path(RESULTS_DIR, scenario,
                      sprintf("CanolaCanDCSU6_%s%d.csv",  scenario, year))
  p_path <- file.path(RESULTS_DIR, scenario,
                      sprintf("PotatoCanDCSU6_%s%d.csv",  scenario, year))

  missing <- c(w_path, c_path, p_path)[!file.exists(c(w_path, c_path, p_path))]
  if (length(missing) > 0) {
    warning("Missing: ", paste(basename(missing), collapse = ", "),
            " [", scenario, " ", year, "]")
    return(NULL)
  }

  wheat_raw  <- read_csv(w_path, show_col_types = FALSE)
  canola_raw <- read_csv(c_path, show_col_types = FALSE)
  potato_raw <- read_csv(p_path, show_col_types = FALSE)

  wheat_prof  <- site_profit_grain(wheat_raw,  bw$price.bu, 36.74,
                                   bw$irri_cost_fix_ac, bw$irri_cost_var_ac)
  canola_prof <- site_profit_grain(canola_raw, bc$price.bu, 44.09,
                                   bc$irri_cost_fix_ac, bc$irri_cost_var_ac)
  potato_prof <- site_profit_potato(potato_raw, bp$price.ton,
                                    bp$irri_cost_fix_ac, bp$irri_cost_var_ac)

  sites <- Reduce(intersect, list(
    unique(wheat_prof$Site_ID),
    unique(canola_prof$Site_ID),
    unique(potato_prof$Site_ID)
  ))

  map_dfr(sites, function(sid) {

    wp <- wheat_prof  %>% filter(Site_ID == sid) %>%
          select(irr = Max_Irrigation_mm, profit = profit_ac)
    cp <- canola_prof %>% filter(Site_ID == sid) %>%
          select(irr = Max_Irrigation_mm, profit = profit_ac)
    pp <- potato_prof %>% filter(Site_ID == sid) %>%
          select(irr = Max_Irrigation_mm, profit = profit_ac)

    # ── rigid: each crop fixed at Q_RIGID ────────────────────────────────────
    w_rigid <- wp$profit[wp$irr == Q_RIGID]
    c_rigid <- cp$profit[cp$irr == Q_RIGID]
    p_rigid <- pp$profit[pp$irr == Q_RIGID]

    if (!length(w_rigid) || !length(c_rigid) || !length(p_rigid)) return(NULL)
    profit_rigid <- w_rigid[1] + c_rigid[1] + p_rigid[1]

    # ── flexible: grid search over all (w, c, p) s.t. w + c + p ≤ W_TOTAL ───
    combos <- expand.grid(w_irr = wp$irr, c_irr = cp$irr, p_irr = pp$irr) %>%
      filter(w_irr + c_irr + p_irr <= W_TOTAL) %>%
      left_join(wp %>% rename(w_irr = irr, w_prof = profit), by = "w_irr") %>%
      left_join(cp %>% rename(c_irr = irr, c_prof = profit), by = "c_irr") %>%
      left_join(pp %>% rename(p_irr = irr, p_prof = profit), by = "p_irr") %>%
      mutate(joint_profit = w_prof + c_prof + p_prof)

    if (nrow(combos) == 0) return(NULL)
    profit_flexible <- max(combos$joint_profit, na.rm = TRUE)

    best <- combos %>% slice_max(joint_profit, n = 1, with_ties = FALSE)

    tibble(
      Site_ID           = sid,
      profit_flexible   = profit_flexible,
      profit_rigid      = profit_rigid,
      benefit           = profit_flexible - profit_rigid,
      benefit_pct       = 100 * (profit_flexible - profit_rigid) / abs(profit_rigid),
      opt_wheat_mm      = best$w_irr,
      opt_canola_mm     = best$c_irr,
      opt_potato_mm     = best$p_irr,
      scenario          = scenario,
      year              = as.character(year)
    )
  })
}

# ── run all combinations ───────────────────────────────────────────────────────

combos_all <- expand.grid(scenario = SCENARIOS, year = YEARS,
                          stringsAsFactors = FALSE)

site_benefits_3c <- pmap_dfr(combos_all, compute_site_benefit_3crop) %>%
  mutate(
    year_lab = factor(YEAR_LABS[.data$year], levels = YEAR_LABS),
    scen_lab = factor(SCEN_LABS[.data$scenario], levels = SCEN_LABS)
  )

saveRDS(site_benefits_3c, file.path(OUT_DIR, "site_benefits_candscu6_3crop.rds"))

# ── Figure: boxplot ────────────────────────────────────────────────────────────

pal <- c("Original precip" = "#2166ac", "20% reduced precip" = "#d73027")

fig <- ggplot(site_benefits_3c,
              aes(x = year_lab, y = benefit, fill = scen_lab)) +
  geom_boxplot(
    position      = position_dodge(0.8),
    width         = 0.65,
    outlier.size  = 0.5,
    outlier.alpha = 0.4,
    linewidth     = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  scale_fill_manual(values = pal, name = "Precipitation scenario") +
  labs(
    title    = "Net economic benefit of flexible irrigation water allocation (3-crop)",
    subtitle = paste0(
      "Wheat-canola-potato rotation, 342 Saskatchewan sites, CanDCS-U6 SSP2-4.5\n",
      "Rigid: 150 mm per crop (450 mm total) | Flexible: optimal split within same 450 mm budget"
    ),
    x = "Representative climate year\n(GS-precipitation quantile)",
    y = "Net benefit of flexible allocation ($/acre, three-crop total)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey30"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUT_DIR, "fig_allocation_benefit_3crop_boxplot.png"),
       fig, width = 9, height = 6, dpi = 300)

fig_pct <- ggplot(site_benefits_3c,
                  aes(x = year_lab, y = benefit_pct, fill = scen_lab)) +
  geom_boxplot(
    position = position_dodge(0.8), width = 0.65,
    outlier.size = 0.5, outlier.alpha = 0.4, linewidth = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  scale_fill_manual(values = pal, name = "Precipitation scenario") +
  labs(
    title    = "Net economic benefit of flexible irrigation water allocation — 3-crop (%)",
    subtitle = paste0(
      "Wheat-canola-potato rotation, 342 Saskatchewan sites, CanDCS-U6 SSP2-4.5\n",
      "Rigid: 150 mm per crop | Flexible: optimal split within 450 mm total budget"
    ),
    x = "Representative climate year",
    y = "Net benefit of flexible allocation\n(% of rigid allocation profit)"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey30"),
        panel.grid.minor = element_blank())

ggsave(file.path(OUT_DIR, "fig_allocation_benefit_3crop_pct_boxplot.png"),
       fig_pct, width = 9, height = 6, dpi = 300)

# ── optimal allocation distribution ───────────────────────────────────────────

alloc_summary <- site_benefits_3c %>%
  group_by(year_lab, scen_lab) %>%
  summarise(
    median_wheat_mm  = median(opt_wheat_mm,  na.rm = TRUE),
    median_canola_mm = median(opt_canola_mm, na.rm = TRUE),
    median_potato_mm = median(opt_potato_mm, na.rm = TRUE),
    .groups = "drop"
  )

# ── Summary table ──────────────────────────────────────────────────────────────

summary_tbl <- site_benefits_3c %>%
  group_by(year_lab, scen_lab) %>%
  summarise(
    n_sites            = n(),
    median_benefit_ac  = round(median(benefit,     na.rm = TRUE), 2),
    mean_benefit_ac    = round(mean(benefit,       na.rm = TRUE), 2),
    median_benefit_pct = round(median(benefit_pct, na.rm = TRUE), 1),
    pct_sites_positive = round(mean(benefit > 0,   na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

write_csv(summary_tbl,    file.path(OUT_DIR, "benefit_summary_candscu6_3crop.csv"))
write_csv(alloc_summary,  file.path(OUT_DIR, "optimal_allocation_3crop.csv"))
print(summary_tbl, n = Inf)
print(alloc_summary, n = Inf)

cat("\nSaved to:", OUT_DIR, "\n")
