# Figure: Net benefit of flexible water allocation — CanDCS-U6 climate projections
#
# Framework (per site × year × precipitation scenario):
#
#   Rigid allocation (binding per-field quota):
#     Each crop gets fixed quota Q = 150 mm  →  W_total = 300 mm
#     profit_rigid(site) = profit_wheat(site, 150) + profit_canola(site, 150)
#
#   Flexible allocation (reallocate within same total budget W_total = 300 mm):
#     profit_flexible(site) = max over all (w, c) s.t. w + c ≤ 300
#                             of [profit_wheat(site, w) + profit_canola(site, c)]
#
#   Net benefit = profit_flexible - profit_rigid  ($/acre, two-crop total)
#
# Boxplot: distribution across 342 sites
#   x    = climate year (2031 driest / 2044 average / 2050 wettest)
#   fill = precipitation scenario (OriPrcp vs RedPrcp)
#   Expected: driest year → highest benefit (binding quota costs most when rainfall lowest)
#             RedPrcp > OriPrcp (precip reduction tightens scarcity)

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

Q_RIGID   <- 150    # mm per crop under rigid allocation
W_TOTAL   <- 300    # mm total budget (= 2 × Q_RIGID)

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

# ── site-level profit for grain crops ────────────────────────────────────────

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

# ── compute per-site allocation benefit ───────────────────────────────────────

compute_site_benefit <- function(scenario, year) {

  w_path <- file.path(RESULTS_DIR, scenario,
                      sprintf("WheatCanDCSU6_%s%d.csv",  scenario, year))
  c_path <- file.path(RESULTS_DIR, scenario,
                      sprintf("CanolaCanDCSU6_%s%d.csv", scenario, year))

  if (!file.exists(w_path) || !file.exists(c_path)) {
    warning("Missing file(s) for ", scenario, " ", year)
    return(NULL)
  }

  wheat_raw  <- read_csv(w_path, show_col_types = FALSE)
  canola_raw <- read_csv(c_path, show_col_types = FALSE)

  wheat_prof  <- site_profit_grain(wheat_raw,  bw$price.bu, 36.74,
                                   bw$irri_cost_fix_ac, bw$irri_cost_var_ac)
  canola_prof <- site_profit_grain(canola_raw, bc$price.bu, 44.09,
                                   bc$irri_cost_fix_ac, bc$irri_cost_var_ac)

  # common sites only
  sites <- intersect(unique(wheat_prof$Site_ID), unique(canola_prof$Site_ID))

  map_dfr(sites, function(sid) {

    wp <- wheat_prof  %>% filter(Site_ID == sid) %>%
          select(irr = Max_Irrigation_mm, profit = profit_ac)
    cp <- canola_prof %>% filter(Site_ID == sid) %>%
          select(irr = Max_Irrigation_mm, profit = profit_ac)

    # ── rigid: profit at fixed quota Q_RIGID for each crop ──────────────────
    w_rigid <- wp$profit[wp$irr == Q_RIGID]
    c_rigid <- cp$profit[cp$irr == Q_RIGID]

    if (!length(w_rigid) || !length(c_rigid)) return(NULL)
    profit_rigid <- w_rigid[1] + c_rigid[1]

    # ── flexible: optimise split over all (w, c) pairs s.t. w + c <= W_TOTAL ─
    combos <- expand.grid(w_irr = wp$irr, c_irr = cp$irr) %>%
      filter(w_irr + c_irr <= W_TOTAL) %>%
      left_join(wp %>% rename(w_irr = irr, w_prof = profit), by = "w_irr") %>%
      left_join(cp %>% rename(c_irr = irr, c_prof = profit), by = "c_irr") %>%
      mutate(joint_profit = w_prof + c_prof)

    if (nrow(combos) == 0) return(NULL)
    profit_flexible <- max(combos$joint_profit, na.rm = TRUE)

    tibble(
      Site_ID           = sid,
      profit_flexible   = profit_flexible,
      profit_rigid      = profit_rigid,
      benefit           = profit_flexible - profit_rigid,
      benefit_pct       = 100 * (profit_flexible - profit_rigid) / abs(profit_rigid),
      scenario          = scenario,
      year              = as.character(year)
    )
  })
}

# ── run all combinations ───────────────────────────────────────────────────────

combos_all <- expand.grid(scenario = SCENARIOS, year = YEARS,
                          stringsAsFactors = FALSE)

site_benefits <- pmap_dfr(combos_all, compute_site_benefit) %>%
  mutate(
    year_lab = factor(YEAR_LABS[year], levels = YEAR_LABS),
    scen_lab = factor(SCEN_LABS[scenario], levels = SCEN_LABS)
  )

saveRDS(site_benefits, file.path(OUT_DIR, "site_benefits_candscu6.rds"))

# ── Figure: boxplot ────────────────────────────────────────────────────────────

pal <- c("Original precip" = "#2166ac", "20% reduced precip" = "#d73027")

fig <- ggplot(site_benefits,
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
    title    = "Net economic benefit of flexible irrigation water allocation",
    subtitle = paste0(
      "Wheat-canola crop rotation, 342 Saskatchewan sites, CanDCS-U6 SSP2-4.5\n",
      "Rigid: 150 mm per crop (300 mm total) | Flexible: optimal split within same 300 mm budget"
    ),
    x = "Representative climate year\n(GS-precipitation quantile)",
    y = "Net benefit of flexible allocation ($/acre, wheat + canola)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey30"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUT_DIR, "fig_allocation_benefit_boxplot.png"),
       fig, width = 9, height = 6, dpi = 300)

# percentage version
fig_pct <- ggplot(site_benefits,
                  aes(x = year_lab, y = benefit_pct, fill = scen_lab)) +
  geom_boxplot(
    position = position_dodge(0.8), width = 0.65,
    outlier.size = 0.5, outlier.alpha = 0.4, linewidth = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  scale_fill_manual(values = pal, name = "Precipitation scenario") +
  labs(
    title    = "Net economic benefit of flexible irrigation water allocation (%)",
    subtitle = paste0(
      "Wheat-canola crop rotation, 342 Saskatchewan sites, CanDCS-U6 SSP2-4.5\n",
      "Rigid: 150 mm per crop | Flexible: optimal split within 300 mm total budget"
    ),
    x = "Representative climate year",
    y = "Net benefit of flexible allocation\n(% of rigid allocation profit)"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey30"),
        panel.grid.minor = element_blank())

ggsave(file.path(OUT_DIR, "fig_allocation_benefit_pct_boxplot.png"),
       fig_pct, width = 9, height = 6, dpi = 300)

# ── Summary table ──────────────────────────────────────────────────────────────

summary_tbl <- site_benefits %>%
  group_by(year_lab, scen_lab) %>%
  summarise(
    n_sites            = n(),
    median_benefit_ac  = round(median(benefit,     na.rm = TRUE), 2),
    mean_benefit_ac    = round(mean(benefit,       na.rm = TRUE), 2),
    median_benefit_pct = round(median(benefit_pct, na.rm = TRUE), 1),
    pct_sites_positive = round(mean(benefit > 0,   na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

write_csv(summary_tbl, file.path(OUT_DIR, "benefit_summary_candscu6.csv"))
print(summary_tbl, n = Inf)

cat("\nSaved to:", OUT_DIR, "\n")
