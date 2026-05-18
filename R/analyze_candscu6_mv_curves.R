# CanDCS-U6 Marginal Value Analysis
# Compares OriPrcp vs RedPrcp (20% precip reduction) across three representative years:
#   2031 = driest (P5 GS-precip)  → highest irrigation demand
#   2044 = average (P50)
#   2050 = wettest (P100)         → lowest irrigation demand
#
# Output: MV curves by crop × scenario × year, shadow prices, allocation benefit

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

RESULTS_DIR <- here::here("outputs/CanDCSU6_results")
OUT_DIR     <- here::here("outputs/figures_candscu6")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

YEARS     <- c(2031, 2044, 2050)
YEAR_LABS <- c("2031" = "Driest (2031)", "2044" = "Average (2044)", "2050" = "Wettest (2050)")
SCENARIOS <- c("OriPrcp", "RedPrcp")
SCEN_LABS <- c("OriPrcp" = "Original precip", "RedPrcp" = "20% reduced precip")

# ── load & inflate budgets ────────────────────────────────────────────────────

inflate <- function(x, yrs, to = 2023)
  adjust_for_inflation(x, as.Date(paste0(yrs, "-07-01")), "CA",
                       to_date = as.Date(paste0(to, "-07-01")))

bg_grain <- read.csv(BUDGET_GRAIN, fileEncoding = "UTF-8-BOM")
bg_grain$irri_cost_fix_ac <- inflate(bg_grain$irri_cost_fix_ac, bg_grain$year)
bg_grain$irri_cost_var_ac <- inflate(bg_grain$irri_cost_var_ac, bg_grain$year)
bg_grain$price.bu         <- inflate(bg_grain$price.bu,         bg_grain$year)

budget_grain <- bg_grain %>%
  group_by(crop) %>%
  summarise(across(c(irri_cost_fix_ac, irri_cost_var_ac, price.bu), mean), .groups = "drop")

bg_potato <- read.csv(BUDGET_POTATO, fileEncoding = "UTF-8-BOM")
bg_potato$irri_cost_fix_ac <- inflate(bg_potato$irri_cost_fix_ac, bg_potato$year)
bg_potato$irri_cost_var_ac <- inflate(bg_potato$irri_cost_var_ac, bg_potato$year)
bg_potato$price.ton        <- inflate(bg_potato$price.ton,        bg_potato$year)

budget_potato <- bg_potato %>%
  summarise(across(c(irri_cost_fix_ac, irri_cost_var_ac, price.ton), mean))

bw <- filter(budget_grain, crop == "wheat")
bc <- filter(budget_grain, crop == "canola")
bp <- budget_potato

# ── MV computation helper ─────────────────────────────────────────────────────

compute_mv_grain <- function(df, yield_col, price_bu, bu_per_tonne,
                             irri_fix, irri_var) {
  df %>%
    mutate(
      yield_bu_ac  = .data[[yield_col]] * bu_per_tonne / 2.47,
      irrq_m3      = 4046.86 * (Total_Irrigation_mm * 0.001),
      irr_level_mm = irrq_m3 / (0.001 * 4046.86),
      irr_level_in = irr_level_mm * 0.03937,
      irri_cost_ac = irri_var * irr_level_in + irri_fix,
      return_ir    = yield_bu_ac * price_bu,
      profit_ir    = return_ir - irri_cost_ac
    ) %>%
    arrange(Site_ID, irrq_m3) %>%
    group_by(Site_ID) %>%
    mutate(
      d_profit = profit_ir - lag(profit_ir),
      d_irrq   = irrq_m3  - lag(irrq_m3)
    ) %>%
    mutate(prof_mv = ifelse(d_irrq == 0 | is.na(d_irrq), NA, d_profit / d_irrq)) %>%
    ungroup() %>%
    group_by(Max_Irrigation_mm) %>%
    summarise(
      mv_mean   = mean(prof_mv, na.rm = TRUE),
      prof_mean = mean(profit_ir, na.rm = TRUE),
      .groups   = "drop"
    )
}

compute_mv_potato <- function(df, price_ton, irri_fix, irri_var) {
  df %>%
    mutate(
      yield_ton_ac = Yield_tonne_per_ha / 2.47,
      irrq_m3      = 4046.86 * (Total_Irrigation_mm * 0.001),
      irr_level_mm = irrq_m3 / (0.001 * 4046.86),
      irr_level_in = irr_level_mm * 0.03937,
      irri_cost_ac = irri_var * irr_level_in + irri_fix,
      return_ir    = yield_ton_ac * price_ton,
      profit_ir    = return_ir - irri_cost_ac
    ) %>%
    arrange(Site_ID, irrq_m3) %>%
    group_by(Site_ID) %>%
    mutate(
      d_profit = profit_ir - lag(profit_ir),
      d_irrq   = irrq_m3  - lag(irrq_m3)
    ) %>%
    mutate(prof_mv = ifelse(d_irrq == 0 | is.na(d_irrq), NA, d_profit / d_irrq)) %>%
    ungroup() %>%
    group_by(Max_Irrigation_mm) %>%
    summarise(
      mv_mean   = mean(prof_mv, na.rm = TRUE),
      prof_mean = mean(profit_ir, na.rm = TRUE),
      .groups   = "drop"
    )
}

# ── load all CanDCS-U6 CSVs and compute MV ───────────────────────────────────

load_and_mv <- function(crop, scenario, year) {
  fname <- switch(crop,
    wheat  = sprintf("WheatCanDCSU6_%s%d.csv",  scenario, year),
    canola = sprintf("CanolaCanDCSU6_%s%d.csv", scenario, year),
    potato = sprintf("PotatoCanDCSU6_%s%d.csv", scenario, year)
  )
  path <- file.path(RESULTS_DIR, scenario, fname)
  if (!file.exists(path)) { warning("Missing: ", path); return(NULL) }

  df <- read_csv(path, show_col_types = FALSE)

  mv <- switch(crop,
    wheat  = compute_mv_grain(df, "Yield_tonne_per_ha", bw$price.bu, 36.74,
                               bw$irri_cost_fix_ac, bw$irri_cost_var_ac),
    canola = compute_mv_grain(df, "Yield_tonne_per_ha", bc$price.bu, 44.09,
                               bc$irri_cost_fix_ac, bc$irri_cost_var_ac),
    potato = compute_mv_potato(df, bp$price.ton, bp$irri_cost_fix_ac, bp$irri_cost_var_ac)
  )
  mv %>% mutate(crop = crop, scenario = scenario, year = as.character(year))
}

combos <- expand.grid(
  crop     = c("wheat", "canola", "potato"),
  scenario = SCENARIOS,
  year     = YEARS,
  stringsAsFactors = FALSE
)

mv_all <- pmap_dfr(combos, load_and_mv) %>%
  mutate(
    year_lab = YEAR_LABS[year],
    scen_lab = SCEN_LABS[scenario],
    year_lab = factor(year_lab, levels = YEAR_LABS),
    scen_lab = factor(scen_lab, levels = SCEN_LABS)
  )

saveRDS(mv_all, file.path(OUT_DIR, "mv_all_candscu6.rds"))

# ── Figure A: MV by year (OriPrcp only) ──────────────────────────────────────
# Shows: dry year = highest water value, wet year = lowest

palette_year <- c("Driest (2031)" = "#d62728",
                  "Average (2044)" = "#ff7f0e",
                  "Wettest (2050)" = "#1f77b4")

plot_mv_by_year <- function(cr, max_irr = 150, max_mv = NULL) {
  df <- mv_all %>%
    filter(crop == cr, scenario == "OriPrcp", Max_Irrigation_mm > 0,
           Max_Irrigation_mm <= max_irr, mv_mean > 0)

  p <- ggplot(df, aes(x = Max_Irrigation_mm, y = mv_mean,
                      colour = year_lab, linetype = year_lab)) +
    geom_line(linewidth = 0.8) +
    geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE,
                linewidth = 0.4, alpha = 0.6) +
    scale_colour_manual(values = palette_year, name = "Climate year") +
    scale_linetype_manual(values = c("solid","dashed","dotted"), name = "Climate year") +
    labs(
      title  = tools::toTitleCase(cr),
      x      = "Maximum seasonal irrigation (mm)",
      y      = "Marginal profit value ($/m³)"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")

  if (!is.null(max_mv)) p <- p + coord_cartesian(ylim = c(0, max_mv))
  p
}

fig_a <- (plot_mv_by_year("wheat",  150) |
          plot_mv_by_year("canola", 150) |
          plot_mv_by_year("potato", 200, max_mv = 15)) +
  plot_annotation(
    title    = "Marginal profit value of irrigation — CanDCS-U6 OriPrcp",
    subtitle = "342 Saskatchewan sites, SSP2-4.5; driest/average/wettest GS-precipitation years",
    theme    = theme(plot.title = element_text(size = 13, face = "bold"))
  )

ggsave(file.path(OUT_DIR, "figA_mv_by_year_oriprcp.png"),
       fig_a, width = 12, height = 5, dpi = 300)

# ── Figure B: OriPrcp vs RedPrcp by year ─────────────────────────────────────
# Shows: 20% precip reduction shifts MV curve up (higher water value)

palette_scen <- c("Original precip" = "#2166ac", "20% reduced precip" = "#d73027")

plot_mv_by_scen <- function(cr, yr, max_irr = 150, max_mv = NULL) {
  df <- mv_all %>%
    filter(crop == cr, year == as.character(yr),
           Max_Irrigation_mm > 0, Max_Irrigation_mm <= max_irr, mv_mean > 0)

  p <- ggplot(df, aes(x = Max_Irrigation_mm, y = mv_mean,
                      colour = scen_lab, linetype = scen_lab)) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = palette_scen, name = NULL) +
    scale_linetype_manual(values = c("solid","dashed"), name = NULL) +
    labs(
      title = sprintf("%s — %s", tools::toTitleCase(cr), YEAR_LABS[as.character(yr)]),
      x = "Max seasonal irrigation (mm)",
      y = "Marginal profit value ($/m³)"
    ) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom")

  if (!is.null(max_mv)) p <- p + coord_cartesian(ylim = c(0, max_mv))
  p
}

panels <- map(YEARS, function(yr) {
  pw <- plot_mv_by_scen("wheat",  yr, 150)
  pc <- plot_mv_by_scen("canola", yr, 150)
  pp <- plot_mv_by_scen("potato", yr, 200, max_mv = 15)
  pw | pc | pp
})

fig_b <- wrap_plots(panels, ncol = 1) +
  plot_annotation(
    title    = "OriPrcp vs 20% reduced precipitation — CanDCS-U6 marginal profit values",
    subtitle = "Rows: driest (2031) / average (2044) / wettest (2050) year",
    theme    = theme(plot.title = element_text(size = 13, face = "bold"))
  )

ggsave(file.path(OUT_DIR, "figB_mv_oriprcp_vs_redprcp.png"),
       fig_b, width = 12, height = 12, dpi = 300)

# ── Table: average shadow price at optimal (Max_Irr = 50mm threshold) ────────
# Shadow price = MV at the margin where irrigation just pays off

shadow_tbl <- mv_all %>%
  filter(Max_Irrigation_mm > 0, mv_mean > 0) %>%
  group_by(crop, scenario, year) %>%
  summarise(
    shadow_price_avg  = mean(mv_mean, na.rm = TRUE),
    shadow_price_peak = max(mv_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year_lab = YEAR_LABS[year],
    scen_lab = SCEN_LABS[scenario]
  ) %>%
  select(crop, year_lab, scen_lab, shadow_price_avg, shadow_price_peak) %>%
  arrange(crop, year_lab, scen_lab)

write_csv(shadow_tbl, file.path(OUT_DIR, "shadow_prices_candscu6.csv"))
print(shadow_tbl)

# ── Allocation benefit: flexible vs per-field (constrained at median irr) ─────
# Constrained: each crop field gets median allocation (status quo binding quota)
# Flexible: optimise total water across crops → pick combo maximising Σ profit

TOTAL_WATER_MM <- 300   # total seasonal water budget (mm) across wheat + canola

wheat_ir  <- mv_all %>% filter(crop == "wheat",  Max_Irrigation_mm <= 150)
canola_ir <- mv_all %>% filter(crop == "canola", Max_Irrigation_mm <= 150)

alloc_benefit <- map_dfr(SCENARIOS, function(scen) {
  map_dfr(as.character(YEARS), function(yr) {
    wdf <- wheat_ir  %>% filter(scenario == scen, year == yr) %>%
      rename(w_irr = Max_Irrigation_mm, w_prof = prof_mean) %>%
      select(w_irr, w_prof)
    cdf <- canola_ir %>% filter(scenario == scen, year == yr) %>%
      rename(c_irr = Max_Irrigation_mm, c_prof = prof_mean) %>%
      select(c_irr, c_prof)

    combos_ac <- expand.grid(w_irr = wdf$w_irr, c_irr = cdf$c_irr) %>%
      filter(w_irr + c_irr <= TOTAL_WATER_MM) %>%
      left_join(wdf, by = "w_irr") %>%
      left_join(cdf, by = "c_irr") %>%
      mutate(total_prof = w_prof + c_prof)

    # Flexible: max total profit
    flex <- combos_ac %>% slice_max(total_prof, n = 1)

    # Constrained: each crop gets TOTAL_WATER_MM/2 = 150mm (binding per-field quota)
    med_irr <- TOTAL_WATER_MM / 2
    w150 <- wdf %>% filter(w_irr == med_irr) %>% pull(w_prof)
    c150 <- cdf %>% filter(c_irr == med_irr) %>% pull(c_prof)
    constrained_prof <- if (length(w150) & length(c150)) w150[1] + c150[1] else NA

    tibble(
      scenario          = scen,
      year              = yr,
      profit_flexible   = flex$total_prof[1],
      profit_constrained = constrained_prof,
      benefit_pct       = 100 * (flex$total_prof[1] - constrained_prof) / abs(constrained_prof),
      w_irr_optimal     = flex$w_irr[1],
      c_irr_optimal     = flex$c_irr[1]
    )
  })
})

write_csv(alloc_benefit, file.path(OUT_DIR, "allocation_benefit_candscu6.csv"))

cat("\n=== Allocation benefit: flexible vs per-field quota ===\n")
print(alloc_benefit %>%
  mutate(year_lab = YEAR_LABS[year], scen_lab = SCEN_LABS[scenario]) %>%
  select(scen_lab, year_lab, profit_flexible, profit_constrained,
         benefit_pct, w_irr_optimal, c_irr_optimal))

cat("\nOutputs saved to:", OUT_DIR, "\n")
cat("  figA_mv_by_year_oriprcp.png\n")
cat("  figB_mv_oriprcp_vs_redprcp.png\n")
cat("  shadow_prices_candscu6.csv\n")
cat("  allocation_benefit_candscu6.csv\n")
