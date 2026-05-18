# Figure 5: Marginal value of irrigation water ($/m3) at different irrigation levels.
# A) Wheat, B) Canola, C) Potato
# Marginal value = incremental profit / incremental irrigation volume between simulation steps

rm(list = ls())
library(tidyverse)
library(purrr)
library(glue)
library(priceR)
library(patchwork)
library(lubridate)

BASE  <- "./Data Main Analysis"
YEARS <- 2018:2023

# ── helpers ──────────────────────────────────────────────────────────────────

read_marginal <- function(crop, years = YEARS) {
  map_dfr(years, ~{
    read_csv(glue("{BASE}/merged_simulation_results_{crop}_marginal_{.x}_irrigation.csv"),
             show_col_types = FALSE) %>% mutate(year = .x)
  })
}

read_rainfed <- function(crop, years = YEARS) {
  map_dfr(years, ~{
    read_csv(glue("{BASE}/{crop}_rainfed_{.x}.csv"),
             show_col_types = FALSE) %>% mutate(year_read = .x)
  })
}

inflate_cols <- function(df, cols, country = "CA", to = 2023) {
  yrs <- df$year
  for (col in cols) df[[col]] <- adjust_for_inflation(df[[col]], yrs, country, to_date = to)
  df
}

# Average value above rainfed baseline: AV(W) = (profit_ir_corrected(W) - profit_rf) / irrq_m3(W)
# Floor correction: a farmer never irrigates if it reduces yield below rainfed — so
# corrected profit uses max(yield_ir, yield_rf) per site-year before averaging across sites.
compute_av <- function(ir_df, rf_df, av_col, site_level = FALSE) {
  rf_base <- rf_df %>%
    select(year, Site_ID, profit_rf = profit_ir)

  joined <- ir_df %>%
    left_join(rf_base, by = c("year", "Site_ID")) %>%
    mutate(profit_ir_floor = pmax(profit_ir, profit_rf))

  if (site_level) {
    # Return per-site AV — captures spatial + temporal variation for LOESS
    return(joined %>%
      mutate(!!av_col := (profit_ir_floor - profit_rf) / irrq_m3) %>%
      select(year, Site_ID, Max_Irrigation_mm, irrq_m3, all_of(av_col)))
  }

  # Default: aggregate to (year × quota) for summary stats
  joined %>%
    group_by(year, Max_Irrigation_mm) %>%
    summarise(profit_ir_floor = mean(profit_ir_floor, na.rm = TRUE),
              profit_rf_mean  = mean(profit_rf,        na.rm = TRUE),
              irrq_m3         = mean(irrq_m3,          na.rm = TRUE),
              .groups = "drop") %>%
    mutate(!!av_col := (profit_ir_floor - profit_rf_mean) / irrq_m3)
}

mv_theme <- function() {
  theme_minimal() %+replace% theme(
    legend.position  = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "black"),
    axis.text.x      = element_text(size = 12, angle = 90),
    axis.text.y      = element_text(size = 12),
    axis.title       = element_text(size = 12),
    axis.ticks       = element_line(linewidth = 0.8)
  )
}

make_mv_plot <- function(data, y_col, x_lim, y_lim, y_breaks, span = 0.75) {
  ggplot(data, aes(x = Max_Irrigation_mm, y = .data[[y_col]])) +
    geom_smooth(aes(color = "Overall"), method = "loess", span = span,
                se = TRUE, linewidth = 0.9) +
    geom_segment(aes(x = x_lim[1], xend = x_lim[2], y = 0, yend = 0),
                 linetype = "dashed", color = "grey", linewidth = 0.5) +
    scale_color_manual(values = c("Overall" = "red4")) +
    scale_x_continuous(name = "Irrigation Level (mm)",
                       breaks = seq(x_lim[1], x_lim[2], by = 10),
                       limits = x_lim,
                       expand = expansion(mult = c(0, 0.05))) +
    scale_y_continuous(name = expression("Average Value ($m"^{-3}*")"),
                       breaks = y_breaks) +
    coord_cartesian(ylim = y_lim) +
    mv_theme()
}

# ── crop budgets ──────────────────────────────────────────────────────────────

budget_grain <- read.csv(glue("{BASE}/CropReturnDarkBrown.csv")) %>%
  inflate_cols(c("dry_cost_ac", "irri_cost_fix_ac", "irri_cost_var_ac", "price.bu"))

budget_potato <- read.csv(glue("{BASE}/CropReturnPotato.csv")) %>%
  inflate_cols(c("irri_cost_fix_ac", "irri_cost_var_ac", "price.ton"))

# ── wheat ─────────────────────────────────────────────────────────────────────

bw <- filter(budget_grain, crop == "wheat")

wheat_ir <- read_marginal("wheat") %>%
  mutate(yield_bu_ac = Yield_tonne_per_ha * 36.74 / 2.47,
         irrq_m3     = 4046.86 * Total_Irrigation_mm * 0.001) %>%
  select(year, Site_ID, Max_Irrigation_mm, yield_bu_ac, irrq_m3) %>%
  left_join(bw, by = "year") %>%
  mutate(irr_in       = irrq_m3 / (0.001 * 4046.86) * 0.03937,
         irri_cost_ac = irri_cost_var_ac * irr_in + irri_cost_fix_ac,
         return_ir    = yield_bu_ac * price.bu,
         profit_ir    = return_ir - irri_cost_ac)

wheat_rf <- read_rainfed("wheat") %>%
  rename(Site_ID = Site) %>%
  mutate(year         = year(`Harvest Date (YYYY/MM/DD)`),
         yield_bu_ac  = `Dry yield (tonne/ha)` * 36.74 / 2.47,
         irrq_m3      = 4046.86 * `Seasonal irrigation (mm)` * 0.001,
         Max_Irrigation_mm = `Seasonal irrigation (mm)`) %>%
  left_join(bw, by = "year") %>%
  mutate(irr_in       = irrq_m3 / (0.001 * 4046.86) * 0.03937,
         irri_cost_ac = irri_cost_var_ac * irr_in + irri_cost_fix_ac,
         return_ir    = yield_bu_ac * price.bu,
         profit_ir    = return_ir - irri_cost_ac) %>%
  select(year, Site_ID, Max_Irrigation_mm, yield_bu_ac, irrq_m3, return_ir, profit_ir)

wheat    <- compute_av(wheat_ir, wheat_rf, "prof_av_wheat")
wheat_sl <- compute_av(wheat_ir, wheat_rf, "prof_av_wheat", site_level = TRUE)

p1 <- make_mv_plot(wheat, "prof_av_wheat",
                   x_lim = c(10, 200), y_lim = c(-0.02, 0.35),
                   y_breaks = seq(0, 0.35, 0.05))
ggsave("./results/images/MV_wheat_2018.png", p1, width = 10, height = 7, dpi = 100)

# ── canola ────────────────────────────────────────────────────────────────────

bc <- filter(budget_grain, crop == "canola")

canola_ir <- read_marginal("canola") %>%
  mutate(yield_bu_ac = Yield_tonne_per_ha * 44.09 / 2.47,
         irrq_m3     = 4046.86 * Total_Irrigation_mm * 0.001) %>%
  select(year, Site_ID, Max_Irrigation_mm, yield_bu_ac, irrq_m3) %>%
  left_join(bc, by = "year") %>%
  mutate(irr_in       = irrq_m3 / (0.001 * 4046.86) * 0.03937,
         irri_cost_ac = irri_cost_var_ac * irr_in + irri_cost_fix_ac,
         return_ir    = yield_bu_ac * price.bu,
         profit_ir    = return_ir - irri_cost_ac)

canola_rf <- read_rainfed("canola") %>%
  rename(Site_ID = Site) %>%
  mutate(year         = year(`Harvest Date (YYYY/MM/DD)`),
         yield_bu_ac  = `Dry yield (tonne/ha)` * 44.09 / 2.47,
         irrq_m3      = 4046.86 * `Seasonal irrigation (mm)` * 0.001,
         Max_Irrigation_mm = `Seasonal irrigation (mm)`) %>%
  left_join(bc, by = "year") %>%
  mutate(irr_in       = irrq_m3 / (0.001 * 4046.86) * 0.03937,
         irri_cost_ac = irri_cost_var_ac * irr_in + irri_cost_fix_ac,
         return_ir    = yield_bu_ac * price.bu,
         profit_ir    = return_ir - irri_cost_ac) %>%
  select(year, Site_ID, Max_Irrigation_mm, yield_bu_ac, irrq_m3, return_ir, profit_ir)

canola    <- compute_av(canola_ir, canola_rf, "prof_av_canola")
canola_sl <- compute_av(canola_ir, canola_rf, "prof_av_canola", site_level = TRUE)

p2 <- make_mv_plot(canola, "prof_av_canola",
                   x_lim = c(10, 200), y_lim = c(-0.05, 0.65),
                   y_breaks = seq(0, 0.65, 0.1))
ggsave("./results/images/MV_canola_2018.png", p2, width = 10, height = 7, dpi = 100)

# ── potato ────────────────────────────────────────────────────────────────────
# No rainfed potato simulation exists (dry-land potato is not grown in SK).
# AV(W) = profit_ir(W) / irrq_m3(W) — equivalent to profit_rf = 0 baseline.

potato_ir <- read_marginal("potato") %>%
  mutate(yield_ton_ac = Yield_tonne_per_ha / 2.47,
         irrq_m3      = 4046.86 * Total_Irrigation_mm * 0.001) %>%
  select(year, Site_ID, Max_Irrigation_mm, yield_ton_ac, irrq_m3) %>%
  left_join(budget_potato, by = "year") %>%
  mutate(irr_in       = irrq_m3 / (0.001 * 4046.86) * 0.03937,
         irri_cost_ac = irri_cost_var_ac * irr_in + irri_cost_fix_ac,
         return_ir    = yield_ton_ac * price.ton,
         profit_ir    = return_ir - irri_cost_ac)

# Build a zero-profit rainfed stub so compute_av works uniformly
potato_rf_stub <- potato_ir %>%
  distinct(year, Site_ID) %>%
  mutate(profit_ir = 0)

potato    <- compute_av(potato_ir, potato_rf_stub, "prof_av_potato")
potato_sl <- compute_av(potato_ir, potato_rf_stub, "prof_av_potato", site_level = TRUE)

p3 <- make_mv_plot(potato, "prof_av_potato",
                   x_lim = c(10, 260), y_lim = c(-0.1, 2.6),
                   y_breaks = seq(0, 2.6, 0.4), span = 1)
ggsave("./results/images/MV_potato_2018.png", p3, width = 10, height = 7, dpi = 100)

# ── panel D: crop-mix-weighted average value ──────────────────────────────────
# AV_wtd(W) = w_wheat×AV_wheat + w_canola×AV_canola + w_potato×AV_potato
# Common quota range 10–200 mm (matches potato and wheat/canola simulation start).

WEIGHTS <- tibble(
  year    = YEARS,
  wheat   = c(24.4, 31.2, 31.7, 34.2, 33.7, 35.4),
  canola  = c(30.0, 22.1, 28.9, 32.0, 31.8, 27.8),
  potato  = c( 5.9,  5.9,  4.1,  4.2,  5.1,  4.3)
) %>%
  mutate(total    = wheat + canola + potato,
         w_wheat  = wheat  / total,
         w_canola = canola / total,
         w_potato = potato / total) %>%
  select(year, w_wheat, w_canola, w_potato)

# Align site-level data to common 50–200 mm range
common_range <- seq(10, 200, by = 10)

interp_to_common <- function(sl_df, av_col, range_mm) {
  sl_df %>%
    group_by(year, Site_ID) %>%
    summarise(
      approx_res = list(approx(Max_Irrigation_mm, .data[[av_col]],
                               xout = range_mm, rule = 2)),
      .groups = "drop"
    ) %>%
    mutate(Max_Irrigation_mm = map(approx_res, "x"),
           av_val             = map(approx_res, "y")) %>%
    select(-approx_res) %>%
    unnest(cols = c(Max_Irrigation_mm, av_val)) %>%
    rename(!!av_col := av_val)
}

wheat_c  <- interp_to_common(wheat_sl,  "prof_av_wheat",  common_range)
canola_c <- interp_to_common(canola_sl, "prof_av_canola", common_range)
potato_c <- interp_to_common(potato_sl, "prof_av_potato", common_range)

weighted_sl <- wheat_c %>%
  inner_join(canola_c, by = c("year", "Site_ID", "Max_Irrigation_mm")) %>%
  inner_join(potato_c, by = c("year", "Site_ID", "Max_Irrigation_mm")) %>%
  left_join(WEIGHTS, by = "year") %>%
  mutate(prof_av_wtd = w_wheat * prof_av_wheat +
                       w_canola * prof_av_canola +
                       w_potato * prof_av_potato)

# Filter Inf/NaN at site level before averaging (irrq_m3=0 at low quotas for some sites)
# This lets the x-axis start at 50mm matching potato panel C
weighted_agg <- weighted_sl %>%
  filter(is.finite(prof_av_wtd)) %>%
  group_by(year, Max_Irrigation_mm) %>%
  summarise(prof_av_wtd = mean(prof_av_wtd, na.rm = TRUE), .groups = "drop")

p4 <- make_mv_plot(weighted_agg, "prof_av_wtd",
                   x_lim = c(10, 200), y_lim = c(0, 0.65),
                   y_breaks = seq(0, 0.65, 0.1), span = 0.75)

# ── combined panel ────────────────────────────────────────────────────────────

p_combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(tag_levels = "A")
ggsave("./Dissertation_Latex_Project/Figures2/MV_value_all_in_one.png",
       plot = p_combined, width = 18, height = 14, dpi = 300)

# ── AV at 150 mm quota (for manuscript text) ──────────────────────────────────

cat("\n=== Average Value at 150 mm quota (pooled across years) ===\n")
av_at_quota <- function(df, col, quota = 150) {
  vals <- df %>%
    group_by(Max_Irrigation_mm) %>%
    summarise(av = mean(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(Max_Irrigation_mm)
  # exact match or linear interpolation between bracketing levels
  if (quota %in% vals$Max_Irrigation_mm) {
    vals$av[vals$Max_Irrigation_mm == quota]
  } else {
    approx(vals$Max_Irrigation_mm, vals$av, xout = quota)$y
  }
}
cat(sprintf("  Wheat  @ 150 mm: $%.2f/m3\n", av_at_quota(wheat,  "prof_av_wheat")))
cat(sprintf("  Canola @ 150 mm: $%.2f/m3\n", av_at_quota(canola, "prof_av_canola")))
cat(sprintf("  Potato @ 150 mm: $%.2f/m3 (interpolated)\n", av_at_quota(potato, "prof_av_potato")))

cat("\n=== AV at each level (wheat, mean across years) ===\n")
wheat %>%
  group_by(Max_Irrigation_mm) %>%
  summarise(av = mean(prof_av_wheat, na.rm = TRUE), .groups = "drop") %>%
  arrange(Max_Irrigation_mm) %>%
  print(n = 20)

cat("\n=== AV at each level (canola, mean across years) ===\n")
canola %>%
  group_by(Max_Irrigation_mm) %>%
  summarise(av = mean(prof_av_canola, na.rm = TRUE), .groups = "drop") %>%
  arrange(Max_Irrigation_mm) %>%
  print(n = 20)

cat("\n=== AV at each level (potato, mean across years) ===\n")
potato %>%
  group_by(Max_Irrigation_mm) %>%
  summarise(av = mean(prof_av_potato, na.rm = TRUE), .groups = "drop") %>%
  arrange(Max_Irrigation_mm) %>%
  print(n = 20)
