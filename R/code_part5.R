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

compute_mv <- function(df, mv_col) {
  df %>%
    group_by(year, Max_Irrigation_mm) %>%
    summarise(return_ir  = mean(return_ir),
              profit_ir  = mean(profit_ir),
              irrq_m3    = mean(irrq_m3),
              .groups = "drop") %>%
    arrange(year, Max_Irrigation_mm) %>%
    mutate(prof_incre  = profit_ir - lag(profit_ir),
           irrq_incre  = irrq_m3   - lag(irrq_m3),
           !!mv_col   := prof_incre / irrq_incre)
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

make_mv_plot <- function(data, y_col, x_lim, y_lim, y_breaks) {
  ggplot(data, aes(x = Max_Irrigation_mm, y = .data[[y_col]])) +
    geom_smooth(aes(color = "Overall"), method = "loess", se = TRUE, linewidth = 0.8) +
    geom_segment(aes(x = x_lim[1], xend = x_lim[2], y = 0, yend = 0),
                 linetype = "dashed", color = "grey", linewidth = 0.5) +
    scale_color_manual(values = c("Overall" = "red4")) +
    scale_x_continuous(name = "Irrigation Level (mm)",
                       breaks = seq(x_lim[1], x_lim[2], by = 10),
                       limits = x_lim,
                       expand = expansion(mult = c(0, 0.05))) +
    scale_y_continuous(name = expression("Marginal Value ($m"^{-3}*")"),
                       breaks = y_breaks, limits = y_lim) +
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

wheat <- compute_mv(bind_rows(wheat_ir, wheat_rf), "prof_mv_wheat")

p1 <- make_mv_plot(wheat, "prof_mv_wheat",
                   x_lim = c(10, 200), y_lim = c(-0.3, 1.2),
                   y_breaks = seq(-1, 1.4, 0.2))
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

canola <- compute_mv(bind_rows(canola_ir, canola_rf), "prof_mv_canola")

p2 <- make_mv_plot(canola, "prof_mv_canola",
                   x_lim = c(10, 200), y_lim = c(-0.4, 1.4),
                   y_breaks = seq(-1, 1.4, 0.2))
ggsave("./results/images/MV_canola_2018.png", p2, width = 10, height = 7, dpi = 100)

# ── potato ────────────────────────────────────────────────────────────────────

potato <- read_marginal("Potato") %>%
  mutate(yield_ton_ac = Yield_tonne_per_ha / 2.47,
         irrq_m3      = 4046.86 * Total_Irrigation_mm * 0.001) %>%
  select(year, Site_ID, Max_Irrigation_mm, yield_ton_ac, irrq_m3) %>%
  left_join(budget_potato, by = "year") %>%
  mutate(irr_in       = irrq_m3 / (0.001 * 4046.86) * 0.03937,
         irri_cost_ac = irri_cost_var_ac * irr_in + irri_cost_fix_ac,
         return_ir    = yield_ton_ac * price.ton,
         profit_ir    = return_ir - irri_cost_ac) %>%
  compute_mv("prof_mv_potato")

p3 <- make_mv_plot(potato, "prof_mv_potato",
                   x_lim = c(10, 260), y_lim = c(-1, 10),
                   y_breaks = seq(-1, 24, 1))
ggsave("./results/images/MV_potato_2018.png", p3, width = 10, height = 7, dpi = 100)

# ── combined panel ────────────────────────────────────────────────────────────

p1 + p2 + p3 +
  plot_layout(ncol = 1, heights = c(1, 1, 1)) +
  plot_annotation(tag_levels = "A")
