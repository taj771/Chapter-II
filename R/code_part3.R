# Figure 3: Average shadow price of irrigation water ($/m3) from 2018 to 2023.
# Wheat and canola: (irrigated profit - rainfed profit) / irrigation volume
# Potato: irrigated profit / irrigation volume (no rainfed potato simulations available)

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

read_years <- function(pattern, years = YEARS) {
  map_dfr(years, ~read_csv(glue(pattern, year = .x), show_col_types = FALSE))
}

inflate_cols <- function(df, cols, country = "CA", to = 2023) {
  yrs <- df$year
  for (col in cols) df[[col]] <- adjust_for_inflation(df[[col]], yrs, country, to_date = to)
  df
}

box_theme <- function() {
  theme_minimal() %+replace% theme(
    legend.position  = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "black"),
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 12),
    axis.ticks       = element_line(linewidth = 0.8),
    plot.title       = element_text(hjust = 0.5, face = "bold")
  )
}

# ── crop budgets ──────────────────────────────────────────────────────────────

budget_grain <- read.csv(glue("{BASE}/CropReturnDarkBrown.csv")) %>%
  inflate_cols(c("dry_cost_ac", "irri_cost_fix_ac", "irri_cost_var_ac", "price.bu")) %>%
  group_by(crop) %>%
  summarise(across(c(dry_cost_ac, irri_cost_fix_ac, irri_cost_var_ac, price.bu), mean),
            .groups = "drop")

budget_potato <- read.csv(glue("{BASE}/CropReturnPotato.csv")) %>%
  inflate_cols(c("irri_cost_fix_ac", "irri_cost_var_ac", "price.ton")) %>%
  summarise(across(c(irri_cost_fix_ac, irri_cost_var_ac, price.ton), mean))

# ── wheat shadow price ────────────────────────────────────────────────────────

wheat_rf <- read_years("{BASE}/wheat_rainfed_{year}.csv") %>%
  mutate(year = year(`Harvest Date (YYYY/MM/DD)`),
         yield_rain_bu_ac = `Dry yield (tonne/ha)` * 36.74 / 2.47) %>%
  select(year, Site, yield_rain_bu_ac)

wheat_ir <- read_years("{BASE}/wheat_netirridemand_{year}.csv") %>%
  mutate(year = year(`Harvest Date (YYYY/MM/DD)`),
         yield_ir_bu_ac = `Dry yield (tonne/ha)` * 36.74 / 2.47,
         irrq_m3 = 4046.86 * `Seasonal irrigation (mm)` * 0.001) %>%
  select(year, Site, yield_ir_bu_ac, irrq_m3)

bw <- filter(budget_grain, crop == "wheat")

wheat <- left_join(wheat_rf, wheat_ir, by = c("year", "Site")) %>%
  mutate(irr_level_in  = irrq_m3 / (0.001 * 4046.86) * 0.03937,
         irri_cost_ac  = bw$irri_cost_var_ac * irr_level_in + bw$irri_cost_fix_ac,
         profit_rf     = yield_rain_bu_ac * bw$price.bu - bw$dry_cost_ac,
         profit_ir     = yield_ir_bu_ac   * bw$price.bu - irri_cost_ac,
         prof_val_mm   = (profit_ir - profit_rf) / irrq_m3) %>%
  select(year, Site, prof_val_mm)

# ── canola shadow price ───────────────────────────────────────────────────────

canola_rf <- read_years("{BASE}/canola_rainfed_{year}.csv") %>%
  mutate(year = year(`Harvest Date (YYYY/MM/DD)`),
         yield_rain_bu_ac = `Dry yield (tonne/ha)` * 44.09 / 2.47) %>%
  select(year, Site, yield_rain_bu_ac)

canola_ir <- read_years("{BASE}/canola_netirridemand_{year}.csv") %>%
  mutate(year = year(`Harvest Date (YYYY/MM/DD)`),
         yield_ir_bu_ac = `Dry yield (tonne/ha)` * 44.09 / 2.47,
         irrq_m3 = 4046.86 * `Seasonal irrigation (mm)` * 0.001) %>%
  select(year, Site, yield_ir_bu_ac, irrq_m3)

bc <- filter(budget_grain, crop == "canola")

canola <- left_join(canola_rf, canola_ir, by = c("year", "Site")) %>%
  mutate(irr_level_in  = irrq_m3 / (0.001 * 4046.86) * 0.03937,
         irri_cost_ac  = bc$irri_cost_var_ac * irr_level_in + bc$irri_cost_fix_ac,
         profit_rf     = yield_rain_bu_ac * bc$price.bu - bc$dry_cost_ac,
         profit_ir     = yield_ir_bu_ac   * bc$price.bu - irri_cost_ac,
         prof_val_mm   = (profit_ir - profit_rf) / irrq_m3) %>%
  select(year, Site, prof_val_mm)

# ── potato shadow price (no rainfed baseline) ─────────────────────────────────

potato <- read_years("{BASE}/potato_netirridemand_{year}.csv") %>%
  mutate(year = year(`Harvest Date (YYYY/MM/DD)`),
         yield_ton_ac = `Fresh yield (tonne/ha)` / 2.47,
         irrq_m3      = 4046.86 * `Seasonal irrigation (mm)` * 0.001) %>%
  mutate(irr_level_in  = irrq_m3 / (0.001 * 4046.86) * 0.03937,
         irri_cost_ac  = budget_potato$irri_cost_var_ac * irr_level_in + budget_potato$irri_cost_fix_ac,
         profit_ir     = yield_ton_ac * budget_potato$price.ton - irri_cost_ac,
         prof_val_mm   = profit_ir / irrq_m3) %>%
  select(year, Site, prof_val_mm)

# ── area-weighted average ─────────────────────────────────────────────────────

weights <- tibble(
  year    = YEARS,
  wheat   = c(24.4, 31.2, 31.7, 34.2, 33.7, 35.4),
  canola  = c(30.0, 22.1, 28.9, 32.0, 31.8, 27.8),
  potato  = c( 5.9,  5.9,  4.1,  4.2,  5.1,  4.3)
) %>%
  mutate(total = wheat + canola + potato,
         w_wheat  = wheat  / total,
         w_canola = canola / total,
         w_potato = potato / total) %>%
  select(year, w_wheat, w_canola, w_potato)

df_all <- wheat  %>% rename(prof_wheat  = prof_val_mm) %>%
  left_join(canola  %>% rename(prof_canola = prof_val_mm), by = c("year", "Site")) %>%
  left_join(potato  %>% rename(prof_potato = prof_val_mm), by = c("year", "Site")) %>%
  left_join(weights, by = "year") %>%
  mutate(prof_weighted = prof_wheat * w_wheat +
                         prof_canola * w_canola +
                         prof_potato * w_potato) %>%
  select(year, Site, prof_wheat, prof_canola, prof_potato, prof_weighted)

# ── plots ─────────────────────────────────────────────────────────────────────

make_panel <- function(data, var, title, color, ylim, ybreaks) {
  data %>%
    filter(variable == var) %>%
    ggplot(aes(x = factor(year), y = value, fill = variable)) +
    geom_boxplot(notch = TRUE, width = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
    scale_fill_manual(values = setNames(color, var)) +
    scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) +
    scale_y_continuous(limits = ylim, breaks = ybreaks) +
    labs(title = title, x = "Year",
         y = expression("Average Value ($m"^{-3}*")")) +
    box_theme()
}

df_long <- df_all %>%
  pivot_longer(-c(year, Site), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable,
    levels = c("prof_wheat", "prof_canola", "prof_potato", "prof_weighted")))

p <- make_panel(df_long, "prof_wheat",    "Wheat",    "springgreen4", c(-1.5, 2),  seq(-1.5, 2,  0.5)) |
     make_panel(df_long, "prof_canola",   "Canola",   "firebrick3",   c(-1.5, 2),  seq(-1.5, 2,  0.5)) /
    (make_panel(df_long, "prof_potato",   "Potato",   "goldenrod1",   c(2, 8),     seq(-2, 8,    0.5)) |
     make_panel(df_long, "prof_weighted", "Weighted", "purple3",      c(-1, 1),    seq(-1, 1,    0.5)))

ggsave("./results/images/AverageValue_Profit_excludePriceCostVariablility.png",
       plot = p, width = 10, height = 7, dpi = 300)
