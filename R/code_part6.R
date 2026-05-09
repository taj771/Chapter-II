# Figure 7: Net economic benefits (%) of flexible vs. fixed (150 mm) allocation.

rm(list = ls())
library(tidyverse)
library(purrr)
library(glue)
library(priceR)
library(lubridate)
library(xtable)

BASE  <- "./AquaCropOPSyData"
YEARS <- 2018:2023

# ── crop budgets ──────────────────────────────────────────────────────────────

inflate_cols <- function(df, cols, country = "CA", to = 2023) {
  yrs <- df$year
  for (col in cols) df[[col]] <- adjust_for_inflation(df[[col]], yrs, country, to_date = to)
  df
}

budget_grain <- read.csv(glue("{BASE}/CropReturn/CropReturnDarkBrown.csv")) %>%
  inflate_cols(c("dry_cost_ac", "irri_cost_fix_ac", "irri_cost_var_ac", "price.bu"))

budget_potato <- read.csv(glue("{BASE}/CropReturn/CropReturnPotato.csv")) %>%
  inflate_cols(c("irri_cost_fix_ac", "irri_cost_var_ac", "price.ton")) %>%
  summarise(across(c(irri_cost_fix_ac, irri_cost_var_ac, price.ton), mean))

# ── helpers ───────────────────────────────────────────────────────────────────

irri_cost <- function(irrq_m3, var_rate, fix) {
  irr_in <- irrq_m3 / (0.001 * 4046.86) * 0.03937
  var_rate * irr_in + fix
}

# Site-averaged profit by irrigation level (for allocation optimization)
level_profit <- function(df, prof_col) {
  df %>%
    group_by(Max_Irrigation_mm) %>%
    summarise(!!prof_col := mean(profit_ir, na.rm = TRUE), .groups = "drop")
}

fixed_val <- function(mv, col, mm = 150) mv[[col]][mv$Max_Irrigation_mm == mm][1]

# ── per-year MV loaders ───────────────────────────────────────────────────────

load_grain_mv <- function(crop, conv, yr, budget, mdir, rfdir, prof_col) {
  bg <- filter(budget, crop == crop, year == yr)

  ir <- read_csv(
    glue("{BASE}/{mdir}/merged_simulation_results_{crop}_marginal_{yr}_irrigation.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(yield_bu_ac  = Yield_tonne_per_ha * conv / 2.47,
           irrq_m3      = 4046.86 * Total_Irrigation_mm * 0.001,
           profit_ir    = yield_bu_ac * bg$price.bu -
                          irri_cost(irrq_m3, bg$irri_cost_var_ac, bg$irri_cost_fix_ac)) %>%
    select(Site_ID, Max_Irrigation_mm, irrq_m3, profit_ir)

  rf <- read_csv(glue("{BASE}/{rfdir}/{crop}_rainfed_{yr}.csv"), show_col_types = FALSE) %>%
    rename(Site_ID = Site) %>%
    mutate(irrq_m3           = 4046.86 * `Seasonal irrigation (mm)` * 0.001,
           Max_Irrigation_mm = `Seasonal irrigation (mm)`,
           yield_bu_ac       = `Dry yield (tonne/ha)` * conv / 2.47,
           profit_ir         = yield_bu_ac * bg$price.bu -
                               irri_cost(irrq_m3, bg$irri_cost_var_ac, bg$irri_cost_fix_ac)) %>%
    select(Site_ID, Max_Irrigation_mm, irrq_m3, profit_ir)

  bind_rows(ir, rf) %>% level_profit(prof_col)
}

load_potato_mv <- function(yr, bp, prof_col) {
  read_csv(
    glue("{BASE}/PotataoMarginal/merged_simulation_results_Potato_marginal_{yr}_irrigation.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(yield_ton_ac = Yield_tonne_per_ha / 2.47,
           irrq_m3      = 4046.86 * Total_Irrigation_mm * 0.001,
           profit_ir    = yield_ton_ac * bp$price.ton -
                          irri_cost(irrq_m3, bp$irri_cost_var_ac, bp$irri_cost_fix_ac)) %>%
    select(Site_ID, Max_Irrigation_mm, irrq_m3, profit_ir) %>%
    level_profit(prof_col)
}

# ── allocation optimization ───────────────────────────────────────────────────

alloc_2crop <- function(mv_a, mv_b, col_a, col_b, quota, range_a, range_b) {
  best <- expand.grid(a = range_a, b = range_b) %>%
    filter(a + b <= quota) %>%
    left_join(rename(mv_a, a = Max_Irrigation_mm), by = "a") %>%
    left_join(rename(mv_b, b = Max_Irrigation_mm), by = "b") %>%
    mutate(tot = .data[[col_a]] + .data[[col_b]]) %>%
    slice_max(tot, n = 1, with_ties = FALSE)

  list(s1 = fixed_val(mv_a, col_a) + fixed_val(mv_b, col_b),
       s2 = best$tot, a = best$a, b = best$b)
}

alloc_3crop <- function(mv_a, mv_b, mv_c, col_a, col_b, col_c, quota,
                         range_a, range_b, range_c) {
  best <- expand.grid(a = range_a, b = range_b, c = range_c) %>%
    filter(a + b + c <= quota) %>%
    left_join(rename(mv_a, a = Max_Irrigation_mm), by = "a") %>%
    left_join(rename(mv_b, b = Max_Irrigation_mm), by = "b") %>%
    left_join(rename(mv_c, c = Max_Irrigation_mm), by = "c") %>%
    mutate(tot = .data[[col_a]] + .data[[col_b]] + .data[[col_c]]) %>%
    slice_max(tot, n = 1, with_ties = FALSE)

  list(s1 = fixed_val(mv_a, col_a) + fixed_val(mv_b, col_b) + fixed_val(mv_c, col_c),
       s2 = best$tot, a = best$a, b = best$b, c = best$c)
}

# ── per-year wrapper ──────────────────────────────────────────────────────────

compute_year <- function(yr, bw, bc, bp) {
  wheat  <- load_grain_mv("wheat",  36.74, yr, bw, "WheatMarginal",  "WheatRainfed",  "prof_wheat")
  canola <- load_grain_mv("canola", 44.09, yr, bc, "canolaMarginal", "canolaRainfed", "prof_canola")
  potato <- load_potato_mv(yr, bp, "prof_potato")

  wr <- seq(0, 200, 10); cr <- seq(0, 200, 10); pr <- seq(0, 260, 10)

  wc  <- alloc_2crop(wheat, canola, "prof_wheat", "prof_canola",  300, wr, cr)
  wp  <- alloc_2crop(wheat, potato, "prof_wheat", "prof_potato",  300, wr, pr)
  cp  <- alloc_2crop(canola, potato, "prof_canola", "prof_potato", 300, cr, pr)
  wcp <- alloc_3crop(wheat, canola, potato,
                     "prof_wheat", "prof_canola", "prof_potato",  450, wr, cr, pr)

  tibble(
    type               = c("wheat-Canola", "wheat-potato", "canola-potato", "wheat-canola-potato"),
    year               = yr,
    wheat_irrigation   = c(wc$a,  wp$a,  0L,    wcp$a),
    canola_irrigation  = c(wc$b,  0L,    cp$a,  wcp$b),
    potato_irrigation  = c(0L,    wp$b,  cp$b,  wcp$c),
    Tot_prof_scenario1 = c(wc$s1, wp$s1, cp$s1, wcp$s1),
    Tot_prof_scenario2 = c(wc$s2, wp$s2, cp$s2, wcp$s2),
    net_benefit        = Tot_prof_scenario2 - Tot_prof_scenario1
  )
}

# ── run all years ─────────────────────────────────────────────────────────────

bw <- filter(budget_grain, crop == "wheat")
bc <- filter(budget_grain, crop == "canola")
bp <- budget_potato

df_all <- map_dfr(YEARS, compute_year, bw, bc, bp) %>%
  # Zero out benefit when optimal allocation doesn't exceed fixed-quota level
  mutate(net_benefit = case_when(
    type == "wheat-Canola"        & wheat_irrigation  <= 149 & canola_irrigation <= 149 ~ 0,
    type == "wheat-potato"        & wheat_irrigation  <= 149 & potato_irrigation <= 149 ~ 0,
    type == "canola-potato"       & canola_irrigation <= 149 & potato_irrigation <= 149 ~ 0,
    type == "wheat-canola-potato" & wheat_irrigation  <= 149 &
                                    canola_irrigation <= 149 &
                                    potato_irrigation <= 149 ~ 0,
    TRUE ~ net_benefit
  )) %>%
  select(year, type, Tot_prof_scenario1, Tot_prof_scenario2, net_benefit) %>%
  arrange(year) %>%
  mutate(ner_benefit_per = net_benefit / Tot_prof_scenario2 * 100) %>%
  filter(type %in% c("wheat-Canola", "wheat-canola-potato"))

# ── LaTeX tables ──────────────────────────────────────────────────────────────

sink("./results/Tables/Table_1_to_appendix.tex")
print(xtable(df_all))
sink()

df_all_average <- df_all %>%
  group_by(type) %>%
  summarise(across(c(Tot_prof_scenario1, Tot_prof_scenario2, net_benefit, ner_benefit_per), mean),
            .groups = "drop")

sink("./results/Tables/Table_1.tex")
print(xtable(df_all_average))
sink()

# ── plot ──────────────────────────────────────────────────────────────────────

df1 <- df_all %>%
  mutate(type = recode(type,
    "wheat-Canola"         = "Crop Rotation: Wheat & Canola",
    "wheat-canola-potato"  = "Crop Rotation: Wheat, Canola & Potato"
  ))

p <- ggplot(df1, aes(x = factor(year), y = ner_benefit_per)) +
  geom_bar(stat = "identity", fill = "maroon", width = 0.3) +
  facet_wrap(~ type, nrow = 1, scales = "fixed") +
  scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = c(0, 0)) +
  labs(x = "Year", y = "Net Economic Benefits (%)") +
  theme_minimal() +
  theme(
    strip.text        = element_text(size = 12, face = "bold"),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    axis.line         = element_line(color = "black"),
    axis.title        = element_text(size = 12),
    axis.text         = element_text(size = 12),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.ticks        = element_line(linewidth = 0.8),
    panel.border      = element_rect(color = "lightgrey", fill = NA, linewidth = 0.8)
  )

ggsave("./results/images/reallocationBenefits.png", plot = p, width = 10, height = 7, dpi = 300)
