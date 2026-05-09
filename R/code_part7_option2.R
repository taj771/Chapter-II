# Figure 8: Net economic benefits (%) of flexible allocation under climate change (SSP2-4.5, -20% precip).
# Two scenarios: wheat-canola rotation and wheat-canola-potato rotation.
# Site-level optimization across 139 sites, years 2030–2050.

rm(list = ls())
library(tidyverse)
library(purrr)
library(furrr)
library(progressr)
library(priceR)
library(glue)

BASE  <- "./AquaCropOPSyData"
YEARS <- 2030:2050

# ── crop budgets (2023 values applied to all climate scenario years) ──────────

inflate_cols <- function(df, cols, country = "CA", to = 2023) {
  yrs <- df$year
  for (col in cols) df[[col]] <- adjust_for_inflation(df[[col]], yrs, country, to_date = to)
  df
}

budget_grain <- read.csv(glue("{BASE}/CropReturn/CropReturnDarkBrown.csv")) %>%
  inflate_cols(c("dry_cost_ac", "irri_cost_fix_ac", "irri_cost_var_ac", "price.bu")) %>%
  filter(year == 2023)  # use 2023 prices for all future scenario years

budget_potato <- read.csv(glue("{BASE}/CropReturn/CropReturnPotato.csv")) %>%
  inflate_cols(c("irri_cost_fix_ac", "irri_cost_var_ac", "price.ton")) %>%
  filter(year == 2023)

# ── crop data loading + profit interpolation ──────────────────────────────────

# Interpolates profit at 1mm resolution per site using approx()
interp_profit <- function(df, irr_col, profit_col) {
  df %>%
    arrange(Site_ID, .data[[irr_col]]) %>%
    group_by(Site_ID) %>%
    summarise(
      interp = list(approx(
        x    = .data[[irr_col]],
        y    = .data[[profit_col]],
        xout = seq(min(.data[[irr_col]]), max(.data[[irr_col]]), by = 1)
      )),
      .groups = "drop"
    ) %>%
    unnest_wider(interp) %>%
    unnest(cols = c(x, y)) %>%
    rename(Max_Irrigation_mm = x, profit_ir = y)
}

load_grain_profit <- function(file, yr, bg, conv) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(year         = yr,
           yield_bu_ac  = Yield_tonne_per_ha * conv / 2.47,
           irrq_m3      = 4046.86 * Total_Irrigation_mm * 0.001,
           irr_in       = irrq_m3 / (0.001 * 4046.86) * 0.03937,
           irri_cost_ac = bg$irri_cost_var_ac * irr_in + bg$irri_cost_fix_ac,
           profit_ir    = yield_bu_ac * bg$price.bu - irri_cost_ac) %>%
    select(Site_ID, Max_Irrigation_mm, irrq_m3, profit_ir) %>%
    interp_profit("Max_Irrigation_mm", "profit_ir")
}

load_potato_profit <- function(file, yr, bp) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(year         = yr,
           yield_ton_ac = Yield_tonne_per_ha / 2.47,
           irrq_m3      = 4046.86 * Total_Irrigation_mm * 0.001,
           irr_in       = irrq_m3 / (0.001 * 4046.86) * 0.03937,
           irri_cost_ac = bp$irri_cost_var_ac * irr_in + bp$irri_cost_fix_ac,
           profit_ir    = yield_ton_ac * bp$price.ton - irri_cost_ac) %>%
    select(Site_ID, Max_Irrigation_mm, irrq_m3, profit_ir) %>%
    interp_profit("Max_Irrigation_mm", "profit_ir")
}

# ── two-crop allocation (wheat + canola, 300 mm quota) ───────────────────────

process_two_crop <- function(yr, bw, bc) {
  wheat_sub <- load_grain_profit(
    glue("{BASE}/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp{yr}.csv"), yr, bw, 36.74
  ) %>% rename(wheat_irrigation = Max_Irrigation_mm, wheat_profit_ir = profit_ir)

  canola_sub <- load_grain_profit(
    glue("{BASE}/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp{yr}.csv"), yr, bc, 44.09
  ) %>% rename(canola_irrigation = Max_Irrigation_mm, canola_profit_ir = profit_ir)

  site_ids <- unique(wheat_sub$Site_ID)

  expand.grid(wheat_irrigation = seq(0, 200, 1),
              canola_irrigation = seq(0, 200, 1),
              Site_ID = site_ids) %>%
    filter(wheat_irrigation + canola_irrigation <= 300) %>%
    left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    mutate(Tot_prof_scenario2 = wheat_profit_ir + canola_profit_ir) %>%
    group_by(Site_ID) %>%
    mutate(prof_wheat_150mm  = wheat_profit_ir[wheat_irrigation  == 150][1],
           prof_canola_150mm = canola_profit_ir[canola_irrigation == 150][1]) %>%
    slice_max(Tot_prof_scenario2, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Tot_prof_scenario1    = prof_wheat_150mm + prof_canola_150mm,
           net_benefit_percent   = (Tot_prof_scenario2 - Tot_prof_scenario1) /
                                    abs(Tot_prof_scenario1) * 100,
           net_benefit_percent   = pmin(net_benefit_percent, 100),
           year                  = yr) %>%
    drop_na()
}

# ── three-crop allocation (wheat + canola + potato, 450 mm quota, parallel) ───

process_three_crop_site <- function(site_id, wheat_sub, canola_sub, potato_sub,
                                     wr, cr, pr) {
  result <- expand.grid(wheat_irrigation  = wr,
                        canola_irrigation = cr,
                        potato_irrigation = pr) %>%
    mutate(Site_ID = site_id) %>%
    filter(wheat_irrigation + canola_irrigation + potato_irrigation <= 450) %>%
    left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    left_join(potato_sub, by = c("Site_ID", "potato_irrigation")) %>%
    mutate(Tot_prof_scenario2 = wheat_profit_ir + canola_profit_ir + potato_profit_ir)

  best <- result %>% slice_max(Tot_prof_scenario2, n = 1, with_ties = FALSE)

  s1 <- sum(
    result$wheat_profit_ir[result$wheat_irrigation   == 150][1],
    result$canola_profit_ir[result$canola_irrigation == 150][1],
    result$potato_profit_ir[result$potato_irrigation == 150][1],
    na.rm = TRUE
  )

  best %>%
    mutate(Tot_prof_scenario1  = s1,
           net_benefit_percent = (Tot_prof_scenario2 - s1) / abs(s1) * 100)
}

process_three_crop <- function(yr, bw, bc, bp, site_ids) {
  wheat_sub <- load_grain_profit(
    glue("{BASE}/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp{yr}.csv"), yr, bw, 36.74
  ) %>% rename(wheat_irrigation = Max_Irrigation_mm, wheat_profit_ir = profit_ir)

  canola_sub <- load_grain_profit(
    glue("{BASE}/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp{yr}.csv"), yr, bc, 44.09
  ) %>% rename(canola_irrigation = Max_Irrigation_mm, canola_profit_ir = profit_ir)

  potato_sub <- load_potato_profit(
    glue("{BASE}/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp{yr}.csv"), yr, bp
  ) %>% rename(potato_irrigation = Max_Irrigation_mm, potato_profit_ir = profit_ir)

  wr <- seq(0, 200, 1); cr <- seq(0, 200, 1); pr <- seq(0, 260, 1)

  with_progress({
    p <- progressor(along = site_ids)
    future_map_dfr(site_ids, function(sid) {
      res <- process_three_crop_site(sid, wheat_sub, canola_sub, potato_sub, wr, cr, pr)
      p()
      res
    })
  }) %>%
    drop_na() %>%
    mutate(net_benefit_percent = pmin(net_benefit_percent, 100), year = yr)
}

# ── run 2-crop scenarios (sequential) ────────────────────────────────────────

bw <- filter(budget_grain,  crop == "wheat")
bc <- filter(budget_grain,  crop == "canola")
bp <- budget_potato

df_wheat_canola <- map_dfr(YEARS, process_two_crop, bw, bc)
write_csv(df_wheat_canola, "Data/Processed/NetBenefits_canola_wheat_2030_2050.csv")

# ── run 3-crop scenarios (parallel per site) ──────────────────────────────────

plan(multisession, workers = parallel::detectCores() - 1)
handlers(global = TRUE)
handlers("progress")

site_ids <- sort(unique(df_wheat_canola$Site_ID))

df_wheat_canola_potato <- map_dfr(YEARS, process_three_crop, bw, bc, bp, site_ids)
write_csv(df_wheat_canola_potato,
          "Data/Processed/NetBenefits_wheat_canola_potato_2030_2050.csv")

plan(sequential)

# ── combine and plot ───────────────────────────────────────────────────────────

df_all <- bind_rows(
  df_wheat_canola %>%
    select(Site_ID, year, net_benefit_percent) %>%
    mutate(type = "Crop Rotation: Wheat & Canola"),
  df_wheat_canola_potato %>%
    select(Site_ID, year, net_benefit_percent) %>%
    mutate(type = "Crop Rotation: Wheat, Canola & Potato")
) %>%
  mutate(net_benefit_percent = pmin(net_benefit_percent, 100))

p <- ggplot(df_all, aes(x = type, y = net_benefit_percent, fill = type, color = type)) +
  geom_violin(trim = FALSE, alpha = 0.2, color = "black", width = 0.6) +
  geom_boxplot(width = 0.05, color = "black", alpha = 0.8, outlier.shape = NA) +
  labs(x = "", y = "Net Benefit (%)") +
  scale_fill_manual(
    values = c("Crop Rotation: Wheat & Canola"        = "darkgreen",
               "Crop Rotation: Wheat, Canola & Potato" = "darkred"),
    labels = c("Wheat & Canola", "Wheat, Canola & Potato"),
    name   = "Crop Rotation:"
  ) +
  scale_color_manual(
    values = c("Crop Rotation: Wheat & Canola"        = "darkgreen",
               "Crop Rotation: Wheat, Canola & Potato" = "darkred"),
    labels = c("Wheat & Canola", "Wheat, Canola & Potato"),
    name   = "Crop Rotation:"
  ) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = seq(-100, 100, by = 10), limits = c(-3, 80), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    legend.position  = "bottom",
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    axis.text.y      = element_text(size = 12),
    axis.title.y     = element_text(size = 12),
    legend.text      = element_text(size = 12),
    axis.ticks.x     = element_blank(),
    axis.ticks.y     = element_line(linewidth = 0.8),
    legend.key.size  = unit(0.3, "cm")
  )

ggsave("./results/images/reallocationBenefitsclimatechnage_option2.png",
       plot = p, width = 10, height = 7, dpi = 300)

# ── summary statistics ────────────────────────────────────────────────────────

df_all %>%
  group_by(type) %>%
  summarise(mean_net_benefit = mean(net_benefit_percent, na.rm = TRUE),
            .groups = "drop")
