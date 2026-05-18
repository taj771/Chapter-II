# Figure 7: Net economic benefits (%) of flexible vs. fixed (150 mm) allocation.
# Site-level grid search with 1 mm interpolation (matches Figure 8 methodology).
# Violin + boxplot shows spatial variation across ERA5 grid cells (sites).
# Variable-cost-only profit: fixed costs cancel in (S2-S1); using them in |S1|
# creates near-zero denominator in near-breakeven years.

rm(list = ls())
library(tidyverse)
library(purrr)
library(glue)
library(priceR)
library(lubridate)


BASE  <- "./Data Main Analysis"
YEARS <- 2018:2023

# ── helpers ───────────────────────────────────────────────────────────────────

inflate_cols <- function(df, cols, country = "CA", to = 2023) {
  yrs <- df$year
  for (col in cols) df[[col]] <- adjust_for_inflation(df[[col]], yrs, country, to_date = to)
  df
}

# Linear interpolation to 1 mm resolution per site
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

# ── crop budgets ──────────────────────────────────────────────────────────────

budget_grain <- read.csv(glue("{BASE}/CropReturnDarkBrown.csv")) %>%
  inflate_cols(c("dry_cost_ac", "irri_cost_fix_ac", "irri_cost_var_ac", "price.bu"))

budget_potato <- read.csv(glue("{BASE}/CropReturnPotato.csv")) %>%
  inflate_cols(c("irri_cost_fix_ac", "irri_cost_var_ac", "price.ton")) %>%
  summarise(across(c(irri_cost_fix_ac, irri_cost_var_ac, price.ton), mean))

# ── per-site profit loaders (variable cost only) ──────────────────────────────

load_grain_site <- function(crop_name, conv, yr, bg) {
  read_csv(
    glue("{BASE}/merged_simulation_results_{crop_name}_marginal_{yr}_irrigation.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(
      irr_in    = Total_Irrigation_mm * 0.03937,
      profit_ir = Yield_tonne_per_ha * conv / 2.47 * bg$price.bu -
                  bg$irri_cost_var_ac * irr_in
    ) %>%
    select(Site_ID, Max_Irrigation_mm, profit_ir) %>%
    interp_profit("Max_Irrigation_mm", "profit_ir")
}

load_potato_site <- function(yr, bp) {
  read_csv(
    glue("{BASE}/merged_simulation_results_potato_marginal_{yr}_irrigation.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(
      irr_in    = Total_Irrigation_mm * 0.03937,
      profit_ir = Yield_tonne_per_ha / 2.47 * bp$price.ton -
                  bp$irri_cost_var_ac * irr_in
    ) %>%
    select(Site_ID, Max_Irrigation_mm, profit_ir) %>%
    interp_profit("Max_Irrigation_mm", "profit_ir")
}

# ── two-crop site-level allocation ────────────────────────────────────────────

process_two_crop <- function(yr, bw, bc) {
  wheat_sub  <- load_grain_site("wheat",  36.74, yr, filter(bw, year == yr)) %>%
    rename(wheat_irrigation = Max_Irrigation_mm, wheat_profit = profit_ir)
  canola_sub <- load_grain_site("canola", 44.09, yr, filter(bc, year == yr)) %>%
    rename(canola_irrigation = Max_Irrigation_mm, canola_profit = profit_ir)

  site_ids <- intersect(unique(wheat_sub$Site_ID), unique(canola_sub$Site_ID))

  # Vectorised grid search across all sites simultaneously
  result <- expand.grid(
    wheat_irrigation  = seq(10, 190, 1),
    canola_irrigation = seq(10, 190, 1),
    Site_ID           = site_ids
  ) %>%
    filter(wheat_irrigation + canola_irrigation <= 300) %>%
    left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    mutate(total_profit = wheat_profit + canola_profit) %>%
    drop_na(total_profit) %>%
    group_by(Site_ID) %>%
    slice_max(total_profit, n = 1, with_ties = FALSE) %>%
    ungroup()

  # S1: profit at fixed 150 mm per crop per site
  s1_wheat  <- wheat_sub  %>% filter(wheat_irrigation  == 150) %>%
    select(Site_ID, s1_wheat  = wheat_profit)
  s1_canola <- canola_sub %>% filter(canola_irrigation == 150) %>%
    select(Site_ID, s1_canola = canola_profit)

  result %>%
    left_join(s1_wheat,  by = "Site_ID") %>%
    left_join(s1_canola, by = "Site_ID") %>%
    mutate(
      s1                  = s1_wheat + s1_canola,
      net_benefit         = total_profit - s1,
      net_benefit_percent = (total_profit - s1) / abs(s1) * 100,
      net_benefit_percent = pmin(net_benefit_percent, 100),
      year                = yr,
      type                = "Crop Rotation: Wheat & Canola"
    ) %>%
    select(Site_ID, year, type, wheat_irrigation, canola_irrigation,
           s1, s2 = total_profit, net_benefit, net_benefit_percent)
}

# ── three-crop site-level allocation ─────────────────────────────────────────
# 5 mm steps to keep grid manageable (~37×37×49 per site, filtered to ≤ 450 mm)

process_three_crop_site <- function(sid, wheat_sub, canola_sub, potato_sub) {
  w <- filter(wheat_sub,  Site_ID == sid)
  c <- filter(canola_sub, Site_ID == sid)
  p <- filter(potato_sub, Site_ID == sid)

  if (nrow(w) == 0 || nrow(c) == 0 || nrow(p) == 0) return(NULL)

  result <- expand.grid(
    wheat_irrigation  = seq(10, 190, 5),
    canola_irrigation = seq(10, 190, 5),
    potato_irrigation = seq(10, 250, 5)
  ) %>%
    filter(wheat_irrigation + canola_irrigation + potato_irrigation <= 450) %>%
    left_join(w %>% rename(wheat_irrigation  = Max_Irrigation_mm,
                           wheat_profit      = profit_ir),
              by = "wheat_irrigation") %>%
    left_join(c %>% rename(canola_irrigation = Max_Irrigation_mm,
                           canola_profit     = profit_ir),
              by = "canola_irrigation") %>%
    left_join(p %>% rename(potato_irrigation = Max_Irrigation_mm,
                           potato_profit     = profit_ir),
              by = "potato_irrigation") %>%
    mutate(total_profit = wheat_profit + canola_profit + potato_profit) %>%
    drop_na(total_profit)

  if (nrow(result) == 0) return(NULL)
  best <- slice_max(result, total_profit, n = 1, with_ties = FALSE)

  s1_w <- approx(w$Max_Irrigation_mm, w$profit_ir, xout = 150)$y
  s1_c <- approx(c$Max_Irrigation_mm, c$profit_ir, xout = 150)$y
  s1_p <- approx(p$Max_Irrigation_mm, p$profit_ir, xout = 150)$y
  s1   <- s1_w + s1_c + s1_p

  tibble(
    Site_ID             = sid,
    wheat_irrigation    = best$wheat_irrigation,
    canola_irrigation   = best$canola_irrigation,
    potato_irrigation   = best$potato_irrigation,
    s1                  = s1,
    s2                  = best$total_profit,
    net_benefit         = best$total_profit - s1,
    net_benefit_percent = pmin((best$total_profit - s1) / abs(s1) * 100, 100)
  )
}

process_three_crop <- function(yr, bw, bc, bp) {
  wheat_sub  <- load_grain_site("wheat",  36.74, yr, filter(bw, year == yr))
  canola_sub <- load_grain_site("canola", 44.09, yr, filter(bc, year == yr))
  potato_sub <- load_potato_site(yr, bp)

  site_ids <- Reduce(intersect, list(
    unique(wheat_sub$Site_ID), unique(canola_sub$Site_ID), unique(potato_sub$Site_ID)
  ))

  map_dfr(site_ids, process_three_crop_site,
          wheat_sub, canola_sub, potato_sub) %>%
    mutate(year = yr, type = "Crop Rotation: Wheat, Canola & Potato")
}

# ── run all years ─────────────────────────────────────────────────────────────

bw <- filter(budget_grain, crop == "wheat")
bc <- filter(budget_grain, crop == "canola")
bp <- budget_potato

cat("Running 2-crop site-level allocation (1 mm grid)...\n")
df_2crop <- map_dfr(YEARS, process_two_crop, bw, bc)

cat("Running 3-crop site-level allocation (5 mm grid)...\n")
df_3crop <- map_dfr(YEARS, process_three_crop, bw, bc, bp)

df_all <- bind_rows(
  df_2crop %>% select(Site_ID, year, type, net_benefit_percent),
  df_3crop %>% select(Site_ID, year, type, net_benefit_percent)
) %>%
  mutate(net_benefit_percent = pmin(net_benefit_percent, 100))

# ── summary stats ─────────────────────────────────────────────────────────────

cat("\n=== Net benefit summary (across sites and years) ===\n")
df_all %>%
  group_by(type) %>%
  summarise(
    mean   = mean(net_benefit_percent, na.rm = TRUE),
    median = median(net_benefit_percent, na.rm = TRUE),
    sd     = sd(net_benefit_percent, na.rm = TRUE),
    n      = n(),
    .groups = "drop"
  ) %>%
  print()

# ── save processed data ───────────────────────────────────────────────────────

dir.create("./results/Tables", showWarnings = FALSE, recursive = TRUE)
write_csv(df_2crop, "./results/Tables/reallocation_2crop_site_level.csv")
write_csv(df_3crop, "./results/Tables/reallocation_3crop_site_level.csv")

# ── plot: two-panel facet by crop rotation, violin+boxplot by year ─────────────

facet_labels <- c(
  "Crop Rotation: Wheat & Canola"         = "Wheat & Canola",
  "Crop Rotation: Wheat, Canola & Potato" = "Wheat, Canola & Potato"
)

fill_vals <- c(
  "Crop Rotation: Wheat & Canola"         = "darkgreen",
  "Crop Rotation: Wheat, Canola & Potato" = "darkred"
)

p <- ggplot(df_all, aes(x = factor(year), y = net_benefit_percent, fill = type)) +
  #geom_violin(trim = TRUE, alpha = 0.25, color = "black", width = 0.7) +
  geom_boxplot(width = 0.12, color = "black", alpha = 0.85, outlier.shape = NA) +
  facet_wrap(~ type, nrow = 1, labeller = labeller(type = facet_labels)) +
  scale_fill_manual(values = fill_vals, guide = "none") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Net Benefit (%)") +
  theme_minimal() +
  theme(
    strip.text       = element_text(size = 12, face = "bold"),
    panel.grid       = element_blank(),
    axis.line        = element_line(color = "black"),
    axis.text.x      = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y      = element_text(size = 11),
    axis.title       = element_text(size = 12),
    axis.ticks       = element_line(linewidth = 0.8),
    panel.spacing    = unit(1.5, "lines")
  )

ggsave("./Dissertation_Latex_Project/Figures2/reallocationBenefits_option2.png",
       plot = p, width = 14, height = 7, dpi = 300)

cat("Saved: Dissertation_Latex_Project/Figures2/reallocationBenefits_option2.png\n")
