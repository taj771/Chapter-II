# Appendix: Parametric KKT flexible allocation — quadratic profit functions
# Per site-year OLS fit: π(W) = α + βW + γW²
# Analytical equimarginal solution via KKT first-order conditions
# Robustness check against grid search in Figure 7
# Output: violin+boxplot of net benefit (%) — matches Figure 8 style

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

irri_cost_ac <- function(irrq_m3, var_rate, fix) {
  irr_in <- irrq_m3 / (0.001 * 4046.86) * 0.03937
  var_rate * irr_in + fix
}

quad_profit <- function(alpha, beta, gamma, w) alpha + beta * w + gamma * w^2

# ── crop budgets ──────────────────────────────────────────────────────────────

budget_grain <- read.csv(glue("{BASE}/CropReturnDarkBrown.csv")) %>%
  inflate_cols(c("dry_cost_ac", "irri_cost_fix_ac", "irri_cost_var_ac", "price.bu"))

budget_potato <- read.csv(glue("{BASE}/CropReturnPotato.csv")) %>%
  inflate_cols(c("irri_cost_fix_ac", "irri_cost_var_ac", "price.ton"))

# ── site-level profit loaders ─────────────────────────────────────────────────

load_grain <- function(crop_name, conv, yr, bg) {
  read_csv(
    glue("{BASE}/merged_simulation_results_{crop_name}_marginal_{yr}_irrigation.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(
      year        = yr,
      yield_bu_ac = Yield_tonne_per_ha * conv / 2.47,
      irrq_m3     = 4046.86 * Total_Irrigation_mm * 0.001,
      profit_ir   = yield_bu_ac * bg$price.bu -
                    irri_cost_ac(irrq_m3, bg$irri_cost_var_ac, bg$irri_cost_fix_ac)
    ) %>%
    select(year, Site_ID, Max_Irrigation_mm, profit_ir)
}

load_potato <- function(yr, bg) {
  read_csv(
    glue("{BASE}/merged_simulation_results_potato_marginal_{yr}_irrigation.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(
      year         = yr,
      yield_ton_ac = Yield_tonne_per_ha / 2.47,
      irrq_m3      = 4046.86 * Total_Irrigation_mm * 0.001,
      profit_ir    = yield_ton_ac * bg$price.ton -
                     irri_cost_ac(irrq_m3, bg$irri_cost_var_ac, bg$irri_cost_fix_ac)
    ) %>%
    select(year, Site_ID, Max_Irrigation_mm, profit_ir)
}

# ── quadratic fitting per site-year ──────────────────────────────────────────
# Keep only concave fits (γ < 0) — convex = no interior maximum

fit_quadratic <- function(df) {
  df %>%
    group_by(year, Site_ID) %>%
    summarise(
      fit = list(tryCatch(
        lm(profit_ir ~ Max_Irrigation_mm + I(Max_Irrigation_mm^2), data = cur_data()),
        error = function(e) NULL
      )),
      n_levels = n(),
      .groups = "drop"
    ) %>%
    filter(!map_lgl(fit, is.null), n_levels >= 5) %>%
    mutate(
      alpha = map_dbl(fit, ~ coef(.x)[1]),
      beta  = map_dbl(fit, ~ coef(.x)[2]),
      gamma = map_dbl(fit, ~ coef(.x)[3]),
      r2    = map_dbl(fit, ~ summary(.x)$r.squared)
    ) %>%
    select(year, Site_ID, alpha, beta, gamma, r2) %>%
    filter(gamma < 0, r2 >= 0.70)   # concave + reasonable fit quality
}

# ── KKT: two-crop (W_a + W_b = Q) ────────────────────────────────────────────
# FOC: β_a + 2γ_a·W_a = β_b + 2γ_b·W_b, W_b = Q - W_a
# → W_a* = (β_b − β_a + 2γ_b·Q) / (2(γ_a + γ_b))

kkt_2crop <- function(params_a, params_b, quota, w_max_a, w_max_b) {
  params_a %>%
    inner_join(params_b, by = c("year", "Site_ID"), suffix = c("_a", "_b")) %>%
    mutate(
      denom   = 2 * (gamma_a + gamma_b),           # negative (both γ < 0)
      w_a_raw = (beta_b - beta_a + 2 * gamma_b * quota) / denom,
      w_a     = pmin(pmax(w_a_raw, 0), w_max_a),
      w_b     = pmin(pmax(quota - w_a, 0), w_max_b),
      # recompute w_a if w_b hit corner
      w_a     = quota - w_b,
      w_a     = pmin(pmax(w_a, 0), w_max_a),
      # profit at optimal
      pi_a_s2 = quad_profit(alpha_a, beta_a, gamma_a, w_a),
      pi_b_s2 = quad_profit(alpha_b, beta_b, gamma_b, w_b),
      s2      = pi_a_s2 + pi_b_s2,
      # profit at fixed 150 mm each
      pi_a_s1 = quad_profit(alpha_a, beta_a, gamma_a, 150),
      pi_b_s1 = quad_profit(alpha_b, beta_b, gamma_b, 150),
      s1      = pi_a_s1 + pi_b_s1,
      net_benefit_percent = (s2 - s1) / abs(s1) * 100,
      net_benefit_percent = pmin(net_benefit_percent, 100)
    ) %>%
    select(year, Site_ID, w_a, w_b, s1, s2, net_benefit_percent) %>%
    drop_na() %>%
    filter(is.finite(net_benefit_percent))
}

# ── KKT: three-crop (W_w + W_c + W_p = Q) ───────────────────────────────────
# λ* = [Q + Σ(β_k / 2γ_k)] / Σ(1 / 2γ_k)
# W_k* = (λ* − β_k) / (2γ_k)
# Corner check: if W_k* < 0, fix W_k* = 0, re-solve over remaining two crops

kkt_3crop <- function(params_w, params_c, params_p, quota,
                      w_max_w = 200, w_max_c = 200, w_max_p = 260) {
  params_w %>%
    inner_join(params_c, by = c("year", "Site_ID"), suffix = c("_w", "_c")) %>%
    inner_join(params_p, by = c("year", "Site_ID")) %>%
    rename(alpha_p = alpha, beta_p = beta, gamma_p = gamma, r2_p = r2) %>%
    mutate(
      # unconstrained λ*
      inv_w  = 1 / (2 * gamma_w),
      inv_c  = 1 / (2 * gamma_c),
      inv_p  = 1 / (2 * gamma_p),
      lambda = (quota + beta_w * inv_w + beta_c * inv_c + beta_p * inv_p) /
               (inv_w + inv_c + inv_p),
      w_w    = (lambda - beta_w) / (2 * gamma_w),
      w_c    = (lambda - beta_c) / (2 * gamma_c),
      w_p    = (lambda - beta_p) / (2 * gamma_p),
      # apply box constraints
      w_w    = pmin(pmax(w_w, 0), w_max_w),
      w_c    = pmin(pmax(w_c, 0), w_max_c),
      w_p    = pmin(pmax(w_p, 0), w_max_p),
      # profit at optimal
      pi_w_s2 = quad_profit(alpha_w, beta_w, gamma_w, w_w),
      pi_c_s2 = quad_profit(alpha_c, beta_c, gamma_c, w_c),
      pi_p_s2 = quad_profit(alpha_p, beta_p, gamma_p, w_p),
      s2      = pi_w_s2 + pi_c_s2 + pi_p_s2,
      # profit at fixed 150 mm each
      pi_w_s1 = quad_profit(alpha_w, beta_w, gamma_w, 150),
      pi_c_s1 = quad_profit(alpha_c, beta_c, gamma_c, 150),
      pi_p_s1 = quad_profit(alpha_p, beta_p, gamma_p, 150),
      s1      = pi_w_s1 + pi_c_s1 + pi_p_s1,
      net_benefit_percent = (s2 - s1) / abs(s1) * 100,
      net_benefit_percent = pmin(net_benefit_percent, 100)
    ) %>%
    select(year, Site_ID, w_w, w_c, w_p, lambda, s1, s2, net_benefit_percent) %>%
    drop_na() %>%
    filter(is.finite(net_benefit_percent))
}

# ── load and fit all crops ────────────────────────────────────────────────────

bw <- filter(budget_grain, crop == "wheat")
bc <- filter(budget_grain, crop == "canola")
bp <- budget_potato

cat("Loading simulation data...\n")

wheat_all  <- map_dfr(YEARS, ~ load_grain("wheat",  36.74, .x, filter(bw, year == .x)))
canola_all <- map_dfr(YEARS, ~ load_grain("canola", 44.09, .x, filter(bc, year == .x)))
potato_all <- map_dfr(YEARS, ~ load_potato(.x, filter(bp, year == .x)))

cat("Fitting quadratics...\n")

params_w <- fit_quadratic(wheat_all)
params_c <- fit_quadratic(canola_all)
params_p <- fit_quadratic(potato_all)

cat(sprintf("Concave fits retained: wheat=%d, canola=%d, potato=%d\n",
            nrow(params_w), nrow(params_c), nrow(params_p)))

# ── shadow price summary ──────────────────────────────────────────────────────
# λ at unconstrained optimum (β + 2γW = 0 → W_opt = -β/(2γ))
# Report mean λ = -β²/(4γ) relative to zero (if W_opt > irrigation range, λ ≈ 0)

shadow_price_summary <- function(params, crop_label, w_min, w_max) {
  params %>%
    mutate(
      w_opt  = -beta / (2 * gamma),
      lambda = ifelse(w_opt > w_max, beta + 2 * gamma * w_max,
               ifelse(w_opt < w_min, beta + 2 * gamma * w_min,
                      0))  # at interior max, λ = 0 (unconstrained)
    ) %>%
    summarise(
      crop        = crop_label,
      mean_lambda = mean(beta + 2 * gamma * 150, na.rm = TRUE),   # MV at 150mm
      .groups     = "drop"
    )
}

cat("\n=== Shadow price (MV at 150 mm) from quadratic fit ===\n")
bind_rows(
  shadow_price_summary(params_w, "wheat",  10, 200),
  shadow_price_summary(params_c, "canola", 10, 200),
  shadow_price_summary(params_p, "potato", 10, 260)
) %>% print()

# ── run KKT ──────────────────────────────────────────────────────────────────

cat("\nSolving KKT allocations...\n")

df_2crop <- kkt_2crop(params_w, params_c, quota = 300, w_max_a = 200, w_max_b = 200)
df_3crop <- kkt_3crop(params_w, params_c, params_p, quota = 450)

cat(sprintf("Site-year pairs: 2-crop=%d, 3-crop=%d\n", nrow(df_2crop), nrow(df_3crop)))

# ── optimal allocation summary ────────────────────────────────────────────────

cat("\n=== Mean optimal allocation (mm) ===\n")
cat(sprintf("  2-crop: W_wheat=%.1f, W_canola=%.1f (quota=300mm)\n",
            mean(df_2crop$w_a), mean(df_2crop$w_b)))
cat(sprintf("  3-crop: W_wheat=%.1f, W_canola=%.1f, W_potato=%.1f (quota=450mm)\n",
            mean(df_3crop$w_w), mean(df_3crop$w_c), mean(df_3crop$w_p)))

# ── net benefit summary ───────────────────────────────────────────────────────

cat("\n=== Net benefit (%) from KKT flexible allocation ===\n")
bind_rows(
  df_2crop %>% mutate(type = "Wheat & Canola"),
  df_3crop %>% mutate(type = "Wheat, Canola & Potato")
) %>%
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

dir.create("./results/KKT", showWarnings = FALSE, recursive = TRUE)
write_csv(df_2crop, "./results/KKT/KKT_wheat_canola_2018_2023.csv")
write_csv(df_3crop, "./results/KKT/KKT_wheat_canola_potato_2018_2023.csv")

# ── plot — violin + boxplot, matching Figure 8 style ─────────────────────────

df_plot <- bind_rows(
  df_2crop %>% mutate(type = "Crop Rotation: Wheat & Canola"),
  df_3crop %>% mutate(type = "Crop Rotation: Wheat, Canola & Potato")
) %>%
  mutate(net_benefit_percent = pmin(net_benefit_percent, 100))

p <- ggplot(df_plot, aes(x = type, y = net_benefit_percent, fill = type, color = type)) +
  geom_violin(trim = FALSE, alpha = 0.2, color = "black", width = 0.6) +
  geom_boxplot(width = 0.05, color = "black", alpha = 0.8, outlier.shape = NA) +
  labs(x = "", y = "Net Benefit (%)") +
  scale_fill_manual(
    values = c("Crop Rotation: Wheat & Canola"         = "darkgreen",
               "Crop Rotation: Wheat, Canola & Potato" = "darkred"),
    labels = c("Wheat & Canola", "Wheat, Canola & Potato"),
    name   = "Crop Rotation:"
  ) +
  scale_color_manual(
    values = c("Crop Rotation: Wheat & Canola"         = "darkgreen",
               "Crop Rotation: Wheat, Canola & Potato" = "darkred"),
    labels = c("Wheat & Canola", "Wheat, Canola & Potato"),
    name   = "Crop Rotation:"
  ) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = seq(-100, 100, by = 10),
                     limits = c(-3, 80), expand = c(0, 0)) +
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

ggsave("./Dissertation_Latex_Project/Figures2/KKT_parametric_benefits.png",
       plot = p, width = 10, height = 7, dpi = 300)

cat("Saved: Dissertation_Latex_Project/Figures2/KKT_parametric_benefits.png\n")

# ── comparison table: KKT vs grid search ─────────────────────────────────────
# For manuscript: report both side-by-side in appendix

cat("\n=== KKT vs grid search comparison ===\n")
cat("Grid search (Figure 7): wheat-canola ~5%, wheat-canola-potato ~14%\n")
cat("KKT parametric:\n")
df_plot %>%
  group_by(type) %>%
  summarise(mean_nb = mean(net_benefit_percent, na.rm = TRUE), .groups = "drop") %>%
  print()
