# Revenue-based mechanism chart: revenue_rigid vs revenue_flex (3-crop, RedPrcp)
# Revenue = yield × price only — no irrigation cost deduction
# Always positive — avoids negative-profit confusion while preserving mechanism

library(tidyverse)
library(priceR)
library(patchwork)
library(here)

ONEDRIVE <- paste0(
  "/Users/tharakajayalath/Library/CloudStorage",
  "/OneDrive-UniversityofSaskatchewan",
  "/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData"
)
BUDGET_GRAIN  <- file.path(ONEDRIVE, "CropReturn/CropReturnDarkBrown.csv")
BUDGET_POTATO <- file.path(ONEDRIVE, "CropReturn/CropReturnPotato.csv")
RESULTS_DIR   <- here::here("CanDCSU6_results")
OUT_DIR       <- here::here("outputs/figures_candscu6")

YEARS <- c(2031, 2044, 2050)
YEAR_LABS <- c(
  "2031" = "Driest\n(2031, P5)",
  "2044" = "Average\n(2044, P50)",
  "2050" = "Wettest\n(2050, P95)"
)

inflate <- function(x, yrs, to = 2023)
  adjust_for_inflation(x, as.Date(paste0(yrs, "-07-01")), "CA",
                       to_date = as.Date(paste0(to, "-07-01")))

# ── Prices (inflated to 2023$) ────────────────────────────────────────────────
bg_grain <- read.csv(BUDGET_GRAIN, fileEncoding = "UTF-8-BOM")
bg_grain$price.bu <- inflate(bg_grain$price.bu, bg_grain$year)
budget_grain <- bg_grain %>%
  group_by(crop) %>%
  summarise(price.bu = mean(price.bu), .groups = "drop")

bw <- filter(budget_grain, crop == "wheat")
bc <- filter(budget_grain, crop == "canola")

bg_potato <- read.csv(BUDGET_POTATO, fileEncoding = "UTF-8-BOM")
bg_potato$price.ton <- inflate(bg_potato$price.ton, bg_potato$year)
bp <- bg_potato %>% summarise(price.ton = mean(price.ton))

cat(sprintf("Prices (2023$): wheat $%.2f/bu, canola $%.2f/bu, potato $%.2f/ton\n",
            bw$price.bu, bc$price.bu, bp$price.ton))

# ── Revenue from yield (per acre) ────────────────────────────────────────────
# wheat/canola: tonne/ha → bu/ac using conversion; potato: tonne/ha → ton/ac
yield_to_rev_grain <- function(yield_t_ha, price_bu, bu_per_tonne)
  (yield_t_ha * bu_per_tonne / 2.47) * price_bu

yield_to_rev_potato <- function(yield_t_ha, price_ton)
  (yield_t_ha / 2.47) * price_ton

# ── Helpers ───────────────────────────────────────────────────────────────────
fn_lookup <- function(fn_list) {
  ids <- map_dbl(fn_list, ~ .x$site)
  setNames(map(fn_list, ~ .x$fn), as.character(ids))
}

build_yield_fns <- function(csv_path) {
  df <- read_csv(csv_path, show_col_types = FALSE)
  df %>%
    group_by(Site_ID) %>%
    group_map(~ list(
      site = .y$Site_ID,
      fn   = approxfun(.x$Max_Irrigation_mm, .x$Yield_tonne_per_ha,
                       method = "linear", rule = 2)
    ))
}

# ── Compute revenues + per-crop breakdown in one loop ─────────────────────────
opt_alloc <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6_3crop.rds")) %>%
  filter(scenario == "RedPrcp") %>%
  mutate(year = as.integer(year))

results <- map_dfr(YEARS, function(yr) {
  cat("Processing year", yr, "...\n")

  w_path <- file.path(RESULTS_DIR, "RedPrcp",
                      sprintf("WheatCanDCSU6_RedPrcp%d.csv", yr))
  c_path <- file.path(RESULTS_DIR, "RedPrcp",
                      sprintf("CanolaCanDCSU6_RedPrcp%d.csv", yr))
  p_path <- file.path(RESULTS_DIR, "RedPrcp",
                      sprintf("PotatoCanDCSU6_RedPrcp%d.csv", yr))

  wf <- fn_lookup(build_yield_fns(w_path))
  cf <- fn_lookup(build_yield_fns(c_path))
  pf <- fn_lookup(build_yield_fns(p_path))

  opt_yr <- opt_alloc %>% filter(year == yr)

  map_dfr(seq_len(nrow(opt_yr)), function(i) {
    row <- opt_yr[i, ]
    sid <- as.character(row$Site_ID)

    if (!sid %in% names(wf)) return(NULL)

    # Rigid: 150mm each crop
    rev_w_rigid <- yield_to_rev_grain(wf[[sid]](150), bw$price.bu, 36.74)
    rev_c_rigid <- yield_to_rev_grain(cf[[sid]](150), bc$price.bu, 44.09)
    rev_p_rigid <- yield_to_rev_potato(pf[[sid]](150), bp$price.ton)
    rev_rigid   <- rev_w_rigid + rev_c_rigid + rev_p_rigid

    # Flex: optimal allocation (interpolated)
    rev_w_flex  <- yield_to_rev_grain(wf[[sid]](row$opt_wheat_mm),  bw$price.bu, 36.74)
    rev_c_flex  <- yield_to_rev_grain(cf[[sid]](row$opt_canola_mm), bc$price.bu, 44.09)
    rev_p_flex  <- yield_to_rev_potato(pf[[sid]](row$opt_potato_mm), bp$price.ton)
    rev_flex    <- rev_w_flex + rev_c_flex + rev_p_flex

    tibble(
      Site_ID     = row$Site_ID,
      year        = yr,
      rev_rigid   = rev_rigid,
      rev_flex    = rev_flex,
      rev_benefit = rev_flex - rev_rigid,
      wheat_ben   = rev_w_flex  - rev_w_rigid,
      canola_ben  = rev_c_flex  - rev_c_rigid,
      potato_ben  = rev_p_flex  - rev_p_rigid
    )
  })
})

results <- results %>%
  mutate(year_lab = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS))

cat("\n=== REVENUE MEDIANS (3-crop, RedPrcp) ===\n")
medians <- results %>%
  group_by(year_lab) %>%
  summarise(
    rigid_med   = median(rev_rigid,   na.rm = TRUE),
    flex_med    = median(rev_flex,    na.rm = TRUE),
    benefit_med = median(rev_benefit, na.rm = TRUE),
    n           = n(),
    .groups     = "drop"
  )
print(medians)

# ── Panel A: Revenue bars ─────────────────────────────────────────────────────
bar_df <- medians %>%
  select(year_lab, rigid_med, flex_med) %>%
  pivot_longer(
    cols      = c(rigid_med, flex_med),
    names_to  = "allocation",
    values_to = "revenue"
  ) %>%
  mutate(
    allocation = factor(
      allocation,
      levels = c("rigid_med", "flex_med"),
      labels = c("Rigid (150 mm per crop)",
                 "Flexible (optimal reallocation)")
    )
  )

alloc_cols <- c(
  "Rigid (150 mm per crop)"       = "#d73027",
  "Flexible (optimal reallocation)" = "#4575b4"
)

benefit_labels <- medians %>%
  mutate(
    label = paste0("+$", round(benefit_med, 0), "/ac")
  )

p_bar <- ggplot(bar_df, aes(x = year_lab, y = revenue, fill = allocation)) +
  geom_col(
    position = position_dodge(0.75), width = 0.65,
    colour = "grey30", linewidth = 0.3
  ) +
  geom_text(
    data = benefit_labels,
    aes(x = year_lab, y = flex_med + 15, label = label),
    inherit.aes = FALSE,
    size = 3.5, colour = "grey20", fontface = "bold"
  ) +
  scale_fill_manual(values = alloc_cols, name = NULL) +
  scale_y_continuous(
    labels = function(x) paste0("$", round(x, 0), "/ac"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "A  Flexible allocation earns more revenue in wetter climate years",
    subtitle = paste0(
      "Revenue gap widens because flex concentrates water on potato\n",
      "when rain already satisfies cereals"
    ),
    x = "Representative climate year (growing-season precipitation quantile)",
    y = "Median gross revenue ($/ac)\n[yield × price, 3-crop total]"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position    = "top",
    legend.text        = element_text(size = 10),
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(size = 9, colour = "grey30"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 10)
  )

# ── Panel B: Per-crop revenue benefit (already computed in main loop) ─────────
crop_med <- results %>%
  mutate(year_lab = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS)) %>%
  group_by(year_lab) %>%
  summarise(
    Wheat  = median(wheat_ben,  na.rm = TRUE),
    Canola = median(canola_ben, na.rm = TRUE),
    Potato = median(potato_ben, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Wheat, Canola, Potato),
               names_to = "crop", values_to = "rev_benefit") %>%
  mutate(crop = factor(crop, levels = c("Wheat", "Canola", "Potato")))

crop_cols <- c(Wheat = "#e8a838", Canola = "#5aae61", Potato = "#9970ab")

p_crop <- ggplot(crop_med, aes(x = year_lab, y = rev_benefit, fill = crop)) +
  geom_col(position = "stack", colour = "grey30", linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_fill_manual(values = crop_cols, name = "Crop") +
  scale_y_continuous(
    labels = function(x) paste0("$", round(x, 0), "/ac"),
    expand = expansion(mult = c(0.1, 0.12))
  ) +
  labs(
    title    = "B  Revenue gain breakdown: potato drives the benefit",
    subtitle = "Flex − rigid revenue, by crop (positive = flex better, negative = flex worse)",
    x        = "Representative climate year",
    y        = "Median revenue gain from\nflexible allocation ($/ac)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position    = "top",
    legend.text        = element_text(size = 10),
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(size = 9, colour = "grey30"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 10)
  )

fig <- p_bar | p_crop
fig <- fig + plot_annotation(
  title    = "Revenue mechanism: flexible allocation concentrates water on potato as cereals become rain-satisfied",
  subtitle = "Three-crop rotation (wheat + canola + potato) | 20% precipitation reduction | 342 Saskatchewan sites, CanDCS-U6 SSP2-4.5",
  theme    = theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey30")
  )
)

ggsave(file.path(OUT_DIR, "fig_mechanism_revenue.png"),
       fig, width = 14, height = 7, dpi = 300)
cat("\nSaved: fig_mechanism_revenue.png\n")

cat("\n=== PER-CROP REVENUE BENEFIT MEDIANS ===\n")
crop_med %>% pivot_wider(names_from = crop, values_from = rev_benefit) %>% print()
