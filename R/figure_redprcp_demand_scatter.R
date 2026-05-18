# Figure: RedPrcp benefit × site water demand
# Panel A: Boxplot — benefit_pct by year (RedPrcp only)
# Panel B: Scatter — x = optimal irrigation demand per site, y = benefit_pct
#           3 climate years as colour/shape to show temporal shift

library(tidyverse)
library(patchwork)
library(here)

OUT_DIR <- here::here("outputs/figures_candscu6")

YEAR_LABS <- c(
  "2031" = "Driest (2031)",
  "2044" = "Average (2044)",
  "2050" = "Wettest (2050)"
)

year_cols <- c(
  "Driest (2031)"  = "#d73027",
  "Average (2044)" = "#f4a582",
  "Wettest (2050)" = "#4575b4"
)

# 2-crop data (RedPrcp only) — for boxplot (Panel A)
crop2_box <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6.rds")) %>%
  filter(scenario == "RedPrcp") %>%
  mutate(year_lab = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS))

# For scatter (Panel B): join profit-based benefit with optimal allocation from theory check
# theory_check_revenue_2031.csv has opt_w_red, opt_c_red for 2031 RedPrcp per site
theory_check <- read_csv(file.path(OUT_DIR, "theory_check_revenue_2031.csv"),
                         show_col_types = FALSE)

crop2_scatter <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6.rds")) %>%
  filter(scenario == "RedPrcp", as.character(year) == "2031") %>%
  left_join(theory_check %>% select(Site_ID, opt_w_red, opt_c_red),
            by = "Site_ID") %>%
  mutate(
    year_lab   = factor(YEAR_LABS["2031"], levels = YEAR_LABS),
    opt_irr_mm = opt_w_red + opt_c_red   # total optimal irrigation under RedPrcp 2031
  ) %>%
  filter(!is.na(opt_irr_mm))

# Panel A: boxplot
pA <- ggplot(crop2_box, aes(x = year_lab, y = benefit_pct, fill = year_lab)) +
  geom_boxplot(
    width = 0.55, outlier.size = 0.5, outlier.alpha = 0.4, linewidth = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  scale_fill_manual(values = year_cols) +
  labs(
    title = "A  Aggregate benefit by climate year",
    x     = "Representative climate year",
    y     = "Benefit of flexible allocation\n(% of rigid allocation profit)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

# Panel B: scatter — 2031 RedPrcp only, opt irrigation demand vs benefit
pB <- ggplot(crop2_scatter, aes(x = opt_irr_mm, y = benefit_pct)) +
  geom_point(alpha = 0.5, size = 1.6, colour = "#d73027") +
  geom_smooth(method = "loess", se = TRUE, linewidth = 0.8,
              colour = "#67001f", fill = "#fddbc7", span = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  labs(
    title = "B  Site-level: benefit vs. water demand (2031, RedPrcp)",
    x     = "Total optimal irrigation used (mm)\n[water demand proxy: 0 = rain-satisfied, 300 = fully water-limited]",
    y     = "Benefit of flexible allocation\n(% of rigid allocation profit)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

fig <- pA | pB
fig <- fig + plot_annotation(
  title    = "Flexible allocation benefit under reduced precipitation scenario (20% precip reduction)",
  subtitle = "Two-crop rotation (wheat + canola), 342 Saskatchewan sites, CanDCS-U6 SSP2-4.5",
  theme = theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey30")
  )
)

ggsave(file.path(OUT_DIR, "fig_redprcp_demand_scatter.png"),
       fig, width = 12, height = 6, dpi = 300)
cat("Saved: fig_redprcp_demand_scatter.png\n")

# Print correlation between water demand and benefit
cat("\n=== Correlation: opt_irr_mm vs benefit_pct (2031 RedPrcp) ===\n")
cat("r =", round(cor(crop2_scatter$opt_irr_mm, crop2_scatter$benefit_pct,
                     use = "complete.obs"), 3), "\n")
cat("Median opt irrigation (mm):", round(median(crop2_scatter$opt_irr_mm), 0), "\n")
cat("Sites using full budget (≥295mm):", sum(crop2_scatter$opt_irr_mm >= 295), "\n")
cat("Sites rain-satisfied (<100mm):", sum(crop2_scatter$opt_irr_mm < 100), "\n")
