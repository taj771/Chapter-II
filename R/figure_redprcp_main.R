# Main figure: RedPrcp-only, 2-panel (2-crop left, 3-crop right)
# Shows drought amplification across 3 representative climate years
# x = climate year, y = benefit_pct, fill = year (colour-coded)

library(tidyverse)
library(patchwork)
library(here)

OUT_DIR <- here::here("outputs/figures_candscu6")

YEAR_LABS <- c(
  "2031" = "Driest\n(2031, P5)",
  "2044" = "Average\n(2044, P50)",
  "2050" = "Wettest\n(2050, P95)"
)

year_cols <- c(
  "Driest\n(2031, P5)"    = "#d73027",
  "Average\n(2044, P50)"  = "#f4a582",
  "Wettest\n(2050, P95)"  = "#4575b4"
)

load_redprcp <- function(rds_file) {
  readRDS(file.path(OUT_DIR, rds_file)) %>%
    filter(scenario == "RedPrcp") %>%
    mutate(
      year_lab = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS)
    )
}

crop2 <- load_redprcp("site_benefits_candscu6.rds")
crop3 <- load_redprcp("site_benefits_candscu6_3crop.rds")

make_panel <- function(df, panel_lab) {
  ggplot(df, aes(x = year_lab, y = benefit_pct, fill = year_lab)) +
    geom_boxplot(
      width = 0.55, outlier.size = 0.6, outlier.alpha = 0.4, linewidth = 0.4
    ) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    scale_fill_manual(values = year_cols) +
    labs(
      tag = panel_lab,
      x   = "Representative climate year (growing-season precipitation quantile)",
      y   = "Net benefit of flexible allocation\n(% of rigid allocation profit)"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(size = 10),
      plot.tag         = element_text(face = "bold", size = 11)
    )
}

p2 <- make_panel(crop2, "A")
p3 <- make_panel(crop3, "B")

fig <- p2 | p3

ggsave(file.path(OUT_DIR, "fig_redprcp_main.png"),
       fig, width = 12, height = 6, dpi = 300)
cat("Saved: fig_redprcp_main.png\n")

# Key numbers for paper
cat("\n=== KEY NUMBERS (RedPrcp only) ===\n")
cat("--- 2-crop ---\n")
crop2 %>%
  group_by(year_lab) %>%
  summarise(
    n           = n(),
    median_pct  = round(median(benefit_pct, na.rm=TRUE), 1),
    mean_pct    = round(mean(benefit_pct, na.rm=TRUE), 1),
    median_ac   = round(median(benefit, na.rm=TRUE), 0),
    pct_pos     = round(mean(benefit > 0, na.rm=TRUE) * 100, 1),
    .groups = "drop"
  ) %>% print()

cat("--- 3-crop ---\n")
crop3 %>%
  group_by(year_lab) %>%
  summarise(
    n           = n(),
    median_pct  = round(median(benefit_pct, na.rm=TRUE), 1),
    mean_pct    = round(mean(benefit_pct, na.rm=TRUE), 1),
    median_ac   = round(median(benefit, na.rm=TRUE), 0),
    pct_pos     = round(mean(benefit > 0, na.rm=TRUE) * 100, 1),
    .groups = "drop"
  ) %>% print()
