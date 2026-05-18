# Figure: 2-panel — 2-crop (left) vs 3-crop (right)
# Shows drought amplification across climate years + crop comparison
# x = climate year (2031/2044/2050), fill = OriPrcp vs RedPrcp

library(tidyverse)
library(patchwork)
library(here)

OUT_DIR <- here::here("outputs/figures_candscu6")

YEAR_LABS <- c(
  "2031" = "Driest\n(2031)",
  "2044" = "Average\n(2044)",
  "2050" = "Wettest\n(2050)"
)
SCEN_LABS <- c(
  "OriPrcp" = "Original precip",
  "RedPrcp" = "20% reduced precip"
)

pal <- c("Original precip" = "#2166ac", "20% reduced precip" = "#d73027")

crop2 <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6.rds")) %>%
  mutate(
    year_lab = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS),
    scen_lab = factor(SCEN_LABS[scenario], levels = SCEN_LABS)
  )

crop3 <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6_3crop.rds")) %>%
  mutate(
    year_lab = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS),
    scen_lab = factor(SCEN_LABS[scenario], levels = SCEN_LABS)
  )

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = year_lab, y = benefit_pct, fill = scen_lab)) +
    geom_boxplot(
      position = position_dodge(0.8), width = 0.65,
      outlier.size = 0.5, outlier.alpha = 0.4, linewidth = 0.4
    ) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    scale_fill_manual(values = pal, name = "Precipitation scenario") +
    labs(
      title = title_label,
      x = "Representative climate year\n(growing season precipitation quantile)",
      y = "Benefit of flexible allocation\n(% of rigid allocation profit)"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position  = "none",
      plot.title       = element_text(face = "bold", size = 11),
      panel.grid.minor = element_blank()
    )
}

p2 <- make_panel(crop2, "A  Two-crop rotation (wheat + canola)")
p3 <- make_panel(crop3, "B  Three-crop rotation (wheat + canola + potato)")

# shared legend
legend_plot <- ggplot(crop2, aes(x = year_lab, y = benefit_pct, fill = scen_lab)) +
  geom_boxplot() +
  scale_fill_manual(values = pal, name = "Precipitation scenario") +
  theme(legend.position = "bottom")
legend_grob <- cowplot::get_legend(legend_plot)

fig <- (p2 | p3) /
  cowplot::plot_grid(legend_grob) +
  plot_layout(heights = c(10, 1)) +
  plot_annotation(
    title    = "Net economic benefit of flexible irrigation water allocation under climate change",
    subtitle = "342 Saskatchewan sites, CanDCS-U6 SSP2-4.5 | Rigid: 150 mm per crop | Flexible: optimal split within same total budget",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, colour = "grey30")
    )
  )

ggsave(file.path(OUT_DIR, "fig_climate_2panel.png"),
       fig, width = 12, height = 6, dpi = 300)

cat("Saved: fig_climate_2panel.png\n")

# Print key numbers
cat("\n=== 2-CROP SUMMARY ===\n")
crop2 %>% group_by(year_lab, scen_lab) %>%
  summarise(median_pct = round(median(benefit_pct, na.rm=TRUE), 1), .groups="drop") %>%
  print()

cat("\n=== 3-CROP SUMMARY ===\n")
crop3 %>% group_by(year_lab, scen_lab) %>%
  summarise(median_pct = round(median(benefit_pct, na.rm=TRUE), 1), .groups="drop") %>%
  print()
