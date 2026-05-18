# Mechanism bar chart: profit_rigid vs profit_flex across climate years (3-crop, RedPrcp)
# Shows WHY benefit rises in wet years: rigid collapses (negative), flex stays robust

library(tidyverse)
library(patchwork)
library(here)

OUT_DIR <- here::here("outputs/figures_candscu6")

YEAR_LABS <- c(
  "2031" = "Driest\n(2031, P5)",
  "2044" = "Average\n(2044, P50)",
  "2050" = "Wettest\n(2050, P95)"
)

crop3 <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6_3crop.rds")) %>%
  filter(scenario == "RedPrcp") %>%
  mutate(year_lab = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS))

# Medians per year
medians <- crop3 %>%
  group_by(year_lab) %>%
  summarise(
    rigid_med   = median(profit_rigid,    na.rm = TRUE),
    flex_med    = median(profit_flexible, na.rm = TRUE),
    benefit_med = median(benefit,         na.rm = TRUE),
    n           = n(),
    .groups = "drop"
  )

cat("=== MEDIANS (3-crop, RedPrcp) ===\n")
print(medians)

# Long format for grouped bars
bar_df <- medians %>%
  select(year_lab, rigid_med, flex_med) %>%
  pivot_longer(
    cols      = c(rigid_med, flex_med),
    names_to  = "allocation",
    values_to = "profit"
  ) %>%
  mutate(
    allocation = factor(
      allocation,
      levels = c("rigid_med", "flex_med"),
      labels = c("Rigid (equal quota, 150 mm each crop)",
                 "Flexible (optimal reallocation)")
    )
  )

alloc_cols <- c(
  "Rigid (equal quota, 150 mm each crop)" = "#d73027",
  "Flexible (optimal reallocation)"       = "#4575b4"
)

# Benefit label positions (just above x-axis)
benefit_labels <- medians %>%
  mutate(
    label = paste0("+$", round(benefit_med, 0), "/ac\n(+",
                   round(100 * benefit_med / abs(rigid_med), 0), "%)")
  )

p_bar <- ggplot(bar_df, aes(x = year_lab, y = profit, fill = allocation)) +
  geom_col(
    position = position_dodge(0.75), width = 0.65,
    colour = "grey30", linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  # Benefit annotation above 0 line
  geom_text(
    data = benefit_labels,
    aes(x = year_lab, y = pmax(flex_med, 0) + 8,
        label = label),
    inherit.aes = FALSE,
    size = 3.2, colour = "grey20", lineheight = 0.9,
    vjust = 0
  ) +
  scale_fill_manual(values = alloc_cols, name = NULL) +
  scale_y_continuous(
    labels = function(x) paste0("$", x, "/ac"),
    expand = expansion(mult = c(0.15, 0.2))
  ) +
  labs(
    title    = "Three-crop rotation: rigid allocation collapses in wet years,\nflexible allocation remains profitable",
    subtitle = paste0(
      "20% precipitation reduction scenario | 342 Saskatchewan sites, CanDCS-U6 SSP2-4.5\n",
      "Medians shown | Rigid: 150 mm per crop (wheat + canola + potato = 450 mm total)\n",
      "Flex: optimal reallocation within same 450 mm total budget"
    ),
    x = "Representative climate year (growing-season precipitation quantile)",
    y = "Median farm profit ($/ac)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position   = "top",
    legend.text       = element_text(size = 10),
    plot.title        = element_text(face = "bold", size = 12),
    plot.subtitle     = element_text(size = 8.5, colour = "grey30"),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x       = element_text(size = 10)
  )

# Panel B: allocation per crop (medians) across years — shows rigid wastes on cereals
alloc_medians <- crop3 %>%
  group_by(year_lab) %>%
  summarise(
    flex_wheat  = median(opt_wheat_mm,  na.rm = TRUE),
    flex_canola = median(opt_canola_mm, na.rm = TRUE),
    flex_potato = median(opt_potato_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = starts_with("flex_"),
    names_to  = "crop",
    values_to = "mm"
  ) %>%
  mutate(
    crop = factor(
      crop,
      levels = c("flex_potato", "flex_canola", "flex_wheat"),
      labels = c("Potato", "Canola", "Wheat")
    ),
    rigid_mm = 150
  )

crop_cols <- c(
  "Wheat"  = "#e8a838",
  "Canola" = "#5aae61",
  "Potato" = "#9970ab"
)

p_alloc <- ggplot(alloc_medians, aes(x = year_lab, y = mm, fill = crop)) +
  geom_col(position = "stack", colour = "grey30", linewidth = 0.3) +
  geom_hline(yintercept = 150, linetype = "dashed",
             colour = "#d73027", linewidth = 0.7) +
  annotate("text", x = 0.55, y = 158, label = "Rigid quota per crop (150 mm)",
           colour = "#d73027", size = 3, hjust = 0) +
  scale_fill_manual(values = crop_cols, name = "Crop (flexible allocation)") +
  scale_y_continuous(
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450),
    labels = function(x) paste0(x, " mm"),
    limits = c(0, 460)
  ) +
  labs(
    title = "Flexible allocation shifts water to potato\nas cereals become rain-satisfied",
    x     = "Representative climate year",
    y     = "Median irrigation allocation (mm)\n[3-crop budget: 150 mm × 3 crops = 450 mm total]"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "top",
    legend.text      = element_text(size = 9),
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x      = element_text(size = 10)
  )

fig <- p_bar | p_alloc
fig <- fig + plot_annotation(
  title = "Mechanism: Why flexible allocation benefits increase in wetter climate years (3-crop rotation)",
  theme = theme(plot.title = element_text(face = "bold", size = 13))
)

ggsave(file.path(OUT_DIR, "fig_mechanism_barchart.png"),
       fig, width = 14, height = 7, dpi = 300)
cat("\nSaved: fig_mechanism_barchart.png\n")

# Key numbers for paper
cat("\n=== KEY NUMBERS FOR PAPER ===\n")
medians %>%
  mutate(
    benefit_pct_of_rigid = round(100 * benefit_med / abs(rigid_med), 1)
  ) %>%
  select(year_lab, rigid_med, flex_med, benefit_med, benefit_pct_of_rigid) %>%
  print()

cat("\n=== ALLOCATION SHIFT (median mm per crop) ===\n")
alloc_medians %>%
  select(year_lab, crop, mm) %>%
  pivot_wider(names_from = crop, values_from = mm) %>%
  print()
