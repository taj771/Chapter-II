# Figure: Benefit by site water demand class
# Classification: 2050 OriPrcp (wettest projected year)
#   "Full demand"  = benefit > 0 in wettest year (irrigation needed even then)
#   "Rain-satisfied" = benefit = 0 in wettest year (rain covers them fully)
# Then show benefit_pct across all 3 years split by class

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

pal_class <- c(
  "Full demand (irrigation-dependent)"  = "#d73027",
  "Rain-satisfied in wet year"          = "#4575b4"
)

classify_by_wet_year <- function(df) {
  wet_class <- df %>%
    filter(as.character(year) == "2050", scenario == "OriPrcp") %>%
    mutate(
      demand_class = ifelse(
        benefit > 0,
        "Full demand (irrigation-dependent)",
        "Rain-satisfied in wet year"
      )
    ) %>%
    select(Site_ID, demand_class)

  df %>%
    left_join(wet_class, by = "Site_ID") %>%
    filter(!is.na(demand_class)) %>%
    mutate(
      year_lab     = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS),
      scen_lab     = factor(SCEN_LABS[scenario], levels = SCEN_LABS),
      demand_class = factor(demand_class,
                            levels = c("Full demand (irrigation-dependent)",
                                       "Rain-satisfied in wet year"))
    )
}

crop2 <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6.rds"))
crop3 <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6_3crop.rds"))

crop2_c <- classify_by_wet_year(crop2)
crop3_c <- classify_by_wet_year(crop3)

cat("=== 2-CROP CLASS SIZES ===\n")
crop2_c %>% filter(as.character(year) == "2050", scenario == "OriPrcp") %>%
  count(demand_class) %>% print()

cat("\n=== 3-CROP CLASS SIZES ===\n")
crop3_c %>% filter(as.character(year) == "2050", scenario == "OriPrcp") %>%
  count(demand_class) %>% print()

make_panel <- function(df, scen_filter, panel_lab) {
  df %>%
    filter(scenario == scen_filter) %>%
    ggplot(aes(x = year_lab, y = benefit_pct, fill = demand_class)) +
    geom_boxplot(
      position = position_dodge(0.8), width = 0.65,
      outlier.size = 0.5, outlier.alpha = 0.4, linewidth = 0.4
    ) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    scale_fill_manual(values = pal_class, name = NULL) +
    labs(
      tag = panel_lab,
      x   = "Representative climate year",
      y   = "Benefit of flexible allocation (%)"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position  = "bottom",
      legend.text      = element_text(size = 9),
      panel.grid.minor = element_blank(),
      plot.tag         = element_text(face = "bold", size = 11)
    )
}

p2 <- make_panel(crop2_c, "OriPrcp", "A")
p3 <- make_panel(crop3_c, "OriPrcp", "B")

fig <- p2 | p3

ggsave(file.path(OUT_DIR, "fig_demand_class_split.png"),
       fig, width = 12, height = 6.5, dpi = 300)
cat("\nSaved: fig_demand_class_split.png\n")

cat("\n=== BENEFIT BY CLASS × YEAR (OriPrcp, 2-crop, medians) ===\n")
crop2_c %>%
  filter(scenario == "OriPrcp") %>%
  group_by(year_lab, demand_class) %>%
  summarise(n = n(), median_pct = round(median(benefit_pct, na.rm = TRUE), 1),
            .groups = "drop") %>%
  print()

cat("\n=== BENEFIT BY CLASS × YEAR (OriPrcp, 3-crop, medians) ===\n")
crop3_c %>%
  filter(scenario == "OriPrcp") %>%
  group_by(year_lab, demand_class) %>%
  summarise(n = n(), median_pct = round(median(benefit_pct, na.rm = TRUE), 1),
            .groups = "drop") %>%
  print()
