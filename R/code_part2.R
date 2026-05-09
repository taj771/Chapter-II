# Figure 2: Cumulative precipitation and irrigation demand during the crop cycle for
# wheat, canola, and potato across 397 fields (2018-2023)

rm(list = ls())
library(tidyverse)
library(purrr)
library(glue)

BASE  <- "./Data Main Analysis"
YEARS <- 2018:2023

# Wheat has precipitation column; canola and potato do not
read_crop_year <- function(crop, year) {
  path <- glue("{BASE}/{crop}_netirridemand_{year}.csv")
  cols <- c("Site", "Year", "Seasonal irrigation (mm)",
            if (crop == "wheat") "Total_Precipitation(mm)")
  read_csv(path, show_col_types = FALSE) %>%
    mutate(Year = year, crop = crop) %>%
    select(any_of(cols)) %>%
    pivot_longer(cols = -c(Site, Year, crop),
                 names_to = "variable", values_to = "value")
}

df <- map_dfr(c("wheat", "canola", "potato"),
              function(crop) map_dfr(YEARS, ~read_crop_year(crop, .x)))

df$variable <- factor(df$variable,
                      levels = c("Total_Precipitation(mm)", "Seasonal irrigation (mm)"))
df$crop <- factor(df$crop, levels = c("wheat", "canola", "potato"))

p <- ggplot(df, aes(x = factor(Year), y = value,
                    fill = interaction(variable, crop))) +
  geom_boxplot(notch = TRUE, width = 0.4,
               position = position_dodge(0.5), outlier.size = 0.4) +
  scale_fill_manual(
    values = c("blue", "springgreen4", "firebrick3", "goldenrod1"),
    labels = c("Precipitation", "Irrigation:Wheat",
               "Irrigation:Canola", "Irrigation:Potato")
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) +
  scale_y_continuous(name = "Precipitation / Irrigation (mm)",
                     limits = c(0, 350), breaks = seq(0, 800, by = 50)) +
  guides(fill = guide_legend(title = NULL)) +
  labs(x = "Year", y = "Precipitation / Irrigation (mm)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line      = element_line(color = "black"),
    legend.position = "bottom",
    axis.text.x    = element_text(size = 18),
    axis.text.y    = element_text(size = 18),
    axis.title.x   = element_text(size = 18),
    axis.title.y   = element_text(size = 18),
    legend.text    = element_text(size = 18),
    axis.ticks     = element_line(linewidth = 0.8)
  )

ggsave("./results/images/PrcpIrriusecropwise.png",
       plot = p, width = 10, height = 7, dpi = 300)
