# Figure 4: Spatial variation in the average weighted shadow price of irrigation water.

rm(list = ls())
library(sf)
library(tmap)
library(MetBrewer)

source("utils.R")   # loads tidyverse, glue, priceR, lubridate + helpers

df_all <- compute_shadow_prices()

# ── spatial join ──────────────────────────────────────────────────────────────

weather_data_final <- read.csv(glue("{BASE}/combined_daymet_weather_data.csv")) %>%
  select(site, longitude, latitude) %>%
  distinct(site, .keep_all = TRUE)

df_sf <- st_as_sf(weather_data_final, coords = c("longitude", "latitude"), crs = 4326)

df_map_grids <- st_read(glue("{BASE}/shapefiles/MergeLakeDiefenbakerfishnet5Km.shp")) %>%
  st_transform(st_crs(df_sf)) %>%
  st_join(df_sf, join = st_intersects)

df_map <- df_map_grids %>%
  rename(Site = site) %>%
  left_join(df_all, by = "Site")   # expands to 6 rows per grid cell (one per year)

# ── median weighted shadow price per grid cell ────────────────────────────────

df_one_map <- df_map %>%
  group_by(Site) %>%
  mutate(prof_val_mm_w_average = median(prof_weighted, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Site, prof_val_mm_w_average) %>%
  distinct(Site, .keep_all = TRUE) %>%
  drop_na() %>%
  mutate(prof_val_mm_w_average = pmax(prof_val_mm_w_average, 0))

# ── map ───────────────────────────────────────────────────────────────────────

palette1 <- met.brewer("Hokusai2", 70, type = "continuous")

p <- tm_shape(df_one_map, projection = 3347) +
  tm_fill(
    col     = "prof_val_mm_w_average",
    palette = palette1,
    title   = "Average Value ($ per m³)",
    lwd     = 0.05,
    showNA  = FALSE,
    breaks  = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1)
  ) +
  tm_layout(
    frame            = FALSE,
    legend.frame     = FALSE,
    legend.position  = c(0.85, 0.45),
    legend.direction = "horizontal"
  ) +
  tm_compass(type = "arrow", position = c(1.1, 1), size = 2) +
  tm_scale_bar(position = c(-0.3, 0.1))

tmap_save(p, filename = "./results/images/AverageValue_profit_map.png",
          width = 10, height = 7, dpi = 300)
