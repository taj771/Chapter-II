# Figure 4: Spatial variation in the average weighted shadow price of irrigation water.
# Grid cells: ERA5-Land 0.1° × 0.1° (~10 km), matching simulation resolution.

rm(list = ls())
library(sf)
library(ggplot2)
library(dplyr)
library(scales)
library(MetBrewer)
library(ggspatial)
library(rnaturalearth)

source("./R/utils.R")

BOUNDARY <- "./Data Main Analysis copy/Shapefiles/MergeLakeDiefenbaker.shp"
LCC      <- 3347   # Statistics Canada Lambert — matches study area map

df_all <- compute_shadow_prices()

# ── District boundary ─────────────────────────────────────────────────────────

district_wgs <- st_read(BOUNDARY, quiet = TRUE) %>%
  st_make_valid() %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_transform(4326)
district_lcc <- st_transform(district_wgs, LCC)

# ── Build ERA5 0.1° grid polygons ────────────────────────────────────────────

sites <- read.csv("./Data/ERA5/era5_grid_sites.csv") %>%
  rename(Site = grid_id)

cell <- 0.1
grid_wgs <- sites %>%
  rowwise() %>%
  mutate(geometry = list(st_polygon(list(matrix(
    c(lon - cell/2, lat - cell/2,
      lon + cell/2, lat - cell/2,
      lon + cell/2, lat + cell/2,
      lon - cell/2, lat + cell/2,
      lon - cell/2, lat - cell/2),
    ncol = 2, byrow = TRUE))))) %>%
  ungroup() %>%
  st_as_sf(crs = 4326)

# ── Median weighted shadow price per ERA5 cell — clipped to study area ────────

dist_union <- st_union(district_wgs)

df_map <- grid_wgs %>%
  left_join(df_all, by = "Site") %>%
  group_by(Site) %>%
  mutate(prof_val_mm_w_average = median(prof_weighted, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Site, prof_val_mm_w_average, geometry) %>%
  distinct(Site, .keep_all = TRUE) %>%
  drop_na() %>%
  mutate(prof_val_mm_w_average = pmax(prof_val_mm_w_average, 0)) %>%
  st_as_sf() %>%
  st_intersection(dist_union) %>%           # clip to study area boundary
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_transform(LCC)

# ── Saskatchewan province boundary ───────────────────────────────────────────

canada      <- ne_states(country = "canada", returnclass = "sf") %>%
  st_transform(LCC)
sk_province <- canada %>% filter(name == "Saskatchewan")

# ── Map extents ───────────────────────────────────────────────────────────────

dist_bbox <- st_bbox(district_lcc)
buf  <- 8000
xlim <- c(dist_bbox["xmin"] - buf, dist_bbox["xmax"] + buf)
ylim <- c(dist_bbox["ymin"] - buf, dist_bbox["ymax"] + buf)

# ── Main map ──────────────────────────────────────────────────────────────────

p_main <- ggplot() +
  geom_sf(data = sk_province, fill = "grey92", colour = "grey50",
          linewidth = 0.5) +
  geom_sf(data = district_lcc, fill = "grey85", colour = "grey60",
          linewidth = 0.4) +
  geom_sf(data = df_map, aes(fill = prof_val_mm_w_average),
          colour = NA) +
  geom_sf(data = district_lcc, fill = NA, colour = "black",
          linewidth = 0.7) +
  scale_fill_distiller(
    palette  = "Blues",
    direction = 1,
    name     = expression("Average Value ($ m"^{-3}*")"),
    limits   = c(0.30, 0.45),
    breaks   = seq(0.30, 0.45, by = 0.05),
    labels   = c("0.30", "0.35", "0.40", "0.45"),
    oob      = scales::squish,
    na.value = "grey80"
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.22,
                   unit_category = "metric",
                   pad_y = unit(0.5, "cm"),
                   text_col = "grey20", line_col = "grey20",
                   bar_cols = c("grey20", "grey80")) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.3, "cm"), pad_y = unit(0.5, "cm"),
                         height = unit(1.2, "cm"), width = unit(1.0, "cm"),
                         style = north_arrow_fancy_orienteering()) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "right",
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 9),
    axis.text        = element_text(size = 8, colour = "grey20"),
    panel.grid       = element_line(colour = "grey80", linewidth = 0.2)
  )

out_dir <- "./Dissertation_Latex_Project/Figures2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out_dir, "AverageValue_profit_map.png"),
       plot = p_main, width = 8, height = 8, dpi = 300)
cat("Saved:", file.path(out_dir, "AverageValue_profit_map.png"), "\n")
