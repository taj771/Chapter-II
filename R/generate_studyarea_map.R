# Generate study area map — ERA5-Land grid + satellite base + inset
# Satellite: ESRI World Imagery via maptiles (no API key)

rm(list = ls())
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(ggspatial)
library(maptiles)
library(tidyterra)
library(terra)
library(nngeo)

OUT_PNG  <- "./Dissertation_Latex_Project/Figures2/studyarea_new.png"
BOUNDARY <- "./Data Main Analysis copy/Shapefiles/MergeLakeDiefenbaker.shp"
SITES_CSV <- "./Data/ERA5/era5_grid_sites.csv"
LCC <- 3347   # Statistics Canada Lambert Conformal Conic

# ── Load district boundary ────────────────────────────────────────────────────
district_wgs <- st_read(BOUNDARY, quiet = TRUE) %>%
  st_make_valid() %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_transform(4326)
district_lcc <- st_transform(district_wgs, LCC)

# ── Build ERA5-Land grid cells, clip to district ──────────────────────────────
sites <- read.csv(SITES_CSV)
cell  <- 0.1
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

dist_union <- st_union(district_wgs)
grid_clipped <- st_intersection(grid_wgs, dist_union) %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_transform(LCC)

sites_sf <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)
sites_in <- sites_sf[lengths(st_intersects(sites_sf, dist_union)) > 0, ] %>%
  st_transform(LCC)

# ── Download satellite tiles (ESRI World Imagery) ─────────────────────────────
# Buffer district by ~5 km for tile download
dist_buf <- st_buffer(district_lcc, 5000)
tiles <- get_tiles(dist_buf, provider = "Esri.WorldImagery", zoom = 10,
                   crop = TRUE, project = TRUE)

# ── Main map: satellite + grid overlay ───────────────────────────────────────
dist_bbox <- st_bbox(district_lcc)
buf <- 8000
xlim <- c(dist_bbox["xmin"] - buf, dist_bbox["xmax"] + buf)
ylim <- c(dist_bbox["ymin"] - buf, dist_bbox["ymax"] + buf)

# Lake Diefenbaker label point (reservoir centre, WGS84 → LCC)
lake_pt <- st_sfc(st_point(c(-106.95, 51.25)), crs = 4326) %>% st_transform(LCC)

p_main <- ggplot() +
  geom_spatraster_rgb(data = tiles) +
  geom_sf(data = grid_clipped, fill = "steelblue2", colour = "deepskyblue3",
          linewidth = 0.3, alpha = 0.30) +
  geom_sf(data = nngeo::st_remove_holes(st_union(district_lcc)),
          fill = NA, colour = "white", linewidth = 0.5) +
  # Lake Diefenbaker label
  geom_sf_label(
    data = st_sf(label = "Lake\nDiefenbaker", geometry = lake_pt),
    aes(label = label),
    size = 3.0, fontface = "italic",
    colour = "white", fill = "black", alpha = 0.55,
    label.padding = unit(0.15, "lines"), label.size = 0
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.22,
                   unit_category = "metric",
                   pad_y = unit(0.5, "cm"),
                   text_col = "white", line_col = "white",
                   bar_cols = c("white", "grey40")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.3, "cm"), pad_y = unit(0.8, "cm"),
                         height = unit(1.2, "cm"), width = unit(1.0, "cm"),
                         style = north_arrow_fancy_orienteering(
                           text_col = "white", line_col = "white",
                           fill = c("white", "black"))) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    axis.text  = element_text(size = 8, colour = "grey20"),
    panel.grid = element_line(colour = "grey70", linewidth = 0.2)
  )

# ── Inset: AB + SK + MB with study area box, bottom-right ────────────────────
canada <- ne_states(country = "canada", returnclass = "sf") %>%
  st_transform(LCC)
provinces_inset <- canada %>% filter(name %in% c("Saskatchewan", "Alberta", "Manitoba"))
study_box <- st_as_sfc(st_bbox(district_lcc)) %>% st_set_crs(LCC)

p_inset <- ggplot() +
  geom_sf(data = provinces_inset, fill = "grey82", colour = "grey50", linewidth = 0.4) +
  geom_sf(data = study_box, fill = "yellow", colour = "red",
          linewidth = 1.0, alpha = 0.5) +
  geom_sf_text(data = provinces_inset, aes(label = postal),
               size = 2.8, colour = "grey20") +
  coord_sf(expand = TRUE) +
  theme_void(base_size = 8) +
  theme(
    panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.7),
    panel.background = element_rect(fill = "aliceblue")
  )

# ── Combine: inset bottom-right ───────────────────────────────────────────────
inset_w <- (xlim[2] - xlim[1]) * 0.36
inset_h <- (ylim[2] - ylim[1]) * 0.36

p_combined <- p_main +
  annotation_custom(
    grob = ggplotGrob(p_inset),
    xmin = xlim[2] - inset_w,
    xmax = xlim[2],
    ymin = ylim[1],
    ymax = ylim[1] + inset_h
  )

ggsave(OUT_PNG, plot = p_combined, width = 8, height = 8, dpi = 300)
cat("Saved:", OUT_PNG, "\n")
cat("Grid cells clipped:", nrow(grid_clipped), "\n")
cat("Site centroids:", nrow(sites_in), "\n")
