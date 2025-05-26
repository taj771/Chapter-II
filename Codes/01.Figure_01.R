study_area <- st_read("./Data/shapefile/MergeLakeDiefenbaker.shp")

cities <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/saskatchewan-latest-free/gis_osm_places_free_1.shp")%>%
  filter(fclass %in% c("city","town"))

study_area_dissolved <- study_area %>%
  group_by(prov) %>%
  summarise()

sk <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/SK.shp")

sk <- st_transform(sk, crs = 3347)


# Ensure same CRS
cities_proj <- st_transform(cities, st_crs(study_area_dissolved))


main_map <- tm_shape(study_area_dissolved, projection = 3347) +
  tm_fill(col = "prov", palette = "#a6dba9") +
  tm_shape(cities_proj) +
  tm_dots(col = "red", size = 0.2) +
  tm_text("name", size = 0.8, just = "top", ymod = -0.4) +
  tm_layout(
    frame = FALSE,
    legend.show = FALSE  # This removes the legend
  ) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(
    frame = FALSE,
    legend.show = FALSE
  ) +
  tm_compass(type = "arrow", position = c(1.1, 1), size = 2) +
  tm_layout(
    frame = FALSE,
    legend.show = FALSE
  )



inset_map <- tm_shape(sk) +
  tm_fill(col = NULL, alpha = 1, fill = "white") +  # ensure white fill
  tm_borders(col = "black") +
  tm_shape(study_area_dissolved) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(legend.show = FALSE, frame = TRUE) +
  tm_layout(
    frame = FALSE,
    legend.show = FALSE
  ) 


tmap_mode("plot")  # Static map mode

# Print main map
print(main_map)

library(grid)

# Print inset map in top-left (adjust position/size if needed)
print(inset_map, vp = viewport(x = 0.15, y = 0.85, width = 0.25, height = 0.25))


