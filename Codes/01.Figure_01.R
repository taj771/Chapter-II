#clear memory
rm(list = ls())

library(sf)
library(maptiles)
library(raster)
library(terra)

study_area <- st_read("./Data/shapefile/MergeLakeDiefenbaker.shp")

study_area_dissolved <- study_area %>%
  dplyr::group_by(prov) %>%       # group by province column
  dplyr::summarise(geometry = st_union(geometry))

# Get bounding box of study area
study_area_bbox <- st_as_sfc(st_bbox(study_area_dissolved))


cities <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/saskatchewan-latest-free/gis_osm_places_free_1.shp")%>%
  dplyr::filter(fclass %in% c("city","town"))


sk <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/SK.shp")
ab <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/AB.shp")
mb <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/MB.shp")


sk <- st_transform(sk, crs = 3347)
ab <- st_transform(ab, crs = 3347)
mb <- st_transform(mb, crs = 3347)

provinces <- rbind(sk,ab,mb)


# Ensure same CRS
cities_proj <- st_transform(cities, st_crs(study_area_dissolved))




p1 <- tm_shape(provinces, projection = 3347) +
  tm_fill(col = "PRNAME", palette = "white") +
  #tm_shape(cities_proj) +
  #tm_dots(col = "red", size = 0.2) +
  tm_text("PRNAME", size = 0.6, just = "top", ymod = -0.4) +
  
  # Add bounding box layer
  tm_shape(study_area_bbox, projection = 3347) +
  tm_borders(col = "black", lwd = 2) +   # border color and width
  tm_fill(col = "#a6dba9", alpha = 0.3) +  # fill color and transparency
  
  
  
  tm_layout(
    frame = FALSE,
    legend.show = FALSE  # This removes the legend
  ) 

cities <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/saskatchewan-latest-free/gis_osm_places_free_1.shp")%>%
  dplyr::filter(fclass %in% c("city","town"))%>%
  #dplyr::filter(population > 10000)%>%
  dplyr::filter(name %in% c("Outlook", "Saskatoon", "Hanley", "Delisle"))


p2 <- tm_shape(study_area_dissolved, projection = 3347) +
  tm_fill(col = "prov", palette = "#a6dba9") +
  tm_shape(cities, projection = 3347) +
  tm_dots(col = "red", size = 0.2) +
  tm_text("name", size = 0.8, just = "top", ymod = -0.4) +
  tm_layout(
    frame = FALSE,
    legend.show = FALSE  # This removes the legend
  ) +
  tm_scale_bar(position = c(0.7, 0.001))+
  tm_layout(
    frame = FALSE,
    legend.show = FALSE
  ) +
  tm_compass(type = "arrow", position = c(1.1, 1), size = 2) +
  tm_layout(
    frame = FALSE,
    legend.show = FALSE
  )


tmap_mode("plot")  # Static map mode

library(grid)

# main map, reduced to 70% of page size, centered
print(
  p2,
  vp = viewport(
    x = 0.5, y = 0.5,   # center
    width = 1,        # 70% of page width
    height = 1        # 70% of page height
  )
)

library(grid)

# Print inset map in top-left (adjust position/size if needed)
print(p1, vp = viewport(x = 0.15, y = 0.85, width = 0.4, height = 0.4))

# New image

# Download satellite imagery (e.g., Esri World Imagery)
raster_img <- get_tiles(study_area_dissolved, provider = "Esri.WorldImagery", crop = TRUE, zoom = 14)

# Plot
plotRGB(raster_img)


# Ensure your polygon has the same CRS as the raster
polygon <- st_transform(study_area_dissolved, crs(raster_img))

# Convert sf polygon to SpatVector (terra format)
polygon_vect <- vect(polygon)

# Clip the raster using terra functions
clipped_raster <- crop(raster_img, polygon_vect, mask = TRUE)

# View the result
plot(clipped_raster)


p2 <- tm_shape(clipped_raster, projection = 3347) +
  tm_rgb() +  # Base layer with the study area raster
  
  tm_shape(t, projection = 3347) +
  tm_fill(col = "name_en", palette = "Blues", 
          title = "", legend.is.portrait = T,
          legend) +
  
  tm_shape(study_area_dissolved, projection = 3347) +
  tm_borders(lwd = 0.5, col = "black") +  # Add border to highlight study area
  
  tm_shape(cities) +
  tm_dots(col = "red", size = 0.2, title = "Cities") +
  tm_text("name", size = 0.8, just = "top", ymod = -0.4) +
  
  tm_scale_bar(position = c(0.7, 0.001)) +
  tm_compass(type = "arrow", position = c(1.1, 1), size = 2) +
  
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0.7,
    legend.frame = F
  ) 

tmap_mode("plot")  # Static map mode

library(grid)

# main map, reduced to 70% of page size, centered
print(
  p2,
  vp = viewport(
    x = 0.5, y = 0.5,   # center
    width = 1,        # 70% of page width
    height = 1        # 70% of page height
  )
)

library(grid)

# Print inset map in top-left (adjust position/size if needed)
print(p1, vp = viewport(x = 0.15, y = 0.85, width = 0.4, height = 0.4))





