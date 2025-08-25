# clear memory
rm(list = ls())

library(rgdal)
library(dplyr)
library(sf)
library(terra)
library(sp)
library(SPEI)
library(dplyr)
library(tidyr)
library(tidyverse)


sz_sk <- terra::vect("./Data/shapefile/soilmap_csd_1.shp")


(
  daymet_select <- FedData::get_daymet(
    #--- supply the vector data in sp ---#
    template = as(sz_sk, "Spatial"),
    #--- label ---#
    label = "csd_sk",
    #--- variables to download ---#
    elements = c("prcp"),
    #--- years ---#
    years = 2023:2023
  )
)


terra::writeRaster(daymet_select$prcp, "Data/shapefile/sk_sub_prcp_20118_.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$prcp, "Data/shapefile/sk_sub_prcp_2019_.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$prcp, "Data/shapefile/sk_sub_prcp_2020_.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$prcp, "Data/shapefile/sk_sub_prcp_2021_.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$prcp, "Data/shapefile/sk_sub_prcp_2022_.tif", filetype = "GTiff", overwrite = TRUE)


r1 <- raster::stack("Data/shapefile/sk_sub_prcp_20118_.tif")
r2 <- raster::stack("Data/shapefile/sk_sub_prcp_2019_.tif")
r3 <- raster::stack("Data/shapefile/sk_sub_prcp_2020_.tif")
r4 <- raster::stack("Data/shapefile/sk_sub_prcp_2021_.tif")
r5 <- raster::stack("Data/shapefile/sk_sub_prcp_2022_.tif")


# reproject to lat/lon for easier visuals
r1 <- raster::projectRaster(r1, crs = "+init=epsg:4326")
r2 <- raster::projectRaster(r2, crs = "+init=epsg:4326")
r3 <- raster::projectRaster(r3, crs = "+init=epsg:4326")
r4 <- raster::projectRaster(r4, crs = "+init=epsg:4326")
r5 <- raster::projectRaster(r5, crs = "+init=epsg:4326")


yr_2018 <- as.data.frame(r1, xy=TRUE)
names(yr_2018)[3:367] <- paste0("DOY-", 1:365)
yr_2019 <- as.data.frame(r2, xy=TRUE)
names(yr_2018)[3:367] <- paste0("DOY-", 1:365)
yr_2020 <- as.data.frame(r3, xy=TRUE)
names(yr_2018)[3:367] <- paste0("DOY-", 1:365)
yr_2021 <- as.data.frame(r4, xy=TRUE)
names(yr_2018)[3:367] <- paste0("DOY-", 1:365)
yr_2022 <- as.data.frame(r5, xy=TRUE)
names(yr_2018)[3:367] <- paste0("DOY-", 1:365)








