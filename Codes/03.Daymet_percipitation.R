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
library(raster)


sz_sk <- terra::vect("./Data/shapefile/sk_soil_sub.shp")


(
  daymet_select <- FedData::get_daymet(
    #--- supply the vector data in sp ---#
    template = as(sz_sk, "Spatial"),
    #--- label ---#
    label = "csd_sk",
    #--- variables to download ---#
    elements = c("prcp","tmax", "tmin", "dayl", "srad", "swe", "vp"),
    #--- years ---#
    years = 2023:2023
  )
)

terra::writeRaster(daymet_select$prcp, "Data/shapefile/sk_sub_prcp.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$tmax, "Data/shapefile/sk_sub_tmax.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$tmin, "Data/shapefile/sk_sub_tmin.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$dayl, "Data/shapefile/sk_sub_dayl.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$srad, "Data/shapefile/sk_sub_srad.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$swe, "Data/shapefile/sk_sub_swe.tif", filetype = "GTiff", overwrite = TRUE)
terra::writeRaster(daymet_select$vp, "Data/shapefile/sk_sub_vp.tif", filetype = "GTiff", overwrite = TRUE)


r1 <- raster::stack("Data/shapefile/sk_sub_prcp.tif")
r2 <- raster::stack("Data/shapefile/sk_sub_tmax.tif")
r3 <- raster::stack("Data/shapefile/sk_sub_tmin.tif")
r4 <- raster::stack("Data/shapefile/sk_sub_dayl.tif")
r5 <- raster::stack("Data/shapefile/sk_sub_srad.tif")
r6 <- raster::stack("Data/shapefile/sk_sub_swe.tif")
r7 <- raster::stack("Data/shapefile/sk_sub_vp.tif")

library(raster)

# reproject to lat/lon for easier visuals
r1 <- raster::projectRaster(r1, crs = "+init=epsg:4326")
r2 <- raster::projectRaster(r2, crs = "+init=epsg:4326")
r3 <- raster::projectRaster(r3, crs = "+init=epsg:4326")
r4 <- raster::projectRaster(r4, crs = "+init=epsg:4326")
r5 <- raster::projectRaster(r5, crs = "+init=epsg:4326")
r6 <- raster::projectRaster(r6, crs = "+init=epsg:4326")
r7 <- raster::projectRaster(r7, crs = "+init=epsg:4326")


prcp <- as.data.frame(r1, xy=TRUE)
names(prcp)[3:367] <- paste0("DOY-", 1:365)
tmax <- as.data.frame(r2, xy=TRUE)
names(tmax)[3:367] <- paste0("DOY-", 1:365)
tmin <- as.data.frame(r3, xy=TRUE)
names(tmin)[3:367] <- paste0("DOY-", 1:365)
dayl <- as.data.frame(r4, xy=TRUE)
names(dayl)[3:367] <- paste0("DOY-", 1:365)
srad <- as.data.frame(r5, xy=TRUE)
names(srad)[3:367] <- paste0("DOY-", 1:365)
swe <- as.data.frame(r6, xy=TRUE)
names(swe)[3:367] <- paste0("DOY-", 1:365)

write_csv(prcp, "Data/prcp_2023.csv")
write_csv(tmax, "Data/tmax_2023.csv")
write_csv(tmin, "Data/tmin_2023.csv")
write_csv(srad, "Data/srad_2023.csv")
write_csv(dayl, "Data/dayl_2023.csv")

########################################

#PRCP
prcp <- read_csv("Data/prcp_2023.csv")
col <- c(paste0("DOY-",1:365))
prcp <- prcp %>% filter(if_all(c(col), ~ !is.na(.)))
prcp <- prcp%>%
  mutate(across("DOY-1":"DOY-365", ~ ifelse(.x < 0, 0, .x)))
csd <- st_read("./Data/shapefile/sk_soil_sub.shp")%>%
  dplyr::select(GeoUID)
prcp <- st_as_sf(prcp, coords = c("x", "y"), crs = 4326)
st_crs(csd) <- st_crs(prcp)
prcp <- st_join(prcp,csd)
prcp <- prcp[!is.na(prcp$GeoUID),]
prcp <- prcp%>%
  dplyr::select("DOY-130":"DOY-250",GeoUID)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)
prcp$GeoUID <- as.numeric(prcp$GeoUID)
prcp <- prcp%>%
  pivot_longer(!GeoUID, names_to = "DOY", values_to = "PRCP")%>%
  group_by(GeoUID,DOY)%>%
  mutate(Avg_prcp = mean(PRCP))%>%
  distinct(GeoUID, .keep_all = T)%>%
  dplyr::select(-PRCP)


#TMAX
tmax <- read_csv("Data/tmax_2023.csv")
col <- c(paste0("DOY-",1:365))
tmax  <- tmax  %>% filter(if_all(c(col), ~ !is.na(.)))
tmax  <- tmax%>%
  mutate(across("DOY-1":"DOY-365", ~ ifelse(.x < 0, 0, .x)))
tmax <- st_as_sf(tmax, coords = c("x", "y"), crs = 4326)
st_crs(csd) <- st_crs(tmax)
tmax <- st_join(tmax,csd)
tmax <- tmax[!is.na(tmax$GeoUID),]
tmax <- tmax%>%
  dplyr::select("DOY-130":"DOY-250",GeoUID)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)
tmax$GeoUID <- as.numeric(tmax$GeoUID)
tmax <- tmax%>%
  pivot_longer(!GeoUID, names_to = "DOY", values_to = "TMAX")%>%
  group_by(GeoUID,DOY)%>%
  mutate(Avg_tmax = mean(TMAX))%>%
  distinct(GeoUID, .keep_all = T)%>%
  dplyr::select(-TMAX)

#TMIN
tmin <- read_csv("Data/tmin_2023.csv")
col <- c(paste0("DOY-",1:365))
tmin  <- tmin  %>% filter(if_all(c(col), ~ !is.na(.)))
tmin  <- tmin%>%
  mutate(across("DOY-1":"DOY-365", ~ ifelse(.x < 0, 0, .x)))
tmin <- st_as_sf(tmin, coords = c("x", "y"), crs = 4326)
st_crs(csd) <- st_crs(tmin)
tmin <- st_join(tmin,csd)
tmin <- tmin[!is.na(tmin$GeoUID),]
tmin <- tmin%>%
  dplyr::select("DOY-130":"DOY-250",GeoUID)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)
tmin$GeoUID <- as.numeric(tmin$GeoUID)
tmin <- tmin%>%
  pivot_longer(!GeoUID, names_to = "DOY", values_to = "TMIN")%>%
  group_by(GeoUID,DOY)%>%
  mutate(Avg_tmin = mean(TMIN))%>%
  distinct(GeoUID, .keep_all = T)%>%
  dplyr::select(-TMIN)


#SRAD
srad <- read_csv("Data/srad_2023.csv")
col <- c(paste0("DOY-",1:365))
srad  <- srad  %>% filter(if_all(c(col), ~ !is.na(.)))
srad  <- srad%>%
  mutate(across("DOY-1":"DOY-365", ~ ifelse(.x < 0, 0, .x)))
srad <- st_as_sf(srad, coords = c("x", "y"), crs = 4326)
st_crs(csd) <- st_crs(srad)
srad <- st_join(srad,csd)
srad <- srad[!is.na(srad$GeoUID),]
srad <- srad%>%
  dplyr::select("DOY-130":"DOY-250",GeoUID)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)
srad$GeoUID <- as.numeric(srad$GeoUID)
srad <- srad%>%
  pivot_longer(!GeoUID, names_to = "DOY", values_to = "SRAD")%>%
  group_by(GeoUID,DOY)%>%
  mutate(Avg_srad = mean(SRAD))%>%
  distinct(GeoUID, .keep_all = T)%>%
  dplyr::select(-SRAD)



#DAYL
dayl <- read_csv("Data/dayl_2023.csv")
col <- c(paste0("DOY-",1:365))
dayl  <- dayl  %>% filter(if_all(c(col), ~ !is.na(.)))
dayl  <- dayl%>%
  mutate(across("DOY-1":"DOY-365", ~ ifelse(.x < 0, 0, .x)))
dayl <- st_as_sf(dayl, coords = c("x", "y"), crs = 4326)
st_crs(csd) <- st_crs(dayl)
dayl <- st_join(dayl,csd)
dayl <- dayl[!is.na(dayl$GeoUID),]
dayl <- dayl%>%
  dplyr::select("DOY-130":"DOY-250",GeoUID)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)
dayl$GeoUID <- as.numeric(dayl$GeoUID)
dayl <- dayl%>%
  pivot_longer(!GeoUID, names_to = "DOY", values_to = "DAYL")%>%
  group_by(GeoUID,DOY)%>%
  mutate(Avg_dayl = mean(DAYL))%>%
  distinct(GeoUID, .keep_all = T)%>%
  dplyr::select(-DAYL)


df <- prcp%>%
  left_join(tmax, by = c("GeoUID","DOY"))%>%
  left_join(tmin, by = c("GeoUID","DOY"))%>%
  left_join(srad, by = c("GeoUID","DOY"))%>%
  left_join(dayl, by = c("GeoUID","DOY"))%>%
  mutate(ra = Avg_srad*Avg_dayl/1000000)
  
csd$GeoUID <- as.numeric(csd$GeoUID)

df_final <- csd%>%
  left_join(df)

t <- st_centroid(csd)%>%
  as.data.frame()
t$geometry <- as.character(t$geometry)
t <- t%>%
  mutate(lat = substring(geometry, 21,26))%>%
  dplyr::select(-geometry)
t$lat <- as.numeric(t$lat)

df_final <- df_final%>%
  left_join(t)


df_final$PET<- hargreaves(df_final$Avg_tmin, df_final$Avg_tmax, Ra = df_final$ra, lat =df_final$lat, Pre = df_final$Avg_prcp, na.rm = T,
                            verbose=TRUE)

# Canola
#initial stage
kc_1 <- df_final%>%
  dplyr::select(DOY)%>%
  distinct(DOY, .keep_all = T)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)%>%
  slice(1:15)%>%
  mutate(k_c = 0.3)

#second stage
kc_2 <- df_final%>%
  dplyr::select(DOY)%>%
  distinct(DOY, .keep_all = T)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)%>%
  slice(16:40)%>%
  mutate(k_c = 0.7)

#third stage
kc_3 <- df_final%>%
  dplyr::select(DOY)%>%
  distinct(DOY, .keep_all = T)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)%>%
  slice(41:90)%>%
  mutate(k_c = 1.05)

#fourth stage
kc_4 <- df_final%>%
  dplyr::select(DOY)%>%
  distinct(DOY, .keep_all = T)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)%>%
  slice(91:120)%>%
  mutate(k_c = 0.6)



kc <- rbind(kc_1,kc_2,kc_3,kc_4)


df_final <- df_final%>%
  left_join(kc)%>%
  mutate(ET_c = PET*k_c)%>%
  mutate(net_irri_req = ET_c - Avg_prcp)

  
st_write(df_final, "Data/shapefile/season_summary_2023.shp", delete_dsn = T)


df_value <- df_final%>%
  group_by(GeoUID)%>%
  mutate(tot_irri_req_mm = sum(net_irri_req, na.rm = T))%>%
  mutate(ave_pecp = mean(Avg_prcp))%>%
  mutate(ave_tmax = mean(Avg_tmax))%>%
  mutate(ave_tmin = mean(Avg_tmin))%>%
  mutate(ave_pet = mean(PET))%>%
  mutate(ave_etc = mean(ET_c))%>%
  mutate(test = ave_etc - ave_pecp)%>%
  dplyr::select(GeoUID,tot_irri_req_mm,ave_pecp,ave_tmax,ave_tmin,ave_pet,ave_etc)%>%
  mutate(ip_vc = 689)%>% # irrigation premium over variable cost for acre
  mutate(ip_tc = 67)%>% # irrigation premium over total cost for acre
  mutate(ip_vc_permm=ip_vc/tot_irri_req_mm)%>% # net profit in irrigated land over dry land from reports
  mutate(ip_tc_permm=ip_tc/tot_irri_req_mm)%>% # net profit in irrigated land over dry land from reports
  distinct(GeoUID, .keep_all = T)



st_write(df_value, "Data/shapefile/valuemap_2023.shp", delete_dsn = T)


mean(df_value$ip_vc_permm, na.rm = T)
mean(df_value$ip_tc_permm, na.rm = T)


  
  
  
  
######################################################################################################################################
######################################################################################################################################
jan_prcp$PET_penman <- penman(jan_prcp$season_mean_tmin, jan_prcp$Jan_tmax_mean, jan_prcp$u ,Ra = jan_prcp$ra, lat = jan_prcp$y, Rs = NULL, tsun = NULL,
                              CC = NULL, ed = NULL, Tdew = NULL, RH = NULL, P = NULL, P0 = NULL,
                              CO2 = NULL, z = NULL, crop='short', na.rm = TRUE, method='ICID',
                              verbose=TRUE)

my_sf <- st_as_sf(df, coords = c("x", "y"), crs = 4326)


pet <- my_sf%>%
  select(PET)

st_write(pet, "Data/shapefile/season_summary_pet_2022.shp", delete_dsn = T)


st_write(my_sf, "Data/shapefile/season_summary_2022.shp", delete_dsn = T)


my_sf <- st_as_sf(df, coords = c("x", "y"), crs = 4326)

st_write(my_sf, "Data/shapefile/sk_sub_ave_2000.shp", delete_dsn = T)


soil <- st_read("Data/shapefile/sas_soilzone.shp")

brown_soil <- soil%>%
  subset(ZONE_EN=="Brown")

black_soil <- soil%>%
  subset(ZONE_EN=="Black")

grey_soil <- soil%>%
  subset(ZONE_EN=="Grey")

darkgrey_soil <- soil%>%
  subset(ZONE_EN=="Dark Grey")

darkbrown_soil <- soil%>%
  subset(ZONE_EN=="Dark Brown")

brown_soil_p <- st_intersection(my_sf, brown_soil)



st_write(brown_soil_p, "Data/shapefile/brownsoil_p.shp")
