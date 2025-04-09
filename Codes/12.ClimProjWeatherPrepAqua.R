# srad is missing in ClimateCanada so I assume it reamins constant based on historical average

# so first derive the historiacl average of srad based on Daymet data

library(dplyr)
library(tidyverse)
library(sf)


Daymet <- read_csv("./AquaCropOPSyData/ClimateData/weather_data_Aqua.csv")%>%
  select(Month,Day,daylength)%>%
  group_by(Month,Day)%>%
  mutate(ave_daylength = mean(daylength))%>%
  distinct(Month, Day, .keep_all = T)%>%
  select(-daylength)%>%
  rename(daylength=ave_daylength)

# Load CMIP projection files - Tmax/min abd Prcp 
CMIP <- st_read("./AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP6All.shp")


# Ensure the shapefile is in WGS 84 (longitude and latitude) coordinate system (EPSG: 4326)
CMIP <- st_transform(CMIP, crs = 4326)

# Extract longitude and latitude coordinates and create new columns
CMIP <- CMIP %>%
  mutate(
    lon = st_coordinates(st_centroid(geometry))[, 1],  # Extracts longitude (X)
    lat = st_coordinates(st_centroid(geometry))[, 2]   # Extracts latitude (Y)
  )%>%
  as.data.frame()%>%
  select(-geometry)%>%
  mutate(
    Year = year(date),
    Month = sprintf("%02d", month(date)),  # Formats month as two digits
    Day = sprintf("%02d", day(date))       # Formats day as two digits
  )

CMIP$Day <- as.character(CMIP$Day)
CMIP$Month <- as.character(CMIP$Month)


CMIP_all <- CMIP%>%
  left_join(Daymet, by = c("Month", "Day"))

CMIP126 <- CMIP_all%>%
  select(site,tminssp126,tmaxssp126,prcpssp126,daylength,date,Year,Month,Day)%>%
  rename(Precipitation=prcpssp126,
         MaxTemp=tmaxssp126,
         MinTemp=tminssp126)%>%
  rename(Date=date)


write_csv(CMIP126, "./AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126.csv")

CMIP245 <- CMIP_all%>%
  select(site,tminssp245,tmaxssp245,prcpssp245,daylength,date,Year,Month,Day)%>%
  rename(Precipitation=prcpssp245,
         MaxTemp=tmaxssp245,
         MinTemp=tminssp245)%>%
  rename(Date=date)


write_csv(CMIP245, "./AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245.csv")

CMIP585 <- CMIP_all%>%
  select(site,tminssp585,tmaxssp585,prcpssp585,daylength,date,Year,Month,Day)%>%
  rename(Precipitation=prcpssp585,
         MaxTemp=tmaxssp585,
         MinTemp=tminssp585)%>%
  rename(Date=date)

write_csv(CMIP585, "./AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585.csv")
