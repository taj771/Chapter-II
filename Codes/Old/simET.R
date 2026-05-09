# clear memory
rm(list = ls())
library(dplyr)
library(simET)
library(rgdal)
library(dplyr)
library(sf)
library(terra)
library(sp)
library(SPEI)
library(dplyr)
library(tidyr)
library(tidyverse)
library(daymetr)



df_weather <- read.csv("./Data/Weather data/weatherstats_saskatoon.csv")%>%
  dplyr::select(date,max_temperature_v,min_temperature_v,precipitation_v)%>%
  dplyr::filter(grepl('2023', date))%>%
  mutate( Latitude=52.17)%>%
  mutate(Longitude=106.7)%>%
  mutate(Altitude=504)%>%
  arrange(desc(row_number()))%>%
  mutate(julian = row_number())%>%
  mutate(date=2023)

df_wind_rh <- read.csv("./Data/Weather data/weatherstats_saskatoon_hourly.csv")%>%
  dplyr::select(date_time_local,wind_speed,relative_humidity)%>%
  dplyr::filter(grepl('2023', date_time_local))%>%
  mutate(date = substring(date_time_local, first=1, last=10))%>%
  dplyr::select(-date_time_local)%>%
  group_by()%>%
  mutate(mu_wind = mean(wind_speed),
         mu_rh = mean(relative_humidity))%>%
  distinct(date, .keep_all = T)%>%
  arrange(desc(row_number()))%>%
  mutate(julian = row_number())%>%
  mutate(date=2023)
  


temp_daymet <- download_daymet(
  lat = 52.17,
  lon = -106.7,
  start = 2023,
  end = 2023
) 

temp_daymet_data <- temp_daymet$data

daymet_ra_2023 <- temp_daymet_data%>%
  filter(year == "2023")%>%
  mutate(ra=srad..W.m.2.*dayl..s./1000000)%>%
  dplyr::select(yday,ra)%>%
  rename("julian" = "yday")


df_final <- df_weather%>%
  left_join(df_wind_rh)%>%
  left_join(daymet_ra_2023)%>%
  #calculating reference ET0
  mutate(ET0 = cal_Rs_from_Na(as = 0.25, bs = 0.5, Na =ra , Latitude, J=julian))%>%
  #calculating GDD
  mutate(base_T = 5)%>%
  mutate(day_ave_T = (max_temperature_v+min_temperature_v)/2)%>%
  mutate(GDD = day_ave_T - base_T)%>%
  # lets say growing season as May - september 
  # filter data for May to Sep
  subset(julian > 120 & julian < 304)%>%
  mutate(GDD = cumsum(GDD))%>%
  mutate(stage = case_when(GDD < 200 ~ "ini",
                           GDD > 201 & GDD < 650 ~ "mid",
                           GDD > 651 & GDD < 1000 ~ "mid 2",
                           ))%>%
  mutate(kc = case_when(stage == "ini" ~ 0.3,
                           stage == "mid" ~ 1.15,
                           stage == "mid 2" ~ 1
  ))%>%
  mutate(ETC = kc*ET0)
  






  
