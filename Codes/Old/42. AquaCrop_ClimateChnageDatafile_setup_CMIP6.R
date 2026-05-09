#clear memory
rm(list = ls())

df_climate <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/weather_data_Aqua.csv')%>%
  select(site,R_s,R_n,e_a)%>%
  group_by(site)%>%
  mutate(R_s = mean(R_s),
         R_n = mean(R_n),
         e_a = mean(e_a))%>%
    ungroup()%>%
    distinct(site,.keep_all = T)


locations <- read.csv("./maps/LakeDiefenbaker/MergeLakeDiefenbakerfishnet5Kmpoints.csv")%>%
  select(grid_id, lat, lon)%>%
  rename("site" = "grid_id")

df_climate <- df_climate%>%
  left_join(locations)%>%
  select(site,lon,lat)

df_climate <- st_as_sf(df_climate, coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = df_climate, color = "red", size = 3) +
  ggtitle("Visualization of Spatial Points") +
  theme_minimal()


CMIP6 <- st_read("./AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP6All.shp")%>%
  filter(year(date) == 2030)%>%
  distinct(site, .keep_all = T)%>%
  select(site)%>%
  rename(site_CMIP = site)


if (st_crs(df_climate) != st_crs(CMIP6)) {
  CMIP6 <- st_transform(CMIP6, st_crs(df_climate))
}


# Find the nearest point from sites_397 to each point in sites_140
nearest_index <- st_nearest_feature(CMIP6, df_climate)


# Extract the nearest points' data
merged_data <- CMIP6 %>%
  mutate(nearest_site_id = df_climate$site[nearest_index])%>%  # Assuming an ID column exists
  as.data.frame()%>%
  select(site_CMIP,nearest_site_id)



df_climate <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/weather_data_Aqua.csv')%>%
  select(Date,site,R_s,R_n,e_a)
  


 
df_climate <- df_climate %>% 
  filter(site %in% merged_data$nearest_site_id)%>%
  rename(nearest_site_id= site)%>%
  left_join(merged_data)%>%
  select(-nearest_site_id)%>%
  drop_na()%>%
  rename(site = site_CMIP)%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  group_by(month_day,site)%>%
  mutate(R_s = mean(R_s),
         R_n = mean(R_n),
         e_a = mean(e_a))%>%
  ungroup()%>%
  distinct(month_day,site, .keep_all = T)%>%
  select(-Date)

#CMIP 126
# original
CMIP_126 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)
  

write_csv(CMIP_126, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_ET.csv')

# 20%
CMIP_126_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_20redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_126_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_20redPrcp_ET.csv')

# 40%
CMIP_126_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_40redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_126_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_40redPrcp_ET.csv')

# 60%
CMIP_126_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_60redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_126_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_60redPrcp_ET.csv')


# 80%
CMIP_126_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_80redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_126_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_80redPrcp_ET.csv')









# CMIP 245

# original
CMIP_245 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)


write_csv(CMIP_245, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_ET.csv')


# 20%

CMIP_245_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_20redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_245_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_20redPrcp_ET.csv')


# 40%

CMIP_245_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_40redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_245_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_40redPrcp_ET.csv')


# 60%

CMIP_245_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_60redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_245_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_60redPrcp_ET.csv')


# 80%

CMIP_245_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_80redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_245_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_80redPrcp_ET.csv')



# CMIP 585

# original
CMIP_585 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)


write_csv(CMIP_585, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_ET.csv')

# 20%
CMIP_585_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_20redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_585_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_20redPrcp_ET.csv')

# 40%
CMIP_585_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_40redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_585_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_40redPrcp_ET.csv')


# 60%
CMIP_585_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_60redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_585_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_60redPrcp_ET.csv')

# 80%
CMIP_585_prcp_red <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_80redPrcp.csv')%>%
  mutate(month_day = format(Date, "%m-%d"))%>%
  left_join(df_climate, by = c("site","month_day"))%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)

write_csv(CMIP_585_prcp_red, '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_80redPrcp_ET.csv')





