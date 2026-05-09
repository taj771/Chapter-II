#clear memory
rm(list = ls())

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(sf)


# MinTemp
# Define the folder path
folder_path <- "AquaCropOPSyData/ClimateData/CMIP6/MinTemp/"

# List all CSV files in the folder with a common pattern (e.g., "common_name")
csv_files <- list.files(path = folder_path, pattern = "candcs_m6_subset_grid_point_dataset_.*\\.csv$", full.names = TRUE)

# Read each CSV file into a list of data frames
list_of_dataframes <- lapply(csv_files, read.csv)

# Optionally, combine all data frames into a single data frame if they have the same structure
MinTemp <- bind_rows(list_of_dataframes)%>%
  mutate(date = as.Date(time))%>%
  filter(year(date) > 2029)%>%
  select(lon,lat,date,tasmin_CanESM5_historical_ssp126_.degC.,tasmin_CanESM5_historical_ssp585_.degC.,tasmin_CanESM5_historical_ssp245_.degC.)

# Identify duplicates in MinTemp
MinTemp <- MinTemp %>%
  distinct(lon, lat, date, .keep_all = TRUE)

  
# MaxTemp
# Define the folder path
folder_path <- "AquaCropOPSyData/ClimateData/CMIP6/MaxTemp/"

# List all CSV files in the folder with a common pattern (e.g., "common_name")
csv_files <- list.files(path = folder_path, pattern = "candcs_m6_subset_grid_point_dataset_.*\\.csv$", full.names = TRUE)

# Read each CSV file into a list of data frames
list_of_dataframes <- lapply(csv_files, read.csv)

# Optionally, combine all data frames into a single data frame if they have the same structure
MaxTemp <- bind_rows(list_of_dataframes)%>%
  mutate(date = as.Date(time))%>%
  filter(year(date) > 2029)%>%
  select(lon,lat,date,tasmax_CanESM5_historical_ssp126_.degC.,tasmax_CanESM5_historical_ssp585_.degC.,tasmax_CanESM5_historical_ssp245_.degC.)


# Identify duplicates in MinTemp
MaxTemp <- MaxTemp %>%
  distinct(lon, lat, date, .keep_all = TRUE)



# Prcp
# Define the folder path
folder_path <- "AquaCropOPSyData/ClimateData/CMIP6/Prcp/"

# List all CSV files in the folder with a common pattern (e.g., "common_name")
csv_files <- list.files(path = folder_path, pattern = "candcs_m6_subset_grid_point_dataset_.*\\.csv$", full.names = TRUE)

# Read each CSV file into a list of data frames
list_of_dataframes <- lapply(csv_files, read.csv)

# Optionally, combine all data frames into a single data frame if they have the same structure
Prcp <- bind_rows(list_of_dataframes)%>%
  mutate(date = as.Date(time))%>%
  filter(year(date) > 2029)%>%
  select(lon,lat,date,pr_CanESM5_historical_ssp126_.kg.m.2.d.1.,pr_CanESM5_historical_ssp585_.kg.m.2.d.1.,pr_CanESM5_historical_ssp245_.kg.m.2.d.1.)

# Identify duplicates in Prcp
Prcp <- Prcp %>%
  distinct(lon, lat, date, .keep_all = TRUE)



CMIP6All <- MinTemp%>%
  left_join(MaxTemp, by = c("lon", "lat", "date"))%>%
  left_join(Prcp, by = c("lon", "lat", "date"))%>%
  group_by(lon,lat)%>%
  mutate(site = cur_group_id()) %>%
  ungroup()%>%
  rename(tminssp126=tasmin_CanESM5_historical_ssp126_.degC.,
         tminssp585=tasmin_CanESM5_historical_ssp585_.degC.,
         tminssp245=tasmin_CanESM5_historical_ssp245_.degC.,
         tmaxssp126=tasmax_CanESM5_historical_ssp126_.degC.,
         tmaxssp585=tasmax_CanESM5_historical_ssp585_.degC.,
         tmaxssp245=tasmax_CanESM5_historical_ssp245_.degC.,
         prcpssp126=pr_CanESM5_historical_ssp126_.kg.m.2.d.1.,
         prcpssp585=pr_CanESM5_historical_ssp585_.kg.m.2.d.1.,
         prcpssp245=pr_CanESM5_historical_ssp245_.kg.m.2.d.1.)%>%
  mutate(year = year(date)) %>%             # Extract the year
  filter(year %in% c(2030, 2031, 2032, 2033,2034, 2035,
                     2036, 2037, 2038, 2039, 2040,
                     2041, 2042, 2043, 2044, 2045,
                     2046, 2047, 2048, 2049, 2050))%>%
  select(-year)



# Convert to sf object with Longitude and Latitude
CMIP6All <- st_as_sf(CMIP6All, coords = c("lon", "lat"), crs = 4326)


st_write(CMIP6All, "./AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP6All.shp", delete_dsn = T)


# map points with 5km grids - overlaping points or closeset point as CMIP data at 10kmX10km

# Load the shapefiles (replace with your file paths)
sf_data <- st_read("./AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP6All.shp")


# Check the current CRS
print(st_crs(sf_data))

# Transform the CRS to WGS84 if it’s not already in longitude/latitude
if (st_crs(sf_data)$epsg != 4326) {
  sf_data <- st_transform(sf_data, crs = 4326)  # WGS84
}

# Extract longitude and latitude
coordinates <- st_coordinates(sf_data)

# Create new columns for longitude and latitude
sf_data <- sf_data %>%
  mutate(
    longitude = coordinates[, 1],  # X is longitude
    latitude = coordinates[, 2]    # Y is latitude
  )

sf_data <- sf_data%>%
  as.data.frame()%>%
  select(-geometry)%>%
  distinct(site, .keep_all = T)%>%
  select(site,longitude,latitude)


points <- st_as_sf(sf_data, coords = c("longitude", "latitude"), crs = 4326)


# Load polygon and point shapefiles (replace with your actual file paths)
polygons <- st_read("./maps/LakeDiefenbaker/MergeLakeDiefenbakerfishnet5Km.shp")

# Check CRS and transform if necessary to ensure both are in the same CRS
if (st_crs(polygons) != st_crs(points)) {
  points <- st_transform(points, st_crs(polygons))
}

# Find nearest point for each polygon
nearest_points <- st_nearest_feature(polygons, points)

# Join the attributes of the nearest points to each polygon
# 'nearest_points' provides indices of the closest points for each polygon
polygons_with_points <- polygons %>%
  mutate(
    nearest_site = points$site[nearest_points]
  ) 

# Now, polygons_with_points contains the polygon data and the attributes of the nearest point




