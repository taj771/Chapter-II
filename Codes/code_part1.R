# Code part 1 - Download Daymet data across the study area from 2018-2023 and
# Create weathe data file for AquaCrop-OS

#clear memory
rm(list = ls())

# Load required packages
if (!require(sf)) {
  install.packages("sf")
}

# Load sf package for spatial operations
library(sf)
library(tidyverse)
library(dplyr)

# Load the shapefile
shapefile <- st_read("./Data Main Analysis/shapefiles/MergeLakeDiefenbaker.shp")

# Fix invalid geometries in the shapefile
shapefile <- st_make_valid(shapefile)

# Check the CRS of the shapefile
st_crs(shapefile)

# If CRS is not properly set, you might need to define it or reproject it.
# Set CRS if missing (example EPSG:4326 for WGS84 latitude/longitude)
if (is.na(st_crs(shapefile))) {
  shapefile <- st_set_crs(shapefile, 4326)  # WGS84 (EPSG:4326)
}

# If necessary, reproject to a projected CRS (e.g., UTM for your region)
shapefile <- st_transform(shapefile, crs = 32613)  # Example: UTM zone 13N (EPSG:32613)



# Check and ensure the CRS of the shapefile is appropriate
st_crs(shapefile)

# Define the cell size for the grid
cell_size <- 5000  # Adjust to a smaller cell size for visibility #10km resolution

# Create the fishnet grid with the same CRS as the shapefile
grid <- st_make_grid(
  shapefile,                   
  cellsize = cell_size,         
  square = TRUE,                
  crs = st_crs(shapefile)       # Ensure CRS matches shapefile
)

# Clip the grid to the shapefile boundaries
fishnet_clipped <- st_intersection(grid, shapefile, sparse = FALSE)


# Plot the shapefile
plot(st_geometry(shapefile), col = "lightblue", border = "darkblue", main = "Shapefile with Fishnet Grid")

# Add the grid to the plot
plot(st_geometry(fishnet_clipped), add = TRUE, col = NA, border = "red")

# Optionally save the fishnet grid as a shapefile
st_write(fishnet_clipped, "./Data Main Analysis/shapefiles/MergeLakeDiefenbakerfishnet5Km.shp", delete_dsn = T)




# Calculate centroids of each grid cell
centroids <- st_centroid(fishnet_clipped)


centroids_wgs84 <- st_transform(centroids, crs = 4326)


# Extract coordinates as a matrix and convert to data frame
centroids_wgs84_df <- st_coordinates(centroids_wgs84)
centroids_wgs84_df <- as.data.frame(centroids_wgs84_df)


# Create a data frame with grid cell ID, longitude, and latitude
grid_points_wgs84 <- data.frame(
  grid_id = seq_len(nrow(centroids_wgs84_df)),  # Create unique ID for each grid cell
  lon = centroids_wgs84_df[, 1],           # Longitude coordinates
  lat = centroids_wgs84_df[, 2]             # Latitude coordinates
)


write_csv(grid_points_wgs84, "./Data Main Analysis/shapefiles/MergeLakeDiefenbakerfishnet5Kmpoints.csv")


# Convert to sf object (assuming 'longitude' and 'latitude' columns exist)
grid_points_sf <- st_as_sf(grid_points_wgs84, coords = c("lon", "lat"), crs = 4326)

st_write(grid_points_sf, "./Data Main Analysis/shapefiles/MergeLakeDiefenbakerfishnet5Kmpoints.shp", delete_dsn = T)


###################### Daymet ##########################

# Install the daymetr package if you don't have it
if (!require(daymetr)) {
  install.packages("daymetr")
}

# Load the daymetr package
library(daymetr)

# Load necessary libraries
library(readr)
# Load the package that contains the download_daymet function
# library(daymet)  # Uncomment if needed

# Read the CSV file
# Load longitude and latitude data from a CSV file
locations <- read.csv("./maps/LakeDiefenbaker/MergeLakeDiefenbakerfishnet5Kmpoints.csv")%>%
  dplyr::select(grid_id, lat, lon)%>%
  rename("site" = "grid_id")



# Define the time period
start_year <- 2018  # Replace with your start year
end_year <- 2023  # Replace with your end year

# Create a list to store all data
weather_data_list <- vector("list", nrow(locations))

# Loop through each location and download Daymet data
for (i in 1:nrow(locations)) {
  weather_data <- tryCatch({
    download_daymet(
      site = locations$site[i],
      lat = locations$lat[i],
      lon = locations$lon[i],
      start = start_year,
      end = end_year,
      internal = TRUE
    )
  }, error = function(e) {
    message(sprintf("Error downloading data for site %s: %s", locations$site[i], e$message))
    return(NULL)
  })
  
  if (!is.null(weather_data)) {
    # Add longitude, latitude, and site information as new columns
    weather_data_df <- weather_data$data
    weather_data_df$latitude <- locations$lat[i]
    weather_data_df$longitude <- locations$lon[i]
    weather_data_df$site <- locations$site[i]
    
    # Store the result in the list
    weather_data_list[[i]] <- weather_data_df
  }
}

# Combine the list into a single data frame
all_weather_data <- do.call(rbind, weather_data_list)

# Check if data was downloaded
if (nrow(all_weather_data) == 0) {
  message("No weather data was downloaded.")
} else {
  print(head(all_weather_data))
}



weather_data_final <- all_weather_data%>%
  mutate(
    Date = as.Date(yday, origin = paste0(year - 1, "-12-31")), # Convert to Date format
    #Year = year(date),
    #Month = month(date),
    #Day = day(date)
  ) %>%
  mutate(R_s=(srad..W.m.2.*dayl..s.)/1000000)%>%
  mutate(e_a = vp..Pa./1000)%>%
  mutate(R_n = R_s*0.15)%>%
  rename(MaxTemp = tmax..deg.c.,
         MinTemp = tmin..deg.c.,
         Precipitation = prcp..mm.day.)%>%
  select(site,Date,MinTemp,MaxTemp,Precipitation,R_s,R_n,e_a)



#write csv
write.csv(weather_data_final, "./AquaCropOPSyData/ClimateData/weather_data_Aqua.csv", row.names = FALSE)
write.csv(weather_data_final, "./Data Main Analysis/weather_data_Aqua.csv", row.names = FALSE)



################################################################################

year_2018 <- all_weather_data %>%
  filter(year == 2019) %>%
  mutate(
    R_s = (srad..W.m.2. * dayl..s.) / 1000000,  # Daily radiation (MJ/m²/day)
) %>%
  mutate(T = (tmax..deg.c. + tmin..deg.c.) / 2,   # Calculate mean temperature
         ETt = 0.000816 * (tmax..deg.c. - tmin..deg.c.)^0.5 * (T + 24.4) * R_s)%>%  # Compute ETHt
  rename(Tmax = tmax..deg.c., Tmin = tmin..deg.c.)

year_2018 <- year_2018%>%
  mutate(
    date = as.Date(yday, origin = paste0(year - 1, "-12-31")), # Convert to Date format
    Year = year(date),
    Month = month(date),
    Day = day(date)
  ) %>%
  rename(`Tmax(c)` = Tmax,
         `Tmin(c)` = Tmin,
         `Prcp(mm)` = prcp..mm.day.,
         `ETt(mm)`= ETt)%>%
  select(site,Day,Month,Year,`Tmin(c)`,`Tmax(c)`,`Prcp(mm)`,`ETt(mm)`)




year_2018_s1 <- year_2018%>%
  filter(site==200)%>%
  select(-site)

write.table(year_2018_s1, 
            file = "./AquaCropOPSyData/ClimateData/weather_data_Aqua_site200.txt", 
            row.names = FALSE, 
            col.names = TRUE, 
            quote = FALSE, 
            sep = " ")



# Constants for the Penman-Monteith equation
gamma <- 0.066      # Psychrometric constant (kPa/°C)
G <- 0              # Soil heat flux density (MJ/m²/day), typically negligible
u2 <- 2             # Wind speed at 2m height (m/s), assumed constant

Tmean <- (year_2018$Tmax + year_2018$Tmin) / 2  # Mean temperature (°C)

# Calculate slope of the saturation vapor pressure curve (delta)
delta <- (4098 * year_2018$es) / ((Tmean + 237.3) ^ 2)

# Calculate Reference Evapotranspiration (ET₀) using Penman-Monteith equation
year_2018 <- year_2018 %>%
  mutate(ET0 = (0.408 * delta * (R_s - G) + gamma * (900 / (Tmean + 273)) * u2 * (year_2018$es - year_2018$ea)) /
           (delta + gamma * (1 + 0.34 * u2)))


year_2018 <- year_2018%>%
  mutate(
    date = as.Date(yday, origin = paste0(year - 1, "-12-31")), # Convert to Date format
    Year = year(date),
    Month = month(date),
    Day = day(date)
  ) %>%
  rename(`Tmax(c)` = Tmax,
         `Tmin(c)` = Tmin,
         `Prcp(mm)` = prcp..mm.day.,
         `ET0(mm)`= ET0)%>%
  select(site,Day,Month,Year,`Tmin(c)`,`Tmax(c)`,`Prcp(mm)`,`ET0(mm)`)




year_2018_s1 <- year_2018%>%
  filter(site==200)%>%
  select(-site)

write.table(year_2018_s1, 
            file = "./AquaCropOPSyData/ClimateData/weather_data_Aqua_site200.txt", 
            row.names = FALSE, 
            col.names = TRUE, 
            quote = FALSE, 
            sep = " ")




# Create shapefile 

# Convert to sf object with WGS84 CRS (EPSG:4326)
all_weather_data_sf <- st_as_sf(all_weather_data, coords = c("longitude", "latitude"), crs = 4326)




study_region <- st_read("./maps/LakeDiefenbaker/MergeLakeDiefenbaker.shp")

study_region_1 <- study_region%>%
  filter(CSD_UID==4707058)


clipped_points <- st_filter(all_weather_data_sf, study_region_1)


ggplot() +
  # Polygon layer (e.g., Saskatchewan boundary)
  geom_sf(data = study_region_1, fill = "lightblue", color = "darkblue", alpha = 0.5) +  
  # Points layer (weather stations)
  geom_sf(data = clipped_points, color = "red", size = 2) +  
  theme_minimal() +
  ggtitle("Weather Stations in Saskatchewan with Boundary")  # Add a title to the plot











# calculation of daily total radiation (MJ/m2/day)

weather_data_final <-  all_weather_data%>%
  mutate(R_s=(srad..W.m.2.*dayl..s.)/1000000)%>%
  rename(T_max=tmax..deg.c.,
         T_min=tmin..deg.c.,
         Precipitation=prcp..mm.day.)%>%
  mutate(R_a = srad..W.m.2.*0.0864)%>%
  mutate(e_hPa = vp..Pa./ 100 )%>%
  mutate(T_mean = (T_max + T_min) / 2)%>%
  mutate(e_s = 6.107* exp((17.27*T_mean)/(T_mean+237.3)))%>% #Saturation vapor pressure - Tetens equation
  mutate(RH = (e_hPa/e_s)*100)


calculate_ET0 <- function(T_max, T_min, R_s, RH, lat) {
  # T_max and T_min are in Celsius, R_s in MJ/m2/day, and RH is in percentage
  
  # Step 1: Calculate the temperature difference (T_max - T_min)
  T_diff <- T_max - T_min
  
  # Step 2: Calculate ET0 using the Modified Penman method
  # Assuming wind speed is negligible (wind speed = 0), thus the second part of the formula is simplified
  ET0 <- (0.408 * R_s * T_diff) / (T_diff + 0.7 * T_diff)
  
  # Return the calculated ET0 value
  return(ET0)
}
# Step 4: Mutate ET0 Column in the Dataframe

df <- weather_data_final %>%
  mutate(ET0 = mapply(calculate_ET0, T_max, T_min, R_s, RH, latitude))


weather_data_final <- df%>%
  mutate(
  date = as.Date(yday, origin = paste0(year - 1, "-12-31")), # Convert to Date format
  Year = year(date),
  Month = month(date),
  Day = day(date)
) %>%
  rename(MaxTemp=T_max,
         MinTemp=T_min,
         ReferenceET = ET0)%>%
  select(site,Year,Month,Day,daylength,MaxTemp,MinTemp,ReferenceET,Precipitation)


#write csv
write.csv(weather_data_final, "./AquaCropOPSyData/ClimateData/weather_data_Aqua.csv", row.names = FALSE)


###########################################


weather_data_final <- read.csv("./AquaCropOPSyData/ClimateData/weather_data_Aqua.csv")%>%
  filter(Year == 2018)







