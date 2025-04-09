# Load your polygon shapefile (modify the path as needed)
polygon_sf <- st_read("./maps/CSD_outlook_6B.shp")

# Check the CRS
if (st_crs(polygon_sf) != 4326) {
  # Transform to WGS 84 (EPSG:4326) if not in geographic coordinates
  polygon_sf <- st_transform(polygon_sf, crs = 4326)
}


distance <- 0.1  # Adjust this value as needed


create_points_within_polygon <- function(polygon, distance) {
  # Get the bounding box of the polygon
  bbox <- st_bbox(polygon)
  
  # Create sequences of x (longitude) and y (latitude) based on the distance
  x_coords <- seq(bbox["xmin"], bbox["xmax"], by = distance)
  y_coords <- seq(bbox["ymin"], bbox["ymax"], by = distance)
  
  # Create a grid of points
  grid_points <- expand.grid(Longitude = x_coords, Latitude = y_coords)
  
  # Convert to an sf object
  points_sf <- st_as_sf(grid_points, coords = c("Longitude", "Latitude"), crs = st_crs(polygon))
  
  # Filter points that are within the polygon
  points_in_polygon <- points_sf[polygon, ]  # Keep only points that fall within the polygon
  return(points_in_polygon)
}

# Generate points within the polygon
points_sf <- create_points_within_polygon(polygon_sf, distance)



library(ggplot2)

ggplot() +
  geom_sf(data = polygon_sf, fill = "lightblue", color = "black") +
  geom_sf(data = points_sf, color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Points within Polygon at Specified Distance")

library(lubridate)

df1 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.367547_lon-107.316256.csv")%>%
  select(-...1)%>%
  mutate(point =1)
df2 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.380513_lon-107.222271.csv")%>%
  select(-...1)%>%
  mutate(point=2)
df3 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.369208_lon-107.105026.csv")%>%
  select(-...1)%>%
  mutate(point=3)
df4 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.45743_lon-107.248535.csv")%>%
  select(-...1)%>%
  mutate(point=4)
df5 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.467911_lon-107.31617.csv")%>%
  select(-...1)%>%
  mutate(point=5)
df6 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.475182_lon-107.214546.csv")%>%
  select(-...1)%>%
  mutate(point=6)
df7 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.477962_lon-107.102966.csv")%>%
  select(-...1)%>%
  mutate(point=7)
df8 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.45743_lon-107.243042.csv")%>%
  select(-...1)%>%
  mutate(point=8)
df9 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.562452_lon-107.210598.csv")%>%
  select(-...1)%>%
  mutate(point=9)
df10 <- read_csv("./AquaCropOPSYData/ClimateData/6B/lat51.568321_lon-107.10537.csv")%>%
  select(-...1)%>%
  mutate(point=10)

df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)


df_final <- df%>%
  mutate(
    Day = day(Date),
    Month = month(Date),
    Year = year(Date)
  )%>%
  select(Day, Month, Year, everything())%>%
  group_by(Date)%>%
  mutate("Tmin(C)"=mean(MinTemp))%>%
  mutate("Tmax(C)"=mean(MaxTemp))%>%
  mutate("Prcp(mm)"=mean(Precipitation))%>%
  mutate("Et0(mm)"=mean(ReferenceET))%>%
  distinct(Date,.keep_all = T)%>%
  ungroup()%>%
  select(-Date,-MinTemp,-MaxTemp,-Precipitation,-ReferenceET,-point)
  
  
write.table(df_final, file = "./AquaCropOPSYData/ClimateData/6B/climate_6B.txt", sep = "\t", row.names = FALSE, quote = FALSE)




