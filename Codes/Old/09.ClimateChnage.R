library(dplyr)
library(tidyverse)

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
  select(grid_id, lat, lon)%>%
  rename("site" = "grid_id")



# Define the time period
start_year <- 1980  # Replace with your start year
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


library(lubridate)


df <- all_weather_data%>%
  filter(yday >= 121 & yday <= 304)%>%
  mutate(T_mean=(tmin..deg.c.+tmax..deg.c.)/2)%>%
  group_by(year,yday,latitude,longitude)%>%
  mutate(ave_Tmin = mean(tmin..deg.c.))%>%
  mutate(ave_Tmax = mean(tmax..deg.c.))%>%
  mutate(ave_Tmean =mean(T_mean ))%>%
  ungroup()%>%
  distinct(year, .keep_all = T)

  
  


ggplot(df, aes(x = year)) +
  geom_point(aes(y = ave_Tmin, color = "Min Temp"), alpha = 0.6) +
  geom_point(aes(y = ave_Tmax, color = "Max Temp"), alpha = 0.6) +
  geom_point(aes(y = ave_Tmean, color = "Ave Temp"), alpha = 0.6) +
  geom_smooth(aes(y = ave_Tmin, color = "Min Temp"), method = "loess", se = FALSE, size = 1) +
  geom_smooth(aes(y = ave_Tmax, color = "Max Temp"), method = "loess", se = FALSE, size = 1) +
  geom_smooth(aes(y = ave_Tmean, color = "Ave Temp"), method = "loess", se = FALSE, size = 1) +
  
  labs(title = "",
       x = "Year",
       y = "Temperature (°C)",
       color = "Temperature") +
  scale_x_continuous(breaks = seq(1980, 2023, by = 5)) +  # Define x-axis breaks, adjust as needed
  #scale_y_continuous(breaks = seq(0, max(df1$prcp, na.rm = TRUE), by = 0.2)) +  # Define y-axis breaks, adjust as needed
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),       # Optional: Remove panel border
    axis.line = element_line(color = "black", size = 0.5),  # Add main axes in black
    axis.ticks = element_line(color = "black", size = 0.5)  # Add ticks in black
  )



df1 <- all_weather_data%>%
  filter(yday >= 121 & yday <= 304)%>%
  group_by(year)%>%
  mutate(prcp = mean(prcp..mm.day.))%>%
  ungroup()%>%
  distinct(year, .keep_all = T)

ggplot(df1, aes(x = year)) +
  geom_point(aes(y = prcp, color = "Average Percipitation"), alpha = 0.6) +
  geom_smooth(aes(y = prcp, color = "Average Percipitation"), method = "loess", se = FALSE, size = 1) +
  
  labs(title = "",
       x = "Year",
       y = "Percipitation (mm)",
       color = "Average Percipitation") +
  scale_x_continuous(breaks = seq(1980, 2023, by = 5)) +  # Define x-axis breaks, adjust as needed
  scale_y_continuous(breaks = seq(0, max(df1$prcp, na.rm = TRUE), by = 0.2)) +  # Define y-axis breaks, adjust as needed
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),       # Optional: Remove panel border
    axis.line = element_line(color = "black", size = 0.5),  # Add main axes in black
    axis.ticks = element_line(color = "black", size = 0.5)  # Add ticks in black
  )




#clear memory
rm(list = ls())

# wheat
library(dplyr)
library(tidyverse)

df1 <- read.csv("./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2016.csv")%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*14.86995818)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))


df_wheat_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_wheat_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  filter(Year==2016)%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)


df1 <- df_wheat_rf%>%
  left_join(df1)


df2 <- read.csv("./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2017.csv")%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*14.86995818)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))

df_wheat_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_wheat_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  filter(Year %in% c(2017))%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)


df2 <- df_wheat_rf%>%
  left_join(df2)

df <- rbind(df1,df2)


return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")

return_wheat <- return%>%
  filter(crop =="wheat")

return_canola <- return%>%
  filter(crop =="canola")

wheat <- df%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  mutate(crop="wheat")
#select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)



#### Canola canoal

df1 <- read.csv("./AquaCropOPSyData/CanolaMarginal/merged_simulation_results_canola_marginal_2016.csv")%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*17.84394982)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))


df_canola_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_canola_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  filter(Year==2016)%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)


df1 <- df_canola_rf%>%
  left_join(df)

df2 <- read.csv("./AquaCropOPSyData/CanolaMarginal/merged_simulation_results_canola_marginal_2017.csv")%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*17.84394982)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))


df_canola_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_canola_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  filter(Year==2017)%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)


df2 <- df_canola_rf%>%
  left_join(df2)

df <- rbind(df1,df2)



canola <- df%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  mutate(crop="canola")
#select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)


df <- rbind(wheat,canola)%>%
  mutate(wheat = ifelse(crop == "wheat", 1, 0))%>%
  mutate(canola = ifelse(crop == "canola", 1, 0))%>%
  rename("site"="Site")



weather <- read_csv("./AquaCropOPSyData/ClimateData/weather_data_Aqua.csv")%>%
  filter(year %in% c(2016,2017))%>%
  filter(yday >= 121 & yday <= 304)%>%
  mutate(T_mean=(MaxTemp+MinTemp)/2)%>%
  group_by(year,site)%>%
  mutate(ave_prcp = sum(Precipitation))%>%
  mutate(ave_temp = mean(T_mean))%>%
  ungroup()%>%
  distinct(year,site, .keep_all = T)%>%
  select(year,site,ave_prcp,ave_temp)
  

df <- df%>%
  left_join(weather, by=c("year","site"))
  
library(fixest)

feols(val_mm ~ log(irrq_m3) + log(irrq_m3)*canola + ave_prcp, data = df)
