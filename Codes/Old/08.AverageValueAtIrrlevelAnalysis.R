#clear memory
rm(list = ls())

# wheat
library(dplyr)
library(tidyverse)
library(priceR)



# Set the directory where your CSV files are located
file_path <- "./AquaCropOPSyData/WheatMarginal"

# Get a list of all files that match the pattern
file_list <- list.files(path = file_path, pattern = "merged_simulation_results_wheat_marginal_\\d{4}\\.csv", full.names = TRUE)

# Load each file and add a "year" column
wheat_marginal <- map_dfr(file_list, ~ {
  year <- gsub(".*_(\\d{4})\\.csv$", "\\1", .x)  # Extract the year from the file name
  read_csv(.x) %>% 
    mutate(year = as.numeric(year))              # Add the year column as a numeric type
})%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*14.86995818)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))
  

df_wheat_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_wheat_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  filter(year %in% c(2016, 2017,2018,2019,2020,2021,2022,2023))%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*14.86995818)%>%
  select(year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)


wheat_marginal <- wheat_marginal%>%
  left_join(df_wheat_rf, by = c("year", "Site"))


##### Crop Return

return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")


dry_cost_ac <- return$dry_cost_ac
irri_cost_ac <- return$irri_cost_ac
price.bu <- return$price.bu
years <- return$year


return$dry_cost_ac <- adjust_for_inflation(dry_cost_ac, years, "CA", to_date = 2023)
return$irri_cost_ac <- adjust_for_inflation(irri_cost_ac, years, "CA", to_date = 2023)
return$price.bu <- adjust_for_inflation(price.bu, years, "CA", to_date = 2023)




return_wheat <- return%>%
  filter(crop =="wheat")

return_canola <- return%>%
  filter(crop =="canola")

wheat <- wheat_marginal%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)
  #select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)


ggplot(wheat, aes(x = as.factor(Max_Irrigation_mm), y = val_mm, fill = as.factor(year))) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.7),  # Adjust width to control spacing between box plots within each irrigation level
    width = 0.5,  # Width of each box plot
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "",
    x = "Irrigation Level (mm)",
    y = expression("Economic Value " ~ "($"*m^3*")"),
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(margin = margin(t = 20))  # Add margin to x-axis labels for spacing
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(-0.5, max(wheat$val_mm, na.rm = TRUE), by = 0.1)) +
  scale_x_discrete(labels = function(x) paste("", x))  # Customize x-axis labels for clarity


#### Alternative 

ggplot(wheat, aes(x = as.factor(Max_Irrigation_mm), y = val_mm)) +
  geom_jitter(aes(color = as.factor(year)), width = 0.2, size = 0.5, alpha = 0.7) +  # Points colored by year
  geom_boxplot(outlier.shape = NA, fill = "lightgrey", color = "black", alpha = 0, width = 0.5) +
  geom_smooth(aes(group = 1), method = "loess", color = "blue", size = 1, se = FALSE)+
  labs(title = "",
       x = "Irrigation Level (mm)",
       y = expression("Economic Value " ~ "($"*m^3*")"),
       color = "") +  # Legend title for year
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),       # Optional: Remove panel border
    axis.line = element_line(color = "black", size = 0.5),  # Add main axes in black
    axis.ticks = element_line(color = "black", size = 0.5)  # Add ticks in black
    #axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels for readability
  ) +
  scale_color_viridis_d() +  # Optional: color palette for year
  guides(
    color = guide_legend(override.aes = list(size = 2.5))  # Increase legend point size
  )+
  scale_y_continuous(breaks = seq(-0.5, max(wheat$val_mm, na.rm = TRUE), by = 0.1))  # Customize y-axis breaks




#### Canola


# Set the directory where your CSV files are located
file_path <- "./AquaCropOPSyData/CanolaMarginal"

# Get a list of all files that match the pattern
file_list <- list.files(path = file_path, pattern = "merged_simulation_results_canola_marginal_\\d{4}\\.csv", full.names = TRUE)

# Load each file and add a "year" column
canola_marginal <- map_dfr(file_list, ~ {
  year <- gsub(".*_(\\d{4})\\.csv$", "\\1", .x)  # Extract the year from the file name
  read_csv(.x) %>% 
    mutate(year = as.numeric(year))              # Add the year column as a numeric type
})%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*14.86995818)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))


df_canola_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_canola_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  filter(year %in% c(2016, 2017,2018,2019,2020,2021,2022,2023))%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*14.86995818)%>%
  select(year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)


canola_marginal <- canola_marginal%>%
  left_join(df_canola_rf, by = c("year", "Site"))

canola <- canola_marginal%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)
#select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)

ggplot(canola, aes(x = as.factor(Max_Irrigation_mm), y = val_mm, fill = as.factor(year))) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.7),  # Adjust width to control spacing between box plots within each irrigation level
    width = 0.5,  # Width of each box plot
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "",
    x = "Irrigation Level (mm)",
    y = expression("Economic Value " ~ "($"*m^3*")"),
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(margin = margin(t = 20))  # Add margin to x-axis labels for spacing
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(-0.5, max(canola$val_mm, na.rm = TRUE), by = 0.1)) +
  scale_x_discrete(labels = function(x) paste("", x))  # Customize x-axis labels for clarity


#### Alternative 

ggplot(canola, aes(x = as.factor(Max_Irrigation_mm), y = val_mm)) +
  geom_jitter(aes(color = as.factor(year)), width = 0.2, size = 0.5, alpha = 0.7) +  # Points colored by year
  geom_boxplot(outlier.shape = NA, fill = "lightgrey", color = "black", alpha = 0,width = 0.5) +
  geom_smooth(aes(group = 1), method = "loess", color = "blue", size = 1, se = FALSE)+
  labs(title = "",
       x = "Irrigation Level",
       y = expression("Economic Value " ~ "($"*m^3*")"),
       color = "Year") +  # Legend title for year
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),       # Optional: Remove panel border
    axis.line = element_line(color = "black", size = 0.5),  # Add main axes in black
    axis.ticks = element_line(color = "black", size = 0.5)  # Add ticks in black
    #axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels for readability
  ) +
  scale_color_viridis_d() +  # Optional: color palette for year
  guides(
    color = guide_legend(override.aes = list(size = 2.5))  # Increase legend point size
  )+
  scale_y_continuous(breaks = seq(-0.5, max(wheat$val_mm, na.rm = TRUE), by = 0.1))  # Customize y-axis breaks






###### Weighted 

wheat_weighted <- wheat%>%
  mutate(wheat_per = 35.4,
         tot_per = 63.2,
         weight = wheat_per/tot_per,
         w_wheat_val = val_mm*weight,
         w_wheat_irri = `Max_Irrigation_mm`*weight)%>%
  select(year,Site,w_wheat_irri,w_wheat_val,Max_Irrigation_mm)

canola_weighted <- canola%>%
  mutate(canola_per = 27.8,
         tot_per = 63.2,
         weight = canola_per/tot_per,
         w_canol_val = val_mm*weight,
         w_canol_irri = `Max_Irrigation_mm`*weight)%>%
  select(year,Site,w_canol_irri,w_canol_val, Max_Irrigation_mm)



df <- wheat_weighted%>%
  left_join(canola_weighted, by = c("year", "Site", "Max_Irrigation_mm"))%>%
  mutate(ave_val =w_canol_val+w_wheat_val,
         ave_irri = w_wheat_irri+w_canol_irri)%>%
  select(year,Site,ave_val,ave_irri,Max_Irrigation_mm)



ggplot(df, aes(x = as.factor(Max_Irrigation_mm), y = ave_val, fill = as.factor(year))) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.7),  # Adjust width to control spacing between box plots within each irrigation level
    width = 0.5,  # Width of each box plot
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "",
    x = "Irrigation Level (mm)",
    y = expression("Economic Value " ~ "($"*m^3*")"),
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(margin = margin(t = 20))  # Add margin to x-axis labels for spacing
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(-0.5, max(canola$val_mm, na.rm = TRUE), by = 0.1)) +
  scale_x_discrete(labels = function(x) paste("", x))  # Customize x-axis labels for clarity





##### Aditional Aditional


wheat_weighted <- wheat%>%
  mutate(wheat_per = 35.4,
         tot_per = 63.2,
         weight = wheat_per/tot_per,
         w_wheat_val = val_mm*weight,
         w_wheat_irri = `Total_Irrigation_mm`*weight)%>%
  select(year,Site,w_wheat_irri,w_wheat_val,Max_Irrigation_mm)


canola_weighted <- canola%>%
  mutate(canola_per = 27.8,
         tot_per = 63.2,
         weight = canola_per/tot_per,
         w_canol_val = val_mm*weight,
         w_canol_irri = `Total_Irrigation_mm`*weight)%>%
  select(year,Site,w_canol_irri,w_canol_val,Max_Irrigation_mm)

df <- wheat_weighted%>%
  left_join(canola_weighted)%>%
  mutate(ave_val =w_canol_val+w_wheat_val,
         ave_irri = w_wheat_irri+w_canol_irri)%>%
  select(year,Site,ave_val,ave_irri)





# Create a plot with loess smoothing colored by year
ggplot(df, aes(x = ave_irri, y = ave_val, color = as.factor(year))) +
  geom_point(alpha = 0.6) +  # Observations
  geom_smooth(method = "loess", size = 1.2) +  # Smooth line using loess
  labs(title = "",
       x = "Level of Irrigation",
       y = "Value",
       color = "Year") +  # Legend title for year
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),       # Optional: Remove panel border
    axis.line = element_line(color = "black", size = 0.5),  # Add main axes in black
    axis.ticks = element_line(color = "black", size = 0.5)  # Add ticks in black
  ) +
  scale_x_continuous(breaks = seq(0, max(wheat$Total_Irrigation_mm, na.rm = TRUE), by = 100)) +  # Customize x-axis breaks
  scale_y_continuous(breaks = seq(-0.5, max(wheat$val_mm, na.rm = TRUE), by = 0.05))  # Customize y-axis breaks
















#### Map Map
weather_data_final <- read.csv("./AquaCropOPSyData/ClimateData/combined_daymet_weather_data.csv")

# Create Climate file to feed AquaCropOPSY

weather_data_Aqua <- weather_data_final%>%
  select(site,year,yday,Precipitation,MaxTemp,MinTemp,daylength,latitude,longitude)%>%
  mutate(Date = as.Date(yday - 1, origin = paste0(year, "-01-01")))


# Separate Year, Month, and Day from the Date column
weather_data_Aqua <- weather_data_Aqua %>%
  mutate(Year = format(Date, "%Y"),
         Month = format(Date, "%m"),
         Day = format(Date, "%d"))%>%
  select(site,longitude,latitude)%>%
  distinct(site,.keep_all = T)



library(sf)

# Assuming your data frame is named df with columns 'longitude' and 'latitude'
# Create an sf object
df_sf <- st_as_sf(weather_data_Aqua, coords = c("longitude", "latitude"), crs = 4326)  # Use the appropriate CRS


df_map_grids <- st_read("./maps/LakeDiefenbaker/MergeLakeDiefenbakerfishnet5Km.shp")

# Ensure both are in the same CRS
df_map_grids <- st_transform(df_map_grids, st_crs(df_sf))



df_map_grids<- df_map_grids %>%
  st_join(df_sf, join = st_intersects)

df_map <- df_map_grids%>%
  rename(Site=site)%>%
  left_join(df)













canola <- canola%>%
  select(year,irrq_m3,Site,val_mm)


library(fixest)

# Run the fixed effects model
fixed_effects_model <- feols(val_mm ~ irrq_m3 | Site + year, data = canola)

# Summary of the model
summary(fixed_effects_model)



