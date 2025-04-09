#clear memory
rm(list = ls())

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(sf)
library(MetBrewer)
library(patchwork)

#### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed

wheat_rf_2018 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2018.csv')
wheat_rf_2019 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2019.csv')
wheat_rf_2020 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2020.csv') 
wheat_rf_2021 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2021.csv')
wheat_rf_2022 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2022.csv')
wheat_rf_2023 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2023.csv')

df_wheat_rf <- rbind(wheat_rf_2018, wheat_rf_2019, wheat_rf_2020, wheat_rf_2021, wheat_rf_2022, wheat_rf_2023) %>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield rain (bu/ac)` = `Dry yield (bu/ac)`) %>%
  select(Year, `Dry yield rain (bu/ac)`, Site)


canola_rf_2018 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2018.csv')
canola_rf_2019 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2019.csv')
canola_rf_2020 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2020.csv') 
canola_rf_2021 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2021.csv')
canola_rf_2022 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2022.csv')
canola_rf_2023 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2023.csv')

df_canola_rf <- rbind(canola_rf_2018,canola_rf_2019,canola_rf_2020,canola_rf_2021,canola_rf_2022,canola_rf_2023)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of wheat = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield rain (bu/ac)` = `Dry yield (bu/ac)`) %>%
  select(Year, `Dry yield rain (bu/ac)`, Site)

### irrigation ### irrigation ### irrigation ### irrigation ### irrigation ### irrigation


wheat_ir_2018 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2018.csv')
wheat_ir_2019 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2019.csv')
wheat_ir_2020 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2020.csv') 
wheat_ir_2021 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2021.csv')
wheat_ir_2022 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2022.csv')
wheat_ir_2023 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2023.csv')

df_wheat_ir <- rbind(wheat_ir_2018, wheat_ir_2019, wheat_ir_2020, wheat_ir_2021, wheat_ir_2022, wheat_ir_2023) %>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield irri (bu/ac)` = `Dry yield (bu/ac)`) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  select(Year, `Dry yield irri (bu/ac)`,irrq_m3,  Site)


canola_ir_2018 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2018.csv')
canola_ir_2019 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2019.csv')
canola_ir_2020 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2020.csv') 
canola_ir_2021 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2021.csv')
canola_ir_2022 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2022.csv')
canola_ir_2023 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2023.csv')

df_canola_ir <- rbind(canola_ir_2018, canola_ir_2019, canola_ir_2020, canola_ir_2021, canola_ir_2022, canola_ir_2023) %>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield irri (bu/ac)` = `Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  select(Year, `Dry yield irri (bu/ac)`,irrq_m3,  Site)



##### Merge rainfed and irrigation dataframes

df_wheat <- df_wheat_rf%>%
  left_join(df_wheat_ir)

df_canola <- df_canola_rf%>%
  left_join(df_canola_ir)

## crop return - crop budget data ## crop return - crop budget data 

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

wheat <- df_wheat%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(reve_dif =return_ir -return_rf)%>%
  mutate(reve_val_mm = reve_dif/irrq_m3)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(prof_val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,reve_val_mm,prof_val_mm)




canola <- df_canola%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(reve_dif =return_ir -return_rf)%>%
  mutate(reve_val_mm = reve_dif/irrq_m3)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(prof_val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,reve_val_mm,prof_val_mm)


# dataframe - weight based on crop mix
year <- c("2018", "2019", "2020","2021","2022","2023")
ar_wheat <- c(24.4,31.2,31.7,34.2,33.7,35.4)
ar_canola <- c(30.0,22.1,28.9,32,31.8,27.8)

# Create dataframe
weights <- data.frame(year = year, canola = ar_canola, wheat = ar_wheat)%>%
  mutate(area=wheat+canola)%>%
  mutate(canola_w=canola/area)%>%
  mutate(wheat_w=wheat/area)


yr2018canola <- weights%>%
  filter(year == 2018) %>%
  pull(canola_w)
yr2018wheat <- weights%>%
  filter(year == 2018) %>%
  pull(wheat_w)
yr2019canola <- weights%>%
  filter(year == 2019) %>%
  pull(canola_w)
yr2019wheat <- weights%>%
  filter(year == 2019) %>%
  pull(wheat_w)
yr2020canola <- weights%>%
  filter(year == 2020) %>%
  pull(canola_w)
yr2020wheat <- weights%>%
  filter(year == 2020) %>%
  pull(wheat_w)
yr2021canola <- weights%>%
  filter(year == 2021) %>%
  pull(canola_w)
yr2021wheat <- weights%>%
  filter(year == 2021) %>%
  pull(wheat_w)
yr2022canola <- weights%>%
  filter(year == 2022) %>%
  pull(canola_w)
yr2022wheat <- weights%>%
  filter(year == 2022) %>%
  pull(wheat_w)
yr2023canola <- weights%>%
  filter(year == 2023) %>%
  pull(canola_w)
yr2023wheat <- weights%>%
  filter(year == 2023) %>%
  pull(wheat_w)


wheat_weighted <- wheat%>%
  filter(year > 2017)%>%
  mutate(
    weight = case_when(
      year == 2018 ~ yr2018wheat,
      year == 2019 ~ yr2019wheat,
      year == 2020 ~ yr2020wheat,
      year == 2021 ~ yr2021wheat,
      year == 2022 ~ yr2022wheat,
      year == 2023 ~ yr2023wheat
    )
  )%>%
  mutate(
    reve_val_mm_wheat = reve_val_mm*weight,
    prof_val_mm_wheat = prof_val_mm*weight)%>%
  select(year,Site,reve_val_mm_wheat,prof_val_mm_wheat)


canola_weighted <- canola%>%
  filter(year > 2017)%>%
  mutate(
    weight = case_when(
      year == 2018 ~ yr2018canola,
      year == 2019 ~ yr2019canola,
      year == 2020 ~ yr2020canola,
      year == 2021 ~ yr2021canola,
      year == 2022 ~ yr2022canola,
      year == 2023 ~ yr2023canola
    )
  )%>%
  mutate(
    reve_val_mm_canola = reve_val_mm*weight,
    prof_val_mm_canola = prof_val_mm*weight)%>%
  select(year,Site,reve_val_mm_canola,prof_val_mm_canola)


df1 <- wheat_weighted%>%
  left_join(canola_weighted, by = c("Site", "year"))%>%
  mutate(reve_val_mm_w =reve_val_mm_wheat+reve_val_mm_canola,
         prof_val_mm_w = prof_val_mm_wheat + prof_val_mm_canola
  )%>%
  select(year,Site,reve_val_mm_w,prof_val_mm_w)



weather_data_Aqua <- read.csv("./AquaCropOPSyData/ClimateData/combined_daymet_weather_data.csv")%>%
  distinct(site, .keep_all = T)%>%
  select(site,latitude,longitude)%>%
  rename(Site=site)


df_all <- df1%>%
  left_join(weather_data_Aqua)


# Create an sf object
df_map <- st_as_sf(df_all, coords = c("longitude", "latitude"), crs = 4326)  # Use the appropriate CRS



df_map_grids <- st_read("./maps/LakeDiefenbaker/MergeLakeDiefenbakerfishnet5Km.shp")

# Ensure both are in the same CRS
df_map_grids <- st_transform(df_map_grids, st_crs(df_map))



df_map_grids<- df_map_grids %>%
  st_join(df_map, join = st_intersects)




# Define range for color scale
ave_val_range <- range(df_map_grids$reve_val_mm_w, na.rm = TRUE)

# Load MetBrewer color palette
mycolors <- met.brewer(
  "Degas",
  10,
  type = "continuous"
)

# Define breaks for the legend
legend_breaks <- seq(ave_val_range[1], ave_val_range[2], length.out = 4)

# Function to create ggplot for each year with the custom color palette
create_plot <- function(data, year) {
  ggplot(data) +
    geom_sf(aes(fill = reve_val_mm_w), color = NA) +   
    scale_fill_gradientn(
      colors = mycolors,  
      name = expression("Average Value ($/m"^3*")"),
      breaks = legend_breaks,  # Use consistent breaks across all plots
      limits = ave_val_range,  
      labels = scales::number_format(accuracy = 0.01)
    ) +
    labs(title = year) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", size = 10)
    )
}

# Create a list of data frames for each year
df_list <- split(df_map_grids, df_map_grids$year)

# Generate the plots
plots <- lapply(names(df_list), function(year) {
  create_plot(df_list[[year]], year)
})

# Combine plots with a shared legend using patchwork
combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "right")

# Display the combined plot
combined_plot

ggsave("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/results/images/AverageValue_revenue_map.png", plot = combined_plot, width = 10, height = 7, dpi = 300)





# Define range for color scale
ave_val_range <- range(df_map_grids$prof_val_mm_w, na.rm = TRUE)

# Load MetBrewer color palette
mycolors <- met.brewer(
  "Degas",
  10,
  type = "continuous"
)

# Define breaks for the legend
legend_breaks <- seq(ave_val_range[1], ave_val_range[2], length.out = 4)

# Function to create ggplot for each year with the custom color palette
create_plot <- function(data, year) {
  ggplot(data) +
    geom_sf(aes(fill = prof_val_mm_w), color = NA) +   
    scale_fill_gradientn(
      colors = mycolors,  
      name = expression("Average Value ($/m"^3*")"),
      breaks = legend_breaks,  # Use consistent breaks across all plots
      limits = ave_val_range,  
      labels = scales::number_format(accuracy = 0.01)
    ) +
    labs(title = year) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", size = 10)
    )
}

# Create a list of data frames for each year
df_list <- split(df_map_grids, df_map_grids$year)

# Generate the plots
plots <- lapply(names(df_list), function(year) {
  create_plot(df_list[[year]], year)
})

# Combine plots with a shared legend using patchwork
combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "right")

# Display the combined plot
combined_plot

ggsave("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/results/images/AverageValue_profit_map.png", plot = combined_plot, width = 10, height = 7, dpi = 300)


