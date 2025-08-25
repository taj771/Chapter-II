#clear memory
rm(list = ls())

# wheat
library(dplyr)
library(tidyverse)
library(fixest)
library(purrr)


#CMIP126
# wheat
# Set the directory path where the CSV files are located
file_path <- "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/CMIP126/WheatCMIP126"


######## Weights ######## Weights ######## Weights ######## Weights ###########

year <- c("2018", "2019", "2020","2021","2022","2023")
ar_wheat <- c(24.4,31.2,31.7,34.2,33.7,35.4)
ar_canola <- c(30.0,22.1,28.9,32,31.8,27.8)

# Create dataframe
weights <- data.frame(year = year, canola = ar_canola, wheat = ar_wheat)%>%
  mutate(area=wheat+canola)%>%
  mutate(canola_w=canola/area)%>%
  mutate(wheat_w=wheat/area)

canola_weights <- mean(weights$canola_w)
wheat_weights <- mean(weights$wheat_w)

###### Wheat ###### Wheat ###### Wheat ###### Wheat ###### Wheat ###### Wheat
######## CMIP126 Irrigation ######## CMIP126 Irrigation ######## CMIP126 Irrigation

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_wheat_300mm_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039|2040)\\.csv$", 
  full.names = TRUE
)

# Process the files and add the Year column based on simulated file name
CMIP126_wheat <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_irri_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Max_Irrigation_mm,Yield_irri_tonne_per_ha,Total_Irrigation_mm)

##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed 

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_wheat_dry_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039||2040)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
CMIP126_rainffed_wheat <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_dry_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Yield_dry_tonne_per_ha)



CMIP126_wheat <- CMIP126_wheat%>%
  #left_join(CMIP126_prcp)%>%
  left_join(CMIP126_rainffed_wheat)%>%
  mutate(`Dry yield irri (bu/ac)`= Yield_irri_tonne_per_ha*14.86995818)%>%
  mutate(`Dry yield rainfed (bu/ac)` = Yield_dry_tonne_per_ha*14.86995818)%>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))


########## Return ####### Return####### Return ############ Return ############ 

return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")%>%
  filter(year==2023)

return_wheat <- return%>%
  filter(crop=="wheat")

return_canola <- return%>%
  filter(crop=="canola")


price.bu <- return_wheat$price.bu
dry_cost_ac <- return_wheat$dry_cost_ac
irri_cost_ac <- return_wheat$irri_cost_ac

CMIP126_wheat <- CMIP126_wheat%>%
  mutate(return_rf = `Dry yield rainfed (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(Year, Site_ID,Max_Irrigation_mm,prof_dif,val_mm,irrq_m3)
  

ggplot(CMIP126_wheat, aes(x = Max_Irrigation_mm, y = val_mm)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),color = "blue", fill = "lightblue") + 
  geom_point(color = "red", size = 1) +   
  labs(title = "Trend Over Time",
       x = "",
       y = "") +
  theme_minimal()





# Create an empty list to store the results
regression_results <- list()

# Loop through each year and run the regression
unique_years <- unique(CMIP126_wheat$Year)

for (yr in unique_years) {
  # Subset the data for the current year
  subset_data <- CMIP126_wheat %>% filter(Year == yr)
  
  # Check if there is enough data to run the regression
  if (nrow(subset_data) > 1) {  # Ensures there is enough data for regression
    # Run the regression
    model <- feols(prof_dif ~ irrq_m3, data = subset_data)
    
    # Extract coefficients and standard errors
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Save the results in the list
    regression_results[[as.character(yr)]] <- data.frame(
      Year = yr,
      Coefficient = estimate,
      Std_Error = std_error
    )
  }
}

# Combine all results into a single data frame
final_results_wheat <- bind_rows(regression_results)


  
CMIP126_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126.csv") %>%
  filter(
    Year >= 2030 & Year <= 2050,                # Filter by year range
    (Month > 5 | (Month == 5 & Day >= 1)),      # Filter for months from May 1st onwards
    (Month < 10 | (Month == 10 & Day <= 30))    # Filter for months until October 30th
  )%>%
  group_by(Year,site)%>%
  mutate(Total_prcp = sum(Precipitation))%>%
  ungroup()%>%
  select(-Precipitation, -Month, -Day)%>%
  distinct(Year,site, .keep_all = T)%>%
  group_by(Year)%>%
  mutate(Ave_prcp = mean(Total_prcp))%>%
  mutate(Ave_prcp20_perc =Ave_prcp*0.2)%>%
  ungroup()%>%
  select(-Total_prcp, -site)%>%
  distinct(Year, .keep_all = T)


wheat <- final_results_wheat%>%
  left_join(CMIP126_prcp)%>%
  mutate(value_per_acre20_red = Coefficient*Ave_prcp20_perc*4.04686)%>% #1 mm over 1 acre in m³
  mutate(weight = wheat_weights)%>%
  mutate(value_per_acre20_red_weight = value_per_acre20_red*weight)


######### canola ######### canola ######### canola ######### canola ######### canola 

######## CMIP126 Irrigation ######## CMIP126 Irrigation ######## CMIP126 Irrigation

# canola
# Set the directory path where the CSV files are located
file_path <- "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/CMIP126/CanolaCMIP126"



# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_canola_300mm_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039|2040)\\.csv$", 
  full.names = TRUE
)

# Process the files and add the Year column based on simulated file name
CMIP126_canola <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_irri_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Max_Irrigation_mm,Yield_irri_tonne_per_ha,Total_Irrigation_mm)

##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed 

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_canola_dry_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039||2040)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
CMIP126_rainffed_canola <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_dry_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Yield_dry_tonne_per_ha)



CMIP126_canola <- CMIP126_canola%>%
  #left_join(CMIP126_prcp)%>%
  left_join(CMIP126_rainffed_canola)%>%
  mutate(`Dry yield irri (bu/ac)`= Yield_irri_tonne_per_ha*14.86995818)%>%
  mutate(`Dry yield rainfed (bu/ac)` = Yield_dry_tonne_per_ha*14.86995818)%>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))



price.bu <- return_canola$price.bu
dry_cost_ac <- return_canola$dry_cost_ac
irri_cost_ac <- return_canola$irri_cost_ac

CMIP126_canola <- CMIP126_canola%>%
  mutate(return_rf = `Dry yield rainfed (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(Year, Site_ID,Max_Irrigation_mm,prof_dif,val_mm,irrq_m3)


# Create an empty list to store the results
regression_results <- list()

# Loop through each year and run the regression
unique_years <- unique(CMIP126_canola$Year)

for (yr in unique_years) {
  # Subset the data for the current year
  subset_data <- CMIP126_canola %>% filter(Year == yr)
  
  # Check if there is enough data to run the regression
  if (nrow(subset_data) > 1) {  # Ensures there is enough data for regression
    # Run the regression
    model <- feols(prof_dif ~ irrq_m3, data = subset_data)
    
    # Extract coefficients and standard errors
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Save the results in the list
    regression_results[[as.character(yr)]] <- data.frame(
      Year = yr,
      Coefficient = estimate,
      Std_Error = std_error
    )
  }
}

# Combine all results into a single data frame
final_results_canola <- bind_rows(regression_results)



canola <- final_results_canola%>%
  left_join(CMIP126_prcp)%>%
  mutate(value_per_acre20_red = Coefficient*Ave_prcp20_perc*4.04686)%>% # 0.01 = coversion of prcp reduction mm to meters 1233.48 use to  conversion factor to convert cubic meters to an acre-foot
  mutate(weight = canola_weights)%>%
  mutate(value_per_acre20_red_weight = value_per_acre20_red*weight)



wheat <- wheat%>%
  select(Year,value_per_acre20_red_weight)%>%
  rename(wheat=value_per_acre20_red_weight)


canola <- canola%>%
  select(Year,value_per_acre20_red_weight)%>%
  rename(canola=value_per_acre20_red_weight)


df126 <- wheat%>%
  left_join(canola)%>%
  mutate(value_per_acre20_red = canola+wheat )%>%
  mutate(emision="CMIP126")



#CMIP245
# wheat
# Set the directory path where the CSV files are located
file_path <- "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/CMIP245/WheatCMIP245"


######## Weights ######## Weights ######## Weights ######## Weights ###########

year <- c("2018", "2019", "2020","2021","2022","2023")
ar_wheat <- c(24.4,31.2,31.7,34.2,33.7,35.4)
ar_canola <- c(30.0,22.1,28.9,32,31.8,27.8)

# Create dataframe
weights <- data.frame(year = year, canola = ar_canola, wheat = ar_wheat)%>%
  mutate(area=wheat+canola)%>%
  mutate(canola_w=canola/area)%>%
  mutate(wheat_w=wheat/area)

canola_weights <- mean(weights$canola_w)
wheat_weights <- mean(weights$wheat_w)

###### Wheat ###### Wheat ###### Wheat ###### Wheat ###### Wheat ###### Wheat
######## CMIP245 Irrigation ######## CMIP245 Irrigation ######## CMIP245 Irrigation

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_wheat_300mm_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039|2040)\\.csv$", 
  full.names = TRUE
)

# Process the files and add the Year column based on simulated file name
CMIP245_wheat <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_irri_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Max_Irrigation_mm,Yield_irri_tonne_per_ha,Total_Irrigation_mm)

##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed 

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_wheat_dry_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039||2040)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
CMIP245_rainffed_wheat <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_dry_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Yield_dry_tonne_per_ha)



CMIP245_wheat <- CMIP245_wheat%>%
  #left_join(CMIP245_prcp)%>%
  left_join(CMIP245_rainffed_wheat)%>%
  mutate(`Dry yield irri (bu/ac)`= Yield_irri_tonne_per_ha*14.86995818)%>%
  mutate(`Dry yield rainfed (bu/ac)` = Yield_dry_tonne_per_ha*14.86995818)%>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))


########## Return ####### Return####### Return ############ Return ############ 

return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")%>%
  filter(year==2023)

return_wheat <- return%>%
  filter(crop=="wheat")

return_canola <- return%>%
  filter(crop=="canola")


price.bu <- return_wheat$price.bu
dry_cost_ac <- return_wheat$dry_cost_ac
irri_cost_ac <- return_wheat$irri_cost_ac

CMIP245_wheat <- CMIP245_wheat%>%
  mutate(return_rf = `Dry yield rainfed (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(Year, Site_ID,Max_Irrigation_mm,prof_dif,val_mm,irrq_m3)


# Create an empty list to store the results
regression_results <- list()

# Loop through each year and run the regression
unique_years <- unique(CMIP245_wheat$Year)

for (yr in unique_years) {
  # Subset the data for the current year
  subset_data <- CMIP245_wheat %>% filter(Year == yr)
  
  # Check if there is enough data to run the regression
  if (nrow(subset_data) > 1) {  # Ensures there is enough data for regression
    # Run the regression
    model <- feols(prof_dif ~ irrq_m3, data = subset_data)
    
    # Extract coefficients and standard errors
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Save the results in the list
    regression_results[[as.character(yr)]] <- data.frame(
      Year = yr,
      Coefficient = estimate,
      Std_Error = std_error
    )
  }
}

# Combine all results into a single data frame
final_results_wheat <- bind_rows(regression_results)



CMIP245_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245.csv") %>%
  filter(
    Year >= 2030 & Year <= 2050,                # Filter by year range
    (Month > 5 | (Month == 5 & Day >= 1)),      # Filter for months from May 1st onwards
    (Month < 10 | (Month == 10 & Day <= 30))    # Filter for months until October 30th
  )%>%
  group_by(Year,site)%>%
  mutate(Total_prcp = sum(Precipitation))%>%
  ungroup()%>%
  select(-Precipitation, -Month, -Day)%>%
  distinct(Year,site, .keep_all = T)%>%
  group_by(Year)%>%
  mutate(Ave_prcp = mean(Total_prcp))%>%
  mutate(Ave_prcp20_perc =Ave_prcp*0.2)%>%
  ungroup()%>%
  select(-Total_prcp, -site)%>%
  distinct(Year, .keep_all = T)


wheat <- final_results_wheat%>%
  left_join(CMIP245_prcp)%>%
  mutate(value_per_acre20_red = Coefficient*Ave_prcp20_perc*4.04686)%>% #1 mm over 1 acre in m³
  mutate(weight = wheat_weights)%>%
  mutate(value_per_acre20_red_weight = value_per_acre20_red*weight)


######### canola ######### canola ######### canola ######### canola ######### canola 

######## CMIP245 Irrigation ######## CMIP245 Irrigation ######## CMIP245 Irrigation

# canola
# Set the directory path where the CSV files are located
file_path <- "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/CMIP245/CanolaCMIP245"



# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_canola_300mm_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039|2040)\\.csv$", 
  full.names = TRUE
)

# Process the files and add the Year column based on simulated file name
CMIP245_canola <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_irri_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Max_Irrigation_mm,Yield_irri_tonne_per_ha,Total_Irrigation_mm)

##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed 

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_canola_dry_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039||2040)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
CMIP245_rainffed_canola <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_dry_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Yield_dry_tonne_per_ha)



CMIP245_canola <- CMIP245_canola%>%
  #left_join(CMIP245_prcp)%>%
  left_join(CMIP245_rainffed_canola)%>%
  mutate(`Dry yield irri (bu/ac)`= Yield_irri_tonne_per_ha*14.86995818)%>%
  mutate(`Dry yield rainfed (bu/ac)` = Yield_dry_tonne_per_ha*14.86995818)%>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))



price.bu <- return_canola$price.bu
dry_cost_ac <- return_canola$dry_cost_ac
irri_cost_ac <- return_canola$irri_cost_ac

CMIP245_canola <- CMIP245_canola%>%
  mutate(return_rf = `Dry yield rainfed (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(Year, Site_ID,Max_Irrigation_mm,prof_dif,val_mm,irrq_m3)


# Create an empty list to store the results
regression_results <- list()

# Loop through each year and run the regression
unique_years <- unique(CMIP245_canola$Year)

for (yr in unique_years) {
  # Subset the data for the current year
  subset_data <- CMIP245_canola %>% filter(Year == yr)
  
  # Check if there is enough data to run the regression
  if (nrow(subset_data) > 1) {  # Ensures there is enough data for regression
    # Run the regression
    model <- feols(prof_dif ~ irrq_m3, data = subset_data)
    
    # Extract coefficients and standard errors
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Save the results in the list
    regression_results[[as.character(yr)]] <- data.frame(
      Year = yr,
      Coefficient = estimate,
      Std_Error = std_error
    )
  }
}

# Combine all results into a single data frame
final_results_canola <- bind_rows(regression_results)



canola <- final_results_canola%>%
  left_join(CMIP245_prcp)%>%
  mutate(value_per_acre20_red = Coefficient*Ave_prcp20_perc*4.04686)%>% # 0.01 = coversion of prcp reduction mm to meters 1233.48 use to  conversion factor to convert cubic meters to an acre-foot
  mutate(weight = canola_weights)%>%
  mutate(value_per_acre20_red_weight = value_per_acre20_red*weight)



wheat <- wheat%>%
  select(Year,value_per_acre20_red_weight)%>%
  rename(wheat=value_per_acre20_red_weight)


canola <- canola%>%
  select(Year,value_per_acre20_red_weight)%>%
  rename(canola=value_per_acre20_red_weight)


df245 <- wheat%>%
  left_join(canola)%>%
  mutate(value_per_acre20_red = canola+wheat )%>%
  mutate(emision="CMIP245")



#CMIP585
# wheat
# Set the directory path where the CSV files are located
file_path <- "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/CMIP585/WheatCMIP585"


######## Weights ######## Weights ######## Weights ######## Weights ###########

year <- c("2018", "2019", "2020","2021","2022","2023")
ar_wheat <- c(24.4,31.2,31.7,34.2,33.7,35.4)
ar_canola <- c(30.0,22.1,28.9,32,31.8,27.8)

# Create dataframe
weights <- data.frame(year = year, canola = ar_canola, wheat = ar_wheat)%>%
  mutate(area=wheat+canola)%>%
  mutate(canola_w=canola/area)%>%
  mutate(wheat_w=wheat/area)

canola_weights <- mean(weights$canola_w)
wheat_weights <- mean(weights$wheat_w)

###### Wheat ###### Wheat ###### Wheat ###### Wheat ###### Wheat ###### Wheat
######## CMIP585 Irrigation ######## CMIP585 Irrigation ######## CMIP585 Irrigation

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_wheat_300mm_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039|2040)\\.csv$", 
  full.names = TRUE
)

# Process the files and add the Year column based on simulated file name
CMIP585_wheat <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_irri_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Max_Irrigation_mm,Yield_irri_tonne_per_ha,Total_Irrigation_mm)

##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed 

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_wheat_dry_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039||2040)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
CMIP585_rainffed_wheat <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_dry_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Yield_dry_tonne_per_ha)



CMIP585_wheat <- CMIP585_wheat%>%
  #left_join(CMIP585_prcp)%>%
  left_join(CMIP585_rainffed_wheat)%>%
  mutate(`Dry yield irri (bu/ac)`= Yield_irri_tonne_per_ha*14.86995818)%>%
  mutate(`Dry yield rainfed (bu/ac)` = Yield_dry_tonne_per_ha*14.86995818)%>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))


########## Return ####### Return####### Return ############ Return ############ 

return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")%>%
  filter(year==2023)

return_wheat <- return%>%
  filter(crop=="wheat")

return_canola <- return%>%
  filter(crop=="canola")


price.bu <- return_wheat$price.bu
dry_cost_ac <- return_wheat$dry_cost_ac
irri_cost_ac <- return_wheat$irri_cost_ac

CMIP585_wheat <- CMIP585_wheat%>%
  mutate(return_rf = `Dry yield rainfed (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(Year, Site_ID,Max_Irrigation_mm,prof_dif,val_mm,irrq_m3)


# Create an empty list to store the results
regression_results <- list()

# Loop through each year and run the regression
unique_years <- unique(CMIP585_wheat$Year)

for (yr in unique_years) {
  # Subset the data for the current year
  subset_data <- CMIP585_wheat %>% filter(Year == yr)
  
  # Check if there is enough data to run the regression
  if (nrow(subset_data) > 1) {  # Ensures there is enough data for regression
    # Run the regression
    model <- feols(prof_dif ~ irrq_m3, data = subset_data)
    
    # Extract coefficients and standard errors
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Save the results in the list
    regression_results[[as.character(yr)]] <- data.frame(
      Year = yr,
      Coefficient = estimate,
      Std_Error = std_error
    )
  }
}

# Combine all results into a single data frame
final_results_wheat <- bind_rows(regression_results)



CMIP585_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585.csv") %>%
  filter(
    Year >= 2030 & Year <= 2050,                # Filter by year range
    (Month > 5 | (Month == 5 & Day >= 1)),      # Filter for months from May 1st onwards
    (Month < 10 | (Month == 10 & Day <= 30))    # Filter for months until October 30th
  )%>%
  group_by(Year,site)%>%
  mutate(Total_prcp = sum(Precipitation))%>%
  ungroup()%>%
  select(-Precipitation, -Month, -Day)%>%
  distinct(Year,site, .keep_all = T)%>%
  group_by(Year)%>%
  mutate(Ave_prcp = mean(Total_prcp))%>%
  mutate(Ave_prcp20_perc =Ave_prcp*0.2)%>%
  ungroup()%>%
  select(-Total_prcp, -site)%>%
  distinct(Year, .keep_all = T)


wheat <- final_results_wheat%>%
  left_join(CMIP585_prcp)%>%
  mutate(value_per_acre20_red = Coefficient*Ave_prcp20_perc*4.04686)%>% #1 mm over 1 acre in m³
  mutate(weight = wheat_weights)%>%
  mutate(value_per_acre20_red_weight = value_per_acre20_red*weight)


######### canola ######### canola ######### canola ######### canola ######### canola 

######## CMIP585 Irrigation ######## CMIP585 Irrigation ######## CMIP585 Irrigation

# canola
# Set the directory path where the CSV files are located
file_path <- "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/CMIP585/CanolaCMIP585"



# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_canola_300mm_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039|2040)\\.csv$", 
  full.names = TRUE
)

# Process the files and add the Year column based on simulated file name
CMIP585_canola <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_irri_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Max_Irrigation_mm,Yield_irri_tonne_per_ha,Total_Irrigation_mm)

##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed 

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_canola_dry_(2030|2031|2032|2033|2034|2035|2036|2037|2038|2039||2040)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
CMIP585_rainffed_canola <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_dry_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Yield_dry_tonne_per_ha)



CMIP585_canola <- CMIP585_canola%>%
  #left_join(CMIP585_prcp)%>%
  left_join(CMIP585_rainffed_canola)%>%
  mutate(`Dry yield irri (bu/ac)`= Yield_irri_tonne_per_ha*14.86995818)%>%
  mutate(`Dry yield rainfed (bu/ac)` = Yield_dry_tonne_per_ha*14.86995818)%>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))



price.bu <- return_canola$price.bu
dry_cost_ac <- return_canola$dry_cost_ac
irri_cost_ac <- return_canola$irri_cost_ac

CMIP585_canola <- CMIP585_canola%>%
  mutate(return_rf = `Dry yield rainfed (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(Year, Site_ID,Max_Irrigation_mm,prof_dif,val_mm,irrq_m3)


# Create an empty list to store the results
regression_results <- list()

# Loop through each year and run the regression
unique_years <- unique(CMIP585_canola$Year)

for (yr in unique_years) {
  # Subset the data for the current year
  subset_data <- CMIP585_canola %>% filter(Year == yr)
  
  # Check if there is enough data to run the regression
  if (nrow(subset_data) > 1) {  # Ensures there is enough data for regression
    # Run the regression
    model <- feols(prof_dif ~ irrq_m3, data = subset_data)
    
    # Extract coefficients and standard errors
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Save the results in the list
    regression_results[[as.character(yr)]] <- data.frame(
      Year = yr,
      Coefficient = estimate,
      Std_Error = std_error
    )
  }
}

# Combine all results into a single data frame
final_results_canola <- bind_rows(regression_results)



canola <- final_results_canola%>%
  left_join(CMIP585_prcp)%>%
  mutate(value_per_acre20_red = Coefficient*Ave_prcp20_perc*4.04686)%>% # 0.01 = coversion of prcp reduction mm to meters 1233.48 use to  conversion factor to convert cubic meters to an acre-foot
  mutate(weight = canola_weights)%>%
  mutate(value_per_acre20_red_weight = value_per_acre20_red*weight)



wheat <- wheat%>%
  select(Year,value_per_acre20_red_weight)%>%
  rename(wheat=value_per_acre20_red_weight)


canola <- canola%>%
  select(Year,value_per_acre20_red_weight)%>%
  rename(canola=value_per_acre20_red_weight)


df585 <- wheat%>%
  left_join(canola)%>%
  mutate(value_per_acre20_red = canola+wheat )%>%
  mutate(emision="CMIP585")




df <- df126%>%
  rbind(df245)%>%
  rbind(df585)

val_126 <- df%>%
  filter(emision == "CMIP126")%>%
  mutate(meanval = mean(value_per_acre20_red))


val_245 <- df%>%
  filter(emision == "CMIP245")%>%
  mutate(meanval = mean(value_per_acre20_red))


val_585 <- df%>%
  filter(emision == "CMIP585")%>%
  mutate(meanval = mean(value_per_acre20_red))



final_results_long <- df %>%
  gather(key = "Variable", value = "Value", value_per_acre20_red)

final_results_long <- final_results_long%>%
  filter(scenario == "Medium")


# Plot the data
ggplot(final_results_long, aes(x = Year, y = Value, color = emision)) +
  geom_line(size = 0.6, linetype = "dashed", alpha = 0.6) +  # Set all lines as dashed
  geom_point() +  # Points at each data point
  #geom_hline(yintercept = 117.53, linetype = "solid", color = "darkgreen", size = 0.8) +
  geom_hline(yintercept = 140, linetype = "solid", color = "darkgreen", size = 0.8) +
  #geom_hline(yintercept = 137, linetype = "solid", color = "maroon", size = 0.8) +
    labs(title = "",
       x = "Year",
       y = "Cost in $ per Acre",
       color = "Variable") +  # Add legend for variables
  theme_minimal() +
  scale_color_manual(values = c("darkgreen"),
                     labels = c("CMIP6-SSP2-4.5")  # Rename legend items
                     )+  # Custom colors for each variable
  scale_x_continuous(
    breaks = seq(2030, 2050, by = 1),  # Specify breaks for each year
    labels = seq(2030, 2050, by = 1)   # Labels for each year
  )+
  scale_y_continuous(
    breaks = seq(0, 300, by = 25),  # Specify breaks for each year
    labels = seq(0, 300, by = 25)   # Labels for each year
  )+
  theme(
    legend.position = "bottom",  # Move the legend to the bottom
    legend.title = element_blank(),  # Remove the legend title
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.ticks = element_line(),  # Keep axis ticks
    axis.text = element_text(size = 12),  # Adjust axis text size if needed
    axis.title = element_text(size = 14),  # Adjust axis title size if needed
    axis.line = element_line()  # Keep the axis lines visible
  )


### PPT
# Create the box plot

final_results_long <- final_results_long%>%
  mutate(scenario = case_when(emision == "CMIP126" ~ "Low",
                              emision == "CMIP245" ~ "Medium",
                              emision == "CMIP585" ~ "High"))

final_results_long$scenario <- factor(final_results_long$scenario, 
                                      levels = c("Low", "Medium", "High")) 

ggplot(final_results_long, aes(x = scenario, y = Value, fill = scenario)) +
  stat_summary(fun.min = min, fun.max = max, 
               geom = "errorbar", width = 0.05, color = "black") +
  geom_boxplot(width = 0.2, lwd = 0.3) +
  theme_minimal() +
  labs(title = "",
       x = "Emission Scenario",
       y = "Cost per acre ($)") +
  scale_fill_manual(values = c("Low" = "darkseagreen4", "Medium" = "slategray", "High" = "firebrick4"))+
  scale_y_continuous(breaks = seq(0, 250, by = 25)) +
  theme(panel.grid = element_blank(),  # Removes all grid lines
        axis.line = element_line(color = "black"),  # Keeps x and y axis lines
        legend.position = "none",
        panel.border = element_blank())  # Removes outer border






