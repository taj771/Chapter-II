#clear memory
rm(list = ls())

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fixest)
library(priceR)

####### Wheat ####### Wheat ####### Wheat ####### Wheat ####### Wheat
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


wheat_marginal <- wheat_marginal%>%
  filter(year %in% c(2018,2019,2020,2021,2022,2023))



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

# crop retun
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


########### Canola ########### Canola ########### Canola ########### Canola 

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

# set subset -wheat of data to use in loops 
wheat2016 <- wheat%>%
  filter(year==2016)
wheat2017 <- wheat%>%
  filter(year==2017)
wheat2018 <- wheat%>%
  filter(year==2018)
wheat2019 <- wheat%>%
  filter(year==2019)
wheat2020 <- wheat%>%
  filter(year==2020)
wheat2021 <- wheat%>%
  filter(year==2021)
wheat2022 <- wheat%>%
  filter(year==2022)
wheat2023 <- wheat%>%
  filter(year==2023)


# set subset -wheat of data to use in loops 
canola2016 <- canola%>%
  filter(year==2016)
canola2017 <- canola%>%
  filter(year==2017)
canola2018 <- canola%>%
  filter(year==2018)
canola2019 <- canola%>%
  filter(year==2019)
canola2020 <- canola%>%
  filter(year==2020)
canola2021 <- canola%>%
  filter(year==2021)
canola2022 <- canola%>%
  filter(year==2022)
canola2023 <- canola%>%
  filter(year==2023)




# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- c(100,120,140,160,180,200,220,240, 260, 280, 300,320,340,360,380,400,420,440,460,480,500)  # Adjust as necessary

##### wheat ##### wheat ##### wheat ##### wheat ##### wheat ##### wheat 
###### 2018 ###### 2018 ###### 2018 ###### 2018 ###### 2018 ###### 2018
# Initialize an empty data frame to store results
results_wheat <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- wheat2018 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_wheat <- rbind(results_wheat, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

wheat2018 <- results_wheat%>%
  mutate(year=2018)

###### 2019 ###### 2019 ###### 2019 ###### 2019 ###### 2019 ###### 2019
# Initialize an empty data frame to store results
results_wheat <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- wheat2019 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_wheat <- rbind(results_wheat, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

wheat2019 <- results_wheat%>%
  mutate(year=2019)

###### 2020 ###### 2020 ###### 2020 ###### 2020 ###### 2020 ###### 2020 
# Initialize an empty data frame to store results
results_wheat <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- wheat2020 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_wheat <- rbind(results_wheat, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

wheat2020 <- results_wheat%>%
  mutate(year=2020)

###### 2021 ###### 2021 ###### 2021 ###### 2021 ###### 2021 ###### 2021 
# Initialize an empty data frame to store results
results_wheat <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- wheat2021 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_wheat <- rbind(results_wheat, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

wheat2021 <- results_wheat%>%
  mutate(year=2021)

###### 2022 ###### 2022 ###### 2022 ###### 2022 ###### 2022 ###### 2022 
# Initialize an empty data frame to store results
results_wheat <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- wheat2022 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_wheat <- rbind(results_wheat, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

wheat2022 <- results_wheat%>%
  mutate(year=2022)

###### 2023 ###### 2023 ###### 2023 ###### 2023 ###### 2023 ###### 2023 
# Initialize an empty data frame to store results
results_wheat <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- wheat2023 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_wheat <- rbind(results_wheat, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

wheat2023 <- results_wheat%>%
  mutate(year=2023)


df_wheat <- rbind(wheat2018,wheat2019,wheat2020,wheat2021,wheat2022,wheat2023)%>%
  rename(mv_wheat = Estimate)%>%
  select(Irrigation_Level,mv_wheat,year)


##### canola ##### canola ##### canola ##### canola ##### canola ##### canola 
###### 2018 ###### 2018 ###### 2018 ###### 2018 ###### 2018 ###### 2018
# Initialize an empty data frame to store results
results_canola <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- canola2018 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_canola <- rbind(results_canola, 
                            data.frame(Irrigation_Level = level, 
                                       Estimate = estimate, 
                                       Std_Error = std_error))
  }
}

canola2018 <- results_canola%>%
  mutate(year=2018)

###### 2019 ###### 2019 ###### 2019 ###### 2019 ###### 2019 ###### 2019
# Initialize an empty data frame to store results
results_canola <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- canola2019 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_canola <- rbind(results_canola, 
                            data.frame(Irrigation_Level = level, 
                                       Estimate = estimate, 
                                       Std_Error = std_error))
  }
}

canola2019 <- results_canola%>%
  mutate(year=2019)

###### 2020 ###### 2020 ###### 2020 ###### 2020 ###### 2020 ###### 2020 
# Initialize an empty data frame to store results
results_canola <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- canola2020 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_canola <- rbind(results_canola, 
                            data.frame(Irrigation_Level = level, 
                                       Estimate = estimate, 
                                       Std_Error = std_error))
  }
}

canola2020 <- results_canola%>%
  mutate(year=2020)

###### 2021 ###### 2021 ###### 2021 ###### 2021 ###### 2021 ###### 2021 
# Initialize an empty data frame to store results
results_canola <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- canola2021 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_canola <- rbind(results_canola, 
                            data.frame(Irrigation_Level = level, 
                                       Estimate = estimate, 
                                       Std_Error = std_error))
  }
}

canola2021 <- results_canola%>%
  mutate(year=2021)

###### 2022 ###### 2022 ###### 2022 ###### 2022 ###### 2022 ###### 2022 
# Initialize an empty data frame to store results
results_canola <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- canola2022 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_canola <- rbind(results_canola, 
                            data.frame(Irrigation_Level = level, 
                                       Estimate = estimate, 
                                       Std_Error = std_error))
  }
}

canola2022 <- results_canola%>%
  mutate(year=2022)

###### 2023 ###### 2023 ###### 2023 ###### 2023 ###### 2023 ###### 2023 
# Initialize an empty data frame to store results
results_canola <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- canola2023 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_canola <- rbind(results_canola, 
                            data.frame(Irrigation_Level = level, 
                                       Estimate = estimate, 
                                       Std_Error = std_error))
  }
}

canola2023 <- results_canola%>%
  mutate(year=2023)



df_canola <- rbind(canola2018,canola2019,canola2020,canola2021,canola2022,canola2023)%>%
  rename(mv_canola = Estimate)%>%
  select(Irrigation_Level,mv_canola,year)

df <- df_wheat%>%
  left_join(df_canola)%>%
  mutate(mv_can_wheat = mv_canola-mv_wheat)%>%
  mutate(acre_to_m3_conversion = 4.04686 )%>% # 1 mm over 1 acre in m³
  mutate(irrigation_volume_per_acre = Irrigation_Level * acre_to_m3_conversion)%>%
  mutate(total_value_per_acre = mv_can_wheat*irrigation_volume_per_acre)%>%
  group_by(Irrigation_Level)%>%
  mutate(ave_val_irri = mean(total_value_per_acre))%>%
  ungroup()%>%
  distinct(Irrigation_Level, .keep_all = T)
  
  
#############################################################################
# potato - here we assume potato do not grow under rainfed condition
# so the when estimating the MV we assume tge dry lanad return for potato is zero 

####### potato ####### potato ####### potato ####### potato ####### potato

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- c(100,120,140,160,180,200,220,240, 260, 280, 300,320,340,360,380,400,420,440,460,480,500)  # Adjust as necessary


# Set the directory where your CSV files are located
file_path <- "./AquaCropOPSyData/PotatoMarginal"

# Get a list of all files that match the pattern
file_list <- list.files(path = file_path, pattern = "merged_simulation_results_potato_marginal_\\d{4}\\.csv", full.names = TRUE)

# Load each file and add a "year" column
potato_marginal <- map_dfr(file_list, ~ {
  year <- gsub(".*_(\\d{4})\\.csv$", "\\1", .x)  # Extract the year from the file name
  read_csv(.x) %>% 
    mutate(year = as.numeric(year))              # Add the year column as a numeric type
})%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (ton/ac)`= `Yield_tonne_per_ha`*0.4047)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))

potato_marginal <- potato_marginal%>%
  filter(year %in% c(2018,2019,2020,2021,2022,2023))



# crop retun
return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")


irri_cost <- return$irri_cost
price.ton <- return$price.ton
years <- return$year

return$irri_cost <- adjust_for_inflation(irri_cost, years, "CA", to_date = 2023)
return$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


potato <- potato_marginal%>%
  left_join(return)%>%
  mutate(return_ir = `Dry yield irri (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost)%>%
  mutate(val_mm = profit_ir/irrq_m3)%>%
  rename(prof_dif=profit_ir)


# set subset -wheat of data to use in loops 
potato2018 <- potato%>%
  filter(year==2018)
potato2019 <- potato%>%
  filter(year==2019)
potato2020 <- potato%>%
  filter(year==2020)
potato2021 <- potato%>%
  filter(year==2021)
potato2022 <- potato%>%
  filter(year==2022)
potato2023 <- potato%>%
  filter(year==2023)




# Initialize an empty data frame to store results
results_potato <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- potato2018 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_potato <- rbind(results_potato, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

potato2018 <- results_potato%>%
  mutate(year=2018)

###### 2019 ###### 2019 ###### 2019 ###### 2019 ###### 2019 ###### 2019
# Initialize an empty data frame to store results
results_potato <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- potato2019 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_potato <- rbind(results_potato, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

potato2019 <- results_potato%>%
  mutate(year=2019)

###### 2020 ###### 2020 ###### 2020 ###### 2020 ###### 2020 ###### 2020 
# Initialize an empty data frame to store results
results_potato <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- potato2020 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_potato <- rbind(results_potato, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

potato2020 <- results_potato%>%
  mutate(year=2020)

###### 2021 ###### 2021 ###### 2021 ###### 2021 ###### 2021 ###### 2021 
# Initialize an empty data frame to store results
results_potato <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- potato2021 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_potato <- rbind(results_potato, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

potato2021 <- results_potato%>%
  mutate(year=2021)

###### 2022 ###### 2022 ###### 2022 ###### 2022 ###### 2022 ###### 2022 
# Initialize an empty data frame to store results
results_potato <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- potato2022 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_potato <- rbind(results_potato, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

potato2022 <- results_potato%>%
  mutate(year=2022)

###### 2023 ###### 2023 ###### 2023 ###### 2023 ###### 2023 ###### 2023 
# Initialize an empty data frame to store results
results_potato <- data.frame(Irrigation_Level = numeric(), Estimate = numeric(), Std_Error = numeric())

# Loop over each irrigation level
for (level in irrigation_levels) {
  
  # Filter data for the specific irrigation level range
  data_subset <- potato2023 %>%
    filter(Total_Irrigation_mm <= level)
  
  # Check if there are enough observations for the model
  if (nrow(data_subset) > 1) {  # Ensure at least two data points
    
    # Fit the fixed effects model
    model <- feols(prof_dif ~ irrq_m3, data = data_subset)
    
    # Extract the coefficient and standard error for irrq_m3
    estimate <- coef(model)["irrq_m3"]
    std_error <- se(model)["irrq_m3"]
    
    # Append to the results data frame
    results_potato <- rbind(results_potato, 
                           data.frame(Irrigation_Level = level, 
                                      Estimate = estimate, 
                                      Std_Error = std_error))
  }
}

potato2023 <- results_potato%>%
  mutate(year=2023)


df_potato <- rbind(potato2018,potato2019,potato2020,potato2021,potato2022,potato2023)%>%
  rename(mv_potato = Estimate)%>%
  select(Irrigation_Level,mv_potato,year)

df_wheat <- df_wheat%>%
  filter(year %in% c(2018, 2019, 2020,2021))

df <- df_wheat%>%
  left_join(df_potato)%>%
  mutate(mv_wheat_pot = mv_potato-mv_wheat)%>%
  mutate(acre_to_m3_conversion = 4.04686 )%>% # 1 mm over 1 acre in m³
  mutate(irrigation_volume_per_acre = Irrigation_Level * acre_to_m3_conversion)%>%
  mutate(total_value_per_acre = mv_wheat_pot*irrigation_volume_per_acre)%>%
  group_by(Irrigation_Level)%>%
  mutate(ave_val_irri = mean(total_value_per_acre))%>%
  ungroup()%>%
  distinct(Irrigation_Level, .keep_all = T)


df_canola <- df_canola%>%
  filter(year %in% c(2018, 2019, 2020,2021))


df <- df_canola%>%
  left_join(df_potato)%>%
  mutate(mv_can_pot = mv_potato-mv_canola)%>%
  mutate(acre_to_m3_conversion = 4.04686 )%>% # 1 mm over 1 acre in m³
  mutate(irrigation_volume_per_acre = Irrigation_Level * acre_to_m3_conversion)%>%
  mutate(total_value_per_acre = mv_can_pot*irrigation_volume_per_acre)%>%
  group_by(Irrigation_Level)%>%
  mutate(ave_val_irri = mean(total_value_per_acre))%>%
  ungroup()%>%
  distinct(Irrigation_Level, .keep_all = T)




