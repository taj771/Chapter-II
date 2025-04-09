#clear memory
rm(list = ls())

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fixest)
library(priceR)
library(purrr)
library(tidyverse)
library(xtable)

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


df_wheat <- rbind(wheat2018,wheat2019,wheat2020,wheat2021,wheat2022,wheat2023)


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



df_canola <- rbind(canola2018,canola2019,canola2020,canola2021,canola2022,canola2023)



### potato ### potato ### potato ### potato ### potato ### potato ### potato
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

potato_marginal <- potato_marginal%>%
  select(Site,year,Max_Irrigation_mm,`Dry yield irri (ton/ac)`,irrq_m3,Total_Irrigation_mm)

return_potato <- return%>%
  filter(crop =="potato")%>%
  select(crop,price.ton,year,irri_cost)

potato <- potato_marginal%>%
  left_join(return_potato)%>%
  mutate(revenue = `Dry yield irri (ton/ac)`*price.ton)%>%
  mutate(profit = revenue-irri_cost)


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


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- c(200,220,240, 260, 280, 300,320,340,360,380,400,420,440,460,480,500)  # Adjust as necessary

### potato ### potato ### potato ### potato ### potato ### potato ### potato
###### 2018 ###### 2018 ###### 2018 ###### 2018 ###### 2018 ###### 2018
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
    model <- feols(profit ~ irrq_m3, data = data_subset)
    
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
    model <- feols(profit ~ irrq_m3, data = data_subset)
    
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
    model <- feols(profit ~ irrq_m3, data = data_subset)
    
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
    model <- feols(profit ~ irrq_m3, data = data_subset)
    
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
    model <- feols(profit ~ irrq_m3, data = data_subset)
    
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
    model <- feols(profit ~ irrq_m3, data = data_subset)
    
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


df_potato <- rbind(potato2018,potato2019,potato2020,potato2021,potato2022,potato2023)




wheat_ydd_irri <- wheat%>%
  select(Site,year,`Dry yield irri (bu/ac)`,`Dry yield rain (bu/ac)`,Max_Irrigation_mm)%>%
  #filter(year == 2021) %>%
  group_by(Max_Irrigation_mm)%>%
  mutate(wheat_yld_irri_bu_ac = mean(`Dry yield irri (bu/ac)`))%>%
  mutate(wheat_yld_rf_bu_ac = mean(`Dry yield rain (bu/ac)`))%>%
  ungroup()%>%
  distinct(Max_Irrigation_mm, .keep_all = T)%>%
  select(-Site,-`Dry yield irri (bu/ac)`,-`Dry yield rain (bu/ac)`)%>%
  rename(Irrigation_Level=Max_Irrigation_mm)%>%
  filter(Irrigation_Level > 199)

return_wheat_2021 <- return_wheat%>%
  select(year,price.bu)%>%
  #filter(year==2021)%>%
  rename(wheat_price_bu =price.bu )

df_wheat_2021 <- df_wheat%>%
  #filter(year==2021)%>%
  rename(mv_wheat = Estimate)%>%
  select(-Std_Error)%>%
  left_join(wheat_ydd_irri)%>%
  left_join(return_wheat_2021)



canola_ydd_irri <- canola%>%
  select(Site,year,`Dry yield irri (bu/ac)`,`Dry yield rain (bu/ac)`,Max_Irrigation_mm)%>%
  #filter(year == 2021) %>%
  group_by(Max_Irrigation_mm)%>%
  mutate(canola_yld_irri_bu_ac = mean(`Dry yield irri (bu/ac)`))%>%
  mutate(canola_yld_rf_bu_ac = mean(`Dry yield rain (bu/ac)`))%>%
  ungroup()%>%
  distinct(Max_Irrigation_mm, .keep_all = T)%>%
  select(-Site,-`Dry yield irri (bu/ac)`,-`Dry yield rain (bu/ac)`)%>%
  rename(Irrigation_Level=Max_Irrigation_mm)%>%
  filter(Irrigation_Level > 199)


return_canola_2021 <- return_canola%>%
  select(year,price.bu)%>%
  #filter(year==2021)%>%
  rename(canola_price_bu =price.bu )


df_canola_2021 <- df_canola%>%
  #filter(year==2021)%>%
  rename(mv_canola = Estimate)%>%
  select(-Std_Error)%>%
  left_join(canola_ydd_irri)%>%
  left_join(return_canola_2021)

potato_ydd_irri <- potato%>%
  select(Site,year,`Dry yield irri (ton/ac)`,Max_Irrigation_mm)%>%
  #filter(year == 2021) %>%
  group_by(Max_Irrigation_mm)%>%
  mutate(wheat_yld_irri_ton_ac = mean(`Dry yield irri (ton/ac)`))%>%
  ungroup()%>%
  distinct(Max_Irrigation_mm, .keep_all = T)%>%
  select(-Site,-`Dry yield irri (ton/ac)`)%>%
  rename(Irrigation_Level=Max_Irrigation_mm)%>%
  filter(Irrigation_Level > 199)


return_potato_2021 <- return_potato%>%
  select(year,price.ton)%>%
  #filter(year==2021)%>%
  rename(potato_price_ton =price.ton )


df_potato_2021 <- df_potato%>%
  #filter(year==2021)%>%
  rename(mv_potato = Estimate)%>%
  select(-Std_Error)%>%
  left_join(potato_ydd_irri)%>%
  left_join(return_potato_2021)



df_all <- df_wheat_2021%>%
  left_join(df_canola_2021)%>%
  left_join(df_potato_2021)


############################
# without reallocation 

# 2018
# Get the maximum value of mv_wheat for the year 
max_mv_wheat <- max(subset(df_all, year == 2018)$mv_wheat, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_wheat <- subset(df_all, year == 2018 & mv_wheat == max_mv_wheat)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_canola <- max(subset(df_all, year == 2018)$mv_canola, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_canola <- subset(df_all, year == 2018 & mv_canola == max_mv_canola)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_potato <- max(subset(df_all, year == 2018)$mv_potato, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_potato <- subset(df_all, year == 2018 & mv_potato == max_mv_potato)$Irrigation_Level



# Create a list
my_list <- list(
  Irrigation_Level_wheat = c(max_mv_irri_wheat),
  Irrigation_Level_canola = c(max_mv_irri_canola),
  Irrigation_Level_potato = c(max_mv_irri_potato),
  mv_wheat =c(max_mv_wheat),
  mv_canola = c(max_mv_canola),
  mv_potato = c(max_mv_potato)
)

# Convert the list to a dataframe
full_allocation <- data.frame(my_list)%>%
  mutate(total_avali = 900)%>%
  mutate(Irrigation_Level_wheat=total_avali-(Irrigation_Level_canola+Irrigation_Level_potato))%>%
  mutate(Irrigation_Level_wheat = ifelse(Irrigation_Level_wheat < 0, 0, Irrigation_Level_wheat))%>%
  mutate(year=2018)


df1 <- full_allocation%>%
  select(Irrigation_Level_potato,year)

df2 <- df_all%>%
  filter(year==2018)%>%
  select(year,Irrigation_Level,mv_potato)%>%
  rename(Irrigation_Level_potato=Irrigation_Level)

df2018_potato <- df1%>%
  left_join(df2)

df1 <- full_allocation%>%
  select(Irrigation_Level_canola,year)

df2 <- df_all%>%
  filter(year==2018)%>%
  select(year,Irrigation_Level,mv_canola)%>%
  rename(Irrigation_Level_canola=Irrigation_Level)

df2018_canola <- df1%>%
  left_join(df2)%>%
  select(-year)

df1 <- full_allocation%>%
  select(Irrigation_Level_wheat,year)

df2 <- df_all%>%
  filter(year==2018)%>%
  select(year,Irrigation_Level,mv_wheat)%>%
  rename(Irrigation_Level_wheat=Irrigation_Level)

df2018_wheat <- df1%>%
  left_join(df2)%>%
  select(-year)

df2018 <- cbind(df2018_potato,df2018_canola,df2018_wheat)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)%>%
  mutate(reallocatio = "yes")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
          realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)




df_2018_wo_realo <- df_all%>%
  filter(year==2018)%>%
  select(year,Irrigation_Level,mv_wheat,mv_canola,mv_potato)%>%
  filter(Irrigation_Level==300)%>%
  rename(Irrigation_Level_wheat = Irrigation_Level)%>%
  mutate(Irrigation_Level_canola = 300)%>%
  mutate(Irrigation_Level_potato = 300)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(reallocatio = "no")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)



df_final_2018 <- rbind(df2018,df_2018_wo_realo)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)

#### 2019

# Get the maximum value of mv_wheat for the year 
max_mv_wheat <- max(subset(df_all, year == 2019)$mv_wheat, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_wheat <- subset(df_all, year == 2019 & mv_wheat == max_mv_wheat)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_canola <- max(subset(df_all, year == 2019)$mv_canola, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_canola <- subset(df_all, year == 2019 & mv_canola == max_mv_canola)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_potato <- max(subset(df_all, year == 2019)$mv_potato, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_potato <- subset(df_all, year == 2019 & mv_potato == max_mv_potato)$Irrigation_Level



# Create a list
my_list <- list(
  Irrigation_Level_wheat = c(max_mv_irri_wheat),
  Irrigation_Level_canola = c(max_mv_irri_canola),
  Irrigation_Level_potato = c(max_mv_irri_potato),
  mv_wheat =c(max_mv_wheat),
  mv_canola = c(max_mv_canola),
  mv_potato = c(max_mv_potato)
)

# Convert the list to a dataframe
full_allocation <- data.frame(my_list)%>%
  mutate(total_avali = 900)%>%
  #mutate(Irrigation_Level_wheat=total_avali-(Irrigation_Level_canola+Irrigation_Level_potato))%>%
  #mutate(Irrigation_Level_wheat = ifelse(Irrigation_Level_wheat < 0, 0, Irrigation_Level_wheat))%>%
  mutate(year=2019)


df1 <- full_allocation%>%
  select(Irrigation_Level_potato,year)

df2 <- df_all%>%
  filter(year==2019)%>%
  select(year,Irrigation_Level,mv_potato)%>%
  rename(Irrigation_Level_potato=Irrigation_Level)

df2019_potato <- df1%>%
  left_join(df2)

df1 <- full_allocation%>%
  select(Irrigation_Level_canola,year)

df2 <- df_all%>%
  filter(year==2019)%>%
  select(year,Irrigation_Level,mv_canola)%>%
  rename(Irrigation_Level_canola=Irrigation_Level)

df2019_canola <- df1%>%
  left_join(df2)%>%
  select(-year)

df1 <- full_allocation%>%
  select(Irrigation_Level_wheat,year)

df2 <- df_all%>%
  filter(year==2019)%>%
  select(year,Irrigation_Level,mv_wheat)%>%
  rename(Irrigation_Level_wheat=Irrigation_Level)

df2019_wheat <- df1%>%
  left_join(df2)%>%
  select(-year)

df2019 <- cbind(df2019_potato,df2019_canola,df2019_wheat)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)%>%
  mutate(reallocatio = "yes")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)




df_2019_wo_realo <- df_all%>%
  filter(year==2019)%>%
  select(year,Irrigation_Level,mv_wheat,mv_canola,mv_potato)%>%
  filter(Irrigation_Level==300)%>%
  rename(Irrigation_Level_wheat = Irrigation_Level)%>%
  mutate(Irrigation_Level_canola = 300)%>%
  mutate(Irrigation_Level_potato = 300)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(reallocatio = "no")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)



df_final_2019 <- rbind(df2019,df_2019_wo_realo)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)


#### 2020

# Get the maximum value of mv_wheat for the year 
max_mv_wheat <- max(subset(df_all, year == 2020)$mv_wheat, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_wheat <- subset(df_all, year == 2020 & mv_wheat == max_mv_wheat)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_canola <- max(subset(df_all, year == 2020)$mv_canola, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_canola <- subset(df_all, year == 2020 & mv_canola == max_mv_canola)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_potato <- max(subset(df_all, year == 2020)$mv_potato, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_potato <- subset(df_all, year == 2020 & mv_potato == max_mv_potato)$Irrigation_Level



# Create a list
my_list <- list(
  Irrigation_Level_wheat = c(max_mv_irri_wheat),
  Irrigation_Level_canola = c(max_mv_irri_canola),
  Irrigation_Level_potato = c(max_mv_irri_potato),
  mv_wheat =c(max_mv_wheat),
  mv_canola = c(max_mv_canola),
  mv_potato = c(max_mv_potato)
)

# Convert the list to a dataframe
full_allocation <- data.frame(my_list)%>%
  mutate(total_avali = 900)%>%
  mutate(Irrigation_Level_wheat=total_avali-(Irrigation_Level_canola+Irrigation_Level_potato))%>%
  mutate(Irrigation_Level_wheat = ifelse(Irrigation_Level_wheat < 0, 0, Irrigation_Level_wheat))%>%
  mutate(year=2020)


df1 <- full_allocation%>%
  select(Irrigation_Level_potato,year)

df2 <- df_all%>%
  filter(year==2020)%>%
  select(year,Irrigation_Level,mv_potato)%>%
  rename(Irrigation_Level_potato=Irrigation_Level)

df2020_potato <- df1%>%
  left_join(df2)

df1 <- full_allocation%>%
  select(Irrigation_Level_canola,year)

df2 <- df_all%>%
  filter(year==2020)%>%
  select(year,Irrigation_Level,mv_canola)%>%
  rename(Irrigation_Level_canola=Irrigation_Level)

df2020_canola <- df1%>%
  left_join(df2)%>%
  select(-year)

df1 <- full_allocation%>%
  select(Irrigation_Level_wheat,year)

df2 <- df_all%>%
  filter(year==2020)%>%
  select(year,Irrigation_Level,mv_wheat)%>%
  rename(Irrigation_Level_wheat=Irrigation_Level)

df2020_wheat <- df1%>%
  left_join(df2)%>%
  select(-year)

df2020 <- cbind(df2020_potato,df2020_canola,df2020_wheat)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)%>%
  mutate(reallocatio = "yes")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)




df_2020_wo_realo <- df_all%>%
  filter(year==2020)%>%
  select(year,Irrigation_Level,mv_wheat,mv_canola,mv_potato)%>%
  filter(Irrigation_Level==300)%>%
  rename(Irrigation_Level_wheat = Irrigation_Level)%>%
  mutate(Irrigation_Level_canola = 300)%>%
  mutate(Irrigation_Level_potato = 300)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(reallocatio = "no")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)



df_final_2020 <- rbind(df2020,df_2020_wo_realo)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)



#### 2021

# Get the maximum value of mv_wheat for the year 
max_mv_wheat <- max(subset(df_all, year == 2021)$mv_wheat, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_wheat <- subset(df_all, year == 2021 & mv_wheat == max_mv_wheat)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_canola <- max(subset(df_all, year == 2021)$mv_canola, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_canola <- subset(df_all, year == 2021 & mv_canola == max_mv_canola)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_potato <- max(subset(df_all, year == 2021)$mv_potato, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_potato <- subset(df_all, year == 2021 & mv_potato == max_mv_potato)$Irrigation_Level



# Create a list
my_list <- list(
  Irrigation_Level_wheat = c(max_mv_irri_wheat),
  Irrigation_Level_canola = c(max_mv_irri_canola),
  Irrigation_Level_potato = c(max_mv_irri_potato),
  mv_wheat =c(max_mv_wheat),
  mv_canola = c(max_mv_canola),
  mv_potato = c(max_mv_potato)
)

# Convert the list to a dataframe
full_allocation <- data.frame(my_list)%>%
  mutate(total_avali = 900)%>%
  mutate(Irrigation_Level_wheat=total_avali-(Irrigation_Level_canola+Irrigation_Level_potato))%>%
  mutate(Irrigation_Level_wheat = ifelse(Irrigation_Level_wheat < 0, 0, Irrigation_Level_wheat))%>%
  mutate(year=2021)


df1 <- full_allocation%>%
  select(Irrigation_Level_potato,year)

df2 <- df_all%>%
  filter(year==2021)%>%
  select(year,Irrigation_Level,mv_potato)%>%
  rename(Irrigation_Level_potato=Irrigation_Level)

df2021_potato <- df1%>%
  left_join(df2)

df1 <- full_allocation%>%
  select(Irrigation_Level_canola,year)

df2 <- df_all%>%
  filter(year==2021)%>%
  select(year,Irrigation_Level,mv_canola)%>%
  rename(Irrigation_Level_canola=Irrigation_Level)

df2021_canola <- df1%>%
  left_join(df2)%>%
  select(-year)

df1 <- full_allocation%>%
  select(Irrigation_Level_wheat,year)

df2 <- df_all%>%
  filter(year==2021)%>%
  select(year,Irrigation_Level,mv_wheat)%>%
  rename(Irrigation_Level_wheat=Irrigation_Level)

df2021_wheat <- df1%>%
  left_join(df2)%>%
  select(-year)

df2021 <- cbind(df2021_potato,df2021_canola,df2021_wheat)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)%>%
  mutate(reallocatio = "yes")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)




df_2021_wo_realo <- df_all%>%
  filter(year==2021)%>%
  select(year,Irrigation_Level,mv_wheat,mv_canola,mv_potato)%>%
  filter(Irrigation_Level==300)%>%
  rename(Irrigation_Level_wheat = Irrigation_Level)%>%
  mutate(Irrigation_Level_canola = 300)%>%
  mutate(Irrigation_Level_potato = 300)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(reallocatio = "no")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)



df_final_2021 <- rbind(df2021,df_2021_wo_realo)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)


#### 2022

# Get the maximum value of mv_wheat for the year 
max_mv_wheat <- max(subset(df_all, year == 2022)$mv_wheat, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_wheat <- subset(df_all, year == 2022 & mv_wheat == max_mv_wheat)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_canola <- max(subset(df_all, year == 2022)$mv_canola, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_canola <- subset(df_all, year == 2022 & mv_canola == max_mv_canola)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_potato <- max(subset(df_all, year == 2022)$mv_potato, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_potato <- subset(df_all, year == 2022 & mv_potato == max_mv_potato)$Irrigation_Level



# Create a list
my_list <- list(
  Irrigation_Level_wheat = c(max_mv_irri_wheat),
  Irrigation_Level_canola = c(max_mv_irri_canola),
  Irrigation_Level_potato = c(max_mv_irri_potato),
  mv_wheat =c(max_mv_wheat),
  mv_canola = c(max_mv_canola),
  mv_potato = c(max_mv_potato)
)

# Convert the list to a dataframe
full_allocation <- data.frame(my_list)%>%
  mutate(total_avali = 900)%>%
  mutate(Irrigation_Level_wheat=total_avali-(Irrigation_Level_canola+Irrigation_Level_potato))%>%
  mutate(Irrigation_Level_wheat = ifelse(Irrigation_Level_wheat < 0, 0, Irrigation_Level_wheat))%>%
  mutate(year=2022)


df1 <- full_allocation%>%
  select(Irrigation_Level_potato,year)

df2 <- df_all%>%
  filter(year==2022)%>%
  select(year,Irrigation_Level,mv_potato)%>%
  rename(Irrigation_Level_potato=Irrigation_Level)

df2022_potato <- df1%>%
  left_join(df2)

df1 <- full_allocation%>%
  select(Irrigation_Level_canola,year)

df2 <- df_all%>%
  filter(year==2022)%>%
  select(year,Irrigation_Level,mv_canola)%>%
  rename(Irrigation_Level_canola=Irrigation_Level)

df2022_canola <- df1%>%
  left_join(df2)%>%
  select(-year)

df1 <- full_allocation%>%
  select(Irrigation_Level_wheat,year)

df2 <- df_all%>%
  filter(year==2022)%>%
  select(year,Irrigation_Level,mv_wheat)%>%
  rename(Irrigation_Level_wheat=Irrigation_Level)

df2022_wheat <- df1%>%
  left_join(df2)%>%
  select(-year)

df2022 <- cbind(df2022_potato,df2022_canola,df2022_wheat)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)%>%
  mutate(reallocatio = "yes")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)




df_2022_wo_realo <- df_all%>%
  filter(year==2022)%>%
  select(year,Irrigation_Level,mv_wheat,mv_canola,mv_potato)%>%
  filter(Irrigation_Level==300)%>%
  rename(Irrigation_Level_wheat = Irrigation_Level)%>%
  mutate(Irrigation_Level_canola = 300)%>%
  mutate(Irrigation_Level_potato = 300)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(reallocatio = "no")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)



df_final_2022 <- rbind(df2022,df_2022_wo_realo)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)


#### 2023

# Get the maximum value of mv_wheat for the year 
max_mv_wheat <- max(subset(df_all, year == 2023)$mv_wheat, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_wheat <- subset(df_all, year == 2023 & mv_wheat == max_mv_wheat)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_canola <- max(subset(df_all, year == 2023)$mv_canola, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_canola <- subset(df_all, year == 2023 & mv_canola == max_mv_canola)$Irrigation_Level


# Get the maximum value of mv_wheat for the year
max_mv_potato <- max(subset(df_all, year == 2023)$mv_potato, na.rm = TRUE)
# Subset to find the irrigation level corresponding to this maximum value
max_mv_irri_potato <- subset(df_all, year == 2023 & mv_potato == max_mv_potato)$Irrigation_Level



# Create a list
my_list <- list(
  Irrigation_Level_wheat = c(max_mv_irri_wheat),
  Irrigation_Level_canola = c(max_mv_irri_canola),
  Irrigation_Level_potato = c(max_mv_irri_potato),
  mv_wheat =c(max_mv_wheat),
  mv_canola = c(max_mv_canola),
  mv_potato = c(max_mv_potato)
)

# Convert the list to a dataframe
full_allocation <- data.frame(my_list)%>%
  mutate(total_avali = 900)%>%
  mutate(Irrigation_Level_wheat=total_avali-(Irrigation_Level_canola+Irrigation_Level_potato))%>%
  mutate(Irrigation_Level_wheat = ifelse(Irrigation_Level_wheat < 0, 0, Irrigation_Level_wheat))%>%
  mutate(year=2023)


df1 <- full_allocation%>%
  select(Irrigation_Level_potato,year)

df2 <- df_all%>%
  filter(year==2023)%>%
  select(year,Irrigation_Level,mv_potato)%>%
  rename(Irrigation_Level_potato=Irrigation_Level)

df2023_potato <- df1%>%
  left_join(df2)

df1 <- full_allocation%>%
  select(Irrigation_Level_canola,year)

df2 <- df_all%>%
  filter(year==2023)%>%
  select(year,Irrigation_Level,mv_canola)%>%
  rename(Irrigation_Level_canola=Irrigation_Level)

df2023_canola <- df1%>%
  left_join(df2)%>%
  select(-year)

df1 <- full_allocation%>%
  select(Irrigation_Level_wheat,year)

df2 <- df_all%>%
  filter(year==2023)%>%
  select(year,Irrigation_Level,mv_wheat)%>%
  rename(Irrigation_Level_wheat=Irrigation_Level)

df2023_wheat <- df1%>%
  left_join(df2)%>%
  select(-year)

df2023 <- cbind(df2023_potato,df2023_canola,df2023_wheat)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)%>%
  mutate(reallocatio = "yes")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)




df_2023_wo_realo <- df_all%>%
  filter(year==2023)%>%
  select(year,Irrigation_Level,mv_wheat,mv_canola,mv_potato)%>%
  filter(Irrigation_Level==300)%>%
  rename(Irrigation_Level_wheat = Irrigation_Level)%>%
  mutate(Irrigation_Level_canola = 300)%>%
  mutate(Irrigation_Level_potato = 300)%>%
  mutate(realocation_pota = Irrigation_Level_potato-300)%>%
  mutate(realocation_cano = Irrigation_Level_canola-300)%>%
  mutate(realocation_wheat = Irrigation_Level_wheat-300)%>%
  mutate(reallocatio = "no")%>%
  select(reallocatio,year,Irrigation_Level_potato,Irrigation_Level_canola,Irrigation_Level_wheat,
         realocation_pota,realocation_cano,realocation_wheat,mv_potato,mv_canola,mv_wheat)



df_final_2023 <- rbind(df2023,df_2023_wo_realo)%>%
  mutate(wheat_vol_peracre = 4.04685642*Irrigation_Level_wheat*mv_wheat)%>%
  mutate(canola_vol_peracre = 4.04685642*Irrigation_Level_canola*mv_canola)%>%
  mutate(potato_vol_peracre = 4.04685642*Irrigation_Level_potato*mv_potato)


df_final1 <- rbind(df_final_2018,df_final_2019,df_final_2020,df_final_2021,df_final_2022,df_final_2023)%>%
  filter(reallocatio=="yes")%>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))%>%
  mutate(total_benefits_w_real = wheat_vol_peracre+canola_vol_peracre+potato_vol_peracre)%>%
  select(year,total_benefits_w_real )


df_final2 <- rbind(df_final_2018,df_final_2019,df_final_2020,df_final_2021,df_final_2022,df_final_2023)%>%
  filter(reallocatio=="no")%>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))%>%
  mutate(total_benefits_wo_real = wheat_vol_peracre+canola_vol_peracre+potato_vol_peracre)%>%
  select(year,total_benefits_wo_real )




df_final <- rbind(df_final_2018,df_final_2019,df_final_2020,df_final_2021,df_final_2022,df_final_2023)%>%
  left_join(df_final1)%>%
  left_join(df_final2)%>%
  mutate(NetBenefit = total_benefits_w_real - total_benefits_wo_real)%>%
  select(reallocatio,year,
         realocation_wheat,realocation_cano,realocation_pota,
       wheat_vol_peracre,canola_vol_peracre,potato_vol_peracre,NetBenefit)




latex_table <- xtable(df_final)


# Save the LaTeX table to a .tex file
sink("./output/Table/ReallocationBenefits.tex")
print(latex_table, type = "latex")
sink()


################################################################################
# Reallocation benefits graph -PPT

data<- read_csv("AquaCropOPSyData/others/ReallocatioBenefits.csv")%>%
  filter(year == "ave")

# Calculate total profit for both old and new scenarios
total_change <- data.frame(
  Category = c("Value per acre ($) - No reallocation", "Value per acre ($) - With reallocation"),
  Change = c(sum(data$Old_Profit), sum(data$New_Profit))  # Sum of old and new profit
)


# Create the irrigation plot

data <- data %>%
  mutate(
    Start_Irrigation = ifelse(Crop == "Wheat", Old_Irrigation - 5, Old_Irrigation + 5),
    End_Irrigation = ifelse(Crop == "Wheat", New_Irrigation + 5, New_Irrigation - 5)
  )

# Create the plot
plot_irrigation <- ggplot(data) +
  # Add points for the old and new irrigation levels
  geom_point(aes(x = Old_Irrigation, y = Crop), color = "maroon", size = 2) +
  geom_point(aes(x = New_Irrigation, y = Crop), color = "darkgreen", size = 2) +
  # Add arrows between old and new irrigation levels
  geom_segment(aes(
    x = Start_Irrigation,
    xend = End_Irrigation,
    y = Crop,
    yend = Crop),
    arrow = arrow(type = "closed", length = unit(0.05, "inches")),
    color = "grey", size = 0.8) +
  # Customize the plot
  labs(title = "", x = "Irrigation Level (mm)", y = "") +
  theme_minimal() +
  # Custom x-axis scale and breaks
  scale_x_continuous(
    breaks = seq(0, 500, by = 20)
  ) +
  # Adjust theme: vertical x-axis labels
  theme(
    axis.line = element_line(color = "black"), # Add main axes lines
    axis.ticks.x = element_line(color = "black", size = 0.5), # Add x-axis tick marks
    axis.ticks.length.x = unit(0.2, "cm"),    # Length of the tick marks
    axis.title = element_text(size = 14),     # Increase axis title size
    axis.text = element_text(size = 12),      # Increase axis text size
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Make x-axis labels vertical
    panel.grid = element_blank(),             # Remove all grid lines
    panel.background = element_blank(),       # Remove background
    panel.spacing = unit(0.5, "lines")        # Adjust panel spacing
  )

# Display the plot
print(plot_irrigation)



# Create the profit plot with separate x-axes for each crop using facet_wrap


# Reorder Crop to bring Wheat first
data <- data %>%
  mutate(Crop = factor(Crop, levels = c("Wheat", setdiff(unique(Crop), "Wheat"))))

# Create the profit plot with adjusted segment positions for each crop
plot_profit <- ggplot(data) +
  # Add points for the old profit (blue) and new profit (green)
  geom_point(aes(x = Old_Profit, y = Crop), color = "red", size = 2) +
  geom_point(aes(x = New_Profit, y = Crop), color = "darkgreen", size = 2) +
  # Add customized arrows for each crop
  geom_segment(
    aes(
      # Adjust arrow start for each crop
      x = case_when(
        Crop == "Wheat" ~ Old_Profit - 5,  # Wheat moves opposite direction
        Crop == "Canola" ~ Old_Profit + 10,  # Canola slightly adjusted
        Crop == "Potato" ~ Old_Profit + 150, # Potato more significantly adjusted
        TRUE ~ Old_Profit  # Default for other crops
      ),
      # Adjust arrow end for each crop
      xend = case_when(
        Crop == "Wheat" ~ New_Profit + 5,  # Wheat moves opposite direction
        Crop == "Canola" ~ New_Profit - 10,  # Canola slightly adjusted
        Crop == "Potato" ~ New_Profit - 150, # Potato more significantly adjusted
        TRUE ~ New_Profit  # Default for other crops
      ),
      y = Crop, yend = Crop
    ),
    arrow = arrow(type = "closed", length = unit(0.05, "inches")),  # Adjusted arrowhead size
    color = "darkgrey", size = 0.8  # Adjusted line thickness
  ) +
  # Customize the plot for profit
  labs(title = "", x = "Value per acre ($)", y = NULL) +  # Remove y-axis label
  theme_minimal() +
  facet_wrap(~ Crop, scales = "free_x") +  # Facet by crop with separate x-axes
  theme(
    axis.line = element_line(color = "black"), # Add main axes lines
    axis.ticks.x = element_line(color = "black", size = 0.5), # Add x-axis tick marks
    axis.ticks.length.x = unit(0.2, "cm"),    # Length of the tick marks
    axis.title = element_text(size = 14),     # Increase axis title size
    axis.text = element_text(size = 12),      # Increase axis text size
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Make x-axis labels vertical
    panel.grid = element_blank(),             # Remove all grid lines
    panel.background = element_blank(),       # Remove background
    panel.spacing = unit(0.5, "lines")        # Adjust panel spacing
  )

# Display the plot
print(plot_profit)


# Create the plot with values on the x-axis and connecting with a horizontal line and arrow
plot_total_change_with_arrow <- ggplot(total_change) +
  # Add point for old profit (maroon)
  geom_point(aes(x = Change[1], y = 1), color = "maroon", size = 3) +  # Old profit point
  # Add point for new profit (darkgreen)
  geom_point(aes(x = Change[2], y = 1), color = "darkgreen", size = 3) +  # New profit point
  # Add a horizontal line with arrow pointing from low to high
  geom_segment(aes(x = min(Change) + 100, xend = max(Change) - 100, y = 1, yend = 1),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), # Add arrow
               color = "grey", size = 0.8) +  # Line connecting the points
  # Customize the plot
  labs(title = "", x = "Total Value per acre ($)", y = "") +
  scale_x_continuous(breaks = seq(0, max(total_change$Change) + 10, by = 500)) +  # Custom x-axis scale and breaks
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.y = element_blank(),  # Remove y-axis title
    legend.position = "none",  # Remove the legend
    axis.title.x = element_text(size = 14),  # Increase x-axis title size
    axis.text = element_text(size = 12),  # Increase axis text size
    axis.ticks.x = element_line(color = "black", size = 0.5),  # Keep x-axis ticks
    axis.line.x = element_line(color = "black")  # Keep main x-axis line
  )

# Display the plot
print(plot_total_change_with_arrow)

