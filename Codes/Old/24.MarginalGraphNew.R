#clear memory
rm(list = ls())

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fixest)
library(priceR)
library(purrr)
library(tidyverse)

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
irrigation_levels <- c(200,220,240, 260, 280, 300,320,340,360,380,400,420,440,460,480,500)  # Adjust as necessary

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


# dataframe
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



wheat_weighted <- df_wheat%>%
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
    w_wheat_val = Estimate*weight)%>%
  select(year,w_wheat_val,Irrigation_Level)


canola_weighted <- df_canola%>%
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
    w_canol_val = Estimate*weight)%>%
  select(year,w_canol_val,Irrigation_Level)

df <- wheat_weighted%>%
  left_join(canola_weighted)%>%
  mutate(ave_val =w_canol_val+w_wheat_val)%>%
  select(year,ave_val,Irrigation_Level)





# Create the plot with free y-axis scales

ggplot(df, aes(x = factor(Irrigation_Level), y = ave_val)) +
  geom_point(size = 1, color = "red") +  # Plot points for estimates
  geom_line(aes(group = year), size = 0.3, color = "#1545b4", linetype = "dashed") +  # Add a dashed line connecting points
  labs(x = "Irrigation Level (mm)", 
       y = expression("Marginal Value ($m"^{-3}*")"), 
       title = "") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),  # Rotate x-axis labels vertically
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),  # Bold and larger text for strip.text
    panel.spacing.x = unit(0.5, "cm"), 
    panel.spacing.y = unit(0.5, "cm"),
    legend.position = "bottom"  # Move the legend to the bottom
  ) +
  facet_wrap(~year, nrow = 2, ncol = 3, scales = "free", strip.position = "top")  # Allow free x and y scales for each plot



#### Average graph #### Average graph #### Average graph #### Average graph

df <- df%>%
  group_by(Irrigation_Level)%>%
  mutate(mean_mar_value = mean(ave_val))%>%
  select(-year)%>%
  distinct(Irrigation_Level, .keep_all = T)


ggplot(df, aes(x = factor(Irrigation_Level), y = mean_mar_value)) +
  geom_point(size = 1.5, color = "red") +  # Plot points for estimates
  geom_line(aes(group = 1),size = 0.5, color = "#1545b4", linetype = "dashed") +  # Add a dashed line connecting points
  labs(x = "Irrigation Level (mm)", 
       y = expression("Marginal Value ($m"^{-3}*")"),  
       title = "") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),  # Rotate x-axis labels vertically
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),  # Bold and larger text for strip.text
    panel.spacing.x = unit(0.5, "cm"), 
    panel.spacing.y = unit(0.5, "cm"),
    legend.position = "bottom"  # Move the legend to the bottom
  ) 









