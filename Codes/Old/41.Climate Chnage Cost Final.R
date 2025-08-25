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
  pattern = "^merged_simulation_results_wheat_300mm_(2030)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
wheat_prcp_red <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_irri_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Max_Irrigation_mm,Yield_irri_tonne_per_ha,Total_Irrigation_mm)%>%
  filter(Max_Irrigation_mm < 301)




##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed 

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_wheat_dry_(2030)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
wheat_prcp_red_dry <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_dry_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Yield_dry_tonne_per_ha)


wheat_prcp_red <- wheat_prcp_red%>%
  #left_join(CMIP126_prcp)%>%
  left_join(wheat_prcp_red_dry)%>%
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

wheat_prcp_red <- wheat_prcp_red%>%
  mutate(return_rf = `Dry yield rainfed (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(Year, Site_ID,Max_Irrigation_mm,prof_dif,val_mm,irrq_m3)





wheat_prcp_red <- wheat_prcp_red%>%
  filter(Year==2030)%>%
  arrange(Site_ID, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(Year,Site_ID, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat_prcp_red)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat_red <- data.frame(
  Max_Irrigation_mm = seq(1, 300, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat_red$mv_pred_wheat_red <- predict(poly_fit, newdata = predicted_wheat_red)



ggplot(predicted_wheat_red, aes(x = Max_Irrigation_mm, y = mv_pred_wheat_red)) +
  geom_point(color = "blue", size = 1) +  # Blue dots with size 3
  labs(x = "Irrigation (mm)", y = "Value ($)", title = "Irrigation vs. Value") +
  theme_minimal()

##################################################################################


###### Wheat ###### Wheat ###### Wheat ###### Wheat ###### Wheat ###### Wheat
######## CMIP126 Irrigation ######## CMIP126 Irrigation ######## CMIP126 Irrigation

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_wheat_300mm_noPrcpChnage_(2030)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
wheat_prcp_orig <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_irri_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Max_Irrigation_mm,Yield_irri_tonne_per_ha,Total_Irrigation_mm)%>%
  filter(Max_Irrigation_mm < 301)




##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed ##### Rain-fed 

# Open files
file_list <- list.files(
  path = file_path, 
  pattern = "^merged_simulation_results_wheat_dry_noPrcpChnage_(2030)\\.csv$", 
  full.names = TRUE
)


# Process the files and add the Year column based on simulated file name
wheat_prcp_ori_dry <- map_dfr(file_list, ~ {
  # Extract the year from the file name
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", .x))
  
  # Read the CSV and add the year column
  read_csv(.x) %>%
    mutate(Year = year)
})%>%
  rename(Yield_dry_tonne_per_ha = Yield_tonne_per_ha)%>%
  select(Site_ID,Year,Yield_dry_tonne_per_ha)


wheat_prcp_orig <- wheat_prcp_orig%>%
  #left_join(CMIP126_prcp)%>%
  left_join(wheat_prcp_ori_dry)%>%
  mutate(`Dry yield irri (bu/ac)`= Yield_irri_tonne_per_ha*14.86995818)%>%
  mutate(`Dry yield rainfed (bu/ac)` = Yield_dry_tonne_per_ha*14.86995818)%>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))



wheat_prcp_orig <- wheat_prcp_orig%>%
  mutate(return_rf = `Dry yield rainfed (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(Year, Site_ID,Max_Irrigation_mm,prof_dif,val_mm,irrq_m3)





wheat_prcp_orig <- wheat_prcp_orig%>%
  filter(Year==2030)%>%
  arrange(Site_ID, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(Year,Site_ID, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat_prcp_orig)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat_ori <- data.frame(
  Max_Irrigation_mm = seq(1, 300, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat_ori$mv_pred_wheat_ori <- predict(poly_fit, newdata = predicted_wheat_ori)



ggplot(predicted_wheat_ori, aes(x = Max_Irrigation_mm, y = mv_pred_wheat_ori)) +
  geom_point(color = "blue", size = 1) +  # Blue dots with size 3
  labs(x = "Irrigation (mm)", y = "Value ($)", title = "Irrigation vs. Value") +
  theme_minimal()

df <- predicted_wheat_red%>%
  left_join(predicted_wheat_ori)

ggplot(df, aes(x = Max_Irrigation_mm)) +
  geom_line(aes(y = mv_pred_wheat_red, color = "reduction"), size = 1) +
  geom_line(aes(y = mv_pred_wheat_ori, color = "Original"), size = 1) +
  labs(
    x = "Irrigation Level (mm)",
    y = "Marginal Value ($)",
    color = "Crop",
    title = "Marginal Value vs. Irrigation for Wheat & Canola"
  ) +
  theme_minimal()


predicted_wheat_red <- predicted_wheat_red %>%
  select(Max_Irrigation_mm, mv_pred_wheat_red) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)


predicted_wheat_ori <- predicted_wheat_ori %>%
  select(Max_Irrigation_mm, mv_pred_wheat_ori) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))

# Compute cumulative AUC using the trapezoidal rule
predicted_wheat_red$cumulative_auc_wheat_red <- cumsum(c(0, diff(predicted_wheat_red$irrq_m3) * (head(predicted_wheat_red$mv_pred_wheat_red, -1) + tail(predicted_wheat_red$mv_pred_wheat_red, -1)) / 2))

predicted_wheat_ori$cumulative_auc_wheat_ori <- cumsum(c(0, diff(predicted_wheat_ori$irrq_m3) * (head(predicted_wheat_ori$mv_pred_wheat_ori, -1) + tail(predicted_wheat_ori$mv_pred_wheat_ori, -1)) / 2))

total_auc_wheat <- sum(diff(predicted_wheat_red$irrq_m3) * 
                         (head(predicted_wheat_red$mv_pred_wheat_red, -1) + 
                            tail(predicted_wheat_red$mv_pred_wheat_red, -1)) / 2)

