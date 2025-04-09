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


wheat <- df_wheat%>%
  select(year,Irrigation_Level,Estimate)%>%
  rename(mv_wheat = Estimate)

canola <- df_canola%>%
  select(year,Irrigation_Level,Estimate)%>%
  rename(mv_canola = Estimate)

# Adjust Irrigation_Level in dataframe 2 to match the pattern
canola <- canola %>%
  mutate(Irrigation_Level = Irrigation_Level - 20)



# Merge the two dataframes by adjusted irrigation levels
df <- left_join(wheat, canola, by = c("Irrigation_Level","year"))%>%
  rename(irri_wheat = Irrigation_Level)%>%
  mutate(irri_canola = irri_wheat+20)%>%
  mutate(net_premium = mv_canola-mv_wheat)%>%
  mutate(vol_peracre = 20*4.04685642)%>%
  mutate(valm3 = net_premium*vol_peracre)%>%
  drop_na()
    
  
# Plotting with ggplot - wheat vs canola 
ggplot(df, aes(x = factor(irri_wheat), y = valm3)) +
  # Add error bars using stat_summary
  stat_summary(
    fun.data = function(x) {
      data.frame(
        ymin = min(x), 
        ymax = max(x)
      )
    },
    geom = "errorbar", 
    width = 0.15, 
    size = 0.2,
    color = "black"  # Color of the error bars
  ) +
  # Add box plot
  geom_boxplot(
    aes(x = factor(irri_wheat), y = valm3),  # Ensure Value is passed in the box plot as well
    fill = "lightblue", 
    color = "black", 
    width = 0.2, 
    size = 0.2,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    name = "Irrigation Level in Wheat (mm)", 
    breaks = unique(df$irri_wheat), 
    labels = unique(df$irri_wheat)  # Bottom x-axis labels
  ) +
  scale_y_continuous(
    name = "Reallocation benefits ($/acre)", 
    breaks = seq(-10, 75, 5), 
    limits = c(-10, 85)
  ) +
  theme_minimal() +
  theme(
    axis.title.x.bottom = element_text(color = "black", size = 12),
    axis.title.x.top = element_text(color = "black", size = 12),
    axis.text.x.bottom = element_text(color = "black",size = 9),
    axis.text.x.top = element_text(color = "black",size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black"),
    axis.ticks.x.top = element_line(color = "black"),
    axis.ticks.x.bottom = element_line(color = "black"),
    axis.title.y = element_text(colour = "black",size = 12),
    axis.text.y.right = element_text(colour = "black",size = 12)
  ) +
  # Annotate the top axis
  annotate("text", 
           x = seq_along(unique(df$irri_wheat)), 
           y = rep(max(df$valm3) + 5, length(unique(df$irri_wheat))), 
           label = unique(df$irri_canola),  # Use `irri_canola` for top x-axis labels
           size = 3, color = "black", hjust = 0.5) +
  annotate("text", 
           x = length(unique(df$irri_wheat)) / 2, 
           y = max(df$valm3) + 10, 
           label = "Irrigation Level in Canola (mm)", 
           size = 4, color = "black", hjust = 0.5) 



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

wheat <- df_wheat%>%
  select(year,Irrigation_Level,Estimate)%>%
  rename(mv_wheat = Estimate)

potato <- df_potato%>%
  select(year,Irrigation_Level,Estimate)%>%
  rename(mv_potato = Estimate)

potato <- potato %>%
  mutate(Irrigation_Level = Irrigation_Level - 20)


# Merge the two dataframes by adjusted irrigation levels
df <- left_join(potato, wheat, by = c("Irrigation_Level","year"))%>%
  rename(irri_wheat = Irrigation_Level)%>%
  mutate(irri_potato = irri_wheat+20)%>%
  mutate(net_premium = mv_potato-mv_wheat)%>%
  mutate(vol_peracre = 20*4.04685642)%>%
  mutate(valm3 = net_premium*vol_peracre)%>%
  drop_na()


# Plotting with ggplot - wheat vs canola 
ggplot(df, aes(x = factor(irri_wheat), y = valm3)) +
  # Add error bars using stat_summary
  stat_summary(
    fun.data = function(x) {
      data.frame(
        ymin = min(x), 
        ymax = max(x)
      )
    },
    geom = "errorbar", 
    width = 0.15, 
    size = 0.2,
    color = "black"  # Color of the error bars
  ) +
  # Add box plot
  geom_boxplot(
    aes(x = factor(irri_wheat), y = valm3),  # Ensure Value is passed in the box plot as well
    fill = "lightblue", 
    color = "black", 
    width = 0.2, 
    size = 0.2,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    name = "Irrigation Level in Wheat (mm)", 
    breaks = unique(df$irri_wheat), 
    labels = unique(df$irri_wheat)  # Bottom x-axis labels
  ) +
  scale_y_continuous(
    name = "Reallocation benefits ($/acre)", 
    breaks = seq(200, 340, 10), 
    limits = c(200, 350)
  ) +
  theme_minimal() +
  theme(
    axis.title.x.bottom = element_text(color = "black", size = 12),
    axis.title.x.top = element_text(color = "black", size = 12),
    axis.text.x.bottom = element_text(color = "black"),
    axis.text.x.top = element_text(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black"),
    axis.ticks.x.top = element_line(color = "black"),
    axis.ticks.x.bottom = element_line(color = "black")
  ) +
  # Annotate the top axis
  annotate("text", 
           x = seq_along(unique(df$irri_wheat)), 
           y = rep(max(df$valm3) + 5, length(unique(df$irri_wheat))), 
           label = unique(df$irri_potato),  # Use `irri_canola` for top x-axis labels
           size = 3, color = "black", hjust = 0.5) +
  annotate("text", 
           x = length(unique(df$irri_potato)) / 2, 
           y = max(df$valm3) + 10, 
           label = "Irrigation Level in Potato (mm)", 
           size = 4, color = "black", hjust = 0.5) 


canola <- df_canola%>%
  select(year,Irrigation_Level,Estimate)%>%
  rename(mv_canola = Estimate)

potato <- df_potato%>%
  select(year,Irrigation_Level,Estimate)%>%
  rename(mv_potato = Estimate)

potato <- potato %>%
  mutate(Irrigation_Level = Irrigation_Level - 20)


# Merge the two dataframes by adjusted irrigation levels
df <- left_join(potato, canola, by = c("Irrigation_Level","year"))%>%
  rename(irri_canola = Irrigation_Level)%>%
  mutate(irri_potato = irri_canola+20)%>%
  mutate(net_premium = mv_potato-mv_canola)%>%
  mutate(vol_peracre = 20*4.04685642)%>%
  mutate(valm3 = net_premium*vol_peracre)%>%
  drop_na()




# Plotting with ggplot - wheat vs canola 
ggplot(df, aes(x = factor(irri_canola), y = valm3)) +
  # Add error bars using stat_summary
  stat_summary(
    fun.data = function(x) {
      data.frame(
        ymin = min(x), 
        ymax = max(x)
      )
    },
    geom = "errorbar", 
    width = 0.15, 
    size = 0.2,
    color = "black"  # Color of the error bars
  ) +
  # Add box plot
  geom_boxplot(
    aes(x = factor(irri_canola), y = valm3),  # Ensure Value is passed in the box plot as well
    fill = "lightblue", 
    color = "black", 
    width = 0.2, 
    size = 0.2,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    name = "Irrigation Level in Canola (mm)", 
    breaks = unique(df$irri_canola), 
    labels = unique(df$irri_canola)  # Bottom x-axis labels
  ) +
  scale_y_continuous(
    name = "Reallocation benefits ($/acre)", 
    breaks = seq(200, 300, 10), 
    limits = c(200, 310)
  ) +
  theme_minimal() +
  theme(
    axis.title.x.bottom = element_text(color = "black", size = 12),
    axis.title.x.top = element_text(color = "black", size = 12),
    axis.text.x.bottom = element_text(color = "black"),
    axis.text.x.top = element_text(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black"),
    axis.ticks.x.top = element_line(color = "black"),
    axis.ticks.x.bottom = element_line(color = "black")
  ) +
  # Annotate the top axis
  annotate("text", 
           x = seq_along(unique(df$irri_canola)), 
           y = rep(max(df$valm3) + 5, length(unique(df$irri_canola))), 
           label = unique(df$irri_potato),  # Use `irri_canola` for top x-axis labels
           size = 3, color = "black", hjust = 0.5) +
  annotate("text", 
           x = length(unique(df$irri_potato)) / 2, 
           y = max(df$valm3) + 10, 
           label = "Irrigation Level in Potato (mm)", 
           size = 4, color = "black", hjust = 0.5) 
















canola <- df_canola%>%
  select(year,Irrigation_Level,Estimate)%>%
  rename(mv_canola = Estimate)

# Adjust Irrigation_Level in dataframe 2 to match the pattern
canola <- canola %>%
  mutate(Irrigation_Level = Irrigation_Level - 20)



# Merge the two dataframes by adjusted irrigation levels
df <- left_join(wheat, canola, by = c("Irrigation_Level","year"))%>%
  rename(irri_wheat = Irrigation_Level)%>%
  mutate(irri_canola = irri_wheat+20)%>%
  mutate(net_premium = mv_canola-mv_wheat)%>%
  mutate(vol_peracre = 20*4.04685642)%>%
  mutate(valm3 = net_premium*vol_peracre)%>%
  drop_na()























df <- df_wheat%>%
  rbind(df_canola)






# Assuming your dataset has a column 'Crop_Type' indicating the crop
ggplot(df, aes(x = as.numeric(Irrigation_Level), y = Estimate, color = crop)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE) +
  labs(x = "Irrigation Level (mm)", 
       y = expression("Marginal Value ($m"^{-3}*")"),  
       title = "") +
  scale_x_continuous(
    breaks = unique(as.numeric(df_wheat$Irrigation_Level)),
    labels = unique(df_wheat$Irrigation_Level)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    legend.position = "bottom"
  )

