#clear memory
rm(list = ls())

# wheat
library(dplyr)
library(tidyverse)
library(fixest)
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



df <- rbind(wheat,canola)%>%
  mutate(wheat = ifelse(crop == "wheat", 1, 0))%>%
  mutate(canola = ifelse(crop == "canola", 1, 0))


###Average graph - not site wise estimations


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100) # Adjust levels as desired

# Initialize the results data frame
results <- data.frame(Irrigation_Level = numeric(), Crop_Type = character(), Estimate = numeric(), Std_Error = numeric())

# List of crops
crops <- list("wheat", "canola")  # Adjust to your crop names

# Loop over each crop and irrigation level
for (crop in crops) {
  for (level in irrigation_levels) {
    
    # Filter the data for each crop and irrigation level range
    data_subset <- subset(get(crop), Total_Irrigation_mm < level)
    
    # Fit the fixed effects model
    model <- feols(val_mm ~ log(irrq_m3) | Site + year, data = data_subset)
    
    # Extract the coefficient and standard error for log(irrq_m3)
    estimate <- coef(model)["log(irrq_m3)"]
    std_error <- se(model)["log(irrq_m3)"]
    
    # Store the result in the results data frame, including crop type
    results <- rbind(results, data.frame(Irrigation_Level = level, Crop_Type = crop, Estimate = estimate, Std_Error = std_error))
  }
}

# Plot the estimates for each crop with a smooth line and without error bars
ggplot(results, aes(x = factor(Irrigation_Level), y = Estimate, color = Crop_Type)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +  # Add points for each estimate
  # Add loess smoothed line with method "lm" for linear or "loess" for non-linear
  geom_smooth(aes(group = Crop_Type), method = "loess", se = FALSE, size = 1, linetype = "dashed") +  # Smoothing line for each crop
  labs(x = "Total Irrigation Level (mm)", 
       y = expression("Marginal Value " ~ "($"*m^3*")"), 
       title = "") +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = c("wheat" = "maroon4", "canola" = "green4"))  # Customize colors for crops


#### Individual curve for each site-year combo 


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

#wheat
# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat$Site)
years <- c(2016)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2016 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_wheat <- rbind(results_wheat, data.frame(Site = site, Year = year, 
                                                         Irrigation_Level = level, Estimate = estimate, 
                                                         Std_Error = std_error))
      }
    }
  }
}


wheat2016 <- results_wheat

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat$Site)
years <- c(2017)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2017 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_wheat <- rbind(results_wheat, data.frame(Site = site, Year = year, 
                                                         Irrigation_Level = level, Estimate = estimate, 
                                                         Std_Error = std_error))
      }
    }
  }
}


wheat2017 <- results_wheat


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat$Site)
years <- c(2018)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2018 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_wheat <- rbind(results_wheat, data.frame(Site = site, Year = year, 
                                                         Irrigation_Level = level, Estimate = estimate, 
                                                         Std_Error = std_error))
      }
    }
  }
}


wheat2018 <- results_wheat

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat$Site)
years <- c(2019)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2019 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_wheat <- rbind(results_wheat, data.frame(Site = site, Year = year, 
                                                         Irrigation_Level = level, Estimate = estimate, 
                                                         Std_Error = std_error))
      }
    }
  }
}


wheat2019 <- results_wheat

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat$Site)
years <- c(2020)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2020 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_wheat <- rbind(results_wheat, data.frame(Site = site, Year = year, 
                                                         Irrigation_Level = level, Estimate = estimate, 
                                                         Std_Error = std_error))
      }
    }
  }
}


wheat2020 <- results_wheat


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat$Site)
years <- c(2021)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2021 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_wheat <- rbind(results_wheat, data.frame(Site = site, Year = year, 
                                                         Irrigation_Level = level, Estimate = estimate, 
                                                         Std_Error = std_error))
      }
    }
  }
}


wheat2021 <- results_wheat



# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat$Site)
years <- c(2022)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2022 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_wheat <- rbind(results_wheat, data.frame(Site = site, Year = year, 
                                                         Irrigation_Level = level, Estimate = estimate, 
                                                         Std_Error = std_error))
      }
    }
  }
}


wheat2022 <- results_wheat


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat$Site)
years <- c(2023)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2023 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_wheat <- rbind(results_wheat, data.frame(Site = site, Year = year, 
                                                         Irrigation_Level = level, Estimate = estimate, 
                                                         Std_Error = std_error))
      }
    }
  }
}


wheat2023 <- results_wheat



df_wheat <- rbind(wheat2018, wheat2019, wheat2020, wheat2021, wheat2022, wheat2023)

df1 <- df_wheat%>%
  group_by(Site,Irrigation_Level)%>%
  mutate(ave_Estimate = mean(Estimate))%>%
  ungroup()%>%
  distinct(Site,Irrigation_Level, .keep_all = T)%>%
  select(-Year, -Estimate)%>%
  mutate(Year = "Average")%>%
  rename(Estimate=ave_Estimate)%>%
  select(Site,Year,Irrigation_Level,Estimate,Std_Error)


df_wheat <- rbind(df_wheat,df1)


ggplot(df_wheat, aes(x = as.factor(Irrigation_Level), y = Estimate, fill = as.factor(Year))) +
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
    y = expression("Marginal Value ($/m"^3*")"),
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
  scale_y_continuous(
    breaks = seq(0.1, 0.8, by = 0.1),  # Specify breaks for y-axis
    limits = c(0.1, 0.8)  # Set limits for y-axis to be from 0.1 to 0.8
  )
scale_x_discrete(labels = function(x) paste("", x))  # Customize x-axis labels for clarity




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



#canola

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola$Site)
years <- c(2016)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2016 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_canola <- rbind(results_canola, data.frame(Site = site, Year = year, 
                                                         Irrigation_Level = level, Estimate = estimate, 
                                                         Std_Error = std_error))
      }
    }
  }
}


canola2016 <- results_canola


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola$Site)
years <- c(2017)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2017 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_canola <- rbind(results_canola, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


canola2017 <- results_canola


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola$Site)
years <- c(2018)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2018 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_canola <- rbind(results_canola, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


canola2018 <- results_canola


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola$Site)
years <- c(2019)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2019 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_canola <- rbind(results_canola, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


canola2019 <- results_canola

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola$Site)
years <- c(2020)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2020 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_canola <- rbind(results_canola, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


canola2020 <- results_canola




# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola$Site)
years <- c(2021)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2021 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_canola <- rbind(results_canola, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


canola2021 <- results_canola


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola$Site)
years <- c(2022)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2022 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_canola <- rbind(results_canola, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


canola2022 <- results_canola

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola$Site)
years <- c(2023)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2023 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_canola <- rbind(results_canola, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


canola2023 <- results_canola





df_canola <- rbind(canola2018,canola2019, canola2020, canola2021, canola2022, canola2023)


df1 <- df_canola%>%
  group_by(Site,Irrigation_Level)%>%
  mutate(ave_Estimate = mean(Estimate))%>%
  ungroup()%>%
  distinct(Site,Irrigation_Level, .keep_all = T)%>%
  select(-Year, -Estimate)%>%
  mutate(Year = "Average")%>%
  rename(Estimate=ave_Estimate)%>%
  select(Site,Year,Irrigation_Level,Estimate,Std_Error)


df_canola <- rbind(df_canola,df1)


ggplot(df_canola, aes(x = as.factor(Irrigation_Level), y = Estimate, fill = as.factor(Year))) +
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
    y = expression("Marginal Value ($/m"^3*")"),
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
  scale_y_continuous(
    breaks = seq(0.1, 0.8, by = 0.1),  # Specify breaks for y-axis
    limits = c(0.1, 0.8)  # Set limits for y-axis to be from 0.1 to 0.8
  )
scale_x_discrete(labels = function(x) paste("", x))  # Customize x-axis labels for clarity



# dataframe
year <- c("2018", "2019", "2020","2021","2022","2023")
ar_wheat <- c(24.4,31.2,31.7,34.2,33.7,35.4)
ar_canola <- c(30.0,22.1,28.9,32,31.8,27.8)

# Create dataframe
weights <- data.frame(Year = year, canola = ar_canola, wheat = ar_wheat)%>%
  mutate(area=wheat+canola)%>%
  mutate(canola_w=canola/area)%>%
  mutate(wheat_w=wheat/area)


yr2018canola <- weights%>%
  filter(Year == 2018) %>%
  pull(canola_w)
yr2018wheat <- weights%>%
  filter(Year == 2018) %>%
  pull(wheat_w)
yr2019canola <- weights%>%
  filter(Year == 2019) %>%
  pull(canola_w)
yr2019wheat <- weights%>%
  filter(Year == 2019) %>%
  pull(wheat_w)
yr2020canola <- weights%>%
  filter(Year == 2020) %>%
  pull(canola_w)
yr2020wheat <- weights%>%
  filter(Year == 2020) %>%
  pull(wheat_w)
yr2021canola <- weights%>%
  filter(Year == 2021) %>%
  pull(canola_w)
yr2021wheat <- weights%>%
  filter(Year == 2021) %>%
  pull(wheat_w)
yr2022canola <- weights%>%
  filter(Year == 2022) %>%
  pull(canola_w)
yr2022wheat <- weights%>%
  filter(Year == 2022) %>%
  pull(wheat_w)
yr2023canola <- weights%>%
  filter(Year == 2023) %>%
  pull(canola_w)
yr2023wheat <- weights%>%
  filter(Year == 2023) %>%
  pull(wheat_w)

df_wheat <- df_wheat%>%
  filter(Year!="Average")

df_canola <- df_canola%>%
  filter(Year!="Average")

wheat_weighted <- df_wheat%>%
  filter(Year > 2017)%>%
  mutate(
    weight = case_when(
      Year == 2018 ~ yr2018wheat,
      Year == 2019 ~ yr2019wheat,
      Year == 2020 ~ yr2020wheat,
      Year == 2021 ~ yr2021wheat,
      Year == 2022 ~ yr2022wheat,
      Year == 2023 ~ yr2023wheat
    )
  )%>%
  mutate(
    w_wheat_val = Estimate*weight)%>%
  select(Year,Site,w_wheat_val,Irrigation_Level)


canola_weighted <- df_canola%>%
  filter(Year > 2017)%>%
  mutate(
    weight = case_when(
      Year == 2018 ~ yr2018canola,
      Year == 2019 ~ yr2019canola,
      Year == 2020 ~ yr2020canola,
      Year == 2021 ~ yr2021canola,
      Year == 2022 ~ yr2022canola,
      Year == 2023 ~ yr2023canola
    )
  )%>%
  mutate(
    w_canol_val = Estimate*weight)%>%
  select(Year,Site,w_canol_val,Irrigation_Level)



df <- wheat_weighted%>%
  left_join(canola_weighted)%>%
  mutate(ave_val =w_canol_val+w_wheat_val)%>%
  select(Year,Site,ave_val,Irrigation_Level)


df1 <- df%>%
  group_by(Site,Irrigation_Level)%>%
  mutate(ave_val_all = mean(ave_val))%>%
  ungroup()%>%
  distinct(Site,Irrigation_Level, .keep_all = T)%>%
  select(-Year, -ave_val)%>%
  mutate(Year = "Average")%>%
  rename(ave_val=ave_val_all)%>%
  select(Year,Site,ave_val,Irrigation_Level)
  

df <- rbind(df,df1)

ggplot(df, aes(x = as.factor(Irrigation_Level), y = ave_val, fill = as.factor(Year))) +
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
    y = expression("Marginal Value ($/m"^3*")"),
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
  scale_y_continuous(
    breaks = seq(0.1, 0.8, by = 0.05),  # Specify breaks for y-axis
    limits = c(0.1, 0.6)  # Set limits for y-axis to be from 0.1 to 0.8
  )+
  scale_x_discrete(labels = function(x) paste("", x))  # Customize x-axis labels for clarity


# Define a custom color palette by selecting specific colors from ColorBrewer
custom_palette <- c(
                    "2018" = "#d73027",      
                    "2019" = "#fc8d59",
                    "2020" = "#fee090",
                    "2021" = "#91bfdb",
                    "2022" = "#d6bfd8",
                    "2023" = "#033027",
                    "Average" = "#1545b4"
)


df <- df%>%
  filter(Irrigation_Level!=800)


# Create the boxplot with the custom color palette
ggplot(df, aes(x = as.factor(Irrigation_Level), y = ave_val, fill = as.factor(Year))) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.7),  # Adjust width to control spacing between box plots within each irrigation level
    width = 0.3,  # Width of each box plot
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "",
    x = "Irrigation Level (mm)",
    y = expression("Marginal Value ($m"^{-3}*")"),
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(margin = margin(t = 10),size = 12),  # Add margin to x-axis labels for spacing
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.y = element_text(size = 12),   # Adjust y-axis title size
    axis.title.x = element_text(size = 12),  # Adjust x-axis title size
    legend.text = element_text(size = 12)    # Adjust legend text size
    
  ) +
  scale_fill_manual(values = custom_palette) +  # Use the custom color palette
  scale_y_continuous(
    breaks = seq(0.1, 0.8, by = 0.05),  # Specify breaks for y-axis
    limits = c(0.1, 0.8)  # Set limits for y-axis to be from 0.1 to 0.6
  ) +
  scale_x_discrete(labels = function(x) paste("", x))  # Customize x-axis labels for clarity



######## Seperate graphs ######## Seperate graphs for year wise 


t <- df%>%
  filter(Year!="Average")


target_values <- data.frame(
  Year = c(2018,2019,2020,2021,2022,2023),
  target = c(0.292903, 0.2844536,0.2729694,0.2580631, 0.3633408,0.3853838)  # Replace with your target values for each year
)


# Assuming `df` is your data frame
# Create a custom color palette
custom_palette <- c(
  "2018" = "#d73027",      
  "2019" = "#fc8d59",
  "2020" = "#fee090",
  "2021" = "#91bfdb",
  "2022" = "#d6bfd8",
  "2023" = "#033027",
  "Average" = "#1545b4"
)



# Example of adding y-axis range limits to your dataset
t <- t %>%
  mutate(y_min = case_when(
    Year == 2018 ~ 0.2,  
    Year == 2019 ~ 0.01,
    Year == 2020 ~ 0.2,  
    Year == 2021 ~ 0.2,
    Year == 2022 ~ 0.6,  
    Year == 2023 ~ 0.2,  
    TRUE ~ 0.0  # Default minimum
  ),
  y_max = case_when(
    Year == 2018 ~ 0.6,  
    Year == 2019 ~ 0.6,
    Year == 2020 ~ 0.7,  
    Year == 2021 ~ 0.85,  
    Year == 2022 ~ 1.1,  
    Year == 2023 ~ 0.85,  
    TRUE ~ 1.0  
  ))





ggplot(t, aes(x = as.factor(Irrigation_Level), y = ave_val, fill = as.factor(Year))) +
  # Add error bars at min and max values
  stat_summary(
    fun.data = function(x) {
      return(data.frame(
        ymin = min(x),
        ymax = max(x)
      ))
    },
    geom = "errorbar",
    width = 0.2,
    color = "black",
    position = position_dodge(0.4)
  ) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.4),  # Adjust width for spacing
    width = 0.2,  # Width of the box plot
    color = "black",
    alpha = 1
  ) +
  geom_hline(
    data = target_values,
    aes(yintercept = target, linetype = "Average value"),
    color = "#7D7D7D",
    size = 0.8,
    show.legend = TRUE  # Ensure the line is added to the legend
  ) +
  labs(
    title = "",
    x = "Irrigation Level (mm)",
    y = expression("Marginal Value ($m"^{-3}*")"),
    fill = "Year",
    linetype = "Legend"  # Title for the linetype legend
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(margin = margin(t = 10), size = 12),
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
  scale_fill_manual(values = custom_palette) +
  scale_x_discrete(labels = function(x) paste("", x)) +
  scale_linetype_manual(
    values = c("Average value" = "dashed")
  ) +
  facet_wrap(~Year, ncol = 2, scales = "free_y") +  # Allow free y scales
  guides(fill = "none")  # Remove the legend for `fill` (Year)


####### Average Graph only 


t <- df%>%
  filter(Year=="Average")


# Define a custom color palette by selecting specific colors from ColorBrewer
custom_palette <- c(
  "2018" = "#d73027",      
  "2019" = "#fc8d59",
  "2020" = "#fee090",
  "2021" = "#91bfdb",
  "2022" = "#d6bfd8",
  "2023" = "#033027",
  "Average" = "#1545b4"
)



# Create the boxplot with the custom color palette
ggplot(t, aes(x = as.factor(Irrigation_Level), y = ave_val, fill = as.factor(Year))) +
  stat_summary(
    fun.data = function(x) {
      return(data.frame(
        ymin = min(x),
        ymax = max(x)
      ))
    },
    geom = "errorbar",
    width = 0.2,
    color = "black",
    position = position_dodge(0.4)
  ) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.3),  # Adjust width to control spacing between box plots within each irrigation level
    width = 0.1,  # Width of each box plot
    color = "black",
    alpha = 1
  ) +
  labs(
    title = "",
    x = "Irrigation Level (mm)",
    y = expression("Marginal Value ($m"^{-3}*")"),
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(margin = margin(t = 10),size = 12),  # Add margin to x-axis labels for spacing
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.y = element_text(size = 12),   # Adjust y-axis title size
    axis.title.x = element_text(size = 12),  # Adjust x-axis title size
    legend.text = element_text(size = 12),    # Adjust legend text size
    legend.position = "none",  # Remove the legend
  ) +
  scale_fill_manual(values = custom_palette) +  # Use the custom color palette
  scale_y_continuous(
    breaks = seq(0.3, 0.6, by = 0.02),  # Specify breaks for y-axis
    limits = c(0.4, 0.6)  # Set limits for y-axis to be from 0.1 to 0.6
  ) +
  scale_x_discrete(labels = function(x) paste("", x))  # Customize x-axis labels for clarity




#################################################################################
#################################################################################
#################################################################################

# Table


canola <- df_canola%>%
  group_by(Irrigation_Level)%>%
  mutate(mar_val = mean(Estimate),
         sd = sd(Std_Error))%>%
  distinct(Irrigation_Level, .keep_all = T)%>%
  ungroup()%>%
  filter(Irrigation_Level!=200)%>%
  mutate(crop="canola")%>%
  select(Irrigation_Level,mar_val,sd)%>%
  rename(canola = mar_val,
         canola_sd =sd)


wheat <- df_wheat%>%
  group_by(Irrigation_Level)%>%
  mutate(mar_val = mean(Estimate),
         sd = sd(Std_Error))%>%
  distinct(Irrigation_Level, .keep_all = T)%>%
  ungroup()%>%
  filter(Irrigation_Level!=200)%>%
  mutate(crop="wheat")%>%
  select(Irrigation_Level,mar_val,sd)%>%
  rename(wheat = mar_val,
         wheat_sd =sd)



weighted <- df%>%
  group_by(Irrigation_Level)%>%
  mutate(mar_val = mean(ave_val),
         sd = sd(ave_val))%>%
  distinct(Irrigation_Level, .keep_all = T)%>%
  ungroup()%>%
  filter(Irrigation_Level!=200)%>%
  mutate(crop="weighted")%>%
  select(Irrigation_Level,mar_val,sd)%>%
  rename(weighted = mar_val,
         weighted_sd =sd)

  
df_all <- canola%>%
  left_join(wheat)%>%
  left_join(weighted)%>%
  select(-weighted_sd,-wheat_sd, -canola_sd)


df_all <- data.frame(lapply(df_all, function(x) {
  if (is.numeric(x)) round(x, 2) else x
}))



library(kableExtra)

# Create a LaTeX table with customization
# Generate LaTeX table without striped rows
latex_table <- kable(df_all, format = "latex", booktabs = TRUE, caption = "Sample Table") %>%
  kable_styling(latex_options = c("hold_position"))  # No "striped"


file_conn <- file("./results/Tables/marginal_historical.tex")
writeLines(latex_table, file_conn)
close(file_conn)




year <- t%>%
  distinct(year)




#### Map Map

df <- df%>%
  filter(Irrigation_Level==700)

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
library(patchwork)

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


df_map <- st_transform(df_map, crs = 4326)


# Calculate the min and max values for ave_val across all years
ave_val_range <- range(df_map$ave_val, na.rm = TRUE)


# Function to create a ggplot for each year with a common color scale
create_plot <- function(data, year) {
  ggplot(data) +
    geom_sf(aes(fill = ave_val), color = NA) +   
    scale_fill_viridis(
      option = "D", 
      name = expression("Marginal Value ($/m"^3*")"),
      breaks = seq(ave_val_range[1], ave_val_range[2], length.out = 6),
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


# List of data frames by year
df_list <- list(
  "2016" = df_map %>% filter(Year == 2016),
  "2017" = df_map %>% filter(Year == 2017),
  "2018" = df_map %>% filter(Year == 2018),
  "2019" = df_map %>% filter(Year == 2019),
  "2020" = df_map %>% filter(Year == 2020)
)

# Generate all plots and store them in a list
plots <- lapply(names(df_list), function(year) {
  create_plot(df_list[[year]], year)
})

# Combine plots with a shared legend using patchwork
combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Display the combined plot
combined_plot


# Set the legend position to the right of the combined plot
combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "right")

# Display the combined plot with the legend on the side
combined_plot




# Check for one year plot manually
ggplot(df_map %>% filter(year == 2020)) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(
    option = "D", 
    name = expression("Average Value ($/m"^3*")"),
    breaks = seq(ave_val_range[1], ave_val_range[2], length.out = 6),
    limits = ave_val_range,
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(title = "2020") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 10)
  )






#########################################
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
  left_join(results)


df_map <- st_transform(df_map, crs = 4326)


library(dplyr)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(scales)


df_2016 <- df_map%>%
  filter(Year==2016)

df_2017 <- df_map%>%
  filter(Year==2017)


p1 <- ggplot(data = df_2016) +
  geom_sf(aes(fill = Estimate), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Marginal Value ($/m"^3*")"),
                     breaks = breaks_pretty(n = 5),  # Automatically generate breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2016") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 12))


p2 <- ggplot(data = df_2017) +
  geom_sf(aes(fill = Estimate), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Marginal Value ($/m"^3*")"),
                     breaks = breaks_pretty(n = 5),  # Automatically generate breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2017") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 12))


library(patchwork)

# Assuming you have a list of plots, for example:
plots <- list(p1, p2)  # Add your ggplot objects here


# Combine the plots in a 3 by 3 grid
combined_plot <- wrap_plots(plots, ncol = 3)

# Display the combined plot
print(combined_plot)





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

results_canola <- results_canola%>%
  rename(site=Site,
         year=Year)


df1 <- results_canola%>%
  left_join(weather, by=c("year","site"))


df2<- canola%>%
  select(year,Site, irrq_m3,Total_Irrigation_mm)%>%
  rename(site=Site)

df2 <- df2%>%
  left_join(df2, by=c("year","site"))



library(fixest)



feols(Estimate ~ log(irrq_m3)  + ave_prcp, data = df1)





