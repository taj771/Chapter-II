#clear memory
rm(list = ls())

# wheat
library(dplyr)
library(tidyverse)
library(fixest)
library(priceR)


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
  mutate(val_mm = profit_ir/irrq_m3)

#########Wheat #########Wheat #########Wheat #########Wheat #########Wheat

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
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(val_mm = profit_ir/irrq_m3)
#select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)


###### Canola ###### Canola ###### Canola ###### Canola ###### Canola

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


canola <- canola_marginal%>%
  left_join(return_canola)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(val_mm = profit_ir/irrq_m3)
#select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)



#wheat

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

### Canola ### Canola ### Canola ### Canola ### Canola ### Canola ### Canola

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


#potato


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_potato <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(potato$Site)
years <- c(2018)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- potato2018 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_potato <- rbind(results_potato, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


potato2018 <- results_potato


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_potato <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(potato$Site)
years <- c(2019)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- potato2019 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_potato <- rbind(results_potato, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


potato2019 <- results_potato

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_potato <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(potato$Site)
years <- c(2020)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- potato2020 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_potato <- rbind(results_potato, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


potato2020 <- results_potato




# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_potato <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(potato$Site)
years <- c(2021)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- potato2021 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_potato <- rbind(results_potato, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


potato2021 <- results_potato


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_potato <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(potato$Site)
years <- c(2022)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- potato2022 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_potato <- rbind(results_potato, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


potato2022 <- results_potato

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_potato <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(potato$Site)
years <- c(2023)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- potato2023 %>%
        filter(Site == site, year == year, Total_Irrigation_mm <= level)
      
      # Check if there are enough observations for the model
      if (nrow(data_subset) > 1) {  # Ensures there is enough data to fit the model
        
        # Fit the fixed effects model
        model <- feols(val_mm ~ log(irrq_m3), data = data_subset)
        
        # Extract the coefficient and standard error for log(irrq_m3)
        estimate <- coef(model)["log(irrq_m3)"]
        std_error <- se(model)["log(irrq_m3)"]
        
        # Store results in the data frame
        results_potato <- rbind(results_potato, data.frame(Site = site, Year = year, 
                                                           Irrigation_Level = level, Estimate = estimate, 
                                                           Std_Error = std_error))
      }
    }
  }
}


potato2023 <- results_potato





df_potato <- rbind(potato2018,potato2019, potato2020, potato2021, potato2022, potato2023)




df1 <- df_wheat%>%
  mutate(crop="wheat")

df2 <- df_canola%>%
  mutate(crop="canola")

df3 <- df_potato%>%
  mutate(crop="potato")


df <- rbind(df1,df2,df3)

# Define a custom color palette by selecting specific colors from ColorBrewer
custom_palette <- c(
  "wheat" = "firebrick3",      
  "canola" = "springgreen4",
  "potato" = "slateblue2"
)


ggplot(df, aes(x = as.factor(Irrigation_Level), y = Estimate, fill = as.factor(crop))) + 
  geom_boxplot(width = 0.3, position = position_dodge(0.8), color = "black") +  # Box plot with dodge and color
  scale_fill_manual(values = custom_palette) +  # Use the custom color palette
  labs(
    title = "",
    x = "Irrigation Level (mm)",
    y = "Seasonal Irrigation (mm)",
    fill = "Crop Type"
  ) +  
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "right"
  )+
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) + # Adjust gap between x-axis labels (year)
  theme_minimal()+
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black") )+
  scale_y_continuous(name = expression("Marginal Value ($m"^{-3}*")"),
                     limits = c(0.4, 0.8), breaks = seq(0,1 , by = 0.05)) + # Customize y-axis scale
  guides(fill = guide_legend(title = NULL)) +  # Remove legend title
  theme(
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12),  # Adjust x-axis title size
    axis.title.y = element_text(size = 12),   # Adjust y-axis title size
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.ticks = element_line(size = 0.8) 
  )


####### wheat ####### wheat ####### wheat ####### wheat ####### wheat

t <- df%>%
  filter(crop=="wheat")%>%
  filter(Irrigation_Level!=800)



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
    Year == 2018 ~ 0.5,  
    Year == 2019 ~ 0.2,
    Year == 2020 ~ 0.7,  
    Year == 2021 ~ 0.5,
    Year == 2022 ~ 1,  
    Year == 2023 ~ 0.6,  
    TRUE ~ 0.0  # Default minimum
  ),
  y_max = case_when(
    Year == 2018 ~ 1.2,  
    Year == 2019 ~ 1,
    Year == 2020 ~ 1.3,  
    Year == 2021 ~ 1.5,  
    Year == 2022 ~ 1.8,  
    Year == 2023 ~ 2,  
    TRUE ~ 1.0  
  ))





ggplot(t, aes(x = as.factor(Irrigation_Level), y = Estimate, fill = as.factor(Year))) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.4),  # Adjust width for spacing
    width = 0.2,  # Width of the box plot
    color = "black",
    alpha = 0.7
  ) +
  geom_blank(aes(y = y_min)) +  # Add invisible points to define the y_min
  geom_blank(aes(y = y_max)) +  # Add invisible points to define the y_max
  #geom_hline(
  #data = target_values,
  #aes(yintercept = target, linetype = "Average value"),
  #color = "#7D7D7D",
  #size = 0.8,
  #show.legend = TRUE  # Ensure the line is added to the legend
  #) +
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


####### Canola ####### Canola ####### Canola ####### Canola ####### Canola

t <- df%>%
  filter(crop=="canola")%>%
  filter(Irrigation_Level!=800)



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
    Year == 2018 ~ 0.5,  
    Year == 2019 ~ 0.2,
    Year == 2020 ~ 0.7,  
    Year == 2021 ~ 0.5,
    Year == 2022 ~ 1,  
    Year == 2023 ~ 0.6,  
    TRUE ~ 0.0  # Default minimum
  ),
  y_max = case_when(
    Year == 2018 ~ 1.2,  
    Year == 2019 ~ 1,
    Year == 2020 ~ 1.3,  
    Year == 2021 ~ 1.5,  
    Year == 2022 ~ 1.8,  
    Year == 2023 ~ 2,  
    TRUE ~ 1.0  
  ))





ggplot(t, aes(x = as.factor(Irrigation_Level), y = Estimate, fill = as.factor(Year))) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.4),  # Adjust width for spacing
    width = 0.2,  # Width of the box plot
    color = "black",
    alpha = 0.7
  ) +
  geom_blank(aes(y = y_min)) +  # Add invisible points to define the y_min
  geom_blank(aes(y = y_max)) +  # Add invisible points to define the y_max
  #geom_hline(
  #data = target_values,
  #aes(yintercept = target, linetype = "Average value"),
  #color = "#7D7D7D",
  #size = 0.8,
  #show.legend = TRUE  # Ensure the line is added to the legend
  #) +
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


##### potato ##### potato ##### potato ##### potato ##### potato

t <- df%>%
  filter(crop=="potato")%>%
  filter(Irrigation_Level!=800)



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
    Year == 2018 ~ 0.5,  
    Year == 2019 ~ 0.2,
    Year == 2020 ~ 0.7,  
    Year == 2021 ~ 0.5,
    Year == 2022 ~ 1,  
    Year == 2023 ~ 0.6,  
    TRUE ~ 0.0  # Default minimum
  ),
  y_max = case_when(
    Year == 2018 ~ 1.2,  
    Year == 2019 ~ 1,
    Year == 2020 ~ 1.3,  
    Year == 2021 ~ 1.5,  
    Year == 2022 ~ 1.8,  
    Year == 2023 ~ 2,  
    TRUE ~ 1.0  
  ))

# Calculate common y-axis breaks
all_breaks <- seq(3, 8, by = 0.5)  # Adjust the range and step as needed




ggplot(t, aes(x = as.factor(Irrigation_Level), y = Estimate, fill = as.factor(Year))) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.4),  # Adjust width for spacing
    width = 0.2,  # Width of the box plot
    color = "black",
    alpha = 0.7
  ) +
  geom_blank(aes(y = y_min)) +  # Add invisible points to define the y_min
  geom_blank(aes(y = y_max)) +  # Add invisible points to define the y_max
  #geom_hline(
  #data = target_values,
  #aes(yintercept = target, linetype = "Average value"),
  #color = "#7D7D7D",
  #size = 0.8,
  #show.legend = TRUE  # Ensure the line is added to the legend
  #) +
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
  scale_y_continuous(
    breaks = all_breaks, limits = c(3, 8) # Set the common breaks
  ) +
  scale_x_discrete(labels = function(x) paste("", x)) +
  scale_linetype_manual(
    values = c("Average value" = "dashed")
  ) +
  facet_wrap(~Year, ncol = 2, scales = "free_y") +  # Allow free y scales
  guides(fill = "none")  # Remove the legend for `fill` (Year)


########## weighted ########## weighted  ########## weighted  ########## weighted 

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



wheat_weighted <- df%>%
  filter(crop=="wheat")%>%
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

canola_weighted <- df%>%
  filter(crop=="canola")%>%
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



df_all <- wheat_weighted%>%
  left_join(canola_weighted)%>%
  mutate(ave_val =w_canol_val+w_wheat_val)%>%
  select(Year,Site,ave_val,Irrigation_Level)




t <- df_all%>%
  filter(Irrigation_Level!=800)



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
    Year == 2018 ~ 0.5,  
    Year == 2019 ~ 0.2,
    Year == 2020 ~ 0.7,  
    Year == 2021 ~ 0.5,
    Year == 2022 ~ 1,  
    Year == 2023 ~ 0.6,  
    TRUE ~ 0.0  # Default minimum
  ),
  y_max = case_when(
    Year == 2018 ~ 1.2,  
    Year == 2019 ~ 1,
    Year == 2020 ~ 1.3,  
    Year == 2021 ~ 1.5,  
    Year == 2022 ~ 1.8,  
    Year == 2023 ~ 2,  
    TRUE ~ 1.0  
  ))





ggplot(t, aes(x = as.factor(Irrigation_Level), y = ave_val, fill = as.factor(Year))) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.4),  # Adjust width for spacing
    width = 0.2,  # Width of the box plot
    color = "black",
    alpha = 0.7
  ) +
  geom_blank(aes(y = y_min)) +  # Add invisible points to define the y_min
  geom_blank(aes(y = y_max)) +  # Add invisible points to define the y_max
  #geom_hline(
  #data = target_values,
  #aes(yintercept = target, linetype = "Average value"),
  #color = "#7D7D7D",
  #size = 0.8,
  #show.legend = TRUE  # Ensure the line is added to the legend
  #) +
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



df_potato <- df%>%
  filter(crop=="potato")%>%
  filter(Irrigation_Level!=800)


df_wht_can <- df_all%>%
  filter(Year>2019)%>%
  filter(Irrigation_Level!=800)


df_all <- df_potato%>%
  left_join(df_wht_can)%>%
  mutate(ave_val = Estimate-ave_val)


# Calculate common y-axis breaks
all_breaks <- seq(0, 8, by = 1)  # Adjust the range and step as needed



# Example of adding y-axis range limits to your dataset
df_all <- df_all %>%
  mutate(y_min = case_when(
    Year == 2020 ~ 2,  
    Year == 2021 ~ 2,
    Year == 2022 ~ 0,  
    Year == 2023 ~ 1,  
    TRUE ~ 0.0  # Default minimum
  ),
  y_max = case_when(
    Year == 2020 ~ 8,  
    Year == 2021 ~ 7,  
    Year == 2022 ~ 6,  
    Year == 2023 ~ 6,  
    TRUE ~ 1.0  
  ))



ggplot(df_all, aes(x = as.factor(Irrigation_Level), y = ave_val, fill = as.factor(Year))) +
  geom_boxplot(
    outlier.shape = NA,
    position = position_dodge(width = 0.4),  # Adjust width for spacing
    width = 0.2,  # Width of the box plot
    color = "black",
    alpha = 0.7
  ) +
  geom_blank(aes(y = y_min)) +  # Add invisible points to define the y_min
  geom_blank(aes(y = y_max)) +  # Add invisible points to define the y_max
  #geom_hline(
  #data = target_values,
  #aes(yintercept = target, linetype = "Average value"),
  #color = "#7D7D7D",
  #size = 0.8,
  #show.legend = TRUE  # Ensure the line is added to the legend
  #) +
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
  scale_y_continuous(
    breaks = all_breaks # Set the common breaks
  )+
  scale_x_discrete(labels = function(x) paste("", x)) +
  scale_linetype_manual(
    values = c("Average value" = "dashed")
  ) +
  facet_wrap(~Year, ncol = 2, scales = "free_y") +  # Allow free y scales
  guides(fill = "none")  # Remove the legend for `fill` (Year)

