#clear memory
rm(list = ls())

# wheat
library(dplyr)
library(tidyverse)
library(fixest)
library(priceR)

########## Return Return Return ############################

return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")



priceCanol <- return%>%
  filter(year == 2023 & crop == "canola") %>%
  pull(price.bu)

drcostCano <- return%>%
  filter(year == 2023 & crop == "canola") %>%
  pull(dry_cost_ac)

irricostCano <- return%>%
  filter(year == 2023 & crop == "canola") %>%
  pull(irri_cost_ac)

priceWheat <- return%>%
  filter(year == 2023 & crop == "wheat") %>%
  pull(price.bu)

drcostWheat <- return%>%
  filter(year == 2023 & crop == "wheat") %>%
  pull(dry_cost_ac)

irricostWheat <- return%>%
  filter(year == 2023 & crop == "wheat") %>%
  pull(irri_cost_ac)


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year > 2016)%>%
  mutate(dry_cost_ac=drcostWheat,
         irri_cost_ac=irricostWheat,
         price.bu = priceWheat)%>%
  select(year,dry_cost_ac, irri_cost_ac, price.bu)%>%
  mutate(year = ifelse(year == 2017, 2030, year),
         year = ifelse(year == 2018, 2040, year),
         year = ifelse(year == 2019, 2050, year),
         year = ifelse(year == 2020, 2060, year),
         year = ifelse(year == 2021, 2080, year),
         year = ifelse(year == 2022, 2100, year)
  )

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year > 2016)%>%
  mutate(dry_cost_ac=drcostCano,
         irri_cost_ac=irricostCano,
         price.bu = priceCanol)%>%
  select(year,dry_cost_ac, irri_cost_ac, price.bu)%>%
  mutate(year = ifelse(year == 2017, 2030, year),
         year = ifelse(year == 2018, 2040, year),
         year = ifelse(year == 2019, 2050, year),
         year = ifelse(year == 2020, 2060, year),
         year = ifelse(year == 2021, 2080, year),
         year = ifelse(year == 2022, 2100, year)
  )


######## weights ######## weights ######## weights ######## weights #### weights


# dataframe
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



####### CMIP585 ####### CMIP585 ####### CMIP585 ####### CMIP585 ####### CMIP585

# wheat
# Set the directory path where the CSV files are located
file_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP585/WheatCMIP585"


# Get a list of all files that match the pattern
file_list <- list.files(path = file_path, pattern = "^merged_simulation_results_wheat_marginal_CMIP585_\\d{4}\\.csv$", full.names = TRUE)

# Load each file and add a "year" column
wheat_marginal <- map_dfr(file_list, ~ {
  year <- gsub(".*_(\\d{4})\\.csv$", "\\1", .x)  # Extract the year from the file name
  read_csv(.x) %>% 
    mutate(year = as.numeric(year))              # Add the year column as a numeric type
})%>%
  filter(Max_Irrigation_mm!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*14.86995818)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))


# Rainfed
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP585/WheatCMIP585"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_dry_wheat_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_wheat_rf <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation.mm.`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`,
         year=Year)

df_wheat_CMIP585 <- wheat_marginal%>%
  left_join(df_wheat_rf, by = c("year", "Site"))

df_wheat_CMIP585 <- df_wheat_CMIP585%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation.mm.`,Max_Irrigation_mm,val_mm,irrq_m3)



#### Individual curve for each site-year combo 


wheat2030 <- df_wheat_CMIP585%>%
  filter(year==2030)
wheat2040 <- df_wheat_CMIP585%>%
  filter(year==2040)
wheat2050 <- df_wheat_CMIP585%>%
  filter(year==2050)
wheat2060 <- df_wheat_CMIP585%>%
  filter(year==2060)
wheat2080 <- df_wheat_CMIP585%>%
  filter(year==2080)
wheat2100 <- df_wheat_CMIP585%>%
  filter(year==2100)


#wheat
# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat2030$Site)
years <- c(2030)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2030 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


wheat2030 <- results_wheat

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat2040$Site)
years <- c(2040)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2040 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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

wheat2040 <- results_wheat


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat2050$Site)
years <- c(2050)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2050 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


wheat2050 <- results_wheat

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat2060$Site)
years <- c(2060)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2060 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


wheat2060 <- results_wheat

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat2080$Site)
years <- c(2080)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2080 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


wheat2080 <- results_wheat


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(wheat2100$Site)
years <- c(2100)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- wheat2100 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


wheat2100 <- results_wheat


df_wheat_CMIP585 <- rbind(wheat2030, wheat2040, wheat2050, wheat2060, wheat2080, wheat2100)


# canola # canola # canola # canola # canola # canola # canola # canola # canola
# Set the directory path where the CSV files are located
file_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP585/CanolaCMIP585"


# Get a list of all files that match the pattern
file_list <- list.files(path = file_path, pattern = "^merged_simulation_results_canola_marginal_CMIP585_\\d{4}\\.csv$", full.names = TRUE)


# Load each file and add a "year" column
canola_marginal <- map_dfr(file_list, ~ {
  year <- gsub(".*_(\\d{4})\\.csv$", "\\1", .x)  # Extract the year from the file name
  read_csv(.x) %>% 
    mutate(year = as.numeric(year))              # Add the year column as a numeric type
})%>%
  filter(Max_Irrigation_mm!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*17.84394982)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))


# Rainfed
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP585/CanolaCMIP585"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_dry_canola_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_canola_rf <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation.mm.`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`,
         year=Year)


df_canola_CMIP585 <- canola_marginal%>%
  left_join(df_canola_rf, by = c("year", "Site"))


df_canola_CMIP585 <- df_canola_CMIP585%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation.mm.`,Max_Irrigation_mm,val_mm,irrq_m3)


#### Individual curve for each site-year combo 


canola2030 <- df_canola_CMIP585%>%
  filter(year==2030)
canola2040 <- df_canola_CMIP585%>%
  filter(year==2040)
canola2050 <- df_canola_CMIP585%>%
  filter(year==2050)
canola2060 <- df_canola_CMIP585%>%
  filter(year==2060)
canola2080 <- df_canola_CMIP585%>%
  filter(year==2080)
canola2100 <- df_canola_CMIP585%>%
  filter(year==2100)


#canoala
# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola2030$Site)
years <- c(2030)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2030 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


canola2030 <- results_canola

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola2040$Site)
years <- c(2040)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2040 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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

canola2040 <- results_canola


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola2050$Site)
years <- c(2050)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2050 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


canola2050 <- results_canola

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola2060$Site)
years <- c(2060)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2060 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


canola2060 <- results_canola

# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_canola <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                             Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola2080$Site)
years <- c(2080)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2080 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


canola2080 <- results_canola


# Define the ranges of Total_Irrigation_mm to analyze
irrigation_levels <- seq(200, 800, by = 100)  # Adjust as necessary

# Initialize an empty data frame to store results
results_wheat <- data.frame(Site = integer(), Year = integer(), Irrigation_Level = numeric(), 
                            Estimate = numeric(), Std_Error = numeric())

# Define the list of sites and years
sites <- unique(canola2100$Site)
years <- c(2100)

# Loop over each site, year, and irrigation level
for (site in sites) {
  for (year in years) {
    for (level in irrigation_levels) {
      
      # Filter data for the specific site, year, and irrigation level range
      data_subset <- canola2100 %>%
        filter(Site == site, year == year, Max_Irrigation_mm <= level)
      
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


canola2100 <- results_canola


df_canola_CMIP585 <- rbind(canola2030, canola2040, canola2050, canola2060, canola2080, canola2100)



df_canola_CMIP585 <- df_canola_CMIP585%>%
  mutate(w_can = Estimate*canola_weights)%>%
  select(Year,Site,Irrigation_Level,w_can)


df_wheat_CMIP585 <- df_wheat_CMIP585%>%
  mutate(w_wht = Estimate*wheat_weights)%>%
  select(Year,Site,Irrigation_Level,w_wht)


df_CMIP585 <- df_wheat_CMIP585%>%
  left_join(df_canola_CMIP585)%>%
  mutate(val_mm=w_can+w_wht)%>%
  select(-w_wht,-w_can)



df1 <- df_CMIP585%>%
  group_by(Site,Irrigation_Level)%>%
  mutate(ave_Estimate = mean(val_mm))%>%
  ungroup()%>%
  distinct(Site,Irrigation_Level, .keep_all = T)%>%
  select(-Year, -val_mm)%>%
  mutate(Year = "Average")%>%
  rename(val_mm=ave_Estimate)%>%
  select(Site,Year,Irrigation_Level,val_mm)


df_CMIP585 <- rbind(df_CMIP585,df1)





# Define a custom color palette by selecting specific colors from ColorBrewer
custom_palette <- c(
  "2030" = "#d73027",      
  "2040" = "#fc8d59",
  "2050" = "#fee090",
  "2060" = "#91bfdb",
  "2080" = "#d6bfd8",
  "2100" = "#033027",
  "Average" = "#1545b4"
)


df_CMIP585 <- df_CMIP585%>%
  filter(Irrigation_Level < 750)

ggplot(df_CMIP585, aes(x = as.factor(Irrigation_Level), y = val_mm, fill = as.factor(Year))) + 
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
  scale_y_continuous(name = expression("Marginal Value ($/m"^3*")"),
                     limits = c(0.3, 0.8), breaks = seq(0,1 , by = 0.05)) + # Customize y-axis scale
  guides(fill = guide_legend(title = NULL)) +  # Remove legend title
  theme(
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12),  # Adjust x-axis title size
    axis.title.y = element_text(size = 12),   # Adjust y-axis title size
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.ticks = element_line(size = 0.8) 
  )


write.csv(df_CMIP585, "./AquaCropOPSyData/ClimateData/CMIP6/CMIP585/Marginal_CMIP585.csv")


######## Seperate graphs ######## Seperate graphs for year wise 

t <- df_CMIP585%>%
  filter(Year!="Average")


#target_values <- data.frame(
#Year = c(2018,2019,2020,2021,2022,2023),
#target = c(0.292903, 0.2844536,0.2729694,0.2580631, 0.3633408,0.3853838)  # Replace with your target values for each year
#)


# Assuming `df` is your data frame
# Create a custom color palette
custom_palette <- c(
  "2030" = "#d73027",      
  "2040" = "#fc8d59",
  "2050" = "#fee090",
  "2060" = "#91bfdb",
  "2080" = "#d6bfd8",
  "2100" = "#033027",
  "Average" = "#1545b4"
)



t <- t %>%
  mutate(y_min = case_when(
    Year == 2030 ~ 0.4,  
    Year == 2040 ~ 0.3,
    Year == 2050 ~ 0.4,  
    Year == 2060 ~ 0.5,
    Year == 2080 ~ 0.5,  
    Year == 2100 ~ 0.3,  
    TRUE ~ 0.0  # Default minimum
  ),
  y_max = case_when(
    Year == 2030 ~ 1,  
    Year == 2040 ~ 1.2,
    Year == 2050 ~ 0.9,  
    Year == 2060 ~ 0.8,  
    Year == 2080 ~ 1,  
    Year == 2100 ~0.8 ,  
    TRUE ~ 1.0  
  ))



# Add rows to enforce y_min and y_max in each Year
t_lims <- t %>%
  select(Year, y_min, y_max) %>%
  distinct() %>%
  pivot_longer(cols = c(y_min, y_max), names_to = "type", values_to = "val_mm") %>%
  mutate(Irrigation_Level = NA)  # NA to avoid plotting these points visibly

# Combine the limits with the main data
t_combined <- bind_rows(t, t_lims)

t_combined <- t_combined %>%
  filter(!is.na(Year) & !is.na(Irrigation_Level) & !is.na(val_mm))


# Calculate common y-axis breaks
all_breaks <- seq(0, 1.5, by = 0.2)  # Adjust the range and step as needed

# Create the plot
ggplot(t_combined, aes(x = as.factor(Irrigation_Level), y = val_mm, fill = as.factor(Year))) +
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
  geom_blank(aes(y = val_mm)) +  # Use blanks to enforce y_min and y_max
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
    breaks = all_breaks  # Set the common breaks
  ) +
  scale_x_discrete(labels = function(x) paste("", x)) +
  scale_linetype_manual(
    values = c("Average value" = "dashed")
  ) +  # Map linetype for the dashed line
  guides(fill = "none") +
  facet_wrap(~Year, ncol = 2, scales = "free_y")  # Independent scales for y, but same breaks


#############################################################################


df_CMIP126 <- read.csv("./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/Marginal_CMIP126.csv")
df_CMIP245 <- read.csv("./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/Marginal_CMIP245.csv")
df_CMIP585 <- read.csv("./AquaCropOPSyData/ClimateData/CMIP6/CMIP585/Marginal_CMIP585.csv")

df_1 <- df_CMIP126%>%
  group_by(Site,Irrigation_Level)%>%
  mutate(ave_val= mean(val_mm))%>%
  distinct(Site,Irrigation_Level, .keep_all = T)%>%
  mutate(emision="CMIP126")



df_2 <- df_CMIP245%>%
  group_by(Site,Irrigation_Level)%>%
  mutate(ave_val= mean(val_mm))%>%
  distinct(Site,Irrigation_Level, .keep_all = T)%>%
  mutate(emision="CMIP245")



df_3 <- df_CMIP585%>%
  group_by(Site,Irrigation_Level)%>%
  mutate(ave_val= mean(val_mm))%>%
  distinct(Site,Irrigation_Level, .keep_all = T)%>%
  mutate(emision="CMIP585")

df <- rbind(df_1,df_2,df_3)

# Define a custom color palette by selecting specific colors from ColorBrewer
custom_palette <- c(
  "CMIP126" = "firebrick3",      
  "CMIP245" = "springgreen4",
  "CMIP585" = "slateblue2"
  )



ggplot(df, aes(x = as.factor(Irrigation_Level), y = val_mm, fill = as.factor(emision))) + 
  # Add error bars at min and max values
  stat_summary(
    fun.data = function(x) {
      data.frame(
        ymin = min(x),
        ymax = max(x)
      )
    },
    geom = "errorbar",
    width = 0.2,
    color = "black",
    position = position_dodge(0.8)  # Align dodge with boxplot
  ) +
  # Add box plot with dodge and color
  geom_boxplot(
    width = 0.3,
    position = position_dodge(0.8),
    color = "black",
    outlier.shape = NA
  ) +
  # Use the custom color palette for fills
  scale_fill_manual(values = custom_palette) +
  # Add labels
  labs(
    title = "",
    x = "Irrigation Level (mm)",
    y = "Seasonal Irrigation (mm)",
    fill = "Emission Scenario"
  ) +
  # Minimal theme with customizations
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "right",
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.ticks = element_line(size = 0.8),  # Adjust axis ticks
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12),  # Adjust x-axis title size
    axis.title.y = element_text(size = 12),  # Adjust y-axis title size
    legend.text = element_text(size = 12)   # Adjust legend text size
  ) +
  # Adjust x-axis spacing
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) +
  # Customize y-axis scale
  scale_y_continuous(
    name = expression("Marginal Value ($m"^{-3}*")"),
    limits = c(0.4, 0.8),
    breaks = seq(0.4, 0.8, by = 0.05)
  ) +
  # Remove legend title
  guides(fill = guide_legend(title = NULL))



