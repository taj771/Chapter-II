#clear memory
rm(list = ls())

library(dplyr)
library(tidyverse)

####### CMIO126 ####### CMIO126 ####### CMIO126 ####### CMIO126 ####### CMIO126

# wheat
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/WheatCMIP126"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_wheat_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_wheat_iri <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal.irrigation..mm.`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal.irrigation..mm.`*0.001))



# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/WheatCMIP126"

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
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_wheat <- df_wheat_rf%>%
  left_join(df_wheat_iri)

# Canola
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/CanolaCMIP126"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_canola_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_canola_iri <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal.irrigation..mm.`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal.irrigation..mm.`*0.001))



# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/CanolaCMIP126"

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
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_canola <- df_canola_rf%>%
  left_join(df_canola_iri)



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


wheat <- df_wheat%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation.mm.`,`Seasonal.irrigation..mm.`,val_mm)



canola <- df_canola%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation.mm.`,`Seasonal.irrigation..mm.`,val_mm)




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


canola_p1 <- canola%>%
  select(year,Site,val_mm)%>%
  mutate(crop="CMIP126")%>%
  mutate(weights = canola_weights)%>%
  mutate(can_val_mm=val_mm*weights)%>%
  select(-weights, -val_mm)

wheat_p1 <- wheat%>%
  select(year,Site,val_mm)%>%
  mutate(crop="CMIP126")%>%
  mutate(weights = wheat_weights)%>%
  mutate(wht_val_mm=val_mm*weights)%>%
  select(-weights, -val_mm)

CMIP126 <- canola_p1%>%
  left_join(wheat_p1)%>%
  mutate(val_mm = can_val_mm+wht_val_mm)

####### CMIP245 ####### CMIP245 ####### CMIP245

# wheat
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP245/WheatCMIP245"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_wheat_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_wheat_iri <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal.irrigation..mm.`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal.irrigation..mm.`*0.001))



# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP245/WheatCMIP245"

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
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_wheat <- df_wheat_rf%>%
  left_join(df_wheat_iri)

# Canola
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP245/CanolaCMIP245"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_canola_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_canola_iri <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal.irrigation..mm.`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal.irrigation..mm.`*0.001))



# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP245/CanolaCMIP245"

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
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_canola <- df_canola_rf%>%
  left_join(df_canola_iri)



wheat <- df_wheat%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation.mm.`,`Seasonal.irrigation..mm.`,val_mm)



canola <- df_canola%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation.mm.`,`Seasonal.irrigation..mm.`,val_mm)


canola_p2 <- canola%>%
  select(year,Site,val_mm)%>%
  mutate(crop="CMIP245")%>%
  mutate(weights = canola_weights)%>%
  mutate(can_val_mm=val_mm*weights)%>%
  select(-weights, -val_mm)

wheat_p2 <- wheat%>%
  select(year,Site,val_mm)%>%
  mutate(crop="CMIP245")%>%
  mutate(weights = wheat_weights)%>%
  mutate(wht_val_mm=val_mm*weights)%>%
  select(-weights, -val_mm)

CMIP245 <- canola_p2%>%
  left_join(wheat_p2)%>%
  mutate(val_mm = can_val_mm+wht_val_mm)


####### CMIP585 ####### CMIP585 ####### CMIP585 ####### CMIP585

# wheat
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP585/WheatCMIP585"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_wheat_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_wheat_iri <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal.irrigation..mm.`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal.irrigation..mm.`*0.001))



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
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_wheat <- df_wheat_rf%>%
  left_join(df_wheat_iri)

# Canola
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP585/CanolaCMIP585"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_canola_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_canola_iri <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal.irrigation..mm.`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal.irrigation..mm.`*0.001))



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
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_canola <- df_canola_rf%>%
  left_join(df_canola_iri)


wheat <- df_wheat%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation.mm.`,`Seasonal.irrigation..mm.`,val_mm)



canola <- df_canola%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation.mm.`,`Seasonal.irrigation..mm.`,val_mm)


canola_p3 <- canola%>%
  select(year,Site,val_mm)%>%
  mutate(crop="CMIP585")%>%
  mutate(weights = canola_weights)%>%
  mutate(can_val_mm=val_mm*weights)%>%
  select(-weights, -val_mm)

wheat_p3 <- wheat%>%
  select(year,Site,val_mm)%>%
  mutate(crop="CMIP585")%>%
  mutate(weights = wheat_weights)%>%
  mutate(wht_val_mm=val_mm*weights)%>%
  select(-weights, -val_mm)

CMIP585 <- canola_p3%>%
  left_join(wheat_p3)%>%
  mutate(val_mm = can_val_mm+wht_val_mm)


df <- rbind(CMIP126,CMIP245,CMIP585)



# Reorder the 'crop' variable so that 'CMIP126', 'CMIP245', and 'CMIP585' are in order
df$crop <- factor(df$crop, levels = c("CMIP126", "CMIP245", "CMIP585"))

ggplot(df, aes(x = factor(year), y = val_mm, fill = crop)) +  
  # Add error bars for min and max values
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
    position = position_dodge(0.5)  # Match dodge width with boxplot for alignment
  ) +
  # Add box plot with dodge and consistent color settings
  geom_boxplot(
    width = 0.3,
    position = position_dodge(0.5),
    color = "black",
    outlier.shape = NA
  ) +
  # Define custom fill colors for each crop type
  scale_fill_manual(
    values = c("CMIP126" = "firebrick3", "CMIP245" = "springgreen4", "CMIP585" = "slateblue2"),
    labels = c("CMIP126", "CMIP245", "CMIP585")
  ) +
  labs(
    title = "",
    x = "Year",
    y = "Seasonal Irrigation (mm)",
    fill = "Crop Type"
  ) +
  # Apply a minimal theme and additional formatting
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(size = 0.8),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "right"
  ) +
  # Customize x-axis spacing and y-axis scaling
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) +
  scale_y_continuous(
    name = expression("Average Value ($/m"^3*")"),
    limits = c(0.38, 0.60),
    breaks = seq(0.38, 0.60, by = 0.02)
  ) +
  # Remove the legend title
  guides(fill = guide_legend(title = NULL))




