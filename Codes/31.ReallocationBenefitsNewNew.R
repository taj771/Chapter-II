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


wheat <- wheat%>%
  select(year,Site,Max_Irrigation_mm,`Dry yield irri (bu/ac)`,price.bu)%>%
  mutate(revenue = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(crop="wheat")


canola <- canola%>%
  select(year,Site,Max_Irrigation_mm,`Dry yield irri (bu/ac)`,price.bu)%>%
  mutate(revenue = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(crop="canola")

potato <- potato%>%
  select(year,Site,Max_Irrigation_mm,`Dry yield irri (ton/ac)`,price.ton)%>%
  mutate(revenue = `Dry yield irri (ton/ac)`*price.ton)%>%
  mutate(crop="potato")
  

ggplot(potato, aes(x = Max_Irrigation_mm, y = revenue)) + 
  geom_smooth(method = "loess", se = FALSE, aes(color = crop), fill = "lightblue") + 
  #geom_point(color = "red", size = 1) +
  labs(title = "Revenue vs Irrigation Level by Crop",
       x = "Max Irrigation (mm)",
       y = "Revenue ($)") +
  theme_minimal()





  

df <- rbind(wheat,canola)


ggplot(df, aes(x = Max_Irrigation_mm, y = revenue, color = crop)) + 
  geom_smooth(method = "loess", se = FALSE, aes(color = crop), fill = "lightblue") + 
  #geom_point(color = "red", size = 1) +
  labs(title = "Revenue vs Irrigation Level by Crop",
       x = "Max Irrigation (mm)",
       y = "Revenue ($)") +
  theme_minimal()



df_wheat <- df%>%
  filter(crop == "wheat")%>%
  group_by(Max_Irrigation_mm)%>%
  mutate(ave_revenue = mean(revenue))%>%
  distinct(Max_Irrigation_mm, .keep_all = T)


df_canola <- df%>%
  filter(crop == "canola")%>%
  group_by(Max_Irrigation_mm)%>%
  mutate(ave_revenue = mean(revenue))%>%
  distinct(Max_Irrigation_mm, .keep_all = T)

df_potato <- potato%>%
  group_by(Max_Irrigation_mm)%>%
  mutate(ave_revenue = mean(revenue))%>%
  distinct(Max_Irrigation_mm, .keep_all = T)
  




ggplot(df_wheat, aes(x = Max_Irrigation_mm, y = ave_revenue)) + 
  geom_smooth(method = "loess", se = FALSE, aes(color = crop), fill = "lightblue") + 
  #geom_point(color = "red", size = 1) +
  labs(title = "Revenue vs Irrigation Level by Crop",
       x = "Max Irrigation (mm)",
       y = "Revenue ($)") +
  theme_minimal()






t <-df_wheat %>%
  group_by(Max_Irrigation_mm) %>%  # Compute differences within each site
  mutate(mv_wheat = ave_revenue - lag(ave_revenue)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)%>%
  left_join(wheat_weight)%>%
  mutate(wmv_wheat =wheat_w*mv_wheat)




