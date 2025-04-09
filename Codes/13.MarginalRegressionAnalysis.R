#clear memory
rm(list = ls())

# wheat
library(dplyr)
library(tidyverse)
library(fixest)
library(modelsummary)



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
  mutate(canola = ifelse(crop == "canola", 1, 0))%>%
  rename(site=Site)


weather <- read.csv("./AquaCropOPSyData/ClimateData/weather_data_Aqua.csv")%>%
  filter(
    (Month > 5 & Month < 10) |  # Includes full months from June to September
      (Month == 5 & Day >= 1) |   # Includes May starting from the 1st
      (Month == 10 & Day <= 31)   # Includes October until the 31st
  )%>%
  group_by(site,year)%>%
  mutate(meanMinT = mean(MinTemp),
         meanMaxT = mean(MaxTemp),
         MeanPrcp = mean(Precipitation))%>%
  mutate(MeanT = (meanMinT+meanMaxT/2))%>%
  ungroup()%>%
  select(year,site,meanMinT,meanMaxT,MeanPrcp,MeanT )%>%
  distinct(year,site, .keep_all = T)

df <- df%>%
  left_join(weather)%>%
  mutate(ye2016 = ifelse(year == 2016, 1, 0),
         ye2017 = ifelse(year == 2017, 1, 0),
         ye2018 = ifelse(year == 2018, 1, 0),
         ye2019 = ifelse(year == 2019, 1, 0),
         ye2020 = ifelse(year == 2020, 1, 0),
         ye2021 = ifelse(year == 2021, 1, 0),
         ye2022 = ifelse(year == 2022, 1, 0),
         ye2023 = ifelse(year == 2023, 1, 0),
         irr100 = ifelse(Max_Irrigation_mm == 100, 1, 0),
         irr200 = ifelse(Max_Irrigation_mm == 200, 1, 0),
         irr300 = ifelse(Max_Irrigation_mm == 300, 1, 0),
         irr400 = ifelse(Max_Irrigation_mm == 400, 1, 0),
         irr500 = ifelse(Max_Irrigation_mm == 500, 1, 0),
         irr600 = ifelse(Max_Irrigation_mm == 600, 1, 0),
         irr700 = ifelse(Max_Irrigation_mm == 700, 1, 0),)

# filter only 2018-2023

df <- df%>%
  filter(year>2017)


cm <- c(
  'log_sd_inter_500' = 'Log(SD)X500m',
  'log(irrq_m3)' = 'Log(Irrigation Water in m³)',
  'dry_cost_ac'='Dry land cost (ac)',
  'irri_cost_ac'='Irrigated land cost (ac)',
  "price.bu" = 'Crop Price (bu/ac)',
  'MeanPrcp' = 'Percipitation (mm)',
  'MeanT' = 'Mean Tempreature (°C)',
  'canola' = 'Canola',
  'log(irrq_m3):canola' = 'Log(Irrigation Water in m³)*Canola',
  'log(irrq_m3):ye2016' = 'Log(Irrigation Water in m³)*Year:2016',
  'log(irrq_m3):ye2017' = 'Log(Irrigation Water in m³)*Year:2017',
  'log(irrq_m3):ye2018' = 'Log(Irrigation Water in m³)*Year:2018',
  'log(irrq_m3):ye2019' = 'Log(Irrigation Water in m³)*Year:2019',
  'log(irrq_m3):ye2020' = 'Log(Irrigation Water in m³)*Year:2020',
  'log(irrq_m3):ye2021' = 'Log(Irrigation Water in m³)*Year:2021',
  'log(irrq_m3):ye2022' = 'Log(Irrigation Water in m³)*Year:2022',
  'log(irrq_m3):ye2023' = 'Log(Irrigation Water in m³)*Year:2023',
  'log(irrq_m3):irr200' = 'Log(Irrigation Water in m³)*Max Irrigation: 200mm',
  'log(irrq_m3):irr300' = 'Log(Irrigation Water in m³)*Max Irrigation: 300mm',
  'log(irrq_m3):irr400' = 'Log(Irrigation Water in m³)*Max Irrigation: 400mm',
  'log(irrq_m3):irr500' = 'Log(Irrigation Water in m³)*Max Irrigation: 500mm',
  'log(irrq_m3):irr600' = 'Log(Irrigation Water in m³)*Max Irrigation: 600mm',
  'log(irrq_m3):irr700' = 'Log(Irrigation Water in m³)*Max Irrigation: 700mm'
  
)


model_list = list(
  model_1 = feols(val_mm ~ log(irrq_m3), data = df),
  model_2 = feols(val_mm ~ log(irrq_m3) + MeanPrcp + MeanT, data = df),
  model_3 = feols(val_mm ~ log(irrq_m3) + canola + canola*log(irrq_m3) + MeanPrcp + MeanT, data = df),
  model_4 = feols(val_mm ~ log(irrq_m3) + canola + canola*log(irrq_m3) + MeanPrcp + MeanT + ye2018*log(irrq_m3) + ye2019*log(irrq_m3) +
                    ye2020*log(irrq_m3) + ye2021*log(irrq_m3) +ye2022*log(irrq_m3), data = df),
  model_5 = feols(val_mm ~ log(irrq_m3) + canola*log(irrq_m3) + MeanPrcp + MeanT + ye2018*log(irrq_m3) + ye2019*log(irrq_m3) +
                    ye2020*log(irrq_m3) + ye2021*log(irrq_m3) +  ye2022*log(irrq_m3) + 
                    irr200*log(irrq_m3) +  irr300*log(irrq_m3) +
                    irr400*log(irrq_m3) +  irr500*log(irrq_m3) +
                    irr600*log(irrq_m3) +  irr700*log(irrq_m3), data = df)
)



modelsummary(model_list,
             coef_map = cm, 
             #gof_map = gm,
             stars = T)



modelsummary(model_list,
             coef_map = cm, 
             #gof_map = gm,
             stars = T,
             output = "./results/Tables/Table1.tex")










m1 <- feols(val_mm ~ log(irrq_m3), data = df)

m2 <- feols(val_mm ~ log(irrq_m3) + crop, data = df)


feols(val_mm ~ log(irrq_m3) + crop + crop*log(irrq_m3), data = df)
feols(val_mm ~ log(irrq_m3) + crop + crop*log(irrq_m3) + MeanPrcp + meanMinT +meanMaxT , data = df)
m <- feols(val_mm ~ log(irrq_m3)  + canola*log(irrq_m3) + MeanPrcp + MeanT + ye2016*log(irrq_m3) + ye2017*log(irrq_m3) +
        ye2018*log(irrq_m3) + ye2019*log(irrq_m3) + 
        irr200*log(irrq_m3) +  irr300*log(irrq_m3) +
        irr400*log(irrq_m3) +  irr500*log(irrq_m3) +
        irr600*log(irrq_m3) + irr700*log(irrq_m3)| site , data = df)

library(modelsummary)

modelsummary(model_5 )




