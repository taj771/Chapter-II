#clear memory
rm(list = ls())

#### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed

wheat_rf_2018 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2018.csv')
wheat_rf_2019 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2019.csv')
wheat_rf_2020 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2020.csv') 
wheat_rf_2021 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2021.csv')
wheat_rf_2022 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2022.csv')
wheat_rf_2023 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2023.csv')

df_wheat_rf <- rbind(wheat_rf_2018, wheat_rf_2019, wheat_rf_2020, wheat_rf_2021, wheat_rf_2022, wheat_rf_2023) %>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield rain (bu/ac)` = `Dry yield (bu/ac)`) %>%
  select(Year, `Dry yield rain (bu/ac)`, Site)


canola_rf_2018 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2018.csv')
canola_rf_2019 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2019.csv')
canola_rf_2020 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2020.csv') 
canola_rf_2021 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2021.csv')
canola_rf_2022 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2022.csv')
canola_rf_2023 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2023.csv')

df_canola_rf <- rbind(canola_rf_2018,canola_rf_2019,canola_rf_2020,canola_rf_2021,canola_rf_2022,canola_rf_2023)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of wheat = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield rain (bu/ac)` = `Dry yield (bu/ac)`) %>%
  select(Year, `Dry yield rain (bu/ac)`, Site)

### irrigation ### irrigation ### irrigation ### irrigation ### irrigation ### irrigation


wheat_ir_2018 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2018.csv')
wheat_ir_2019 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2019.csv')
wheat_ir_2020 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2020.csv') 
wheat_ir_2021 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2021.csv')
wheat_ir_2022 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2022.csv')
wheat_ir_2023 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2023.csv')

df_wheat_ir <- rbind(wheat_ir_2018, wheat_ir_2019, wheat_ir_2020, wheat_ir_2021, wheat_ir_2022, wheat_ir_2023) %>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield irri (bu/ac)` = `Dry yield (bu/ac)`) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  select(Year, `Dry yield irri (bu/ac)`,irrq_m3,  Site)


canola_ir_2018 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2018.csv')
canola_ir_2019 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2019.csv')
canola_ir_2020 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2020.csv') 
canola_ir_2021 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2021.csv')
canola_ir_2022 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2022.csv')
canola_ir_2023 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2023.csv')

df_canola_ir <- rbind(canola_ir_2018, canola_ir_2019, canola_ir_2020, canola_ir_2021, canola_ir_2022, canola_ir_2023) %>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield irri (bu/ac)` = `Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  select(Year, `Dry yield irri (bu/ac)`,irrq_m3,  Site)



##### Merge rainfed and irrigation dataframes

df_wheat <- df_wheat_rf%>%
  left_join(df_wheat_ir)

df_canola <- df_canola_rf%>%
  left_join(df_canola_ir)

## crop return - crop budget data ## crop return - crop budget data 

return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")


dry_cost_ac <- return$dry_cost_ac
irri_cost_fix_ac <- return$irri_cost_fix_ac
irri_cost_var_ac <- return$irri_cost_var_ac
price.bu <- return$price.bu
years <- return$year


return$dry_cost_ac <- adjust_for_inflation(dry_cost_ac, years, "CA", to_date = 2023)
return$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return$price.bu <- adjust_for_inflation(price.bu, years, "CA", to_date = 2023)


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  mutate(dry_cost_ac_ave = mean(dry_cost_ac),
         irri_cost_fix_ac = mean(irri_cost_fix_ac),
         irri_cost_var_ac = mean(irri_cost_var_ac),
         price.bu = mean(price.bu))

return_canola <- return%>%
  filter(crop =="canola")%>%
  mutate(dry_cost_ac_ave = mean(dry_cost_ac),
         irri_cost_fix_ac = mean(irri_cost_fix_ac),
         irri_cost_var_ac = mean(irri_cost_var_ac),
         price.bu = mean(price.bu))

wheat <- df_wheat%>%
  mutate(crop = "wheat")%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(reve_dif =return_ir -return_rf)%>%
  mutate(reve_val_mm = reve_dif/irrq_m3)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(prof_val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,reve_val_mm,prof_val_mm)




canola <- df_canola%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(reve_dif =return_ir -return_rf)%>%
  mutate(reve_val_mm = reve_dif/irrq_m3)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(prof_val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,reve_val_mm,prof_val_mm)


potato_ir_2018 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2018.csv')
potato_ir_2019 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2019.csv')
potato_ir_2020 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2020.csv') 
potato_ir_2021 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2021.csv')
potato_ir_2022 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2022.csv')
potato_ir_2023 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2023.csv')



return_potatao <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")


irri_cost_fix_ac <- return_potatao$irri_cost_fix_ac
irri_cost_var_ac <- return_potatao$irri_cost_var_ac
price.ton <- return_potatao$price.ton
years <- return_potatao$year

return_potatao$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potatao$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potatao$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


return_potatao <- return_potatao %>%
  mutate(dry_cost_ac_ave = mean(dry_cost_ac),
         irri_cost_fix_ac = mean(irri_cost_fix_ac),
         irri_cost_var_ac = mean(irri_cost_var_ac),
         price.bu = mean(price.bu))

potato <- rbind(potato_ir_2018, potato_ir_2019, potato_ir_2020, potato_ir_2021, potato_ir_2022, potato_ir_2023) %>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (ton/ac)` = `Fresh yield (tonne/ha)`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  select(year, `Dry yield (ton/ac)`,irrq_m3,  Site)%>%
  
  
  mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
         irr_level_inc = irr_level_mm * 0.03937,
         irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
         irri_cost = irri_cost_var_ac + irri_cost_fix_ac,
         return_ir = `Dry yield (ton/ac)` * price.ton,
         profit_ir = return_ir - irri_cost)%>%
  mutate(reve_val_mm = return_ir/irrq_m3,
         prof_val_mm = profit_ir/irrq_m3)%>%
  select(year,Site,reve_val_mm,prof_val_mm)




# dataframe - weight based on crop mix
year <- c("2018", "2019", "2020","2021","2022","2023")
ar_wheat <- c(24.4,31.2,31.7,34.2,33.7,35.4)
ar_canola <- c(30.0,22.1,28.9,32,31.8,27.8)
ar_potato <- c(5.9,5.9,4.1,4.2,5.1,4.3)

# Create dataframe
weights <- data.frame(year = year, canola = ar_canola, wheat = ar_wheat, potato = ar_potato)%>%
  mutate(area=wheat+canola+potato)%>%
  mutate(canola_w=canola/area)%>%
  mutate(wheat_w=wheat/area)%>%
  mutate(potato_w = potato/area)


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

yr2018potato <- weights%>%
  filter(year == 2018) %>%
  pull(potato_w)
yr2019potato <- weights%>%
  filter(year == 2019) %>%
  pull(potato_w)
yr2020potato <- weights%>%
  filter(year == 2020) %>%
  pull(potato_w)
yr2021potato <- weights%>%
  filter(year == 2021) %>%
  pull(potato_w)
yr2022potato <- weights%>%
  filter(year == 2022) %>%
  pull(potato_w)
yr2023potato <- weights%>%
  filter(year == 2023) %>%
  pull(potato_w)

wheat_weighted <- wheat%>%
  filter(year > 2017)%>%
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
    reve_val_mm_wheat = reve_val_mm*weight,
    prof_val_mm_wheat = prof_val_mm*weight)%>%
  select(year,Site,reve_val_mm_wheat,prof_val_mm_wheat)


canola_weighted <- canola%>%
  filter(year > 2017)%>%
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
    reve_val_mm_canola = reve_val_mm*weight,
    prof_val_mm_canola = prof_val_mm*weight)%>%
  select(year,Site,reve_val_mm_canola,prof_val_mm_canola)

potato_weighted <- potato%>%
  filter(year > 2017)%>%
  mutate(
    weight = case_when(
      year == 2018 ~ yr2018potato,
      year == 2019 ~ yr2019potato,
      year == 2020 ~ yr2020potato,
      year == 2021 ~ yr2021potato,
      year == 2022 ~ yr2022potato,
      year == 2023 ~ yr2023potato
    )
  )%>%
  mutate(
    reve_val_mm_potato = reve_val_mm*weight,
    prof_val_mm_potato = prof_val_mm*weight)%>%
  select(year,Site,reve_val_mm_potato,prof_val_mm_potato)




df1 <- wheat_weighted%>%
  left_join(canola_weighted, by = c("Site", "year"))%>%
  left_join(potato_weighted, by = c("Site", "year"))%>%
  mutate(reve_val_mm_w =reve_val_mm_wheat+reve_val_mm_canola+reve_val_mm_potato,
         prof_val_mm_w = prof_val_mm_wheat + prof_val_mm_canola+prof_val_mm_potato
  )%>%
  select(year,Site,reve_val_mm_w,prof_val_mm_w)


wheat <-wheat%>%
  rename(reve_val_mm_wheat = reve_val_mm,
         prof_val_mm_wheat = prof_val_mm)

canola <-canola%>%
  rename(reve_val_mm_canola = reve_val_mm,
         prof_val_mm_canola = prof_val_mm)


potato <-potato%>%
  rename(reve_val_mm_potato = reve_val_mm,
         prof_val_mm_potato = prof_val_mm)


df_all <- wheat%>%
  left_join(canola)%>%
  left_join(potato)%>%
  left_join(df1)


weather_data_final <- read.csv("./AquaCropOPSyData/ClimateData/combined_daymet_weather_data.csv")%>%
  select(site,longitude,latitude)%>%
  distinct(site,.keep_all = T)

df_sf <-   st_as_sf(weather_data_final, coords = c("longitude", "latitude"), crs = 4326)  # Use the appropriate CRS

df_map_grids <- st_read("./maps/LakeDiefenbaker/MergeLakeDiefenbakerfishnet5Km.shp")

# Ensure both are in the same CRS
df_map_grids <- st_transform(df_map_grids, st_crs(df_sf))


df_map_grids<- df_map_grids %>%
  st_join(df_sf, join = st_intersects)

df_map <- df_map_grids%>%
  rename(Site=site)%>%
  left_join(df_all)




df_one_map <- df_map%>%
  #filter(year %in% c(2018,2021,2023))%>%
  group_by(Site)%>%
  mutate(prof_val_mm_w_average = median(prof_val_mm_w))%>%
  ungroup()%>%
  select(Site,prof_val_mm_w_average)%>%
  distinct(Site, .keep_all = T)%>%
  drop_na()%>%
  mutate(prof_val_mm_w_average = ifelse(prof_val_mm_w_average < 0, 0, prof_val_mm_w_average))



st_write(df_one_map, "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/Chapter-II/Data/shapefile/value_map.shp")


library(MetBrewer)
palette1 <- met.brewer("VanGogh1", 70, type = "continuous")  # Generate palette



# Create the map and assign to a variable
p <- tm_shape(df_one_map, projection = 3347) +
  tm_fill(
    col = "prof_val_mm_w_average",
    palette = palette1,
    title = "",  # no title
    lwd = 0.05,
    showNA = FALSE,
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1)
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = FALSE,
    legend.position = c("right", "bottom"),
    legend.direction = "horizontal"
  )

# Save to PNG
tmap_save(p, filename = "./results/images/AverageValue_profit_map.png", width = 10, height = 7, dpi = 300)
