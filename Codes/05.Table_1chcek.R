#clear memory
rm(list = ls())

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
  filter(crop =="wheat")

return_canola <- return%>%
  filter(crop =="canola")

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2018 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2018_irrigation.csv')%>%
  mutate(year=2018)


df_wheat_ir <- rbind(wheat_ir_2018)

df_wheat_ir <- df_wheat_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of wheat = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_0_irri_2018 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2018.csv')

df_wheat_0_irri <- rbind(df_wheat_0_irri_2018)

df_wheat_0_irri <- df_wheat_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_ir <- rbind(df_wheat_ir,df_wheat_0_irri)


wheat <- df_wheat_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_wheat = return_ir - lag(return_ir),
         prof_incre_wheat = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_wheat / irrq_m3_incre),
         prof_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_wheat / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_wheat = ifelse(all(is.na(reve_mv_wheat)), 0, mean(reve_mv_wheat, na.rm = TRUE)),
         prof_mv_wheat = ifelse(all(is.na(prof_mv_wheat)), 0, mean(prof_mv_wheat, na.rm = TRUE)),
         
         reve_wheat = mean(return_ir) ,
         prof_wheat = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_wheat, prof_mv_wheat,reve_wheat,prof_wheat)


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2018 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2018_irrigation.csv')%>%
  mutate(year=2018)


df_canola_ir <- rbind(canola_ir_2018)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 44.09  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_0_irri_2018 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2018.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2018)


df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_ir <- rbind(df_canola_ir,df_canola_0_irri)



canola <- df_canola_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_canola = return_ir - lag(return_ir),
         prof_incre_canola = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_canola / irrq_m3_incre),
         prof_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_canola / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_canola = ifelse(all(is.na(reve_mv_canola)), 0, mean(reve_mv_canola, na.rm = TRUE)),
         prof_mv_canola = ifelse(all(is.na(prof_mv_canola)), 0, mean(prof_mv_canola, na.rm = TRUE)),
         reve_canola = mean(return_ir) ,
         prof_canola = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_canola, prof_mv_canola,reve_canola,prof_canola)


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2018 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2018_irrigation.csv')%>%
  mutate(year=2018)



df_potato_ir <- rbind(potato_ir_2018)

df_potatao_ir <- df_potato_ir%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)



## crop return - crop budget data ## crop return - crop budget data 

return_potatao <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")


irri_cost_fix_ac <- return_potatao$irri_cost_fix_ac
irri_cost_var_ac <- return_potatao$irri_cost_var_ac
price.ton <- return_potatao$price.ton
years <- return_potatao$year

return_potatao$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potatao$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potatao$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


potatao <- df_potatao_ir %>%
  left_join(return_potatao) %>%
  mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
         irr_level_inc = irr_level_mm * 0.03937,
         irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
         irri_cost = irri_cost_var_ac + irri_cost_fix_ac,
         return_ir = `Dry yield (ton/ac)` * price.ton,
         profit_ir = return_ir - irri_cost) %>%
  arrange(Site_ID, irrq_m3) %>%
  group_by(Site_ID) %>%
  mutate(reve_incre_potatao = return_ir - lag(return_ir),
         prof_incre_potatao = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potatao / irrq_m3_incre),
         prof_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potatao / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potatao = ifelse(all(is.na(reve_mv_potatao)), 0, mean(reve_mv_potatao, na.rm = TRUE)),
         prof_mv_potatao = ifelse(all(is.na(prof_mv_potatao)), 0, mean(prof_mv_potatao, na.rm = TRUE)),
         reve_potatao = mean(return_ir) ,
         prof_potatao = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE)%>%
  select(Max_Irrigation_mm, reve_mv_potatao, prof_mv_potatao,reve_potatao,prof_potatao)

################################################################################

### 1. Two crop scenario - Wheat - Canola

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola, -prof_mv_canola)

df_wheat_canola <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  

  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = NA)%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  
  mutate(max_potato_prof = 0)%>%
  slice(1)%>%
  mutate(potato_irrigation = 0)%>%
  mutate(prof_potatao = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 2. Two crop scenario - Wheat - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
#canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + potato_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)


potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = NA)%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_canola_prof = 0)%>%
  slice(1)%>%
  mutate(canola_irrigation = 0)%>%
  mutate(prof_canola = 0)%>%
  mutate(type = "wheat-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 3. Two crop scenario - Canola - Potato
# Define the irrigation ranges for each crop
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(canola_irrigation + potato_irrigation <= 300)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao, -prof_mv_potatao)

df_canola_potato <- allocations%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = NA)%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%

  mutate(max_wheat_prof = 0)%>%
  slice(1)%>%
  mutate(wheat_irrigation = 0)%>%
  mutate(prof_wheat = 0)%>%
  mutate(type = "canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 4. Three crop scenario - Wheat Canola - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation + potato_irrigation <= 450)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
 
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


df_2018 <- rbind(df_wheat_canola,df_wheat_potato,df_canola_potato,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2018)%>%

  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit)


########## 2019 2019 2019 2019 2019 2019 2019 2019 2019 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2019 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2019_irrigation.csv')%>%
  mutate(year=2019)


df_wheat_ir <- rbind(wheat_ir_2019)

df_wheat_ir <- df_wheat_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of wheat = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_0_irri_2019 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2019.csv')

df_wheat_0_irri <- rbind(df_wheat_0_irri_2019)

df_wheat_0_irri <- df_wheat_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_ir <- rbind(df_wheat_ir,df_wheat_0_irri)


wheat <- df_wheat_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_wheat = return_ir - lag(return_ir),
         prof_incre_wheat = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_wheat / irrq_m3_incre),
         prof_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_wheat / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_wheat = ifelse(all(is.na(reve_mv_wheat)), 0, mean(reve_mv_wheat, na.rm = TRUE)),
         prof_mv_wheat = ifelse(all(is.na(prof_mv_wheat)), 0, mean(prof_mv_wheat, na.rm = TRUE)),
         
         reve_wheat = mean(return_ir) ,
         prof_wheat = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_wheat, prof_mv_wheat,reve_wheat,prof_wheat)


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2019 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2019_irrigation.csv')%>%
  mutate(year=2019)


df_canola_ir <- rbind(canola_ir_2019)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 44.09  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_0_irri_2019 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2019.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2019)


df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_ir <- rbind(df_canola_ir,df_canola_0_irri)



canola <- df_canola_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_canola = return_ir - lag(return_ir),
         prof_incre_canola = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_canola / irrq_m3_incre),
         prof_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_canola / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_canola = ifelse(all(is.na(reve_mv_canola)), 0, mean(reve_mv_canola, na.rm = TRUE)),
         prof_mv_canola = ifelse(all(is.na(prof_mv_canola)), 0, mean(prof_mv_canola, na.rm = TRUE)),
         reve_canola = mean(return_ir) ,
         prof_canola = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_canola, prof_mv_canola,reve_canola,prof_canola)


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2019 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2019_irrigation.csv')%>%
  mutate(year=2019)



df_potato_ir <- rbind(potato_ir_2019)

df_potatao_ir <- df_potato_ir%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)


potatao <- df_potatao_ir %>%
  left_join(return_potatao) %>%
  mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
         irr_level_inc = irr_level_mm * 0.03937,
         irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
         irri_cost = irri_cost_var_ac + irri_cost_fix_ac,
         return_ir = `Dry yield (ton/ac)` * price.ton,
         profit_ir = return_ir - irri_cost) %>%
  arrange(Site_ID, irrq_m3) %>%
  group_by(Site_ID) %>%
  mutate(reve_incre_potatao = return_ir - lag(return_ir),
         prof_incre_potatao = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potatao / irrq_m3_incre),
         prof_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potatao / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potatao = ifelse(all(is.na(reve_mv_potatao)), 0, mean(reve_mv_potatao, na.rm = TRUE)),
         prof_mv_potatao = ifelse(all(is.na(prof_mv_potatao)), 0, mean(prof_mv_potatao, na.rm = TRUE)),
         reve_potatao = mean(return_ir) ,
         prof_potatao = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE)%>%
  select(Max_Irrigation_mm, reve_mv_potatao, prof_mv_potatao,reve_potatao,prof_potatao)

################################################################################

### 1. Two crop scenario - Wheat - Canola

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola, -prof_mv_canola)

df_wheat_canola <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  
  
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = NA)%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  
  mutate(max_potato_prof = 0)%>%
  slice(1)%>%
  mutate(potato_irrigation = 0)%>%
  mutate(prof_potatao = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 2. Two crop scenario - Wheat - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
#canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + potato_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)


potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = NA)%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_canola_prof = 0)%>%
  slice(1)%>%
  mutate(canola_irrigation = 0)%>%
  mutate(prof_canola = 0)%>%
  mutate(type = "wheat-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 3. Two crop scenario - Canola - Potato
# Define the irrigation ranges for each crop
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(canola_irrigation + potato_irrigation <= 300)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao, -prof_mv_potatao)

df_canola_potato <- allocations%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = NA)%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_wheat_prof = 0)%>%
  slice(1)%>%
  mutate(wheat_irrigation = 0)%>%
  mutate(prof_wheat = 0)%>%
  mutate(type = "canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 4. Three crop scenario - Wheat Canola - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation + potato_irrigation <= 450)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


df_2019 <- rbind(df_wheat_canola,df_wheat_potato,df_canola_potato,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2019)%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit)


####### 2020 2020 2020 2020 2020 2020 2020 

########## 2020 2020 2020 2020 2020 2020 2020 2020 2020 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2020 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2020_irrigation.csv')%>%
  mutate(year=2020)


df_wheat_ir <- rbind(wheat_ir_2020)

df_wheat_ir <- df_wheat_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of wheat = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_0_irri_2020 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2020.csv')

df_wheat_0_irri <- rbind(df_wheat_0_irri_2020)

df_wheat_0_irri <- df_wheat_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_ir <- rbind(df_wheat_ir,df_wheat_0_irri)


wheat <- df_wheat_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_wheat = return_ir - lag(return_ir),
         prof_incre_wheat = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_wheat / irrq_m3_incre),
         prof_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_wheat / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_wheat = ifelse(all(is.na(reve_mv_wheat)), 0, mean(reve_mv_wheat, na.rm = TRUE)),
         prof_mv_wheat = ifelse(all(is.na(prof_mv_wheat)), 0, mean(prof_mv_wheat, na.rm = TRUE)),
         
         reve_wheat = mean(return_ir) ,
         prof_wheat = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_wheat, prof_mv_wheat,reve_wheat,prof_wheat)


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2020 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2020_irrigation.csv')%>%
  mutate(year=2020)


df_canola_ir <- rbind(canola_ir_2020)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 44.09  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_0_irri_2020 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2020.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2020)


df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_ir <- rbind(df_canola_ir,df_canola_0_irri)



canola <- df_canola_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_canola = return_ir - lag(return_ir),
         prof_incre_canola = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_canola / irrq_m3_incre),
         prof_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_canola / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_canola = ifelse(all(is.na(reve_mv_canola)), 0, mean(reve_mv_canola, na.rm = TRUE)),
         prof_mv_canola = ifelse(all(is.na(prof_mv_canola)), 0, mean(prof_mv_canola, na.rm = TRUE)),
         reve_canola = mean(return_ir) ,
         prof_canola = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_canola, prof_mv_canola,reve_canola,prof_canola)


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2020 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2020_irrigation.csv')%>%
  mutate(year=2020)



df_potato_ir <- rbind(potato_ir_2020)

df_potatao_ir <- df_potato_ir%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)


potatao <- df_potatao_ir %>%
  left_join(return_potatao) %>%
  mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
         irr_level_inc = irr_level_mm * 0.03937,
         irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
         irri_cost = irri_cost_var_ac + irri_cost_fix_ac,
         return_ir = `Dry yield (ton/ac)` * price.ton,
         profit_ir = return_ir - irri_cost) %>%
  arrange(Site_ID, irrq_m3) %>%
  group_by(Site_ID) %>%
  mutate(reve_incre_potatao = return_ir - lag(return_ir),
         prof_incre_potatao = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potatao / irrq_m3_incre),
         prof_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potatao / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potatao = ifelse(all(is.na(reve_mv_potatao)), 0, mean(reve_mv_potatao, na.rm = TRUE)),
         prof_mv_potatao = ifelse(all(is.na(prof_mv_potatao)), 0, mean(prof_mv_potatao, na.rm = TRUE)),
         reve_potatao = mean(return_ir) ,
         prof_potatao = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE)%>%
  select(Max_Irrigation_mm, reve_mv_potatao, prof_mv_potatao,reve_potatao,prof_potatao)

################################################################################

### 1. Two crop scenario - Wheat - Canola

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola, -prof_mv_canola)

df_wheat_canola <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  
  
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = NA)%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  
  mutate(max_potato_prof = 0)%>%
  slice(1)%>%
  mutate(potato_irrigation = 0)%>%
  mutate(prof_potatao = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 2. Two crop scenario - Wheat - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
#canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + potato_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)


potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = NA)%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_canola_prof = 0)%>%
  slice(1)%>%
  mutate(canola_irrigation = 0)%>%
  mutate(prof_canola = 0)%>%
  mutate(type = "wheat-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 3. Two crop scenario - Canola - Potato
# Define the irrigation ranges for each crop
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(canola_irrigation + potato_irrigation <= 300)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao, -prof_mv_potatao)

df_canola_potato <- allocations%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = NA)%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_wheat_prof = 0)%>%
  slice(1)%>%
  mutate(wheat_irrigation = 0)%>%
  mutate(prof_wheat = 0)%>%
  mutate(type = "canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 4. Three crop scenario - Wheat Canola - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation + potato_irrigation <= 450)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


df_2020 <- rbind(df_wheat_canola,df_wheat_potato,df_canola_potato,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2020)%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit)

########## 2021 2021 2021 2021 2021 2021 2021 2021 2021 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2021 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2021_irrigation.csv')%>%
  mutate(year=2021)


df_wheat_ir <- rbind(wheat_ir_2021)

df_wheat_ir <- df_wheat_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of wheat = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_0_irri_2021 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2021.csv')

df_wheat_0_irri <- rbind(df_wheat_0_irri_2021)

df_wheat_0_irri <- df_wheat_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_ir <- rbind(df_wheat_ir,df_wheat_0_irri)


wheat <- df_wheat_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_wheat = return_ir - lag(return_ir),
         prof_incre_wheat = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_wheat / irrq_m3_incre),
         prof_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_wheat / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_wheat = ifelse(all(is.na(reve_mv_wheat)), 0, mean(reve_mv_wheat, na.rm = TRUE)),
         prof_mv_wheat = ifelse(all(is.na(prof_mv_wheat)), 0, mean(prof_mv_wheat, na.rm = TRUE)),
         
         reve_wheat = mean(return_ir) ,
         prof_wheat = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_wheat, prof_mv_wheat,reve_wheat,prof_wheat)


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2021 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2021_irrigation.csv')%>%
  mutate(year=2021)


df_canola_ir <- rbind(canola_ir_2021)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 44.09  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_0_irri_2021 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2021.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2021)


df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_ir <- rbind(df_canola_ir,df_canola_0_irri)



canola <- df_canola_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_canola = return_ir - lag(return_ir),
         prof_incre_canola = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_canola / irrq_m3_incre),
         prof_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_canola / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_canola = ifelse(all(is.na(reve_mv_canola)), 0, mean(reve_mv_canola, na.rm = TRUE)),
         prof_mv_canola = ifelse(all(is.na(prof_mv_canola)), 0, mean(prof_mv_canola, na.rm = TRUE)),
         reve_canola = mean(return_ir) ,
         prof_canola = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_canola, prof_mv_canola,reve_canola,prof_canola)


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2021 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2021_irrigation.csv')%>%
  mutate(year=2021)



df_potato_ir <- rbind(potato_ir_2021)

df_potatao_ir <- df_potato_ir%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)


potatao <- df_potatao_ir %>%
  left_join(return_potatao) %>%
  mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
         irr_level_inc = irr_level_mm * 0.03937,
         irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
         irri_cost = irri_cost_var_ac + irri_cost_fix_ac,
         return_ir = `Dry yield (ton/ac)` * price.ton,
         profit_ir = return_ir - irri_cost) %>%
  arrange(Site_ID, irrq_m3) %>%
  group_by(Site_ID) %>%
  mutate(reve_incre_potatao = return_ir - lag(return_ir),
         prof_incre_potatao = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potatao / irrq_m3_incre),
         prof_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potatao / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potatao = ifelse(all(is.na(reve_mv_potatao)), 0, mean(reve_mv_potatao, na.rm = TRUE)),
         prof_mv_potatao = ifelse(all(is.na(prof_mv_potatao)), 0, mean(prof_mv_potatao, na.rm = TRUE)),
         reve_potatao = mean(return_ir) ,
         prof_potatao = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE)%>%
  select(Max_Irrigation_mm, reve_mv_potatao, prof_mv_potatao,reve_potatao,prof_potatao)

################################################################################

### 1. Two crop scenario - Wheat - Canola

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola, -prof_mv_canola)

df_wheat_canola <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  
  
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = NA)%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  
  mutate(max_potato_prof = 0)%>%
  slice(1)%>%
  mutate(potato_irrigation = 0)%>%
  mutate(prof_potatao = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 2. Two crop scenario - Wheat - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
#canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + potato_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)


potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = NA)%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_canola_prof = 0)%>%
  slice(1)%>%
  mutate(canola_irrigation = 0)%>%
  mutate(prof_canola = 0)%>%
  mutate(type = "wheat-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 3. Two crop scenario - Canola - Potato
# Define the irrigation ranges for each crop
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(canola_irrigation + potato_irrigation <= 300)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao, -prof_mv_potatao)

df_canola_potato <- allocations%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = NA)%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_wheat_prof = 0)%>%
  slice(1)%>%
  mutate(wheat_irrigation = 0)%>%
  mutate(prof_wheat = 0)%>%
  mutate(type = "canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 4. Three crop scenario - Wheat Canola - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation + potato_irrigation <= 450)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


df_2021 <- rbind(df_wheat_canola,df_wheat_potato,df_canola_potato,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2021)%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit)



########## 2022 2022 2022 2022 2022 2022 2022 2022 2022 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2022 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2022_irrigation.csv')%>%
  mutate(year=2022)


df_wheat_ir <- rbind(wheat_ir_2022)

df_wheat_ir <- df_wheat_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of wheat = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_0_irri_2022 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2022.csv')

df_wheat_0_irri <- rbind(df_wheat_0_irri_2022)

df_wheat_0_irri <- df_wheat_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_ir <- rbind(df_wheat_ir,df_wheat_0_irri)


wheat <- df_wheat_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_wheat = return_ir - lag(return_ir),
         prof_incre_wheat = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_wheat / irrq_m3_incre),
         prof_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_wheat / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_wheat = ifelse(all(is.na(reve_mv_wheat)), 0, mean(reve_mv_wheat, na.rm = TRUE)),
         prof_mv_wheat = ifelse(all(is.na(prof_mv_wheat)), 0, mean(prof_mv_wheat, na.rm = TRUE)),
         
         reve_wheat = mean(return_ir) ,
         prof_wheat = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_wheat, prof_mv_wheat,reve_wheat,prof_wheat)


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2022 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2022_irrigation.csv')%>%
  mutate(year=2022)


df_canola_ir <- rbind(canola_ir_2022)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 44.09  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_0_irri_2022 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2022.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2022)


df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_ir <- rbind(df_canola_ir,df_canola_0_irri)



canola <- df_canola_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_canola = return_ir - lag(return_ir),
         prof_incre_canola = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_canola / irrq_m3_incre),
         prof_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_canola / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_canola = ifelse(all(is.na(reve_mv_canola)), 0, mean(reve_mv_canola, na.rm = TRUE)),
         prof_mv_canola = ifelse(all(is.na(prof_mv_canola)), 0, mean(prof_mv_canola, na.rm = TRUE)),
         reve_canola = mean(return_ir) ,
         prof_canola = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_canola, prof_mv_canola,reve_canola,prof_canola)


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2022 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2022_irrigation.csv')%>%
  mutate(year=2022)



df_potato_ir <- rbind(potato_ir_2022)

df_potatao_ir <- df_potato_ir%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)


potatao <- df_potatao_ir %>%
  left_join(return_potatao) %>%
  mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
         irr_level_inc = irr_level_mm * 0.03937,
         irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
         irri_cost = irri_cost_var_ac + irri_cost_fix_ac,
         return_ir = `Dry yield (ton/ac)` * price.ton,
         profit_ir = return_ir - irri_cost) %>%
  arrange(Site_ID, irrq_m3) %>%
  group_by(Site_ID) %>%
  mutate(reve_incre_potatao = return_ir - lag(return_ir),
         prof_incre_potatao = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potatao / irrq_m3_incre),
         prof_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potatao / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potatao = ifelse(all(is.na(reve_mv_potatao)), 0, mean(reve_mv_potatao, na.rm = TRUE)),
         prof_mv_potatao = ifelse(all(is.na(prof_mv_potatao)), 0, mean(prof_mv_potatao, na.rm = TRUE)),
         reve_potatao = mean(return_ir) ,
         prof_potatao = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE)%>%
  select(Max_Irrigation_mm, reve_mv_potatao, prof_mv_potatao,reve_potatao,prof_potatao)

################################################################################

### 1. Two crop scenario - Wheat - Canola

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola, -prof_mv_canola)

df_wheat_canola <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  
  
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = NA)%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  
  mutate(max_potato_prof = 0)%>%
  slice(1)%>%
  mutate(potato_irrigation = 0)%>%
  mutate(prof_potatao = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 2. Two crop scenario - Wheat - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
#canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + potato_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)


potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = NA)%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_canola_prof = 0)%>%
  slice(1)%>%
  mutate(canola_irrigation = 0)%>%
  mutate(prof_canola = 0)%>%
  mutate(type = "wheat-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 3. Two crop scenario - Canola - Potato
# Define the irrigation ranges for each crop
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(canola_irrigation + potato_irrigation <= 300)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao, -prof_mv_potatao)

df_canola_potato <- allocations%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = NA)%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_wheat_prof = 0)%>%
  slice(1)%>%
  mutate(wheat_irrigation = 0)%>%
  mutate(prof_wheat = 0)%>%
  mutate(type = "canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 4. Three crop scenario - Wheat Canola - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation + potato_irrigation <= 450)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


df_2022 <- rbind(df_wheat_canola,df_wheat_potato,df_canola_potato,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2022)%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit)


########## 2023 2023 2023 2023 2023 2023 2023 2023 2023 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2023 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2023_irrigation.csv')%>%
  mutate(year=2023)


df_wheat_ir <- rbind(wheat_ir_2023)

df_wheat_ir <- df_wheat_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of wheat = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_0_irri_2023 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2023.csv')

df_wheat_0_irri <- rbind(df_wheat_0_irri_2023)

df_wheat_0_irri <- df_wheat_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of wheat = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_wheat)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_wheat_ir <- rbind(df_wheat_ir,df_wheat_0_irri)


wheat <- df_wheat_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_wheat = return_ir - lag(return_ir),
         prof_incre_wheat = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_wheat / irrq_m3_incre),
         prof_mv_wheat = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_wheat / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_wheat = ifelse(all(is.na(reve_mv_wheat)), 0, mean(reve_mv_wheat, na.rm = TRUE)),
         prof_mv_wheat = ifelse(all(is.na(prof_mv_wheat)), 0, mean(prof_mv_wheat, na.rm = TRUE)),
         
         reve_wheat = mean(return_ir) ,
         prof_wheat = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_wheat, prof_mv_wheat,reve_wheat,prof_wheat)


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2023 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2023_irrigation.csv')%>%
  mutate(year=2023)


df_canola_ir <- rbind(canola_ir_2023)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 44.09  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (bu/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_0_irri_2023 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2023.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2023)


df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  left_join(return_canola)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  rename(Max_Irrigation_mm=`Seasonal irrigation (mm)`)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)


df_canola_ir <- rbind(df_canola_ir,df_canola_0_irri)



canola <- df_canola_ir %>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_canola = return_ir - lag(return_ir),
         prof_incre_canola = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_canola / irrq_m3_incre),
         prof_mv_canola = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_canola / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_canola = ifelse(all(is.na(reve_mv_canola)), 0, mean(reve_mv_canola, na.rm = TRUE)),
         prof_mv_canola = ifelse(all(is.na(prof_mv_canola)), 0, mean(prof_mv_canola, na.rm = TRUE)),
         reve_canola = mean(return_ir) ,
         prof_canola = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_canola, prof_mv_canola,reve_canola,prof_canola)


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2023 <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2023_irrigation.csv')%>%
  mutate(year=2023)



df_potato_ir <- rbind(potato_ir_2023)

df_potatao_ir <- df_potato_ir%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)


potatao <- df_potatao_ir %>%
  left_join(return_potatao) %>%
  mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
         irr_level_inc = irr_level_mm * 0.03937,
         irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
         irri_cost = irri_cost_var_ac + irri_cost_fix_ac,
         return_ir = `Dry yield (ton/ac)` * price.ton,
         profit_ir = return_ir - irri_cost) %>%
  arrange(Site_ID, irrq_m3) %>%
  group_by(Site_ID) %>%
  mutate(reve_incre_potatao = return_ir - lag(return_ir),
         prof_incre_potatao = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  mutate(reve_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potatao / irrq_m3_incre),
         prof_mv_potatao = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potatao / irrq_m3_incre)) %>%
  ungroup() %>%
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potatao = ifelse(all(is.na(reve_mv_potatao)), 0, mean(reve_mv_potatao, na.rm = TRUE)),
         prof_mv_potatao = ifelse(all(is.na(prof_mv_potatao)), 0, mean(prof_mv_potatao, na.rm = TRUE)),
         reve_potatao = mean(return_ir) ,
         prof_potatao = mean(profit_ir)
  ) %>%
  ungroup() %>%
  distinct(Max_Irrigation_mm, .keep_all = TRUE)%>%
  select(Max_Irrigation_mm, reve_mv_potatao, prof_mv_potatao,reve_potatao,prof_potatao)

################################################################################

### 1. Two crop scenario - Wheat - Canola

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola, -prof_mv_canola)

df_wheat_canola <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  
  
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = NA)%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  
  mutate(max_potato_prof = 0)%>%
  slice(1)%>%
  mutate(potato_irrigation = 0)%>%
  mutate(prof_potatao = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 2. Two crop scenario - Wheat - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
#canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + potato_irrigation <= 300)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)


potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = NA)%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_canola_prof = 0)%>%
  slice(1)%>%
  mutate(canola_irrigation = 0)%>%
  mutate(prof_canola = 0)%>%
  mutate(type = "wheat-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 3. Two crop scenario - Canola - Potato
# Define the irrigation ranges for each crop
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(canola_irrigation + potato_irrigation <= 300)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao, -prof_mv_potatao)

df_canola_potato <- allocations%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = NA)%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  mutate(max_wheat_prof = 0)%>%
  slice(1)%>%
  mutate(wheat_irrigation = 0)%>%
  mutate(prof_wheat = 0)%>%
  mutate(type = "canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


### 4. Three crop scenario - Wheat Canola - Potato

# Define the irrigation ranges for each crop
wheat_irrigation_range <- seq(0, 200, 10)   # Wheat irrigation levels (0-200mm)
canola_irrigation_range <- seq(0, 200, 10)   # Canola irrigation levels (0-200mm)
potato_irrigation_range <- seq(0, 260, 10)   # Potato irrigation levels (0-260mm)

# Generate all possible combinations of irrigation levels
allocations <- expand.grid(
  wheat_irrigation = wheat_irrigation_range,
  canola_irrigation = canola_irrigation_range,
  potato_irrigation = potato_irrigation_range
)

# Filter combinations where the total irrigation is <= 300mm
allocations <- allocations %>%
  filter(wheat_irrigation + canola_irrigation + potato_irrigation <= 450)

wheat_sub <- wheat%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potatao_sub <- potatao%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potatao,-reve_potatao,-prof_mv_potatao)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potatao_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potatao), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potatao, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potatao, 
         Tot_prof_scenario2,Tot_prof_scenario1)


df_2023 <- rbind(df_wheat_canola,df_wheat_potato,df_canola_potato,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2023)%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit)

df_all <- rbind(df_2018, df_2019, df_2020, df_2021,df_2022, df_2023) %>%
  mutate(
    net_benefit = case_when(
      type == "wheat-Canola" & wheat_irrigation <= 149 & canola_irrigation <= 149 ~ 0,
      type == "wheat-potato" & wheat_irrigation <= 149 & potato_irrigation <= 149 ~ 0,
      type == "canola-potato" & canola_irrigation <= 149 & potato_irrigation <= 149 ~ 0,
      type == "wheat-canola-potato" & wheat_irrigation <= 149 & canola_irrigation <= 149 & potato_irrigation <= 149 ~ 0,
      TRUE ~ net_benefit  # Keep the original value for other types
    )
  )%>%
  select(year,type,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit)%>%
  arrange(year)


df_all <- df_all%>%
  mutate(ner_benefit_per = net_benefit/Tot_prof_scenario2*100)%>%
  filter(type %in% c("wheat-Canola","wheat-canola-potato"))



library(xtable)

# Convert the dataframe to LaTeX table
latex_table <- xtable(df_all)

# Save the LaTeX table to a .tex file
sink( "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/results/Tables/Table_1_to_appendix.tex") 
print(latex_table)
sink()  # Reset output back to console


df_all_average <- df_all%>%
  group_by(type)%>%
  mutate(#ave_wheat_irrigation = mean(wheat_irrigation),
    #ave_canola_irrigation = mean(canola_irrigation),
    #ave_potato_irrigation = mean(potato_irrigation),
    ave_Tot_prof_scenario1 = mean(Tot_prof_scenario1),
    ave_Tot_prof_scenario2 = mean(Tot_prof_scenario2),
    ave_net_benefit = mean(net_benefit),
    ave_net_benefit_per = mean(ner_benefit_per))%>%
  select(type,
         ave_Tot_prof_scenario1,ave_Tot_prof_scenario2,ave_net_benefit,ave_net_benefit_per)%>%
  distinct(type, .keep_all = T)


# Convert the dataframe to LaTeX table
latex_table <- xtable(df_all_average)

# Save the LaTeX table to a .tex file
sink( "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/results/Tables/Table_1.tex") 
print(latex_table)
sink()  # Reset output back to console



df1 <- df_all %>%
  filter(type %in% c("wheat-Canola","wheat-canola-potato"))

df2 <- df_all_average%>%
  filter(type %in% c("wheat-Canola","wheat-canola-potato"))

# Plot
p <- ggplot(df1, aes(x = year, y = ner_benefit_per, fill = type)) +
  geom_col(position = position_dodge(width = 0.4), width = 0.3) +  # Bar plot
  geom_text(aes(label = paste0(round(ner_benefit_per, 1), "%")), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4) +  # Add percentage labels
  geom_hline(data = df2, aes(yintercept = ave_net_benefit_per, color = type), 
             linetype = "dashed", size = 0.6, show.legend = FALSE) +  # Horizontal average line per type
  scale_x_continuous(breaks = seq(2015, 2023, by = 1), expand = c(0, 0)) +  
  scale_y_continuous(breaks = seq(-100, 100, by = 5), limits = c(-0.5, 35), expand = c(0, 0)) +  
  labs(x = "Year", y = "Net Benefit (%)", fill = NULL) +  # Remove legend title
  scale_fill_manual(values = c("wheat-Canola" = "darkgreen", "wheat-canola-potato" = "darkred"), 
                    labels = c("Wheat & Canola", "Wheat, Canola & Potato"),
                    name = "Allocation between") +  # Rename legend labels for fill
  scale_color_manual(values = c("wheat-Canola" = "darkgreen", "wheat-canola-potato" = "darkred")) +  # Match color for horizontal lines
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank(),          # Remove all grid lines
        axis.line = element_line(color = "black"), # Keep the axis lines
        panel.border = element_rect(color = "white", fill = NA),
        axis.text.x = element_text(size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        axis.title.x = element_text(size = 12),  # Adjust x-axis title size
        axis.title.y = element_text(size = 12),   # Adjust y-axis title size
        legend.text = element_text(size = 12),    # Adjust legend text size
        axis.ticks = element_line(size = 0.8),
        legend.key.size = unit(0.3, "cm"))  # Adjust size of legend boxes



ggsave("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/results/images/reallocationBenefits.png", plot = p, width = 10, height = 7, dpi = 300)





