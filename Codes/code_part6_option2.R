# Figure 7: Net economic benefits (%) of flexible allocation of irrigation water


#clear memory
rm(list = ls())

library(dplyr)
library(purrr)
library(priceR)
library(tidyverse)
library(readr)
library(furrr)
library(progressr)

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

##### 2018 ##### 2018 ##### 2018 ##### 2018 ##### 2018 ##### 2018 ##### 2018 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2018 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2018_irrigation.csv')%>%
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


df_wheat_0_irri_2018 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2018.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2018 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")


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


return_canola <- return%>%
  filter(crop =="canola")

return_canola <- return%>%
  filter(crop =="canola")

### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2018 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2018_irrigation.csv')%>%
  mutate(year=2018)


df_canola_ir <- rbind(canola_ir_2018)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2018 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2018.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2018)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2018 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### 1. Two crop scenario - Wheat - Canola

# Define site IDs
site_ids <- 1:397

# Expand grid to include Site_ID
allocations <- expand.grid(
  wheat_irrigation = seq(0, 200, 1),
  canola_irrigation = seq(0, 200, 1),
  Site_ID = site_ids
)

wheat_sub <- df_wheat_0_irri_2018%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2018%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)


df_wheat_canola_2018 <- allocations %>%
  left_join(wheat_sub, by = c("Site_ID", "wheat_irrigation")) %>%
  left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
  mutate(
    Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir), na.rm = TRUE)
  )%>%
  mutate(total_irrigation = canola_irrigation+wheat_irrigation)%>%
  filter(total_irrigation <= 300)%>%
  group_by(Site_ID) %>%
  mutate(
    prof_wheat_150mm  = wheat_profit_ir[wheat_irrigation == 150][1],
    prof_canola_150mm = canola_profit_ir[canola_irrigation == 150][1]
  ) %>%
  ungroup()%>%
  group_by(Site_ID) %>%
  slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(
    Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE)
  )%>%
  mutate(
    net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100
  )%>%
  mutate(year = 2018)%>%
  
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)


##### 2019 ##### 2019 ##### 2019 ##### 2019 ##### 2019 ##### 2019 ##### 2019 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2019 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2019_irrigation.csv')%>%
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


df_wheat_0_irri_2019 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2019.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2019 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")

### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2019 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2019_irrigation.csv')%>%
  mutate(year=2019)


df_canola_ir <- rbind(canola_ir_2019)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2019 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2019.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2019)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2019 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### 1. Two crop scenario - Wheat - Canola

# Define site IDs
site_ids <- 1:397

# Expand grid to include Site_ID
allocations <- expand.grid(
  wheat_irrigation = seq(0, 200, 1),
  canola_irrigation = seq(0, 200, 1),
  Site_ID = site_ids
)

wheat_sub <- df_wheat_0_irri_2019%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2019%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)


df_wheat_canola_2019 <- allocations %>%
  left_join(wheat_sub, by = c("Site_ID", "wheat_irrigation")) %>%
  left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
  mutate(
    Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir), na.rm = TRUE)
  )%>%
  mutate(total_irrigation = canola_irrigation+wheat_irrigation)%>%
  filter(total_irrigation <= 300)%>%
  group_by(Site_ID) %>%
  mutate(
    prof_wheat_150mm  = wheat_profit_ir[wheat_irrigation == 150][1],
    prof_canola_150mm = canola_profit_ir[canola_irrigation == 150][1]
  ) %>%
  ungroup()%>%
  group_by(Site_ID) %>%
  slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(
    Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE)
  )%>%
  mutate(
    net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100
  )%>%
  mutate(year = 2019)%>%
  
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)




##### 2020 ##### 2020 ##### 2020 ##### 2020 ##### 2020 ##### 2020 ##### 2020 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2020 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2020_irrigation.csv')%>%
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


df_wheat_0_irri_2020 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2020.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2020 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")

### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2020 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2020_irrigation.csv')%>%
  mutate(year=2020)


df_canola_ir <- rbind(canola_ir_2020)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2020 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2020.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2020)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2020 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### 1. Two crop scenario - Wheat - Canola

# Define site IDs
site_ids <- 1:397

# Expand grid to include Site_ID
allocations <- expand.grid(
  wheat_irrigation = seq(0, 200, 1),
  canola_irrigation = seq(0, 200, 1),
  Site_ID = site_ids
)

wheat_sub <- df_wheat_0_irri_2020%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2020%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)


df_wheat_canola_2020 <- allocations %>%
  left_join(wheat_sub, by = c("Site_ID", "wheat_irrigation")) %>%
  left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
  mutate(
    Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir), na.rm = TRUE)
  )%>%
  mutate(total_irrigation = canola_irrigation+wheat_irrigation)%>%
  filter(total_irrigation <= 300)%>%
  group_by(Site_ID) %>%
  mutate(
    prof_wheat_150mm  = wheat_profit_ir[wheat_irrigation == 150][1],
    prof_canola_150mm = canola_profit_ir[canola_irrigation == 150][1]
  ) %>%
  ungroup()%>%
  group_by(Site_ID) %>%
  slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(
    Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE)
  )%>%
  mutate(
    net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100
  )%>%
  mutate(year = 2020)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)





##### 2021 ##### 2021 ##### 2021 ##### 2021 ##### 2021 ##### 2021 ##### 2021 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2021 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2021_irrigation.csv')%>%
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


df_wheat_0_irri_2021 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2021.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2021 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")

### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2021 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2021_irrigation.csv')%>%
  mutate(year=2021)


df_canola_ir <- rbind(canola_ir_2021)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2021 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2021.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2021)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2021 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### 1. Two crop scenario - Wheat - Canola

# Define site IDs
site_ids <- 1:397

# Expand grid to include Site_ID
allocations <- expand.grid(
  wheat_irrigation = seq(0, 200, 1),
  canola_irrigation = seq(0, 200, 1),
  Site_ID = site_ids
)

wheat_sub <- df_wheat_0_irri_2021%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2021%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)


df_wheat_canola_2021 <- allocations %>%
  left_join(wheat_sub, by = c("Site_ID", "wheat_irrigation")) %>%
  left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
  mutate(
    Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir), na.rm = TRUE)
  )%>%
  mutate(total_irrigation = canola_irrigation+wheat_irrigation)%>%
  filter(total_irrigation <= 300)%>%
  group_by(Site_ID) %>%
  mutate(
    prof_wheat_150mm  = wheat_profit_ir[wheat_irrigation == 150][1],
    prof_canola_150mm = canola_profit_ir[canola_irrigation == 150][1]
  ) %>%
  ungroup()%>%
  group_by(Site_ID) %>%
  slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(
    Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE)
  )%>%
  mutate(
    net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100
  )%>%
  mutate(year = 2021)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)





##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2022 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2022_irrigation.csv')%>%
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


df_wheat_0_irri_2022 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2022.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # 
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2022 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")

### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2022 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2022_irrigation.csv')%>%
  mutate(year=2022)


df_canola_ir <- rbind(canola_ir_2022)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2022 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2022.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2022)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2022 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### 1. Two crop scenario - Wheat - Canola

# Define site IDs
site_ids <- 1:397

# Expand grid to include Site_ID
allocations <- expand.grid(
  wheat_irrigation = seq(0, 200, 1),
  canola_irrigation = seq(0, 200, 1),
  Site_ID = site_ids
)

wheat_sub <- df_wheat_0_irri_2022%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2022%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)


df_wheat_canola_2022 <- allocations %>%
  left_join(wheat_sub, by = c("Site_ID", "wheat_irrigation")) %>%
  left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
  mutate(
    Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir), na.rm = TRUE)
  )%>%
  mutate(total_irrigation = canola_irrigation+wheat_irrigation)%>%
  filter(total_irrigation <= 300)%>%
  group_by(Site_ID) %>%
  mutate(
    prof_wheat_150mm  = wheat_profit_ir[wheat_irrigation == 150][1],
    prof_canola_150mm = canola_profit_ir[canola_irrigation == 150][1]
  ) %>%
  ungroup()%>%
  group_by(Site_ID) %>%
  slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(
    Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE)
  )%>%
  mutate(
    net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100
  )%>%
  mutate(year = 2022)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)







##### 2023 ##### 2023 ##### 2023 ##### 2023 ##### 2023 ##### 2023 ##### 2023 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2023 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2023_irrigation.csv')%>%
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


df_wheat_0_irri_2023 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2023.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2023 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")

### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2023 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2023_irrigation.csv')%>%
  mutate(year=2023)


df_canola_ir <- rbind(canola_ir_2023)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2023 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2023.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2023)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2023 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### 1. Two crop scenario - Wheat - Canola

# Define site IDs
site_ids <- 1:397

# Expand grid to include Site_ID
allocations <- expand.grid(
  wheat_irrigation = seq(0, 200, 1),
  canola_irrigation = seq(0, 200, 1),
  Site_ID = site_ids
)

wheat_sub <- df_wheat_0_irri_2023%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2023%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)


df_wheat_canola_2023 <- allocations %>%
  left_join(wheat_sub, by = c("Site_ID", "wheat_irrigation")) %>%
  left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
  mutate(
    Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir), na.rm = TRUE)
  )%>%
  mutate(total_irrigation = canola_irrigation+wheat_irrigation)%>%
  filter(total_irrigation <= 300)%>%
  group_by(Site_ID) %>%
  mutate(
    prof_wheat_150mm  = wheat_profit_ir[wheat_irrigation == 150][1],
    prof_canola_150mm = canola_profit_ir[canola_irrigation == 150][1]
  ) %>%
  ungroup()%>%
  group_by(Site_ID) %>%
  slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  mutate(
    Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE)
  )%>%
  mutate(
    net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100
  )%>%
  mutate(year = 2023)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)



df_wheat_canola_2018 <- df_wheat_canola_2018%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0))

df_wheat_canola_2019 <- df_wheat_canola_2019%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0))


df_wheat_canola_2020 <- df_wheat_canola_2020%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0))

df_wheat_canola_2021 <- df_wheat_canola_2021%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0))

df_wheat_canola_2022 <- df_wheat_canola_2022%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0))

df_wheat_canola_2023 <- df_wheat_canola_2023%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0))





df_wheat_canola <- df_wheat_canola_2018%>%
  rbind(df_wheat_canola_2019)%>%
  rbind(df_wheat_canola_2020)%>%
  rbind(df_wheat_canola_2021)%>%
  rbind(df_wheat_canola_2022)%>%
  rbind(df_wheat_canola_2023)%>%
  mutate(type = "Crop Rotation: Wheat & Canola")

  
##### Three Crop Rotations ##### Three Crop Rotations ##### Three Crop Rotations
##### Three Crop Rotations ##### Three Crop Rotations ##### Three Crop Rotations
##### Three Crop Rotations ##### Three Crop Rotations ##### Three Crop Rotations


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

##### 2018 ##### 2018 ##### 2018 ##### 2018 ##### 2018 ##### 2018 ##### 2018 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2018 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2018_irrigation.csv')%>%
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


df_wheat_0_irri_2018 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2018.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2018 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")


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


return_canola <- return%>%
  filter(crop =="canola")

return_canola <- return%>%
  filter(crop =="canola")

### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2018 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2018_irrigation.csv')%>%
  mutate(year=2018)


df_canola_ir <- rbind(canola_ir_2018)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2018 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2018.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2018)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2018 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")




### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2018 <- read_csv('./AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2018_irrigation.csv')%>%
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
  ungroup() 


# Step 1: interpolate each field separately - marginal value
potato_interp_all_marginal_value <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_potatao,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
potato_interp_all_marginal_profit <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_potato_0_irri_2018 <- potato_interp_all_marginal_value%>%
  left_join(potato_interp_all_marginal_profit)%>%
  mutate(crop = "potato")



wheat_sub <- df_wheat_0_irri_2018%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2018%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)



potato_sub <- df_potato_0_irri_2018%>%
  rename(potato_irrigation = Max_Irrigation_mm,
         potato_prof_mv = prof_mv,
         potato_profit_ir= profit_ir)%>%
  select(-crop)

# Define irrigation ranges
wheat_seq  <- seq(0, 200, 1)
canola_seq <- seq(0, 200, 1)
potato_seq <- seq(0, 260, 1)

# Define site IDs
site_ids <- 1:397

# Output folder for site CSVs
output_dir <- "Data/Processed/site_results"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2 Site-level processing function

process_site <- function(site_id, wheat_sub, canola_sub, potato_sub) {
  
  # Generate all irrigation allocation combinations for this site
  alloc_site <- expand.grid(
    wheat_irrigation  = wheat_seq,
    canola_irrigation = canola_seq,
    potato_irrigation = potato_seq
  ) %>%
    mutate(Site_ID = site_id)
  
  # Join profit data for all three crops
  result_site <- alloc_site %>%
    left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    left_join(potato_sub, by = c("Site_ID", "potato_irrigation")) %>%
    mutate(
      total_irrigation = wheat_irrigation + canola_irrigation + potato_irrigation,
      Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir, potato_profit_ir), na.rm = TRUE)
    ) %>%
    #  Apply water constraint total availability 150X3=450mm
    filter(total_irrigation <= 450)
  
  # Select best allocation under water constraint - highest net profit
  best_allocation <- result_site %>%
    slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE)
  
  # Get profit when each crop is allocated 150 mm (fixed scenario)
  prof_wheat_150mm <- result_site %>% filter(wheat_irrigation == 150) %>% pull(wheat_profit_ir) %>% first()
  prof_canola_150mm <- result_site %>% filter(canola_irrigation == 150) %>% pull(canola_profit_ir) %>% first()
  prof_potato_150mm <- result_site %>% filter(potato_irrigation == 150) %>% pull(potato_profit_ir) %>% first()
  
  Tot_prof_scenario1 <- sum(prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm, na.rm = TRUE)
  
  # Add fixed-scenario and net benefit comparison
  best_allocation <- best_allocation %>%
    mutate(
      prof_wheat_150mm = prof_wheat_150mm,
      prof_canola_150mm = prof_canola_150mm,
      prof_potato_150mm = prof_potato_150mm,
      Tot_prof_scenario1 = Tot_prof_scenario1,
      net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario2)) * 100
    )
  
  # Save site-specific CSV
  write_csv(best_allocation, file.path(output_dir, paste0("site_", site_id, "_optimal.csv")))
  
  return(best_allocation)
}

# Run in parallel with progress bar

plan(multisession, workers = parallel::detectCores() - 1)
handlers(global = TRUE)
handlers("progress")

best_all_sites <- with_progress({
  p <- progressor(along = site_ids)
  
  future_map_dfr(site_ids, function(sid) {
    result <- process_site(sid, wheat_sub, canola_sub, potato_sub)
    p(message = sprintf("Processed site %d / %d", sid, length(site_ids)))
    return(result)
  })
})

# Reset back to sequential
plan(sequential)

# 4 Save combined results

write_csv(best_all_sites, "Data/Processed/best_allocations_all_sites_2018.csv")


##### 2019 ##### 2019 ##### 2019 ##### 2019 ##### 2019 ##### 2019 ##### 2019 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2019 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2019_irrigation.csv')%>%
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


df_wheat_0_irri_2019 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2019.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2019 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2019 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2019_irrigation.csv')%>%
  mutate(year=2019)


df_canola_ir <- rbind(canola_ir_2019)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2019 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2019.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2019)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2019 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2019 <- read_csv('./AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2019_irrigation.csv')%>%
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
  ungroup() 


# Step 1: interpolate each field separately - marginal value
potato_interp_all_marginal_value <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_potatao,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
potato_interp_all_marginal_profit <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_potato_0_irri_2019 <- potato_interp_all_marginal_value%>%
  left_join(potato_interp_all_marginal_profit)%>%
  mutate(crop = "potato")



wheat_sub <- df_wheat_0_irri_2019%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2019%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)



potato_sub <- df_potato_0_irri_2019%>%
  rename(potato_irrigation = Max_Irrigation_mm,
         potato_prof_mv = prof_mv,
         potato_profit_ir= profit_ir)%>%
  select(-crop)

# Define irrigation ranges
wheat_seq  <- seq(0, 200, 1)
canola_seq <- seq(0, 200, 1)
potato_seq <- seq(0, 260, 1)

# Define site IDs
site_ids <- 1:397

# Output folder for site CSVs
output_dir <- "Data/Processed/site_results"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2 Site-level processing function

process_site <- function(site_id, wheat_sub, canola_sub, potato_sub) {
  
  # Generate all irrigation allocation combinations for this site
  alloc_site <- expand.grid(
    wheat_irrigation  = wheat_seq,
    canola_irrigation = canola_seq,
    potato_irrigation = potato_seq
  ) %>%
    mutate(Site_ID = site_id)
  
  # Join profit data for all three crops
  result_site <- alloc_site %>%
    left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    left_join(potato_sub, by = c("Site_ID", "potato_irrigation")) %>%
    mutate(
      total_irrigation = wheat_irrigation + canola_irrigation + potato_irrigation,
      Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir, potato_profit_ir), na.rm = TRUE)
    ) %>%
    #  Apply water constraint total availability 150X3=450mm
    filter(total_irrigation <= 450)
  
  # Select best allocation under water constraint - highest net profit
  best_allocation <- result_site %>%
    slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE)
  
  # Get profit when each crop is allocated 150 mm (fixed scenario)
  prof_wheat_150mm <- result_site %>% filter(wheat_irrigation == 150) %>% pull(wheat_profit_ir) %>% first()
  prof_canola_150mm <- result_site %>% filter(canola_irrigation == 150) %>% pull(canola_profit_ir) %>% first()
  prof_potato_150mm <- result_site %>% filter(potato_irrigation == 150) %>% pull(potato_profit_ir) %>% first()
  
  Tot_prof_scenario1 <- sum(prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm, na.rm = TRUE)
  
  # Add fixed-scenario and net benefit comparison
  best_allocation <- best_allocation %>%
    mutate(
      prof_wheat_150mm = prof_wheat_150mm,
      prof_canola_150mm = prof_canola_150mm,
      prof_potato_150mm = prof_potato_150mm,
      Tot_prof_scenario1 = Tot_prof_scenario1,
      net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario2)) * 100
    )
  
  # Save site-specific CSV
  write_csv(best_allocation, file.path(output_dir, paste0("site_", site_id, "_optimal.csv")))
  
  return(best_allocation)
}

# Run in parallel with progress bar

plan(multisession, workers = parallel::detectCores() - 1)
handlers(global = TRUE)
handlers("progress")

best_all_sites <- with_progress({
  p <- progressor(along = site_ids)
  
  future_map_dfr(site_ids, function(sid) {
    result <- process_site(sid, wheat_sub, canola_sub, potato_sub)
    p(message = sprintf("Processed site %d / %d", sid, length(site_ids)))
    return(result)
  })
})

# Reset back to sequential
plan(sequential)

# 4 Save combined results

write_csv(best_all_sites, "Data/Processed/best_allocations_all_sites_2019.csv")

##### 2020 ##### 2020 ##### 2020 ##### 2020 ##### 2020 ##### 2020 ##### 2020 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2020 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2020_irrigation.csv')%>%
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


df_wheat_0_irri_2020 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2020.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2020 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2020 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2020_irrigation.csv')%>%
  mutate(year=2020)


df_canola_ir <- rbind(canola_ir_2020)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2020 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2020.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2020)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2020 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2020 <- read_csv('./AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2020_irrigation.csv')%>%
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
  ungroup() 


# Step 1: interpolate each field separately - marginal value
potato_interp_all_marginal_value <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_potatao,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
potato_interp_all_marginal_profit <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_potato_0_irri_2020 <- potato_interp_all_marginal_value%>%
  left_join(potato_interp_all_marginal_profit)%>%
  mutate(crop = "potato")



wheat_sub <- df_wheat_0_irri_2020%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2020%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)



potato_sub <- df_potato_0_irri_2020%>%
  rename(potato_irrigation = Max_Irrigation_mm,
         potato_prof_mv = prof_mv,
         potato_profit_ir= profit_ir)%>%
  select(-crop)

# Define irrigation ranges
wheat_seq  <- seq(0, 200, 1)
canola_seq <- seq(0, 200, 1)
potato_seq <- seq(0, 260, 1)

# Define site IDs
site_ids <- 1:397

# Output folder for site CSVs
output_dir <- "Data/Processed/site_results"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2 Site-level processing function

process_site <- function(site_id, wheat_sub, canola_sub, potato_sub) {
  
  # Generate all irrigation allocation combinations for this site
  alloc_site <- expand.grid(
    wheat_irrigation  = wheat_seq,
    canola_irrigation = canola_seq,
    potato_irrigation = potato_seq
  ) %>%
    mutate(Site_ID = site_id)
  
  # Join profit data for all three crops
  result_site <- alloc_site %>%
    left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    left_join(potato_sub, by = c("Site_ID", "potato_irrigation")) %>%
    mutate(
      total_irrigation = wheat_irrigation + canola_irrigation + potato_irrigation,
      Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir, potato_profit_ir), na.rm = TRUE)
    ) %>%
    #  Apply water constraint total availability 150X3=450mm
    filter(total_irrigation <= 450)
  
  # Select best allocation under water constraint - highest net profit
  best_allocation <- result_site %>%
    slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE)
  
  # Get profit when each crop is allocated 150 mm (fixed scenario)
  prof_wheat_150mm <- result_site %>% filter(wheat_irrigation == 150) %>% pull(wheat_profit_ir) %>% first()
  prof_canola_150mm <- result_site %>% filter(canola_irrigation == 150) %>% pull(canola_profit_ir) %>% first()
  prof_potato_150mm <- result_site %>% filter(potato_irrigation == 150) %>% pull(potato_profit_ir) %>% first()
  
  Tot_prof_scenario1 <- sum(prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm, na.rm = TRUE)
  
  # Add fixed-scenario and net benefit comparison
  best_allocation <- best_allocation %>%
    mutate(
      prof_wheat_150mm = prof_wheat_150mm,
      prof_canola_150mm = prof_canola_150mm,
      prof_potato_150mm = prof_potato_150mm,
      Tot_prof_scenario1 = Tot_prof_scenario1,
      net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario2)) * 100
    )
  
  # Save site-specific CSV
  write_csv(best_allocation, file.path(output_dir, paste0("site_", site_id, "_optimal.csv")))
  
  return(best_allocation)
}

# Run in parallel with progress bar

plan(multisession, workers = parallel::detectCores() - 1)
handlers(global = TRUE)
handlers("progress")

best_all_sites <- with_progress({
  p <- progressor(along = site_ids)
  
  future_map_dfr(site_ids, function(sid) {
    result <- process_site(sid, wheat_sub, canola_sub, potato_sub)
    p(message = sprintf("Processed site %d / %d", sid, length(site_ids)))
    return(result)
  })
})

# Reset back to sequential
plan(sequential)

# 4 Save combined results

write_csv(best_all_sites, "Data/Processed/best_allocations_all_sites_2020.csv")

##### 2021 ##### 2021 ##### 2021 ##### 2021 ##### 2021 ##### 2021 ##### 2021 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2021 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2021_irrigation.csv')%>%
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


df_wheat_0_irri_2021 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2021.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2021 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2021 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2021_irrigation.csv')%>%
  mutate(year=2021)


df_canola_ir <- rbind(canola_ir_2021)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2021 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2021.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2021)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2021 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2021 <- read_csv('./AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2021_irrigation.csv')%>%
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
  ungroup() 


# Step 1: interpolate each field separately - marginal value
potato_interp_all_marginal_value <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_potatao,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
potato_interp_all_marginal_profit <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_potato_0_irri_2021 <- potato_interp_all_marginal_value%>%
  left_join(potato_interp_all_marginal_profit)%>%
  mutate(crop = "potato")



wheat_sub <- df_wheat_0_irri_2021%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2021%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)



potato_sub <- df_potato_0_irri_2021%>%
  rename(potato_irrigation = Max_Irrigation_mm,
         potato_prof_mv = prof_mv,
         potato_profit_ir= profit_ir)%>%
  select(-crop)

# Define irrigation ranges
wheat_seq  <- seq(0, 200, 1)
canola_seq <- seq(0, 200, 1)
potato_seq <- seq(0, 260, 1)

# Define site IDs
site_ids <- 1:397

# Output folder for site CSVs
output_dir <- "Data/Processed/site_results"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2 Site-level processing function

process_site <- function(site_id, wheat_sub, canola_sub, potato_sub) {
  
  # Generate all irrigation allocation combinations for this site
  alloc_site <- expand.grid(
    wheat_irrigation  = wheat_seq,
    canola_irrigation = canola_seq,
    potato_irrigation = potato_seq
  ) %>%
    mutate(Site_ID = site_id)
  
  # Join profit data for all three crops
  result_site <- alloc_site %>%
    left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    left_join(potato_sub, by = c("Site_ID", "potato_irrigation")) %>%
    mutate(
      total_irrigation = wheat_irrigation + canola_irrigation + potato_irrigation,
      Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir, potato_profit_ir), na.rm = TRUE)
    ) %>%
    #  Apply water constraint total availability 150X3=450mm
    filter(total_irrigation <= 450)
  
  # Select best allocation under water constraint - highest net profit
  best_allocation <- result_site %>%
    slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE)
  
  # Get profit when each crop is allocated 150 mm (fixed scenario)
  prof_wheat_150mm <- result_site %>% filter(wheat_irrigation == 150) %>% pull(wheat_profit_ir) %>% first()
  prof_canola_150mm <- result_site %>% filter(canola_irrigation == 150) %>% pull(canola_profit_ir) %>% first()
  prof_potato_150mm <- result_site %>% filter(potato_irrigation == 150) %>% pull(potato_profit_ir) %>% first()
  
  Tot_prof_scenario1 <- sum(prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm, na.rm = TRUE)
  
  # Add fixed-scenario and net benefit comparison
  best_allocation <- best_allocation %>%
    mutate(
      prof_wheat_150mm = prof_wheat_150mm,
      prof_canola_150mm = prof_canola_150mm,
      prof_potato_150mm = prof_potato_150mm,
      Tot_prof_scenario1 = Tot_prof_scenario1,
      net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario2)) * 100
    )
  
  # Save site-specific CSV
  write_csv(best_allocation, file.path(output_dir, paste0("site_", site_id, "_optimal.csv")))
  
  return(best_allocation)
}

# Run in parallel with progress bar

plan(multisession, workers = parallel::detectCores() - 1)
handlers(global = TRUE)
handlers("progress")

best_all_sites <- with_progress({
  p <- progressor(along = site_ids)
  
  future_map_dfr(site_ids, function(sid) {
    result <- process_site(sid, wheat_sub, canola_sub, potato_sub)
    p(message = sprintf("Processed site %d / %d", sid, length(site_ids)))
    return(result)
  })
})

# Reset back to sequential
plan(sequential)

# 4 Save combined results

write_csv(best_all_sites, "Data/Processed/best_allocations_all_sites_2021.csv")

##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 ##### 2022 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2022 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2022_irrigation.csv')%>%
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


df_wheat_0_irri_2022 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2022.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2022 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2022 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2022_irrigation.csv')%>%
  mutate(year=2022)


df_canola_ir <- rbind(canola_ir_2022)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2022 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2022.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2022)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2022 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2022 <- read_csv('./AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2022_irrigation.csv')%>%
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
  ungroup() 


# Step 1: interpolate each field separately - marginal value
potato_interp_all_marginal_value <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_potatao,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
potato_interp_all_marginal_profit <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_potato_0_irri_2022 <- potato_interp_all_marginal_value%>%
  left_join(potato_interp_all_marginal_profit)%>%
  mutate(crop = "potato")



wheat_sub <- df_wheat_0_irri_2022%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2022%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)



potato_sub <- df_potato_0_irri_2022%>%
  rename(potato_irrigation = Max_Irrigation_mm,
         potato_prof_mv = prof_mv,
         potato_profit_ir= profit_ir)%>%
  select(-crop)

# Define irrigation ranges
wheat_seq  <- seq(0, 200, 1)
canola_seq <- seq(0, 200, 1)
potato_seq <- seq(0, 260, 1)

# Define site IDs
site_ids <- 1:397

# Output folder for site CSVs
output_dir <- "Data/Processed/site_results"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2 Site-level processing function

process_site <- function(site_id, wheat_sub, canola_sub, potato_sub) {
  
  # Generate all irrigation allocation combinations for this site
  alloc_site <- expand.grid(
    wheat_irrigation  = wheat_seq,
    canola_irrigation = canola_seq,
    potato_irrigation = potato_seq
  ) %>%
    mutate(Site_ID = site_id)
  
  # Join profit data for all three crops
  result_site <- alloc_site %>%
    left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    left_join(potato_sub, by = c("Site_ID", "potato_irrigation")) %>%
    mutate(
      total_irrigation = wheat_irrigation + canola_irrigation + potato_irrigation,
      Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir, potato_profit_ir), na.rm = TRUE)
    ) %>%
    #  Apply water constraint total availability 150X3=450mm
    filter(total_irrigation <= 450)
  
  # Select best allocation under water constraint - highest net profit
  best_allocation <- result_site %>%
    slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE)
  
  # Get profit when each crop is allocated 150 mm (fixed scenario)
  prof_wheat_150mm <- result_site %>% filter(wheat_irrigation == 150) %>% pull(wheat_profit_ir) %>% first()
  prof_canola_150mm <- result_site %>% filter(canola_irrigation == 150) %>% pull(canola_profit_ir) %>% first()
  prof_potato_150mm <- result_site %>% filter(potato_irrigation == 150) %>% pull(potato_profit_ir) %>% first()
  
  Tot_prof_scenario1 <- sum(prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm, na.rm = TRUE)
  
  # Add fixed-scenario and net benefit comparison
  best_allocation <- best_allocation %>%
    mutate(
      prof_wheat_150mm = prof_wheat_150mm,
      prof_canola_150mm = prof_canola_150mm,
      prof_potato_150mm = prof_potato_150mm,
      Tot_prof_scenario1 = Tot_prof_scenario1,
      net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario2)) * 100
    )
  
  # Save site-specific CSV
  write_csv(best_allocation, file.path(output_dir, paste0("site_", site_id, "_optimal.csv")))
  
  return(best_allocation)
}

# Run in parallel with progress bar

plan(multisession, workers = parallel::detectCores() - 1)
handlers(global = TRUE)
handlers("progress")

best_all_sites <- with_progress({
  p <- progressor(along = site_ids)
  
  future_map_dfr(site_ids, function(sid) {
    result <- process_site(sid, wheat_sub, canola_sub, potato_sub)
    p(message = sprintf("Processed site %d / %d", sid, length(site_ids)))
    return(result)
  })
})

# Reset back to sequential
plan(sequential)

# 4 Save combined results

write_csv(best_all_sites, "Data/Processed/best_allocations_all_sites_2022.csv")

##### 2023 ##### 2023 ##### 2023 ##### 2023 ##### 2023 ##### 2023 ##### 2023 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir_2023 <- read_csv('./AquaCropOPSyData/WheatMarginal/merged_simulation_results_wheat_marginal_2023_irrigation.csv')%>%
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


df_wheat_0_irri_2023 <- read_csv('./AquaCropOPSyData/WheatRainfed/wheat_rainfed_2023.csv')

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
  ungroup()


# Step 1: interpolate each field separately - marginal value
wheat_interp_all_marginal_value <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_wheat,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
wheat_interp_all_marginal_profit <- wheat %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_wheat_0_irri_2023 <- wheat_interp_all_marginal_value%>%
  left_join(wheat_interp_all_marginal_profit)%>%
  mutate(crop = "wheat")


### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2023 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2023_irrigation.csv')%>%
  mutate(year=2023)


df_canola_ir <- rbind(canola_ir_2023)

df_canola_ir <- df_canola_ir%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of canola = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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


df_canola_0_irri_2023 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2023.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2023)

df_canola_0_irri <- df_canola_0_irri%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 36.74  # #1 tone of canola = 36.74 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
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
  ungroup()


# Step 1: interpolate each field separately - marginal value
canola_interp_all_marginal_value <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_canola,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
canola_interp_all_marginal_profit <- canola %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_canola_0_irri_2023 <- canola_interp_all_marginal_value%>%
  left_join(canola_interp_all_marginal_profit)%>%
  mutate(crop= "canola")


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2023 <- read_csv('./AquaCropOPSyData/PotataoMarginal/merged_simulation_results_Potato_marginal_2023_irrigation.csv')%>%
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
  ungroup() 


# Step 1: interpolate each field separately - marginal value
potato_interp_all_marginal_value <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = prof_mv_potatao,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, prof_mv = y)


# Step 1: interpolate each field separately - profit
potato_interp_all_marginal_profit <- potatao %>%
  group_by(Site_ID) %>%        # <-- replace with your field identifier column name
  summarise(
    interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))
  ) %>%
  unnest_wider(interp) %>%
  unnest(cols = c(x, y)) %>%
  rename(Max_Irrigation_mm = x, profit_ir = y)


df_potato_0_irri_2023 <- potato_interp_all_marginal_value%>%
  left_join(potato_interp_all_marginal_profit)%>%
  mutate(crop = "potato")



wheat_sub <- df_wheat_0_irri_2023%>%
  rename(wheat_irrigation = Max_Irrigation_mm,
         wheat_prof_mv = prof_mv,
         wheat_profit_ir= profit_ir)%>%
  select(-crop)



canola_sub <- df_canola_0_irri_2023%>%
  rename(canola_irrigation = Max_Irrigation_mm,
         canola_prof_mv = prof_mv,
         canola_profit_ir= profit_ir)%>%
  select(-crop)



potato_sub <- df_potato_0_irri_2023%>%
  rename(potato_irrigation = Max_Irrigation_mm,
         potato_prof_mv = prof_mv,
         potato_profit_ir= profit_ir)%>%
  select(-crop)

# Define irrigation ranges
wheat_seq  <- seq(0, 200, 1)
canola_seq <- seq(0, 200, 1)
potato_seq <- seq(0, 260, 1)

# Define site IDs
site_ids <- 1:397

# Output folder for site CSVs
output_dir <- "Data/Processed/site_results"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 2 Site-level processing function

process_site <- function(site_id, wheat_sub, canola_sub, potato_sub) {
  
  # Generate all irrigation allocation combinations for this site
  alloc_site <- expand.grid(
    wheat_irrigation  = wheat_seq,
    canola_irrigation = canola_seq,
    potato_irrigation = potato_seq
  ) %>%
    mutate(Site_ID = site_id)
  
  # Join profit data for all three crops
  result_site <- alloc_site %>%
    left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    left_join(potato_sub, by = c("Site_ID", "potato_irrigation")) %>%
    mutate(
      total_irrigation = wheat_irrigation + canola_irrigation + potato_irrigation,
      Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir, potato_profit_ir), na.rm = TRUE)
    ) %>%
    #  Apply water constraint total availability 150X3=450mm
    filter(total_irrigation <= 450)
  
  # Select best allocation under water constraint - highest net profit
  best_allocation <- result_site %>%
    slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE)
  
  # Get profit when each crop is allocated 150 mm (fixed scenario)
  prof_wheat_150mm <- result_site %>% filter(wheat_irrigation == 150) %>% pull(wheat_profit_ir) %>% first()
  prof_canola_150mm <- result_site %>% filter(canola_irrigation == 150) %>% pull(canola_profit_ir) %>% first()
  prof_potato_150mm <- result_site %>% filter(potato_irrigation == 150) %>% pull(potato_profit_ir) %>% first()
  
  Tot_prof_scenario1 <- sum(prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm, na.rm = TRUE)
  
  # Add fixed-scenario and net benefit comparison
  best_allocation <- best_allocation %>%
    mutate(
      prof_wheat_150mm = prof_wheat_150mm,
      prof_canola_150mm = prof_canola_150mm,
      prof_potato_150mm = prof_potato_150mm,
      Tot_prof_scenario1 = Tot_prof_scenario1,
      net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario2)) * 100
    )
  
  # Save site-specific CSV
  write_csv(best_allocation, file.path(output_dir, paste0("site_", site_id, "_optimal.csv")))
  
  return(best_allocation)
}

# Run in parallel with progress bar

plan(multisession, workers = parallel::detectCores() - 1)
handlers(global = TRUE)
handlers("progress")

best_all_sites <- with_progress({
  p <- progressor(along = site_ids)
  
  future_map_dfr(site_ids, function(sid) {
    result <- process_site(sid, wheat_sub, canola_sub, potato_sub)
    p(message = sprintf("Processed site %d / %d", sid, length(site_ids)))
    return(result)
  })
})

# Reset back to sequential
plan(sequential)

# 4 Save combined results

write_csv(best_all_sites, "Data/Processed/best_allocations_all_sites_2023.csv")



df_wheat_canola_potato_2018 <- read_csv("Data/Processed/best_allocations_all_sites_2018.csv")%>%
  mutate(year = 2018)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(prof_potato_150mm = ifelse(potato_irrigation <= 150,potato_profit_ir,prof_potato_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm,prof_potato_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0),
         Flex_allocation_2_potato = ifelse(potato_irrigation > 150, potato_irrigation - 150, 0))

df_wheat_canola_potato_2019 <- read_csv("Data/Processed/best_allocations_all_sites_2019.csv")%>%
  mutate(year = 2019)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(prof_potato_150mm = ifelse(potato_irrigation <= 150,potato_profit_ir,prof_potato_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm,prof_potato_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0),
         Flex_allocation_2_potato = ifelse(potato_irrigation > 150, potato_irrigation - 150, 0))

df_wheat_canola_potato_2020 <- read_csv("Data/Processed/best_allocations_all_sites_2020.csv")%>%
  mutate(year = 2020)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(prof_potato_150mm = ifelse(potato_irrigation <= 150,potato_profit_ir,prof_potato_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm,prof_potato_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0),
         Flex_allocation_2_potato = ifelse(potato_irrigation > 150, potato_irrigation - 150, 0))

df_wheat_canola_potato_2021 <- read_csv("Data/Processed/best_allocations_all_sites_2021.csv")%>%
  mutate(year = 2021)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(prof_potato_150mm = ifelse(potato_irrigation <= 150,potato_profit_ir,prof_potato_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm,prof_potato_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0),
         Flex_allocation_2_potato = ifelse(potato_irrigation > 150, potato_irrigation - 150, 0))

df_wheat_canola_potato_2022 <- read_csv("Data/Processed/best_allocations_all_sites_2022.csv")%>%
  mutate(year = 2022)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(prof_potato_150mm = ifelse(potato_irrigation <= 150,potato_profit_ir,prof_potato_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm,prof_potato_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0),
         Flex_allocation_2_potato = ifelse(potato_irrigation > 150, potato_irrigation - 150, 0))

df_wheat_canola_potato_2023 <- read_csv("Data/Processed/best_allocations_all_sites_2023.csv")%>%
  mutate(year = 2023)%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(prof_potato_150mm = ifelse(potato_irrigation <= 150,potato_profit_ir,prof_potato_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm,prof_potato_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0),
         Flex_allocation_2_potato = ifelse(potato_irrigation > 150, potato_irrigation - 150, 0))



df_wheat_canola_potato <- df_wheat_canola_potato_2018%>%
  rbind(df_wheat_canola_potato_2019)%>%
  rbind(df_wheat_canola_potato_2020)%>%
  rbind(df_wheat_canola_potato_2021)%>%
  rbind(df_wheat_canola_potato_2022)%>%
  rbind(df_wheat_canola_potato_2023)%>%
  mutate(type = "Crop Rotation: Wheat, Canola & Potato")
  
  
  
df_wheat_canola <- df_wheat_canola%>%
  select(Site_ID, year, net_benefit_percent,type)%>%
  filter(!(year == 2022) | (year == 2022 & net_benefit_percent < 1.5))


df_wheat_canola_potato <- df_wheat_canola_potato%>%
  select(Site_ID, year, net_benefit_percent,type)




df_all <- df_wheat_canola%>%
  rbind(df_wheat_canola_potato)%>%
  mutate(net_benefit_percent = ifelse(net_benefit_percent > 100, 100, net_benefit_percent))


p <- ggplot(df_all, aes(x = factor(year), y = net_benefit_percent)) +
  geom_boxplot(
    notch = T,
    width = 0.4,
    position = position_dodge(0.5),
    fill = "maroon",      
    color = "black",      
    outlier.shape = NA    
  ) +
  facet_wrap(~ type, nrow = 1, scales = "fixed") +
  labs(
    x = "Year",
    y = "Net Economic Benefits (%)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.8),
    axis.ticks = element_line(size = 0.8)
  ) +
  scale_y_continuous(
    breaks = seq(0, 60, by = 5),
    limits = c(0, 60)
  )


ggsave("./results/images/reallocationBenefits_option2.png", plot = p, width = 10, height = 7, dpi = 300)


t <- df_all%>%
  filter(type =="Crop Rotation: Wheat & Canola")

median(t$net_benefit_percent)

t <- df_all%>%
  filter(type =="Crop Rotation: Wheat, Canola & Potato")%>%
  filter(year==2019)

mean(t$net_benefit_percent)


