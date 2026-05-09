# Figure 7: Net economic benefits (%) of flexible allocation of irrigation water


#clear memory
rm(list = ls())

library(dplyr)
library(purrr)
library(priceR)
library(tidyverse)

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

