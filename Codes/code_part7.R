# Figure 8: Net economic benefits (%) of flexible irrigation water allocation under a 20%
# reduction in projected daily precipitation from 2030 to 2050.


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
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2030.csv")%>%
  mutate(year=2030)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2030.csv")%>%
  mutate(year=2030)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2030.csv")%>%
  mutate(year=2030)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2030 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2030)%>%
  
  mutate(ner_benefit_per = net_benefit/Tot_prof_scenario2*100)%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2031 2031 2031 2031 2031 2031 2031 2031 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2031.csv")%>%
  mutate(year=2031)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2031.csv")%>%
  mutate(year=2031)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2031.csv")%>%
  mutate(year=2031)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2031 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2031)%>%
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2032 2032 2032 2032 2032 2032 2032 2032 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2032.csv")%>%
  mutate(year=2032)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2032.csv")%>%
  mutate(year=2032)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2032.csv")%>%
  mutate(year=2032)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2032 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2032)%>%
  
  mutate(ner_benefit_per = net_benefit/Tot_prof_scenario2*100)%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2033 2033 2033 2033 2033 2033 2033 2033 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2033.csv")%>%
  mutate(year=2033)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2033.csv")%>%
  mutate(year=2033)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2033.csv")%>%
  mutate(year=2033)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2033 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2033)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)



######## 2034 2034 2034 2034 2034 2034 2034 2034 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2034.csv")%>%
  mutate(year=2034)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2034.csv")%>%
  mutate(year=2034)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2034.csv")%>%
  mutate(year=2034)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2034 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2034)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2035 2035 2035 2035 2035 2035 2035 2035 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2035.csv")%>%
  mutate(year=2035)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2035.csv")%>%
  mutate(year=2035)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2035.csv")%>%
  mutate(year=2035)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2035 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2035)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2036 2036 2036 2036 2036 2036 2036 2036 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2036.csv")%>%
  mutate(year=2036)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2036.csv")%>%
  mutate(year=2036)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2036.csv")%>%
  mutate(year=2036)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2036 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2036)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)



######## 2037 2037 2037 2037 2037 2037 2037 2037 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2037.csv")%>%
  mutate(year=2037)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2037.csv")%>%
  mutate(year=2037)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2037.csv")%>%
  mutate(year=2037)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2037 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2037)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2038 2038 2038 2038 2038 2038 2038 2038 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2038.csv")%>%
  mutate(year=2038)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2038.csv")%>%
  mutate(year=2038)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2038.csv")%>%
  mutate(year=2038)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2038 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2038)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2039 2039 2039 2039 2039 2039 2039 2039 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2039.csv")%>%
  mutate(year=2039)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2039.csv")%>%
  mutate(year=2039)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2039.csv")%>%
  mutate(year=2039)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2039 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2039)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

######## 2040 2040 2040 2040 2040 2040 2040 2040 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2040.csv")%>%
  mutate(year=2040)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2040.csv")%>%
  mutate(year=2040)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2040.csv")%>%
  mutate(year=2040)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2040 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2040)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

######## 2041 2041 2041 2041 2041 2041 2041 2041 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2041.csv")%>%
  mutate(year=2041)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2041.csv")%>%
  mutate(year=2041)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2041.csv")%>%
  mutate(year=2041)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2041 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2041)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

######## 2042 2042 2042 2042 2042 2042 2042 2042 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2042.csv")%>%
  mutate(year=2042)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2042.csv")%>%
  mutate(year=2042)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2042.csv")%>%
  mutate(year=2042)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2042 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2042)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2043 2043 2043 2043 2043 2043 2043 2043 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2043.csv")%>%
  mutate(year=2043)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2043.csv")%>%
  mutate(year=2043)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2043.csv")%>%
  mutate(year=2043)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2043 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2043)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2044 2044 2044 2044 2044 2044 2044 2044 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2044.csv")%>%
  mutate(year=2044)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2044.csv")%>%
  mutate(year=2044)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2044.csv")%>%
  mutate(year=2044)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2044 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2044)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


######## 2045 2045 2045 2045 2045 2045 2045 2045 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2045.csv")%>%
  mutate(year=2045)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2045.csv")%>%
  mutate(year=2045)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2045.csv")%>%
  mutate(year=2045)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2045 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2045)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

######## 2046 2046 2046 2046 2046 2046 2046 2046 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2046.csv")%>%
  mutate(year=2046)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2046.csv")%>%
  mutate(year=2046)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2046.csv")%>%
  mutate(year=2046)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2046 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2046)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

######## 2047 2047 2047 2047 2047 2047 2047 2047 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2047.csv")%>%
  mutate(year=2047)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2047.csv")%>%
  mutate(year=2047)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2047.csv")%>%
  mutate(year=2047)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2047 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2047)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

######## 2048 2048 2048 2048 2048 2048 2048 2048 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2048.csv")%>%
  mutate(year=2048)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2048.csv")%>%
  mutate(year=2048)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2048.csv")%>%
  mutate(year=2048)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2048 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2048)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

######## 2049 2049 2049 2049 2049 2049 2049 2049 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2049.csv")%>%
  mutate(year=2049)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2049.csv")%>%
  mutate(year=2049)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2049.csv")%>%
  mutate(year=2049)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2049 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2049)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

######## 2050 2050 2050 2050 2050 2050 2050 2050 

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp2050.csv")%>%
  mutate(year=2050)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp2050.csv")%>%
  mutate(year=2050)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp2050.csv")%>%
  mutate(year=2050)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )

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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2050 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2050)%>%
  
  mutate(
    ner_benefit_per = net_benefit / pmax(abs(Tot_prof_scenario1), abs(Tot_prof_scenario2)) * 100
  )%>%  
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)



df_all <- rbind(df_2030,df_2031,df_2032,df_2033,df_2034,df_2035,df_2036,df_2037,df_2038,df_2039,df_2040,
                df_2041,df_2042,df_2043,df_2044,df_2045,df_2046,df_2047,df_2048,df_2049,df_2050)



df_all_20prcred <- df_all%>%
  select(year,type,net_benefit)%>%
  rename(net_benefit_20perred = net_benefit)





###############################################################################
###NO Prcp change NO Prcp change NO Prcp change NO Prcp change NO Prcp change
###############################################################################

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
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2030.csv")%>%
  mutate(year=2030)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2030.csv")%>%
  mutate(year=2030)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2030.csv")%>%
  mutate(year=2030)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2030 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2030)%>%
  
  mutate(ner_benefit_per = net_benefit/Tot_prof_scenario2*100)%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

#### 2031 #### 2031 #### 2031 #### 2031 #### 2031 #### 2031 #### 2031 #### 2031 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2031.csv")%>%
  mutate(year=2031)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2031.csv")%>%
  mutate(year=2031)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2031.csv")%>%
  mutate(year=2031)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2031 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2031)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)



#### 2032 #### 2032 #### 2032 #### 2032 #### 2032 #### 2032 #### 2032 #### 2032 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2032.csv")%>%
  mutate(year=2032)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2032.csv")%>%
  mutate(year=2032)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2032.csv")%>%
  mutate(year=2032)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2032 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2032)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2033 #### 2033 #### 2033 #### 2033 #### 2033 #### 2033 #### 2033 #### 2033 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2033.csv")%>%
  mutate(year=2033)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2033.csv")%>%
  mutate(year=2033)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2033.csv")%>%
  mutate(year=2033)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2033 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2033)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2034 #### 2034 #### 2034 #### 2034 #### 2034 #### 2034 #### 2034 #### 2034 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2034.csv")%>%
  mutate(year=2034)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2034.csv")%>%
  mutate(year=2034)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2034.csv")%>%
  mutate(year=2034)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2034 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2034)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2035 #### 2035 #### 2035 #### 2035 #### 2035 #### 2035 #### 2035 #### 2035 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2035.csv")%>%
  mutate(year=2035)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2035.csv")%>%
  mutate(year=2035)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2035.csv")%>%
  mutate(year=2035)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2035 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2035)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

#### 2036 #### 2036 #### 2036 #### 2036 #### 2036 #### 2036 #### 2036 #### 2036 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2036.csv")%>%
  mutate(year=2036)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2036.csv")%>%
  mutate(year=2036)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2036.csv")%>%
  mutate(year=2036)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2036 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2036)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2037 #### 2037 #### 2037 #### 2037 #### 2037 #### 2037 #### 2037 #### 2037 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2037.csv")%>%
  mutate(year=2037)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2037.csv")%>%
  mutate(year=2037)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2037.csv")%>%
  mutate(year=2037)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2037 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2037)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2038 #### 2038 #### 2038 #### 2038 #### 2038 #### 2038 #### 2038 #### 2038 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2038.csv")%>%
  mutate(year=2038)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2038.csv")%>%
  mutate(year=2038)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2038.csv")%>%
  mutate(year=2038)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2038 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2038)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2039 #### 2039 #### 2039 #### 2039 #### 2039 #### 2039 #### 2039 #### 2039 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2039.csv")%>%
  mutate(year=2039)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2039.csv")%>%
  mutate(year=2039)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2039.csv")%>%
  mutate(year=2039)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2039 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2039)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)

#### 2040 #### 2040 #### 2040 #### 2040 #### 2040 #### 2040 #### 2040 #### 2040 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2040.csv")%>%
  mutate(year=2040)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2040.csv")%>%
  mutate(year=2040)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2040.csv")%>%
  mutate(year=2040)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2040 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2040)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2041 #### 2041 #### 2041 #### 2041 #### 2041 #### 2041 #### 2041 #### 2041 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2041.csv")%>%
  mutate(year=2041)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2041.csv")%>%
  mutate(year=2041)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2041.csv")%>%
  mutate(year=2041)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2041 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2041)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2042 #### 2042 #### 2042 #### 2042 #### 2042 #### 2042 #### 2042 #### 2042 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2042.csv")%>%
  mutate(year=2042)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2042.csv")%>%
  mutate(year=2042)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2042.csv")%>%
  mutate(year=2042)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2042 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2042)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2043 #### 2043 #### 2043 #### 2043 #### 2043 #### 2043 #### 2043 #### 2043 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2043.csv")%>%
  mutate(year=2043)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2043.csv")%>%
  mutate(year=2043)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2043.csv")%>%
  mutate(year=2043)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2043 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2043)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2044 #### 2044 #### 2044 #### 2044 #### 2044 #### 2044 #### 2044 #### 2044 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2044.csv")%>%
  mutate(year=2044)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2044.csv")%>%
  mutate(year=2044)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2044.csv")%>%
  mutate(year=2044)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2044 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2044)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2045 #### 2045 #### 2045 #### 2045 #### 2045 #### 2045 #### 2045 #### 2045 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2045.csv")%>%
  mutate(year=2045)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2045.csv")%>%
  mutate(year=2045)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2045.csv")%>%
  mutate(year=2045)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2045 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2045)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2046 #### 2046 #### 2046 #### 2046 #### 2046 #### 2046 #### 2046 #### 2046 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2046.csv")%>%
  mutate(year=2046)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2046.csv")%>%
  mutate(year=2046)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2046.csv")%>%
  mutate(year=2046)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2046 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2046)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2047 #### 2047 #### 2047 #### 2047 #### 2047 #### 2047 #### 2047 #### 2047 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2047.csv")%>%
  mutate(year=2047)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2047.csv")%>%
  mutate(year=2047)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2047.csv")%>%
  mutate(year=2047)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2047 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2047)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2048 #### 2048 #### 2048 #### 2048 #### 2048 #### 2048 #### 2048 #### 2048 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2048.csv")%>%
  mutate(year=2048)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2048.csv")%>%
  mutate(year=2048)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2048.csv")%>%
  mutate(year=2048)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2048 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2048)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2049 #### 2049 #### 2049 #### 2049 #### 2049 #### 2049 #### 2049 #### 2049 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2049.csv")%>%
  mutate(year=2049)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2049.csv")%>%
  mutate(year=2049)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2049.csv")%>%
  mutate(year=2049)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2049 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2049)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)


#### 2050 #### 2050 #### 2050 #### 2050 #### 2050 #### 2050 #### 2050 #### 2050 

### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 


return_wheat <- return%>%
  filter(crop =="wheat")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


return_canola <- return%>%
  filter(crop =="canola")

wheat_ir <- read_csv("AquaCropOPSyData/WheatCMIP245/OriPrcp/WheatCMIP245_OriPrcp2050.csv")%>%
  mutate(year=2050)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )

### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola ### Canola  ### Canola 

return_canola <- return%>%
  filter(crop =="canola")%>%
  filter(year == 2023)

irri_cost_var_ac <- return_canola$irri_cost_var_ac
irri_cost_fix_ac <- return_canola$irri_cost_fix_ac
price.bu <- return_canola$price.bu

canola_ir <- read_csv("AquaCropOPSyData/CanolaCMIP245/OriPrcp/CanolaCMIP245_OriPrcp2050.csv")%>%
  mutate(year=2050)%>%
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
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.bu = price.bu)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
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



ggplot(canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 200, by = 10),
    limits = c(10,200)
  )


### potato  ### potato ### potato  ### potato ### potato  ### potato ### potato  ### potato 


return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")%>%
  filter(year==2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton
years <- return_potato$year

return_potato$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potato$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potato$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


irri_cost_fix_ac <- return_potato$irri_cost_fix_ac
irri_cost_var_ac <- return_potato$irri_cost_var_ac
price.ton <- return_potato$price.ton

potato_ir <- read_csv("AquaCropOPSyData/PotatoCMIP245/OriPrcp/PotataoCMIP245_OriPrcp2050.csv")%>%
  mutate(year=2050)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year,Max_Irrigation_mm, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)%>%
  mutate(irr_level_mm = irrq_m3/(0.001*4046.86))%>%
  mutate(irr_level_inc = irr_level_mm*0.03937)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac)%>%
  mutate(irri_cost_fix_ac = irri_cost_fix_ac)%>%
  mutate(price.ton = price.ton)%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac*irr_level_inc)%>%
  mutate(irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  select(Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir)%>%
  arrange(Site_ID, irrq_m3) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potato = return_ir - lag(return_ir),
         prof_incre_potato = profit_ir - lag(profit_ir),
         irrq_m3_incre = irrq_m3 - lag(irrq_m3)) %>%
  
  mutate(reve_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_potato / irrq_m3_incre),
         prof_mv_potato = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_potato / irrq_m3_incre)) %>%
  ungroup() %>%
  
  group_by(Max_Irrigation_mm) %>%
  mutate(reve_mv_potato = ifelse(all(is.na(reve_mv_potato)), 0, mean(reve_mv_potato, na.rm = TRUE)),
         prof_mv_potato = ifelse(all(is.na(prof_mv_potato)), 0, mean(prof_mv_potato, na.rm = TRUE)),
         
         reve_potato = mean(return_ir) ,
         prof_potato = mean(profit_ir)
  ) %>%
  ungroup() %>%
  
  distinct(Max_Irrigation_mm, .keep_all = TRUE) %>%
  select(Max_Irrigation_mm, reve_mv_potato, prof_mv_potato,reve_potato,prof_potato)



ggplot(potato_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 20, by = 2),
    limits = c(0,20)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,260)
  )


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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
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
  mutate(prof_potato = 0)%>%
  mutate(type = "wheat-Canola")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
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

wheat_sub <- wheat_ir%>%
  rename(wheat_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_wheat,-reve_wheat,-prof_mv_wheat)

canola_sub <- canola_ir%>%
  rename(canola_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_canola,-reve_canola,-prof_mv_canola)

potato_sub <- potato_ir%>%
  rename(potato_irrigation = Max_Irrigation_mm )%>%
  select(-reve_mv_potato,-reve_potato,-prof_mv_potato)

df_wheat_canola_potato <- allocations%>%
  left_join(wheat_sub)%>%
  left_join(canola_sub)%>%
  left_join(potato_sub)%>%
  mutate(Tot_prof_scenario2 = rowSums(select(., prof_wheat, prof_canola, prof_potato), na.rm = TRUE))%>%
  arrange(desc(Tot_prof_scenario2)) %>%
  
  group_by(wheat_irrigation) %>%
  mutate(mean_prof_wheat = mean(prof_wheat, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(canola_irrigation) %>%
  mutate(mean_prof_canola = mean(prof_canola, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(potato_irrigation) %>%
  mutate(mean_prof_potato = mean(prof_potato, na.rm = TRUE)) %>%
  ungroup() %>%
  
  mutate(prof_wheat_150mm = mean_prof_wheat[wheat_irrigation == 150][1])%>%
  mutate(prof_canola_150mm = mean_prof_canola[canola_irrigation == 150][1])%>%
  mutate(prof_potato_150mm = mean_prof_potato[potato_irrigation == 150][1])%>%
  
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm), na.rm = TRUE))%>%
  
  
  
  slice(1)%>%
  mutate(type = "wheat-canola-potato")%>%
  select(type, wheat_irrigation, canola_irrigation, potato_irrigation,
         prof_wheat_150mm,prof_canola_150mm,prof_potato_150mm,
         prof_wheat, prof_canola, prof_potato, 
         Tot_prof_scenario2,Tot_prof_scenario1)



df_2050 <- rbind(df_wheat_canola,df_wheat_canola_potato)%>%
  
  mutate(net_benefit = Tot_prof_scenario2 - Tot_prof_scenario1)%>%
  
  mutate(year = 2050)%>%
  
  mutate(ner_benefit_per = abs(net_benefit/Tot_prof_scenario2*100))%>%
  
  select(type,year,wheat_irrigation, canola_irrigation, potato_irrigation,Tot_prof_scenario1,Tot_prof_scenario2,net_benefit,ner_benefit_per)




df_all <- rbind(df_2030,df_2031,df_2032,df_2033,df_2034,df_2035,df_2036,df_2037,df_2038,df_2039,df_2040,
                df_2041,df_2042,df_2043,df_2044,df_2045,df_2046,df_2047,df_2048,df_2049,df_2050)

df_all_noprcpchane <- df_all%>%
  select(year,type,net_benefit)%>%
  rename(net_benefit_nochange = net_benefit)

###############################################################################

df_all <- df_all_20prcred%>%
  left_join(df_all_noprcpchane)%>%
  mutate(ner_benefit_per = (net_benefit_20perred-net_benefit_nochange)/net_benefit_20perred*100)%>%
  mutate(ner_benefit_per = ifelse(ner_benefit_per < 0, 0, ner_benefit_per))
  
p <- ggplot(df_all, aes(x = type, y = ner_benefit_per, fill = type, color = type)) +
  geom_boxplot(outlier.shape = NA, width = 0.15, color = "black", alpha = 0.4) +
  geom_jitter(width = 0.07, alpha = 0.6) +
  labs(
    title = "",
    x = "",
    y = "Net Benefit (%)"
  ) +
  scale_fill_manual(
    values = c("wheat-Canola" = "darkgreen", "wheat-canola-potato" = "darkred"),
    labels = c("Wheat & Canola", "Wheat, Canola & Potato"),
    name = "Allocation between"
  ) +
  scale_color_manual(
    values = c("wheat-Canola" = "darkgreen", "wheat-canola-potato" = "darkred"),
    labels = c("Wheat & Canola", "Wheat, Canola & Potato"),
    name = "Allocation between"
  ) +
  scale_x_discrete(labels = NULL) +  # Remove x-axis category labels
  scale_y_continuous(breaks = seq(-100, 100, by = 10), limits = c(-3, 80), expand = c(0, 0)) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "white", fill = NA),
    axis.text.x = element_blank(),  # Hide x-axis text
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.8),
    legend.key.size = unit(0.3, "cm")
  )

ggsave("./results/images/reallocationBenefitsclimatechnage.png", plot = p, width = 10, height = 7, dpi = 300)



# Stop execution here
stop("Stopping execution after this line")


################################################################################
# Other graphical options
################################################################################





df1 <- df_all %>%
  filter(type %in% c("wheat-Canola","wheat-canola-potato"))%>%
  mutate(
    net_benefit = ifelse(Tot_prof_scenario1 < 0 & Tot_prof_scenario2 < 0, 0, net_benefit),
    ner_benefit_per = case_when(
      Tot_prof_scenario1 < 0 & Tot_prof_scenario2 < 0 ~ 0,
      xor(Tot_prof_scenario1 < 0, Tot_prof_scenario2 < 0) ~ 100,
      TRUE ~ ner_benefit_per
    )
  )


df_1 <- df1%>%
  filter(type == "wheat-Canola")%>%
  rename(net_bene_Wht_can = ner_benefit_per)%>%
  select(year,net_bene_Wht_can)

df_2 <- df1%>%
  filter(type == "wheat-canola-potato")%>%
  rename(net_bene_Wht_can_pot = ner_benefit_per)%>%
  select(year,net_bene_Wht_can_pot)

df_all <- df_1%>%
  left_join(df_2, by = c("year"))


# library
library(ggplot2)
library(ggExtra)


p <- ggplot(df_all, aes(x = net_bene_Wht_can, y = net_bene_Wht_can_pot))+
  geom_point() +
  theme(legend.position="none")


ggMarginal(p, type="boxplot")








df_all_average <- df1%>%
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




df2 <- df_all_average%>%
  filter(type %in% c("wheat-Canola","wheat-canola-potato"))



p <- ggplot(df1, aes(x = year, y = ner_benefit_per, fill = type)) +
  geom_col(position = position_dodge(width = 0.4), width = 0.3) +  # Bar plot
  geom_text(aes(label = paste0(round(ner_benefit_per, 1), "%")), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4) +  # Add percentage labels
  geom_hline(data = df2, aes(yintercept = ave_net_benefit_per, color = type), 
  linetype = "dashed", size = 0.6, show.legend = FALSE) +  # Horizontal average line per type
  scale_x_continuous(breaks = seq(2030, 2035, by = 1), expand = c(0, 0)) +  
  scale_y_continuous(breaks = seq(0, 200, by = 10), limits = c(-0.5, 105), expand = c(0, 0)) +  
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


#ggsave("./results/images/reallocationBenefitsclimatechnage.png", plot = p, width = 10, height = 7, dpi = 300)


df_prcp <- read.csv("./AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_ET.csv")%>%
  #mutate(date = as.Date(date))  # adjust format if needed, e.g. as.Date(date, format = "%Y-%m-%d")%>%
  filter(year(Date) >= 2030 & year(Date) <= 2035,
         month(Date) >= 5 & month(Date) <= 10,
         !(month(Date) == 10 & day(Date) > 30))%>%
  mutate(year = year(Date))%>%
  group_by(year,site)%>%
  mutate(Tot_precp = sum(Precipitation))%>%
  group_by(year)%>%
  mutate(Tot_precp = mean(Tot_precp))%>%
  distinct(year, .keep_all = T)%>%
  select(year,Tot_precp)


df1 <- df1%>%
  left_join(df_prcp)


df2 <- df1%>%
  filter(type=="wheat-Canola")

ggplot(df2, aes(x = Tot_precp, y = net_benefit)) +
  geom_point(color = "darkgreen") +
  #geom_smooth(method = "loess", color = "blue", se = TRUE) +
  labs(
    x = "Precipitation (mm)",
    y = "Net Benefit ($)",
    title = "Net Benefit vs Precipitation"
  ) +
  theme_minimal()





# Scale precipitation to match net_benefit visually
scale_factor <- max(df1$ner_benefit_per, na.rm = TRUE) / max(df1$Tot_precp, na.rm = TRUE)


df2 <- df1%>%
  filter(type=="wheat-Canola")

ggplot(df2, aes(x = year)) +
  geom_line(aes(y = ner_benefit_per, color = "Net Benefit"), size = 1.2) +
  geom_line(aes(y = Tot_precp * scale_factor, color = "Precipitation"), linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "Net Benefit ($)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Precipitation (mm)")
  ) +
  scale_color_manual(values = c("Net Benefit" = "blue", "Precipitation" = "green")) +
  labs(x = "Year", color = "Legend", title = "Net Benefit and Precipitation Over Time") +
  theme_minimal()
  
  
  
  group_by(year = year(Date), site) %>%                              
  summarise(total_precip = sum(Precipitation, na.rm = TRUE))%>%
  group_by(Tot_precp,)


library(ggplot2)

ggplot(df1, aes(x = type, y = ner_benefit_per)) +
    geom_boxplot(fill = "skyblue", outlier.shape = NA, width = 0.2) +  # hides outlier dots from boxplot
    geom_jitter(width = 0.2, alpha = 0.6, color = "darkblue") +  # adds scattered points
    labs(
      title = "Net Benefit by Type",
      x = "Type",
      y = "Net Benefit"
    ) +
    theme_minimal()
  



