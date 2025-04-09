#clear memory
rm(list = ls())


### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat ### Wheat  ### Wheat 

df_wheat_ir <- read_csv("AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_2.csv")%>%
  mutate(year=2018)%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74  # #1 tone of wheat = 36.74 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield irri (bu/ac)` = `Dry yield (bu/ac)`) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year, `Dry yield irri (bu/ac)`,irrq_m3,  Site_ID)


df_wheat_0_irri <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2018.csv')%>%
  filter(Site %in% 20:30)%>%
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
  rename(`Dry yield irri (bu/ac)` = `Dry yield (bu/ac)`) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  select(year, `Dry yield irri (bu/ac)`,irrq_m3, Site_ID)
  


df_wheat_ir <- rbind(df_wheat_ir,df_wheat_0_irri)


df_wheat_rf <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatRainfed/wheat_rainfed_2018.csv')%>%
  filter(Site %in% 20:30)%>%
  rename(Site_ID = Site)%>%
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
  select(Year, `Dry yield rain (bu/ac)`, Site_ID)



df_wheat <- df_wheat_rf%>%
  left_join(df_wheat_ir)


## crop return - crop budget data ## crop return - crop budget data 

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

wheat <- df_wheat%>%
  #filter(Year > 2014)%>%
  #rename(year = Year)%>%
  filter(`Dry yield irri (bu/ac)`!=0)%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(reve_dif =return_ir -return_rf)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  
  arrange(Site_ID,irrq_m3 ) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_wheat = (reve_dif)-lag(reve_dif))%>%
  
  mutate(irrq_m3_incre = irrq_m3-lag(irrq_m3))%>%
  
  mutate(reve_mv_wheat = reve_incre_wheat/irrq_m3_incre)%>%
  
  ungroup()%>%
  
  mutate(irr_level = irrq_m3/(0.001*4046.86))%>%
  
  group_by(irr_level)%>%
    
  mutate(reve_mv_wheat = mean(reve_mv_wheat))%>%
  ungroup()%>%
  distinct(irr_level, .keep_all = T)
  
  

ggplot(wheat, aes(x = irr_level, y = reve_mv_wheat)) +
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
  


### Canola ### Canola ### Canola ### Canola ### Canola ### Canola ### Canola
df_canola_ir <- read_csv("AquaCropOPSyData/canolaMarginal/TempDirectory/merged_results_2.csv")%>%
  mutate(year=2018)%>%
  mutate(
    `Dry yield (bu/ha)` = `Yield_tonne_per_ha` *44.09  # #1 tone of canola = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield irri (bu/ac)` = `Dry yield (bu/ac)`) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year, `Dry yield irri (bu/ac)`,irrq_m3,  Site_ID)


df_canola_0_irri <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2018.csv')%>%
  filter(Site %in% 10:20)%>%
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
  rename(`Dry yield irri (bu/ac)` = `Dry yield (bu/ac)`) %>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))%>%
  select(year, `Dry yield irri (bu/ac)`,irrq_m3, Site_ID)



df_canola_ir <- rbind(df_canola_ir,df_canola_0_irri)


df_canola_rf <- read_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/canolaRainfed/canola_rainfed_2018.csv')%>%
  filter(Site %in% 20:30)%>%
  rename(Site_ID = Site)%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  ) %>%
  mutate(
    `Dry yield (bu/ha)` = `Dry yield (tonne/ha)` * 44.09  # #1 tone of canola = 44.09 bu (60lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(
    `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47  # Convert bu/ha to bu/ac # 1 ha = 2.47 acres
  ) %>%
  rename(`Dry yield rain (bu/ac)` = `Dry yield (bu/ac)`) %>%
  select(Year, `Dry yield rain (bu/ac)`, Site_ID)



df_canola <- df_canola_rf%>%
  left_join(df_canola_ir)


## crop return - crop budget data ## crop return - crop budget data 

return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")


dry_cost_ac <- return$dry_cost_ac
irri_cost_ac <- return$irri_cost_ac
price.bu <- return$price.bu
years <- return$year


return$dry_cost_ac <- adjust_for_inflation(dry_cost_ac, years, "CA", to_date = 2023)
return$irri_cost_ac <- adjust_for_inflation(irri_cost_ac, years, "CA", to_date = 2023)
return$price.bu <- adjust_for_inflation(price.bu, years, "CA", to_date = 2023)


return_canola <- return%>%
  filter(crop =="canola")

return_canola <- return%>%
  filter(crop =="canola")

canola <- df_canola%>%
  #filter(Year > 2014)%>%
  #rename(year = Year)%>%
  filter(`Dry yield irri (bu/ac)`!=0)%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(reve_dif =return_ir -return_rf)%>%
  
  #mutate(across(c(reve_dif), ~ if_else(. < 0, 0, .)))%>%
  
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  
  #mutate(across(c(prof_dif), ~ if_else(. < 0, 0, .)))%>%
  
  
  arrange(Site_ID,irrq_m3 ) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_canola = (reve_dif)-lag(reve_dif))%>%
  
  mutate(irrq_m3_incre = irrq_m3-lag(irrq_m3))%>%
  
  mutate(reve_mv_canola = reve_incre_canola/irrq_m3_incre)%>%
  
  ungroup()%>%
  
  mutate(irr_level = irrq_m3/(0.001*4046.86))%>%
  
  group_by(irr_level)%>%
  
  mutate(reve_mv_canola = mean(reve_mv_canola))%>%
  ungroup()%>%
  distinct(irr_level, .keep_all = T)



ggplot(canola, aes(x = irr_level, y = reve_mv_canola)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.1),
    limits = c(0,2)
  )+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(10,200)
  )


### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 

df_potatao_ir <- read_csv("AquaCropOPSyData/potataoMarginal/TempDirectory/merged_results_1.csv")%>%
  mutate(year=2018)%>%
  mutate(
    `Dry yield (ton/ac)` = `Yield_tonne_per_ha`/2.47  # #1 tone of potatao = 44.09 bu (50lbs/bushels) ref: https://www.rayglen.com/grain-conversion-calculator/
  ) %>%
  mutate(irrq_m3 = 4046.86*(Total_Irrigation_mm*0.001))%>%
  select(year, `Dry yield (ton/ac)`,irrq_m3,  Site_ID)



## crop return - crop budget data ## crop return - crop budget data 

return_potatao <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")


irri_cost <- return_potatao$irri_cost
price.ton <- return_potatao$price.ton
years <- return_potatao$year

return_potatao$irri_cost <- adjust_for_inflation(irri_cost, years, "CA", to_date = 2023)
return_potatao$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)


potatao <- df_potatao_ir%>%
  #filter(Year > 2014)%>%
  #rename(year = Year)%>%
  left_join(return_potatao)%>%
  mutate(return_ir = `Dry yield (ton/ac)`*price.ton)%>%
  mutate(reve_dif =return_ir -0)%>%
  mutate(profit_ir = return_ir - irri_cost)%>%
  mutate(prof_dif = profit_ir - 0)%>%
  
  arrange(Site_ID,irrq_m3 ) %>%  # Ensure correct order within each Site
  group_by(Site_ID) %>%  # Compute differences within each site
  mutate(reve_incre_potatao = (reve_dif)-lag(reve_dif))%>%
  
  mutate(irrq_m3_incre = irrq_m3-lag(irrq_m3))%>%
  
  mutate(reve_mv_potatao = reve_incre_potatao/irrq_m3_incre)%>%
  
  ungroup()%>%
  
  mutate(irr_level = irrq_m3/(0.001*4046.86))%>%
  
  group_by(irr_level)%>%
  
  mutate(reve_mv_potatao = mean(reve_mv_potatao))%>%
  ungroup()%>%
  distinct(irr_level, .keep_all = T)



ggplot(potatao, aes(x = irr_level, y = reve_mv_potatao)) +
  geom_point(color = "blue", size = 0.5,  shape = 1) +  
  geom_line(color = "blue", size = 0.1)+
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM"))+
  scale_x_continuous(
    breaks = seq(0, 260, by = 10),
    limits = c(20,260)
  )







