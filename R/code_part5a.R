# Figure 5: Marginal value of irrigation water (m−3) at different irrigation levels for A) Wheat, B) Canola, and C) Potato.


#clear memory
rm(list = ls())

## crop return - crop budget data ## crop return - crop budget data 

return <- read.csv("./Data Main Analysis/CropReturnDarkBrown.csv")


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

wheat_ir_2018 <- read_csv('./Data Main Analysis/merged_simulation_results_wheat_marginal_2018_irrigation.csv')%>%
  mutate(year=2018)
wheat_ir_2019 <- read_csv('./Data Main Analysis/merged_simulation_results_wheat_marginal_2019_irrigation.csv')%>%
  mutate(year=2019)
wheat_ir_2020 <- read_csv('./Data Main Analysis/merged_simulation_results_wheat_marginal_2020_irrigation.csv')%>%
  mutate(year=2020) 
wheat_ir_2021 <- read_csv('./Data Main Analysis/merged_simulation_results_wheat_marginal_2021_irrigation.csv')%>%
  mutate(year=2021)
wheat_ir_2022 <- read_csv('./Data Main Analysis/merged_simulation_results_wheat_marginal_2022_irrigation.csv')%>%
  mutate(year=2022)
wheat_ir_2023 <- read_csv('./Data Main Analysis/merged_simulation_results_wheat_marginal_2023_irrigation.csv')%>%
  mutate(year=2023)

df_wheat_ir <- rbind(wheat_ir_2018,wheat_ir_2019,wheat_ir_2020,wheat_ir_2021,wheat_ir_2022,wheat_ir_2023)

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
  select(year,Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir,`Dry yield (bu/ac)`)


df_wheat_0_irri_2018 <- read_csv('./Data Main Analysis/wheat_rainfed_2018.csv')
df_wheat_0_irri_2019 <- read_csv('./Data Main Analysis/wheat_rainfed_2019.csv')
df_wheat_0_irri_2020 <- read_csv('./Data Main Analysis/wheat_rainfed_2020.csv')
df_wheat_0_irri_2021 <- read_csv('./Data Main Analysis/wheat_rainfed_2021.csv')
df_wheat_0_irri_2022 <- read_csv('./Data Main Analysis/wheat_rainfed_2022.csv')
df_wheat_0_irri_2023 <- read_csv('./Data Main Analysis/wheat_rainfed_2023.csv')

df_wheat_0_irri <- rbind(df_wheat_0_irri_2018,df_wheat_0_irri_2019,df_wheat_0_irri_2020,df_wheat_0_irri_2021,df_wheat_0_irri_2022)

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
  select(year,Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir,`Dry yield (bu/ac)`)


df_wheat_ir <- df_wheat_ir%>%
  
  mutate(profit_ir = ifelse(Max_Irrigation_mm == 0, return_ir-638.75, profit_ir))%>%
  
  mutate(value_per_m3 = profit_ir/40.4686)%>%
  
  
  # Sort by year, then site_id, then irrigation_level
  arrange(year, Site_ID, irr_level_mm)%>%
  
  group_by(year, Site_ID) %>%
  
  mutate(
    # Calculate marginal profit: profit at current level - profit at previous level
    prof_mv_wheat = value_per_m3 - lag(value_per_m3, default = first(value_per_m3))
  ) %>%
  
  mutate(
    prof_mv_wheat = ifelse(
      Max_Irrigation_mm %in% c(10, 20),
      prof_mv_wheat[Max_Irrigation_mm == 30],  # Value when irrigation = 30
      prof_mv_wheat  # Keep original value otherwise
    )
  ) %>% 
  ungroup()%>%
  filter(!Max_Irrigation_mm %in% c(0))


df_wheat_ir <- df_wheat_ir%>%
  group_by(Site_ID, Max_Irrigation_mm)%>%
  mutate(prof_mv_wheat = mean(prof_mv_wheat))%>%
  distinct(Max_Irrigation_mm, Site_ID, .keep_all = T)


# Plot - Wheat


p1 <- ggplot(df_wheat_ir, aes(x = Max_Irrigation_mm, y = prof_mv_wheat)) +  
  
  geom_point(alpha = 0.2,           # Semi-transparent
             size = 0.1,            # Small points
             color = "gray50") +    # Neutral color
  
  
  geom_smooth(method = "gam",           # More efficient than LOESS
              formula = y ~ s(x, k = 4),  # k controls smoothness
              se = TRUE,
              size = 0.8) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none",  
        legend.title = element_blank(),  
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.ticks = element_line(size = 0.8)) +
  labs(title = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  geom_segment(aes(x = 10, xend = 200, y = 0, yend = 0), 
               linetype = "dashed", color = "grey", size = 0.5) +
  
  # 4. Zoom to desired view
  coord_cartesian(
    ylim = c(-0.05, 1.6),     # Slight buffer
    xlim = c(10, 205)           # Slight buffer
  ) +
  
  
  scale_x_continuous(name = "Irrigation Level (mm)",
                     breaks = seq(10, 200, by = 10),
                     limits = c(10,200),
                     expand = expansion(mult = c(0, 0.05))) + # No gap at start, small gap at end +
  scale_y_continuous(
    name = expression("Marginal Value ($ m"^{-3}*")"),
    breaks = seq(0, 1.6, by = 0.2),
    expand = expansion(mult = c(0.02, 0.02))
  )



### canola  ### canola ### canola  ### canola ### canola  ### canola ### canola  ### canola 

canola_ir_2018 <- read_csv('./Data Main Analysis/merged_simulation_results_canola_marginal_2018_irrigation.csv')%>%
  mutate(year=2018)
canola_ir_2019 <- read_csv('./Data Main Analysis/merged_simulation_results_canola_marginal_2019_irrigation.csv')%>%
  mutate(year=2019)
canola_ir_2020 <- read_csv('./Data Main Analysis/merged_simulation_results_canola_marginal_2020_irrigation.csv')%>%
  mutate(year=2020) 
canola_ir_2021 <- read_csv('./Data Main Analysis/merged_simulation_results_canola_marginal_2021_irrigation.csv')%>%
  mutate(year=2021)
canola_ir_2022 <- read_csv('./Data Main Analysis/merged_simulation_results_canola_marginal_2022_irrigation.csv')%>%
  mutate(year=2022)
#canola_ir_2023 <- read_csv('./AquaCropOPSyData/canolaMarginal/merged_simulation_results_canola_marginal_2023_irrigation.csv')%>%
#mutate(year=2018)

df_canola_ir <- rbind(canola_ir_2018,canola_ir_2019,canola_ir_2020,canola_ir_2021,canola_ir_2022)

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
  select(year, Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir,`Dry yield (bu/ac)`)


df_canola_0_irri_2018 <- read_csv('./Data Main Analysis/canola_rainfed_2018.csv')
df_canola_0_irri_2019 <- read_csv('./Data Main Analysis/canola_rainfed_2019.csv')
df_canola_0_irri_2020 <- read_csv('./Data Main Analysis/canola_rainfed_2020.csv')
df_canola_0_irri_2021 <- read_csv('./Data Main Analysis/canola_rainfed_2021.csv')
df_canola_0_irri_2022 <- read_csv('./Data Main Analysis/canola_rainfed_2022.csv')
#df_canola_0_irri_2023 <- read_csv('./AquaCropOPSyData/canolaRainfed/canola_rainfed_2023.csv')

df_canola_0_irri <- rbind(df_canola_0_irri_2018,df_canola_0_irri_2019,df_canola_0_irri_2020,df_canola_0_irri_2021,df_canola_0_irri_2022)


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
  select(year,Site_ID,Max_Irrigation_mm,irr_level_mm,irrq_m3,return_ir,profit_ir,`Dry yield (bu/ac)`)


df_canola_ir <- rbind(df_canola_ir,df_canola_0_irri)




df_canola_ir <- df_canola_ir%>%
  
  mutate(profit_ir = ifelse(Max_Irrigation_mm == 0, return_ir-638.75, profit_ir))%>%
  
  mutate(value_per_m3 = profit_ir/40.4686)%>%
  
  
  # Sort by year, then site_id, then irrigation_level
  arrange(year, Site_ID, irr_level_mm)%>%
  
  group_by(year, Site_ID) %>%
  
  mutate(
    # Calculate marginal profit: profit at current level - profit at previous level
    prof_mv_canola = value_per_m3 - lag(value_per_m3, default = first(value_per_m3))
  ) %>%
  
  mutate(
    prof_mv_canola = ifelse(
      Max_Irrigation_mm %in% c(10, 20),
      prof_mv_canola[Max_Irrigation_mm == 30],  # Value when irrigation = 30
      prof_mv_canola  # Keep original value otherwise
    )
  ) %>% 
  ungroup()%>%
  filter(!Max_Irrigation_mm %in% c(0))


# Plot - canola

p2 <- ggplot(df_canola_ir, aes(x = Max_Irrigation_mm, y = prof_mv_canola)) +  
  # Smoothed line for 2018
  #geom_smooth(data = wheat %>% filter(year == 2018), aes(color = "2018"), 
  #method = "loess", se = T, size = 0.8, linetype = "dashed" ) +
  
  # Smoothed line for 2019
  #geom_smooth(data = wheat %>% filter(year == 2019), aes(color = "2019"), 
  #method = "loess", se = T, size = 0.8, linetype = "dashed") +
  
  # Smoothed line for all years combined
  
  geom_smooth(method = "gam",           # More efficient than LOESS
              formula = y ~ s(x, k = 4),  # k controls smoothness
              se = TRUE,
              size = 0.8) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none",  
        legend.title = element_blank(),  
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.ticks = element_line(size = 0.8)) +
  labs(title = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  geom_segment(aes(x = 10, xend = 200, y = 0, yend = 0), 
               linetype = "dashed", color = "grey", size = 0.5) +
  
  scale_x_continuous(name = "Irrigation Level (mm)",
                     breaks = seq(10, 200, by = 10),
                     limits = c(10,200),
                     expand = expansion(mult = c(0, 0.05))) + # No gap at start, small gap at end +
  scale_y_continuous(
    name = expression("Marginal Value ($ m"^{-3}*")"),
    breaks = seq(-0.2, 1.6, by = 0.2)
  )



ggsave("./results/images/MV_canola_2018.png", plot = p2, width = 10, height = 7, dpi = 100)


p_build <- ggplot_build(p2)

smooth_data <- p_build$data[[1]]  # The first element corresponds to geom_smooth()



### Potato ### Potato ### Potato ### Potato ### Potato ### Potato 


potato_ir_2018 <- read_csv('./Data Main Analysis/merged_simulation_results_Potato_marginal_2018_irrigation.csv')%>%
  mutate(year=2018)
potato_ir_2019 <- read_csv('./Data Main Analysis/merged_simulation_results_Potato_marginal_2019_irrigation.csv')%>%
  mutate(year=2019)
potato_ir_2020 <- read_csv('./Data Main Analysis/merged_simulation_results_Potato_marginal_2020_irrigation.csv')%>%
  mutate(year=2020) 
potato_ir_2021 <- read_csv('./Data Main Analysis/merged_simulation_results_Potato_marginal_2021_irrigation.csv')%>%
  mutate(year=2021)
potato_ir_2022 <- read_csv('./Data Main Analysis/merged_simulation_results_Potato_marginal_2022_irrigation.csv')%>%
  mutate(year=2022)
potato_ir_2023 <- read_csv('./Data Main Analysis/merged_simulation_results_Potato_marginal_2023_irrigation.csv')%>%
  mutate(year=2023)


df_potato_ir <- rbind(potato_ir_2018,potato_ir_2019,potato_ir_2020,potato_ir_2021,potato_ir_2022,potato_ir_2023)

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
         profit_ir = return_ir - irri_cost) 


df_potatao_ir <- potatao%>%
  
  #mutate(profit_ir = ifelse(Max_Irrigation_mm == 0, return_ir-638.75, profit_ir))%>%
  
  mutate(value_per_m3 = profit_ir/40.4686)%>%
  
  
  # Sort by year, then site_id, then irrigation_level
  arrange(year, Site_ID, irr_level_mm)%>%
  
  group_by(year, Site_ID) %>%
  
  mutate(
    # Calculate marginal profit: profit at current level - profit at previous level
    prof_mv_potato = value_per_m3 - lag(value_per_m3, default = first(value_per_m3))
  ) %>%
  
  mutate(
    prof_mv_potato = ifelse(
      Max_Irrigation_mm %in% c(10, 20),
      prof_mv_potato[Max_Irrigation_mm == 30],  # Value when irrigation = 30
      prof_mv_potato  # Keep original value otherwise
    )
  ) %>% 
  ungroup()%>%
  filter(!Max_Irrigation_mm %in% c(0))






p3 <- ggplot(df_potatao_ir, aes(x = Max_Irrigation_mm, y = prof_mv_potato)) +  
  # Smoothed line for 2018
  #geom_smooth(data = wheat %>% filter(year == 2018), aes(color = "2018"), 
  #method = "loess", se = T, size = 0.8, linetype = "dashed" ) +
  
  # Smoothed line for 2019
  #geom_smooth(data = wheat %>% filter(year == 2019), aes(color = "2019"), 
  #method = "loess", se = T, size = 0.8, linetype = "dashed") +
  
  # Smoothed line for all years combined
  
  geom_smooth(method = "gam",           # More efficient than LOESS
              formula = y ~ s(x, k = 4),  # k controls smoothness
              se = TRUE,
              size = 0.8) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none",  
        legend.title = element_blank(),  
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.ticks = element_line(size = 0.8)) +
  labs(title = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  geom_segment(aes(x = 10, xend = 260, y = 0, yend = 0), 
               linetype = "dashed", color = "grey", size = 0.5) +
  
  scale_x_continuous(name = "Irrigation Level (mm)",
                     breaks = seq(10, 260, by = 10),
                     limits = c(10,260),
                     expand = expansion(mult = c(0, 0.05))) + # No gap at start, small gap at end +
  scale_y_continuous(
    name = expression("Marginal Value ($ m"^{-3}*")"),
    breaks = seq(0, 8, by = 1)
  )


ggsave("./results/images/MV_potato_2018.png", plot = p3, width = 10, height = 7, dpi = 100)



p_build <- ggplot_build(p3)

smooth_data <- p_build$data[[1]]  # The first element corresponds to geom_smooth()


library(patchwork)
p1 + p2 + p3 + 
  plot_layout(ncol = 1, heights = c(1, 1, 1)) + 
  plot_annotation(tag_levels = 'A') 



