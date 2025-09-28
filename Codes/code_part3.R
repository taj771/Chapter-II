# Figure 3: Average shadow price of a cubic meter of irrigation water from 2018 to 2023.


#clear memory
rm(list = ls())

#### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed #### Rain-fed

wheat_rf_2018 <- read_csv('./Data Main Analysis/wheat_rainfed_2018.csv')
wheat_rf_2019 <- read_csv('./Data Main Analysis/wheat_rainfed_2019.csv')
wheat_rf_2020 <- read_csv('./Data Main Analysis/wheat_rainfed_2020.csv') 
wheat_rf_2021 <- read_csv('./Data Main Analysis/wheat_rainfed_2021.csv')
wheat_rf_2022 <- read_csv('./Data Main Analysis/wheat_rainfed_2022.csv')
wheat_rf_2023 <- read_csv('./Data Main Analysis/wheat_rainfed_2023.csv')

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


canola_rf_2018 <- read_csv('./Data Main Analysis/canola_rainfed_2018.csv')
canola_rf_2019 <- read_csv('./Data Main Analysis/canola_rainfed_2019.csv')
canola_rf_2020 <- read_csv('./Data Main Analysis/canola_rainfed_2020.csv') 
canola_rf_2021 <- read_csv('./Data Main Analysis/canola_rainfed_2021.csv')
canola_rf_2022 <- read_csv('./Data Main Analysis/canola_rainfed_2022.csv')
canola_rf_2023 <- read_csv('./Data Main Analysis/canola_rainfed_2023.csv')

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


wheat_ir_2018 <- read_csv('./Data Main Analysis/wheat_netirridemand_2018.csv')
wheat_ir_2019 <- read_csv('./Data Main Analysis/wheat_netirridemand_2019.csv')
wheat_ir_2020 <- read_csv('./Data Main Analysis/wheat_netirridemand_2020.csv') 
wheat_ir_2021 <- read_csv('./Data Main Analysis/wheat_netirridemand_2021.csv')
wheat_ir_2022 <- read_csv('./Data Main Analysis/wheat_netirridemand_2022.csv')
wheat_ir_2023 <- read_csv('./Data Main Analysis/wheat_netirridemand_2023.csv')

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


canola_ir_2018 <- read_csv('./Data Main Analysis/canola_netirridemand_2018.csv')
canola_ir_2019 <- read_csv('./Data Main Analysis/canola_netirridemand_2019.csv')
canola_ir_2020 <- read_csv('./Data Main Analysis/canola_netirridemand_2020.csv') 
canola_ir_2021 <- read_csv('./Data Main Analysis/canola_netirridemand_2021.csv')
canola_ir_2022 <- read_csv('./Data Main Analysis/canola_netirridemand_2022.csv')
canola_ir_2023 <- read_csv('./Data Main Analysis/canola_netirridemand_2023.csv')

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

wheat <- df_wheat%>%
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


potato_ir_2018 <- read_csv('./Data Main Analysis/potato_netirridemand_2018.csv')
potato_ir_2019 <- read_csv('./Data Main Analysis/potato_netirridemand_2019.csv')
potato_ir_2020 <- read_csv('./Data Main Analysis/potato_netirridemand_2020.csv') 
potato_ir_2021 <- read_csv('./Data Main Analysis/potato_netirridemand_2021.csv')
potato_ir_2022 <- read_csv('./Data Main Analysis/potato_netirridemand_2022.csv')
potato_ir_2023 <- read_csv('./Data Main Analysis/potato_netirridemand_2023.csv')



return_potatao <- read.csv("./Data Main Analysis/CropReturnPotato.csv")


irri_cost_fix_ac <- return_potatao$irri_cost_fix_ac
irri_cost_var_ac <- return_potatao$irri_cost_var_ac
price.ton <- return_potatao$price.ton
years <- return_potatao$year

return_potatao$irri_cost_fix_ac <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
return_potatao$irri_cost_var_ac <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
return_potatao$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)

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


df_profit <- df_all%>%
  select(Site,year,prof_val_mm_wheat,prof_val_mm_canola,prof_val_mm_potato,prof_val_mm_w)%>%
  pivot_longer(cols = c(`prof_val_mm_wheat`, `prof_val_mm_canola`,prof_val_mm_potato,`prof_val_mm_w`), names_to = "variable", values_to = "value")
  
df_profit$variable <- factor(df_profit$variable, levels = c("prof_val_mm_wheat", "prof_val_mm_canola","prof_val_mm_potato","prof_val_mm_w"))


# Split datasets by crop
wheat_plot <- df_profit %>% 
  filter(variable == "prof_val_mm_wheat") %>%
  ggplot(aes(x = factor(year), y = value, fill = variable)) +
  geom_boxplot(notch = TRUE, width = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.6) +  # Add horizontal line at y = 0
  labs(title = "Wheat", x = "Year", y = expression("Average Value ($m"^{-3}*")")) +
  scale_fill_manual(values = c("prof_val_mm_wheat" = "springgreen4")) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) +
  scale_y_continuous(limits = c(-1.5, 2), breaks = seq(-1.5, 2, by = 0.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.ticks = element_line(size = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold")  # center and bold the title
  )

canola_plot <- df_profit %>% 
  filter(variable == "prof_val_mm_canola") %>%
  ggplot(aes(x = factor(year), y = value, fill = variable)) +
  geom_boxplot(notch = TRUE, width = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.6) +  # Add horizontal line at y = 0
  labs(title = "Canola", x = "Year", y = expression("Average Value ($m"^{-3}*")")) +
  scale_fill_manual(values = c("prof_val_mm_canola" = "firebrick3")) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) +
  scale_y_continuous(limits = c(-1.5, 2), breaks = seq(-1.5, 2, by = 0.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.ticks = element_line(size = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold")  # center and bold the title
  )



potato_plot <- df_profit %>% 
  filter(variable == "prof_val_mm_potato") %>%
  ggplot(aes(x = factor(year), y = value, fill = variable)) +
  geom_boxplot(notch = TRUE, width = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.6) +  # Add horizontal line at y = 0
  labs(title = "Potato", x = "Year", y = expression("Average Value ($m"^{-3}*")")) +
  scale_fill_manual(values = c("prof_val_mm_potato" = "goldenrod1")) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) +
  scale_y_continuous(limits = c(2, 8), breaks = seq(-2, 8, by = 0.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.ticks = element_line(size = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold")  # center and bold the title
  )


weighted_plot <- df_profit %>% 
  filter(variable == "prof_val_mm_w") %>%
  ggplot(aes(x = factor(year), y = value, fill = variable)) +
  geom_boxplot(notch = TRUE, width = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.6) +  # Add horizontal line at y = 0
  labs(title = "Weighted", x = "Year", y = expression("Average Value ($m"^{-3}*")")) +
  scale_fill_manual(values = c("prof_val_mm_w" = "purple3")) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.ticks = element_line(size = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold")  # center and bold the title
  )


library(patchwork)

# Combine all plots
p <- (wheat_plot | canola_plot) / (potato_plot | weighted_plot)


ggsave("./results/images/AverageValue_Profit.png", plot = p, width = 10, height = 7, dpi = 300)



stop()
###################################################################################################################################
###################################################################################################################################
p <- ggplot(df_profit, aes(x = factor(year), y = value,  fill = interaction(variable))) +
  geom_boxplot(notch = TRUE, width = 0.25, position = position_dodge(0.5),outlier.size = 0.4) +
  labs(x = "Year", y = "Value (mm)", title = "Irrigation & Precipitation for Wheat & Canola by Year") +
  theme_minimal()+
  scale_fill_manual(values = c("springgreen4", "firebrick3","goldenrod1", "purple3"),
                    labels = c("Wheat","Canola","Potato", "Weighted")) +
  theme(legend.position = "top") +
  labs(
    title = "",
    x = "Year",
    y = expression("Average Value ($m"^{-3}*")"),
    fill = "Crop Type"
  ) +  
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) + # Adjust gap between x-axis labels (year)
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(
                     limits = c(-0.5, 4), breaks = seq(-2, 4, by = 0.5)) + # Customize y-axis scale
  guides(fill = guide_legend(title = NULL)) +  # Remove legend title
  theme(
    legend.position = "bottom",  # Move the legend to the bottom
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12),  # Adjust x-axis title size
    axis.title.y = element_text(size = 12),   # Adjust y-axis title size
    legend.text = element_text(size = 12),    # Adjust legend text size
    axis.ticks = element_line(size = 0.8)
  ) 


ggsave("./results/images/AverageValue_Profit.png", plot = p, width = 10, height = 7, dpi = 300)


################################################################################
### Two x axis grapg since potato has high level of average value

# Scale potato values down for plotting
df_profit <- df_profit %>%
  mutate(scaled_value = ifelse(grepl("potato", variable), value / 10, value))

p <- ggplot(df_profit, aes(x = factor(year), y = scaled_value, fill = variable)) +
  geom_boxplot(notch = TRUE, width = 0.25, position = position_dodge(0.5), outlier.size = 0.4) +
  scale_fill_manual(
    values = c("springgreen4", "firebrick3", "goldenrod1", "purple3"),
    labels = c("Wheat", "Canola", "Potato", "Weighted")
  ) +
  labs(
    x = "Year",
    y = expression("Average Value ($m"^{-3}*") [Wheat/Canola/Weighted]"),
    fill = "Crop Type"
  ) +
  scale_y_continuous(
    limits = c(-0.5, 1.5), 
    breaks = seq(-0.5, 1.5, 0.5),
    sec.axis = sec_axis(~ . * 10, name = expression("Average Value ($m"^{-3}*") [Potato]"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.ticks = element_line(size = 0.8),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  guides(fill = guide_legend(title = NULL)) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08)))


ggsave("./results/images/AverageValue_twoaxis_Profit.png", plot = p, width = 10, height = 7, dpi = 300)



df_profit <- df_all%>%
  select(Site,year,prof_val_mm_wheat,prof_val_mm_canola,prof_val_mm_w)%>%
  pivot_longer(cols = c(`prof_val_mm_wheat`, `prof_val_mm_canola`,`prof_val_mm_w`), names_to = "variable", values_to = "value")

df_profit$variable <- factor(df_profit$variable, levels = c("prof_val_mm_wheat", "prof_val_mm_canola","prof_val_mm_w"))


p <- ggplot(df_profit, aes(x = factor(year), y = value,  fill = interaction(variable))) +
  geom_boxplot(notch = TRUE, width = 0.25, position = position_dodge(0.5),outlier.size = 0.4) +
  labs(x = "Year", y = "Value (mm)", title = "Irrigation & Precipitation for Wheat & Canola by Year") +
  theme_minimal()+
  scale_fill_manual(values = c("springgreen4", "firebrick3", "purple3"),
                    labels = c("Wheat","Canola", "Weighted")) +
  theme(legend.position = "top") +
  labs(
    title = "",
    x = "Year",
    y = expression("Average Value ($m"^{-3}*")"),
    fill = "Crop Type"
  ) +  
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) + # Adjust gap between x-axis labels (year)
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(
                     limits = c(-1.5, 1.5), breaks = seq(-2, 2, by = 0.5)) + # Customize y-axis scale
  guides(fill = guide_legend(title = NULL)) +  # Remove legend title
  theme(
    legend.position = "bottom",  # Move the legend to the bottom
    axis.text.x = element_text(size = 18),  # Adjust x-axis text size
    axis.text.y = element_text(size = 18),  # Adjust y-axis text size
    axis.title.x = element_text(size = 18),  # Adjust x-axis title size
    axis.title.y = element_text(size = 18),   # Adjust y-axis title size
    legend.text = element_text(size = 18),    # Adjust legend text size
    axis.ticks = element_line(size = 0.8)
  ) 

ggsave("./results/images/AverageValue_Profit.png", plot = p, width = 10, height = 7, dpi = 300)

