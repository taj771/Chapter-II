# Define the range of years
years <- 2030:2035




# Read and merge all files
merged_wheat <- map_dfr(years, ~read_csv(paste0("AquaCropOPSyData/WheatCMIP126/OriPrcp/WheatCMIP126_OriPrcp", .x, ".csv")) %>%
                          mutate(year = .x) %>%
                          rename(yld_ori = Yield_tonne_per_ha,
                                 irq_ori = Total_Irrigation_mm) %>%
                          select(year, Site_ID, yld_ori, irq_ori))



merged_wheat_20redP <- map_dfr(years, ~read_csv(paste0("AquaCropOPSyData/WheatCMIP126/RedPrcp/150mm/WheatCMIP126_20redPrcp", .x, ".csv")) %>%
                                 mutate(year = .x) %>%
                                 rename(yld_20red = Yield_tonne_per_ha,
                                        irq_20red = Total_Irrigation_mm) %>%
                                 select(year, Site_ID, yld_20red, irq_20red))



merged_wheat_40redP <- map_dfr(years, ~read_csv(paste0("AquaCropOPSyData/WheatCMIP126/RedPrcp/150mm/WheatCMIP126_40redPrcp", .x, ".csv")) %>%
                                 mutate(year = .x) %>%
                                 rename(yld_40red = Yield_tonne_per_ha,
                                        irq_40red = Total_Irrigation_mm) %>%
                                 select(year, Site_ID, yld_40red, irq_40red))


merged_wheat_60redP <- map_dfr(years, ~read_csv(paste0("AquaCropOPSyData/WheatCMIP126/RedPrcp/150mm/WheatCMIP126_60redPrcp", .x, ".csv")) %>%
                                 mutate(year = .x) %>%
                                 rename(yld_60red = Yield_tonne_per_ha,
                                        irq_60red = Total_Irrigation_mm) %>%
                                 select(year, Site_ID, yld_60red, irq_60red))

merged_wheat_80redP <- map_dfr(years, ~read_csv(paste0("AquaCropOPSyData/WheatCMIP126/RedPrcp/150mm/WheatCMIP126_80redPrcp", .x, ".csv")) %>%
                                 mutate(year = .x) %>%
                                 rename(yld_80red = Yield_tonne_per_ha,
                                        irq_80red = Total_Irrigation_mm) %>%
                                 select(year, Site_ID, yld_80red, irq_80red))



df_all <- merged_wheat%>%
  left_join(merged_wheat_20redP)%>%
  left_join(merged_wheat_40redP)%>%
  left_join(merged_wheat_60redP)%>%
  left_join(merged_wheat_80redP)%>%
  group_by(year)%>%
  mutate(yld_dif20 = yld_ori-yld_20red,
         irri_dif20 = irq_20red -irq_ori,
         yld_dif40 = yld_ori-yld_40red,
         irri_dif40 = irq_40red -irq_ori,
         yld_dif60 = yld_ori-yld_60red,
         irri_dif60 = irq_60red -irq_ori,
         yld_dif80 = yld_ori-yld_80red,
         irri_dif80 = irq_80red -irq_ori)%>%
  mutate(yld_dif20 = mean(yld_dif20),
         irri_dif20 = mean(irri_dif20),
         yld_dif40 = mean(yld_dif40),
         irri_dif40 = mean(irri_dif40),
         yld_dif60 = mean(yld_dif60),
         irri_dif60 = mean(irri_dif60),
         yld_dif80 = mean(yld_dif80),
         irri_dif80 = mean(irri_dif80))%>%
  ungroup()%>%
  select(year,Site_ID,yld_dif20,irri_dif20,yld_dif40,irri_dif40,
         yld_dif60,irri_dif60,yld_dif80,irri_dif80)%>%
  distinct(year, .keep_all = T)


df_final <- df_all%>%
  mutate(across(starts_with("yld"), ~ .x * 14.87449))%>%
  mutate(across(starts_with("irri"), ~ .x**0.03937))



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
  filter(year==2023)

irri_cost_var_ac <- return_wheat$irri_cost_var_ac
irri_cost_fix_ac <- return_wheat$irri_cost_fix_ac
price.bu <- return_wheat$price.bu


df_final <- df_final%>%
  mutate(irri_cost_var_ac = irri_cost_var_ac,
         irri_cost_fix_ac = irri_cost_fix_ac,
         price.bu = price.bu)%>%
  mutate(irri_cost_var_ac_20 = irri_cost_var_ac*irri_dif20,
         irri_cost_var_ac_40 = irri_cost_var_ac*irri_dif40,
         irri_cost_var_ac_60 = irri_cost_var_ac*irri_dif60,
         irri_cost_var_ac_80 = irri_cost_var_ac*irri_dif80)%>%
  mutate(irri_cost_ac_20 = irri_cost_var_ac_20 + irri_cost_fix_ac,
         irri_cost_ac_40 = irri_cost_var_ac_40 + irri_cost_fix_ac,
         irri_cost_ac_60 = irri_cost_var_ac_60 + irri_cost_fix_ac,
         irri_cost_ac_80 = irri_cost_var_ac_80 + irri_cost_fix_ac,)%>%
  mutate(return_ir20 = yld_dif20*price.bu,
         return_ir40 = yld_dif40*price.bu,
         return_ir60 = yld_dif60*price.bu,
         return_ir80 = yld_dif80*price.bu,)%>%
  mutate(profit_ir20 = return_ir20 - irri_cost_ac_20,
         profit_ir40 = return_ir40 - irri_cost_ac_40,
         profit_ir60 = return_ir60 - irri_cost_ac_60,
         profit_ir80 = return_ir80 - irri_cost_ac_80)%>%
  select(year,return_ir20,return_ir40,return_ir60,return_ir80)




df_final_long <- df_final %>%
  pivot_longer(cols = -year, names_to = "scenario", values_to = "returns")


custom_labels <- c("return_ir20" = "20%", 
                   "return_ir40" = "40%", 
                   "return_ir60" = "60%", 
                   "return_ir80" = "80%")

# Plot the line chart
p <- ggplot(df_final_long, aes(x = year, y = returns, color = scenario, group = scenario)) +
  geom_line(size = 0.6, linetype = "dashed") +  
  geom_point(size = 1.2) +  
  labs(title = "",
       x = "Year",
       y = "Changes in revenue ($ per acre)",
       color = "Precipitation Reduce by ") +
  scale_color_manual(values = c("return_ir20" = "blue", 
                                "return_ir40" = "green", 
                                "return_ir60" = "orange", 
                                "return_ir80" = "red"),
                     labels = custom_labels) +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)) +  # Custom breaks
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
        axis.ticks = element_line(size = 0.8))  # Optionally add a border around the plot


ggsave("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/results/images/ClimateChnageCost150mm.png", plot = p, width = 10, height = 7, dpi = 300)

