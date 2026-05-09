#clear memory
rm(list = ls())


library(dplyr)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(scales)
library(priceR)


### wheat

df_wheat_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_wheat_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_wheat_iri <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_wheat_Irrigation.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal irrigation (mm)`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))


df_wheat <- df_wheat_rf%>%
  left_join(df_wheat_iri)

#### Canola


df_canola_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_canola_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_canola_iri <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_canola_Irrigation.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    Year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal irrigation (mm)`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal irrigation (mm)`*0.001))

df_canola <- df_canola_rf%>%
  left_join(df_canola_iri)


##### filter year 2015-2021

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
  rename(year = Year)%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)


canola <- df_canola%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)





canola <- canola%>%
  group_by(year)%>%
  mutate(`Total_Precipitation(mm)`=mean(`Total_Precipitation(mm)`))%>%
  mutate(`Seasonal irrigation (mm)`=mean(`Seasonal irrigation (mm)`))%>%
  mutate(val_mm=mean(val_mm))%>%
  distinct(year, .keep_all = T)%>%
  select(year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)%>%
  rename(economic_value=val_mm,
         percipitation=`Total_Precipitation(mm)`,
         irrigation=`Seasonal irrigation (mm)`)

df_long <- canola %>%
  pivot_longer(cols = c("percipitation", "irrigation"), names_to = "type", values_to = "amount")




# Plot
ggplot(df_long, aes(x = factor(year), y = amount, fill = type)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_line(data = df_long, aes(x = factor(year), y = economic_value * 1000, group = 1, color = "Economic Value"), size = 0.5) +
  geom_point(data = df_long, aes(x = factor(year), y = economic_value * 1000, color = "Economic Value"), size = 3) +
  geom_text(data = df_long, aes(x = factor(year), y = economic_value * 1000, label = sprintf("$%.2f", economic_value)),
            color = "darkred", size = 4, vjust = -0.5) +
  scale_y_continuous(
    name = "Precipitation / Irrigation (mm)",
    breaks = seq(0, 800, by = 50),
    sec.axis = sec_axis(~ . / 1000, name = expression("Average Value ($/m"^3*")"),breaks = seq(0, 1, by = 0.05)),
    expand = c(0,0)
  ) +
  #scale_y_discrete(expand = c(0, 0)) +  # Remove padding from x-axis
  labs(title = "", x = "Year") +
  scale_fill_manual(name = "", values = c("percipitation" = "dodgerblue1", "irrigation" = "skyblue"),
                    guide = guide_legend(override.aes = list(shape = NA))) +
  scale_color_manual(name = "", values = "darkred", guide = guide_legend(override.aes = list(linetype = "solid", size = 0.8, shape = NA))) +
  theme_minimal() +
  theme(legend.position = "top")+
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),                # Remove all gridlines
    axis.line = element_line(color = "black")    # Keep main x and y axis lines
  )








wheat <- wheat%>%
  group_by(year)%>%
  mutate(`Total_Precipitation(mm)`=mean(`Total_Precipitation(mm)`))%>%
  mutate(`Seasonal irrigation (mm)`=mean(`Seasonal irrigation (mm)`))%>%
  mutate(val_mm=mean(val_mm))%>%
  distinct(year, .keep_all = T)%>%
  select(year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)%>%
  rename(economic_value=val_mm,
         percipitation=`Total_Precipitation(mm)`,
         irrigation=`Seasonal irrigation (mm)`)

df_long <- wheat %>%
  pivot_longer(cols = c("percipitation", "irrigation"), names_to = "type", values_to = "amount")



# Plot
ggplot(df_long, aes(x = factor(year), y = amount, fill = type)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_line(data = df_long, aes(x = factor(year), y = economic_value * 1000, group = 1, color = "Economic Value"), size = 0.5) +
  geom_point(data = df_long, aes(x = factor(year), y = economic_value * 1000, color = "Economic Value"), size = 3) +
  geom_text(data = df_long, aes(x = factor(year), y = economic_value * 1000, label = sprintf("$%.2f", economic_value)),
            color = "darkred", size = 4, vjust = -0.5) +
  scale_y_continuous(
    name = "Precipitation / Irrigation (mm)",
    breaks = seq(0, 850, by = 50),
    sec.axis = sec_axis(~ . / 1000, name = expression("Average Value ($/m"^3*")"),breaks = seq(0, 0.8, by = 0.05)),
    expand = c(0,0)
  ) +
  labs(title = "", x = "Year") +
  scale_fill_manual(name = "", values = c("percipitation" = "dodgerblue1", "irrigation" = "skyblue"),
                    guide = guide_legend(override.aes = list(shape = NA))) +
  scale_color_manual(name = "", values = "darkred", guide = guide_legend(override.aes = list(linetype = "solid", size = 0.8, shape = NA))) +
  theme_minimal() +
  theme(legend.position = "top")+  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),                # Remove all gridlines
    axis.line = element_line(color = "black")    # Keep main x and y axis lines
  )


#### Weight


wheat <- df_wheat%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)


canola <- df_canola%>%
  #filter(Year > 2014)%>%
  rename(year = Year)%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)%>%
  select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)


# dataframe
year <- c("2018", "2019", "2020","2021","2022","2023")
ar_wheat <- c(24.4,31.2,31.7,34.2,33.7,35.4)
ar_canola <- c(30.0,22.1,28.9,32,31.8,27.8)

# Create dataframe
weights <- data.frame(year = year, canola = ar_canola, wheat = ar_wheat)%>%
  mutate(area=wheat+canola)%>%
  mutate(canola_w=canola/area)%>%
  mutate(wheat_w=wheat/area)
  

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
         w_wheat_val = val_mm*weight,
         w_wheat_irri = `Seasonal irrigation (mm)`*weight)%>%
  select(year,Site,w_wheat_irri,w_wheat_val)
  
  
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
    w_canol_val = val_mm*weight,
    w_canol_irri = `Seasonal irrigation (mm)`*weight)%>%
  select(year,Site,w_canol_irri,w_canol_val,`Total_Precipitation(mm)`)


df1 <- wheat_weighted%>%
  left_join(canola_weighted, by = c("Site", "year"))%>%
  mutate(ave_val =w_canol_val+w_wheat_val,
         ave_irri = w_wheat_irri+w_canol_irri)%>%
  select(year,Site,ave_val,ave_irri,`Total_Precipitation(mm)`)


df2 <- df1%>%
  group_by(year)%>%
  mutate(`Total_Precipitation(mm)`=mean(`Total_Precipitation(mm)`))%>%
  mutate(`Seasonal irrigation (mm)`=mean(ave_irri))%>%
  mutate(val_mm=mean(ave_val))%>%
  distinct(year, .keep_all = T)%>%
  select(year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)%>%
  rename(economic_value=val_mm,
         percipitation=`Total_Precipitation(mm)`,
         irrigation=`Seasonal irrigation (mm)`)




df_long <- df2 %>%
  pivot_longer(cols = c("percipitation", "irrigation"), names_to = "type", values_to = "amount")




# Plot
ggplot(df_long, aes(x = factor(year), y = amount, fill = type)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_line(data = df_long, aes(x = factor(year), y = economic_value * 1000, group = 1, color = "Economic Value"), size = 0.5) +
  geom_point(data = df_long, aes(x = factor(year), y = economic_value * 1000, color = "Economic Value"), size = 3) +
  geom_text(data = df_long, aes(x = factor(year), y = economic_value * 1000, label = sprintf("$%.2f", economic_value)),
            color = "darkred", size = 4, vjust = -1.1) +
  scale_y_continuous(
    name = "Precipitation / Irrigation (mm)",
    breaks = seq(0, 850, by = 50),
    sec.axis = sec_axis(~ . / 1000, name = expression("Average Value ($/m"^3*")"),breaks = seq(0, 0.8, by = 0.05)),
    expand = c(0,0)
  ) +
  labs(title = "", x = "Year") +
  scale_fill_manual(name = "", values = c("percipitation" = "dodgerblue1", "irrigation" = "skyblue"),
                    guide = guide_legend(override.aes = list(shape = NA))) +
  scale_color_manual(name = "", values = "darkred", guide = guide_legend(override.aes = list(linetype = "solid", size = 0.8, shape = NA))) +
  theme_minimal() +
  theme(legend.position = "top")+  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),                # Remove all gridlines
    axis.line = element_line(color = "black")    # Keep main x and y axis lines
  )


########## New-plot ####### New-plot ####### New-plot

canola_p1 <- df_canola%>%
  select(Year,Site,`Seasonal irrigation (mm)`)%>%
  mutate(crop="canola")


wheat_p1 <- df_wheat%>%
  select(Year,Site,`Seasonal irrigation (mm)`)%>%
  mutate(crop="wheat")


prcp_p1 <- df_wheat%>%
  select(Year,Site,`Total_Precipitation(mm)`)%>%
  rename(`Seasonal irrigation (mm)`=`Total_Precipitation(mm)`)%>%
  mutate(crop="Prcp")




df <- rbind(canola_p1,wheat_p1,prcp_p1)


df <- df%>%
  filter(Year > 2017)

# Reorder the 'crop' variable so that 'Prcp' comes first
df$crop <- factor(df$crop, levels = c("Prcp", "wheat", "canola"))

# Assuming `df` is the data frame you're using
ggplot(df, aes(x = factor(Year), y = `Seasonal irrigation (mm)`, fill = crop)) +  
  # Add error bars at min and max values
  stat_summary(fun.data = function(x) {
    return(data.frame(
      ymin = min(x),
      ymax = max(x)
    ))
  }, geom = "errorbar", width = 0.2, color = "black", position = position_dodge(0.5))+
  geom_boxplot(width = 0.3, position = position_dodge(0.5), color = "black",outlier.size = 0) +  # Box plot with dodge and color
  scale_fill_manual(values = c("wheat" = "firebrick3", "canola" = "springgreen4", "Prcp" = "blue"),
                    labels = c("Precipitation","Irrigation:Wheat", "Irrigation:Canola")) +  # Custom colors for each crop
  labs(
    title = "",
    x = "Year",
    y = "Seasonal Irrigation (mm)",
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
  scale_y_continuous(name = "Precipitation / Irrigation (mm)",
                     limits = c(100, 750), breaks = seq(0, 800, by = 50)) + # Customize y-axis scale
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


#### PPT 

df1 <- df %>%
  mutate(crop1 = case_when(
    crop == "Prcp" ~ "Precipitation",
    crop == "wheat" ~ "Irrigation:Wheat",
    crop == "canola" ~ "Irrigation:Canola"
  ))


df1$crop1 <- factor(df1$crop1, 
                    levels = c("Precipitation", "Irrigation:Canola", "Irrigation:Wheat"))


ggplot(df1, aes(x = crop1, y = `Seasonal irrigation (mm)`, fill = crop1)) +
  stat_summary(fun.min = min, fun.max = max, 
               geom = "errorbar", width = 0.05, color = "black") +
  geom_boxplot(width = 0.2, lwd = 0.3, outliers = F) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "Precipitation/Seasonal Irrigation (mm)") +
  scale_fill_manual(values = c("Precipitation" = "blue", "Irrigation:Wheat" = "firebrick3", "Irrigation:Canola" = "springgreen4"))+
  scale_y_continuous(breaks = seq(0, 750, by = 50)) +
  theme(panel.grid = element_blank(),  # Removes all grid lines
        axis.line = element_line(color = "black"),  # Keeps x and y axis lines
        legend.position = "none",
        panel.border = element_blank())  # Removes outer border








#### Plor 2 plot 2 plot 2 plot 2 plot 2##############


canola_p2 <- canola%>%
  select(year,Site,val_mm)%>%
  mutate(crop="canola")

wheat_p2 <- wheat%>%
  select(year,Site,val_mm)%>%
  mutate(crop="wheat")

w_can <- canola_weighted%>%
  select(year,Site,w_canol_val)

w_whe <- wheat_weighted%>%
  select(year,Site,w_wheat_val)

w_all_p2 <- w_can%>%
  left_join(w_whe)%>%
  mutate(val_mm = w_canol_val+w_wheat_val)%>%
  mutate(crop="weighted")%>%
  select(year,Site,val_mm,crop)
  

df <- rbind(canola_p2,wheat_p2,w_all_p2)




df <- df%>%
  filter(year > 2017)

# Reorder the 'crop' variable so that 'Prcp' comes first
df$crop <- factor(df$crop, levels = c("wheat", "canola","weighted"))

ggplot(df, aes(x = factor(year), y = val_mm, fill = crop)) +  
  # Add error bars at min and max values
  stat_summary(fun.data = function(x) {
    return(data.frame(
      ymin = min(x),
      ymax = max(x)
    ))
  }, geom = "errorbar", width = 0.2, color = "black", position = position_dodge(0.5))+
  geom_boxplot(width = 0.3, position = position_dodge(0.5), color = "black",outlier.size = 0) +  # Box plot with dodge and color
  scale_fill_manual(values = c("wheat" = "firebrick3", "canola" = "springgreen4", "weighted" = "slateblue2"),
                    labels = c("Wheat", "Canola", "Weighted")) +  # Custom colors for each crop
  labs(
    title = "",
    x = "Year",
    y = "Seasonal Irrigation (mm)",
    fill = "Crop Type"
  ) +  
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "right"
  )+
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) + # Adjust gap between x-axis labels (year)
  theme_minimal()+
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black") )+
  scale_y_continuous(name = expression("Average Value ($m"^{-3}*")"),
                     limits = c(0.1, 0.55), breaks = seq(0,0.6 , by = 0.05)) + # Customize y-axis scale
  guides(fill = guide_legend(title = NULL)) +  # Remove legend title
  theme(
    legend.position = "bottom",  # Move the legend to the bottom
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12),  # Adjust x-axis title size
    axis.title.y = element_text(size = 12),   # Adjust y-axis title size
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.ticks = element_line(size = 0.8) 
  )

########## This made for presentation - seminar
df_plot_average <- df%>%
  group_by(Site,crop)%>%
  mutate(ave_val = mean(val_mm))%>%
  ungroup()%>%
  distinct(Site,crop, .keep_all = T)


ggplot(df_plot_average, aes(x = factor(crop), y = ave_val, fill = crop)) +  
  # Add error bars at min and max values
  stat_summary(fun.data = function(x) {
    return(data.frame(
      ymin = min(x),
      ymax = max(x)
    ))
  }, geom = "errorbar", width = 0.2, color = "black", position = position_dodge(0.5))+
  geom_boxplot(
    width = 0.3,
    position = position_dodge(0.5),
    color = "black",
    outlier.shape = NA  # Suppress outlier dots
  ) +
    scale_fill_manual(values = c("wheat" = "firebrick3", "canola" = "springgreen4", "weighted" = "slateblue2"),
                    labels = c("Wheat", "Canola", "Weighted")) +  # Custom colors for each crop
  labs(
    title = "",
    x = "",
    y = "",
    fill = ""
  ) +  
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "right"
  )+
  #scale_x_discrete(expand = expansion(mult = c(0.08, 0.08))) + # Adjust gap between x-axis labels (year)
  theme_minimal()+
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black") )+
  scale_y_continuous(name = expression("Average Value ($m"^{-3}*")"),
                     limits = c(0.2, 0.45), breaks = seq(0,0.6 , by = 0.03)) + # Customize y-axis scale
  guides(fill = guide_legend(title = NULL)) +  # Remove legend title
  theme(
    legend.position = "none",  # Move the legend to the bottom
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12),  # Adjust x-axis title size
    axis.title.y = element_text(size = 12),   # Adjust y-axis title size
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.ticks = element_line(size = 0.8) 
  )






# Create the box plot and violin plot with jitter points behind
ggplot(df1, aes(x = factor(year), y = ave_val)) +
  #geom_point(position = position_jitter(width = 0.1), 
             #alpha = 0.6, size = 1, shape = 1, show.legend = FALSE) +  # Jittered points layer first
  geom_violin(alpha = 0.5, position = position_dodge(0.9), fill = "white") +  # Add violin plot
  geom_boxplot(width = 0.2, position = position_dodge(0.9), color = "black", fill = NA) +  # Add box plot
  labs(title = "",
       x = "Year",
       y = expression("Average Value ($/m"^3*")")) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") + # Optional: match point color to fill
  scale_y_continuous(limits = c(0.09, 0.45), breaks = seq(0, 0.45, by = 0.05)) + # Customize y-axis scale
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black") )


###############################

# Table 


t <- df%>%
  group_by(year,crop)%>%
  mutate(ave_val = mean(val_mm),
         ave_sd = sd(val_mm))%>%
  distinct(year, crop, .keep_all = T)%>%
  ungroup()

year <- t%>%
  distinct(year)

list1  <- as.list(year)

canola <- t%>%
  filter(crop == "canola")%>%
  select(year,ave_val,ave_sd)%>%
  rename(ave_val_can = ave_val,
         ave_sd_can =ave_sd)

wheat <- t%>%
  filter(crop == "wheat")%>%
  select(year,ave_val,ave_sd)%>%
  rename(ave_val_whe = ave_val,
         ave_sd_whe =ave_sd)


weigthed <- t%>%
  filter(crop == "weighted")%>%
  select(year,ave_val,ave_sd)%>%
  rename(ave_val_wheight = ave_val,
         ave_sd_wheight =ave_sd)


df <- year%>%
  left_join(canola)%>%
  left_join(wheat)%>%
  left_join(weigthed)

df <- data.frame(lapply(df, function(x) {
  if (is.numeric(x)) round(x, 2) else x
}))


library(kableExtra)

# Create a LaTeX table with customization
# Generate LaTeX table without striped rows
latex_table <- kable(df, format = "latex", booktabs = TRUE, caption = "Sample Table") %>%
  kable_styling(latex_options = c("hold_position"))  # No "striped"


file_conn <- file("./results/Tables/ave_historical.tex")
writeLines(latex_table, file_conn)
close(file_conn)



#### Map Map
weather_data_final <- read.csv("./AquaCropOPSyData/ClimateData/combined_daymet_weather_data.csv")

# Create Climate file to feed AquaCropOPSY

weather_data_Aqua <- weather_data_final%>%
  select(site,year,yday,Precipitation,MaxTemp,MinTemp,daylength,latitude,longitude)%>%
  mutate(Date = as.Date(yday - 1, origin = paste0(year, "-01-01")))


# Separate Year, Month, and Day from the Date column
weather_data_Aqua <- weather_data_Aqua %>%
  mutate(Year = format(Date, "%Y"),
         Month = format(Date, "%m"),
         Day = format(Date, "%d"))%>%
  select(site,longitude,latitude)%>%
  distinct(site,.keep_all = T)



library(sf)

# Assuming your data frame is named df with columns 'longitude' and 'latitude'
# Create an sf object
df_sf <- st_as_sf(weather_data_Aqua, coords = c("longitude", "latitude"), crs = 4326)  # Use the appropriate CRS


df_map_grids <- st_read("./maps/LakeDiefenbaker/MergeLakeDiefenbakerfishnet5Km.shp")

# Ensure both are in the same CRS
df_map_grids <- st_transform(df_map_grids, st_crs(df_sf))



df_map_grids<- df_map_grids %>%
  st_join(df_sf, join = st_intersects)

df_map <- df_map_grids%>%
  rename(Site=site)%>%
  left_join(df1)


df_map <- st_transform(df_map, crs = 4326)



df_2015 <- df_map%>%
  filter(year==2015)

df_2016 <- df_map%>%
  filter(year==2016)

df_2017 <- df_map%>%
  filter(year==2017)

df_2018 <- df_map%>%
  filter(year==2018)

df_2019 <- df_map%>%
  filter(year==2019)

df_2020 <- df_map%>%
  filter(year==2020)

df_2021 <- df_map%>%
  filter(year==2021)

df_2022 <- df_map%>%
  filter(year==2022)

df_2023 <- df_map%>%
  filter(year==2023)


p1 <- ggplot(data = df_2015) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(option = "D", 
                     name = expression("Average Value ($/m"^3*")"),
                     breaks = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45),  # Define custom breaks
                     limits = c(0.05, 0.45),
                     labels = scales::number_format(accuracy = 0.2)) +  # Format labels
  labs(title = "2015") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 12))



p1 <- ggplot(data = df_2015) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Average Value ($/m"^3*")"),
                     breaks = breaks_pretty(n = 5),  # Automatically generate breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2015") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 12))


p2 <- ggplot(data = df_2016) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Average Value ($/m"^3*")"),
                     breaks = breaks_pretty(n = 5),  # Automatically generate breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2016") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 12))


p3 <- ggplot(data = df_2017) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Average Value ($/m"^3*")"),
                     breaks = breaks_pretty(n = 5),  # Automatically generate breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2017") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 12))




# Define manual breaks
manual_breaks <- c(0.27, 0.28, 0.29, 0.30, 0.31, 0.32)  # Replace with appropriate values for your data

p4 <- ggplot(data = df_2018) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(
    option = "D", 
    name = expression("Average Value ($/m"^3*")"),
    breaks = manual_breaks,  # Manually set breaks
    labels = scales::number_format(accuracy = 0.01)  # Format labels
  ) +
  labs(title = "2018") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 9)
  )



# Define manual breaks
manual_breaks <- c(0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.30, 0.31)  # Replace with appropriate values for your data


p5 <- ggplot(data = df_2019) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Average Value ($/m"^3*")"),
                     breaks = manual_breaks,  # Manually set breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2019") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 9))



# Define manual breaks
manual_breaks <- c(0.24, 0.25, 0.26, 0.27, 0.28, 0.29)  # Replace with appropriate values for your data



p6 <- ggplot(data = df_2020) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Average Value ($/m"^3*")"),
                     breaks = manual_breaks,  # Manually set breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2020") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 9))


# Define manual breaks
manual_breaks <- c(0.23, 0.24, 0.25, 0.26, 0.27)  # Replace with appropriate values for your data



p7 <- ggplot(data = df_2021) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Average Value ($/m"^3*")"),
                     breaks = manual_breaks,  # Manually set breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2021") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 9))



# Define manual breaks
manual_breaks <- c(0.34, 0.35, 0.36, 0.37, 0.38, 0.39, 0.40)  # Replace with appropriate values for your data


p8 <- ggplot(data = df_2022) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Average Value ($/m"^3*")"),
                     breaks = manual_breaks,  # Manually set breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2022") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 9))

# Define manual breaks
manual_breaks <- c(0.34, 0.35, 0.36, 0.37, 0.38, 0.39, 0.40, 0.41,0.42)  # Replace with appropriate values for your data


p9 <- ggplot(data = df_2023) +
  geom_sf(aes(fill = ave_val), color = NA) +   
  scale_fill_viridis(option = "D", name =  expression("Average Value ($/m"^3*")"),
                     breaks = manual_breaks,  # Manually set breaks
                     labels = scales::number_format(accuracy = 0.01)) +  # Format labels
  labs(title = "2023") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold",size = 9))



library(patchwork)

# Assuming you have a list of plots, for example:
plots <- list(p4, p5, p6, p7, p8, p9)  # Add your ggplot objects here


# Combine the plots in a 3 by 3 grid
combined_plot <- wrap_plots(plots, ncol = 2)

# Display the combined plot
print(combined_plot)


# Single Legend

# Define ave_val_range
ave_val_range <- range(df_map$ave_val, na.rm = TRUE)

# Load the MetBrewer package
library(MetBrewer)

# Create the custom color palette using "VanGogh2" from MetBrewer
mycolors <- met.brewer(
  "Degas",
  10,
  type = "continuous"
)

# Function to create ggplot for each year with the custom color palette
create_plot <- function(data, year) {
  ggplot(data) +
    geom_sf(aes(fill = ave_val), color = NA) +   
    scale_fill_gradientn(
      colors = mycolors,  # Use the custom color palette
      name = expression("Average Value ($/m"^3*")"),
      breaks = seq(ave_val_range[1], ave_val_range[2], length.out = 6),  # Adjust breaks
      limits = ave_val_range,
      labels = scales::number_format(accuracy = 0.01)
    ) +
    labs(title = year) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", size = 10)
    )
}

# List of data frames by year
df_list <- list(
  "2018" = df_map %>% filter(year == 2018),
  "2019" = df_map %>% filter(year == 2019),
  "2020" = df_map %>% filter(year == 2020),
  "2021" = df_map %>% filter(year == 2021),
  "2022" = df_map %>% filter(year == 2022),
  "2023" = df_map %>% filter(year == 2023)
)

# Generate the plots
plots <- lapply(names(df_list), function(year) {
  create_plot(df_list[[year]], year)
})

# Combine plots with a shared legend using patchwork
combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Display the combined plot
combined_plot

#Veronese



# Set the legend position to the right of the combined plot
combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "right")

# Display the combined plot with the legend on the side
combined_plot



###################




# Define the color palette and breaks manually
manual_colors <- c("#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")  # Example palette
manual_breaks <- c(0.1, 0.3, 0.5, 0.7, 0.9)  # Replace with appropriate values for your data

# Updated function to create ggplot with manual colors
create_plot <- function(data, year) {
  ggplot(data) +
    geom_sf(aes(fill = ave_val), color = NA) +   
    scale_fill_gradientn(
      colors = manual_colors,
      name = expression("Average Value ($/m"^3*")"),
      breaks = manual_breaks,
      limits = ave_val_range,
      labels = scales::number_format(accuracy = 0.01)
    ) +
    labs(title = year) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", size = 10)
    )
}

# Generate the plots
plots <- lapply(names(df_list), function(year) {
  create_plot(df_list[[year]], year)
})

# Combine plots with a shared legend using patchwork
combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Display the combined plot
combined_plot







