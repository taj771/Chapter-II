#clear memory
rm(list = ls())

climate_data <- read_csv('./AquaCropOPSyData/ClimateData/weather_data_Aqua.csv')

wheat_2018 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2018.csv')%>%
  mutate(Year = 2018)%>%
  select(Site,Year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`, `Total_Precipitation(mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "wheat")

wheat_2019 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2019.csv')%>%
  mutate(Year = 2019)%>%
  select(Site,Year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`, `Total_Precipitation(mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "wheat")

wheat_2020 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2020.csv')%>%
  mutate(Year = 2020)%>%
  select(Site,Year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`, `Total_Precipitation(mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "wheat")

wheat_2021 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2021.csv')%>%
  mutate(Year = 2021)%>%
  select(Site,Year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`, `Total_Precipitation(mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "wheat")

wheat_2022 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2022.csv')%>%
  mutate(Year = 2022)%>%
  select(Site,Year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`, `Total_Precipitation(mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "wheat")

wheat_2023 <- read_csv('./AquaCropOPSyData/WheatNetIrriDeamad/wheat_netirridemand_2023.csv')%>%
  mutate(Year = 2023)%>%
  select(Site,Year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`, `Total_Precipitation(mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "wheat")


canola_2018 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2018.csv')%>%
  mutate(Year = 2018)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "canola")

canola_2019 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2019.csv')%>%
  mutate(Year = 2019)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "canola")

canola_2020 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2020.csv')%>%
  mutate(Year = 2020)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "canola")

canola_2021 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2021.csv')%>%
  mutate(Year = 2021)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "canola")

canola_2022 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2022.csv')%>%
  mutate(Year = 2022)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "canola")

canola_2023 <- read_csv('./AquaCropOPSyData/canolaNetIrriDeamad/canola_netirridemand_2023.csv')%>%
  mutate(Year = 2023)%>%
  select(Site,Year,,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "canola")


potato_2018 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2018.csv')%>%
  mutate(Year = 2018)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "potato")

potato_2019 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2019.csv')%>%
  mutate(Year = 2019)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "potato")

potato_2020 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2020.csv')%>%
  mutate(Year = 2020)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "potato")

potato_2021 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2021.csv')%>%
  mutate(Year = 2021)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "potato")

potato_2022 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2022.csv')%>%
  mutate(Year = 2022)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "potato")

potato_2023 <- read_csv('AquaCropOPSyData/PotatoNetIrriDemand/potato_netirridemand_2023.csv')%>%
  mutate(Year = 2023)%>%
  select(Site,Year,`Seasonal irrigation (mm)`)%>%
  pivot_longer(cols = c(`Seasonal irrigation (mm)`), names_to = "variable", values_to = "value")%>%
  mutate(crop = "potato")



df <- rbind(wheat_2018,wheat_2019,wheat_2020,wheat_2021,wheat_2022,wheat_2023,
            canola_2018,canola_2019,canola_2020,canola_2021,canola_2022,canola_2023,
            potato_2018,potato_2019,potato_2020,potato_2021,potato_2022,potato_2023)

df$variable <- factor(df$variable, levels = c("Total_Precipitation(mm)", "Seasonal irrigation (mm)"))
df$crop <- factor(df$crop, levels = c("None", "wheat", "canola","potato"))


p <- ggplot(df, aes(x = factor(Year), y = value,  fill = interaction(variable, crop))) +
  geom_boxplot(notch = TRUE, width = 0.4, position = position_dodge(0.5),outlier.size = 0.4) +
  labs(x = "Year", y = "Value (mm)", title = "Irrigation & Precipitation for Wheat & Canola by Year") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "springgreen4", "firebrick3","goldenrod1"),
                    labels = c("Precipitation","Irrigation:Wheat", "Irrigation:Canola", "Irrigation:Potato")) +  # Custom colors for each crop) +
  theme(legend.position = "top") +
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
                     limits = c(0, 350), breaks = seq(0, 800, by = 50)) + # Customize y-axis scale
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

ggsave("./results/images/PrcpIrriusecropwise.png", plot = p, width = 10, height = 7, dpi = 300)
