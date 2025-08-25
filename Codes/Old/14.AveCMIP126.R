#clear memory
rm(list = ls())

library(dplyr)
library(tidyverse)

# wheat
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/WheatCMIP126"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_wheat_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_wheat_iri <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal.irrigation..mm.`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal.irrigation..mm.`*0.001))



# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/WheatCMIP126"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_dry_wheat_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_wheat_rf <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*14.86995818)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation.mm.`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_wheat <- df_wheat_rf%>%
  left_join(df_wheat_iri)

# Canola
# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/CanolaCMIP126"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_canola_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_canola_iri <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,`Seasonal.irrigation..mm.`,Site)%>%
  rename(`Dry yield irri (bu/ac)`=`Dry yield (bu/ac)`)%>%
  mutate(irrq_m3 = 4046.86*(`Seasonal.irrigation..mm.`*0.001))



# Set the directory path where the CSV files are located
folder_path <- "./AquaCropOPSyData/ClimateData/CMIP6/CMIP126/CanolaCMIP126"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "^merged_simulation_results_dry_canola_\\d{4}\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- lapply(csv_files, read.csv)

# Combine all data frames into one
df_canola_rf <- do.call(rbind, list_of_dfs)%>%
  mutate(
    Day = day(`Harvest.Date..YYYY.MM.DD.`),
    Month = month(`Harvest.Date..YYYY.MM.DD.`),
    Year = year(`Harvest.Date..YYYY.MM.DD.`)
  )%>%
  mutate(`Dry yield (bu/ac)`= `Dry.yield..tonne.ha.`*17.84394982)%>%
  select(Year,`Dry yield (bu/ac)`,Site,`Total_Precipitation.mm.`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)

df_canola <- df_canola_rf%>%
  left_join(df_canola_iri)



return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")

return_wheat <- return%>%
  filter(crop =="wheat")%>%
  mutate(dry_cost_ac=mean(dry_cost_ac),
         irri_cost_ac= mean(irri_cost_ac),
         price.bu = mean(price.bu))%>%
  select(year,dry_cost_ac, irri_cost_ac, price.bu)%>%
  mutate(year = ifelse(year == 2017, 2030, year),
         year = ifelse(year == 2018, 2040, year),
         year = ifelse(year == 2019, 2050, year),
         year = ifelse(year == 2020, 2060, year),
         year = ifelse(year == 2021, 2080, year),
         year = ifelse(year == 2022, 2100, year)
         )

return_canola <- return%>%
  filter(crop =="canola")%>%
  mutate(cost_ac=mean(dry_cost_ac),
         cost_ac= mean(irri_cost_ac),
         price.bu = mean(price.bu))%>%
  select(year,dry_cost_ac, irri_cost_ac, price.bu)%>%
  mutate(year = ifelse(year == 2017, 2030, year),
         year = ifelse(year == 2018, 2040, year),
         year = ifelse(year == 2019, 2050, year),
         year = ifelse(year == 2020, 2060, year),
         year = ifelse(year == 2021, 2080, year),
         year = ifelse(year == 2022, 2100, year)
  )


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
  select(year, Site,`Total_Precipitation.mm.`,`Seasonal.irrigation..mm.`,val_mm)



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
  select(year, Site,`Total_Precipitation.mm.`,`Seasonal.irrigation..mm.`,val_mm)



canola <- canola%>%
  group_by(year)%>%
  mutate(`Total_Precipitation(mm)`=mean(`Total_Precipitation.mm.`))%>%
  mutate(`Seasonal irrigation (mm)`=mean(`Seasonal.irrigation..mm.`))%>%
  mutate(val_mm=mean(val_mm))%>%
  distinct(year, .keep_all = T)%>%
  select(year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm, Site)%>%
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
  mutate(`Total_Precipitation(mm)`=mean(`Total_Precipitation.mm.`))%>%
  mutate(`Seasonal irrigation (mm)`=mean(`Seasonal.irrigation..mm.`))%>%
  mutate(val_mm=mean(val_mm))%>%
  distinct(year, .keep_all = T)%>%
  select(year,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm, Site)%>%
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

# crop mix weight 

wheat_weighted <- wheat%>%
  mutate(wheat_per = 35.4,
         tot_per = 63.2,
         weight = wheat_per/tot_per,
         w_wheat_val = economic_value*weight,
         w_wheat_irri = `irrigation`*weight)%>%
  select(year,Site,w_wheat_irri,w_wheat_val,percipitation)



canola_weighted <- canola%>%
  mutate(canola_per = 27.8,
         tot_per = 63.2,
         weight = canola_per/tot_per,
         w_canol_val = economic_value*weight,
         w_canol_irri = `irrigation`*weight)%>%
  select(year,Site,w_canol_irri,w_canol_val)

df <- wheat_weighted%>%
  left_join(canola_weighted)%>%
  mutate(ave_val =w_canol_val+w_wheat_val,
         ave_irri = w_wheat_irri+w_canol_irri)%>%
  select(year,Site,ave_val,ave_irri,percipitation)



df_long <- df %>%
  pivot_longer(cols = c("percipitation", "ave_irri"), names_to = "type", values_to = "amount")




# Create the box plot and violin plot with jitter points behind
ggplot(df_long, aes(x = factor(year), y = amount, fill = type)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_line(data = df_long, aes(x = factor(year), y = ave_val * 1000, group = 1, color = "ave_val"), size = 0.5) +
  geom_point(data = df_long, aes(x = factor(year), y = ave_val * 1000, color = "ave_val"), size = 3) +
  geom_text(data = df_long, aes(x = factor(year), y = ave_val * 1000, label = sprintf("$%.2f", ave_val)),
            color = "darkred", size = 4, vjust = -0.5) +
  scale_y_continuous(
    name = "Precipitation / Irrigation (mm)",
    breaks = seq(0, 900, by = 50),
    sec.axis = sec_axis(~ . / 1000, name = expression("Average Value ($/m"^3*")"),breaks = seq(0, 0.9, by = 0.05)),
    expand = c(0,0)
  ) +
  labs(title = "", x = "Year") +
  scale_fill_manual(name = "", values = c("percipitation" = "dodgerblue1", "ave_irri" = "skyblue"),
                    labels = c("percipitation" = "Precipitation", "ave_irri" = "Irrigation"),
                    guide = guide_legend(override.aes = list(shape = NA))) +
  scale_color_manual(name = "", values = "darkred", 
                     labels = c("ave_val" = "Average Value ($/m³)"), # Custom label for dark red line
                     guide = guide_legend(override.aes = list(linetype = "solid", size = 0.8, shape = NA))) +
  theme_minimal() +
  theme(legend.position = "top")+  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),                # Remove all gridlines
    axis.line = element_line(color = "black")    # Keep main x and y axis lines
  )
