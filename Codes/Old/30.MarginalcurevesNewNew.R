#clear memory
rm(list = ls())

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fixest)
library(priceR)

####### Wheat ####### Wheat ####### Wheat ####### Wheat ####### Wheat
# Set the directory where your CSV files are located
file_path <- "./AquaCropOPSyData/WheatMarginal"

# Get a list of all files that match the pattern
file_list <- list.files(path = file_path, pattern = "merged_simulation_results_wheat_marginal_\\d{4}\\.csv", full.names = TRUE)

# Load each file and add a "year" column
wheat_marginal <- map_dfr(file_list, ~ {
  year <- gsub(".*_(\\d{4})\\.csv$", "\\1", .x)  # Extract the year from the file name
  read_csv(.x) %>% 
    mutate(year = as.numeric(year))              # Add the year column as a numeric type
})%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*14.86995818)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))


wheat_marginal <- wheat_marginal%>%
  filter(year %in% c(2018,2019,2020,2021,2022,2023))



df_wheat_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_wheat_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  filter(year %in% c(2016, 2017,2018,2019,2020,2021,2022,2023))%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*14.86995818)%>%
  select(year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)


wheat_marginal <- wheat_marginal%>%
  left_join(df_wheat_rf, by = c("year", "Site"))

# crop retun
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

wheat <- wheat_marginal%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)
#select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)


########### Canola ########### Canola ########### Canola ########### Canola 

# Set the directory where your CSV files are located
file_path <- "./AquaCropOPSyData/CanolaMarginal"

# Get a list of all files that match the pattern
file_list <- list.files(path = file_path, pattern = "merged_simulation_results_canola_marginal_\\d{4}\\.csv", full.names = TRUE)

# Load each file and add a "year" column
canola_marginal <- map_dfr(file_list, ~ {
  year <- gsub(".*_(\\d{4})\\.csv$", "\\1", .x)  # Extract the year from the file name
  read_csv(.x) %>% 
    mutate(year = as.numeric(year))              # Add the year column as a numeric type
})%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (bu/ac)`= `Yield_tonne_per_ha`*14.86995818)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))


df_canola_rf <- read_csv("./AquaCropOPSyData/CropSimulateData/merged_simulation_results_canola_dry.csv")%>%
  mutate(
    Day = day(`Harvest Date (YYYY/MM/DD)`),
    Month = month(`Harvest Date (YYYY/MM/DD)`),
    year = year(`Harvest Date (YYYY/MM/DD)`)
  )%>%
  filter(year %in% c(2016, 2017,2018,2019,2020,2021,2022,2023))%>%
  mutate(`Dry yield (bu/ac)`= `Dry yield (tonne/ha)`*14.86995818)%>%
  select(year,`Dry yield (bu/ac)`,Site,`Total_Precipitation(mm)`)%>%
  rename(`Dry yield rain (bu/ac)`=`Dry yield (bu/ac)`)


canola_marginal <- canola_marginal%>%
  left_join(df_canola_rf, by = c("year", "Site"))

canola <- canola_marginal%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)
#select(year, Site,`Total_Precipitation(mm)`,`Seasonal irrigation (mm)`,val_mm)

# set subset -wheat of data to use in loops 
wheat2016 <- wheat%>%
  filter(year==2016)
wheat2017 <- wheat%>%
  filter(year==2017)
wheat2018 <- wheat%>%
  filter(year==2018)
wheat2019 <- wheat%>%
  filter(year==2019)
wheat2020 <- wheat%>%
  filter(year==2020)
wheat2021 <- wheat%>%
  filter(year==2021)
wheat2022 <- wheat%>%
  filter(year==2022)
wheat2023 <- wheat%>%
  filter(year==2023)


# set subset -wheat of data to use in loops 
canola2016 <- canola%>%
  filter(year==2016)
canola2017 <- canola%>%
  filter(year==2017)
canola2018 <- canola%>%
  filter(year==2018)
canola2019 <- canola%>%
  filter(year==2019)
canola2020 <- canola%>%
  filter(year==2020)
canola2021 <- canola%>%
  filter(year==2021)
canola2022 <- canola%>%
  filter(year==2022)
canola2023 <- canola%>%
  filter(year==2023)



year <- c("2018", "2019", "2020","2021","2022","2023")
ar_wheat <- c(24.4,31.2,31.7,34.2,33.7,35.4)
ar_canola <- c(30.0,22.1,28.9,32,31.8,27.8)

# Create dataframe
weights <- data.frame(year = year, canola = ar_canola, wheat = ar_wheat)%>%
  mutate(area=wheat+canola)%>%
  mutate(canola_w=canola/area)%>%
  mutate(wheat_w=wheat/area)

wheat_weight <- weights%>%
  select(year,wheat_w)

wheat_weight$year <- as.numeric(wheat_weight$year)

canola_weight <- weights%>%
  select(year,canola_w)

canola_weight$year <- as.numeric(canola_weight$year)

#### 2018 #### 2018 #### 2018 #### 2018 #### 2018 #### 2018 #### 2018 #### 2018
wheat2018 <-wheat2018 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)%>%
  left_join(wheat_weight)%>%
  mutate(wmv_wheat =wheat_w*mv_wheat)


canola2018 <-canola2018 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)%>%
  left_join(canola_weight)%>%
  mutate(wmv_canola =canola_w*mv_canola)%>%
  select(-year)

df_2018 <- wheat2018%>%
  left_join(canola2018, by = c("Site", "Max_Irrigation_mm"))%>%
  mutate(mv_tot = wmv_canola+wmv_wheat)%>%
  select(year, Site,Max_Irrigation_mm,mv_tot )



ggplot(df_2018, aes(x = Max_Irrigation_mm, y = mv_tot)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),color = "blue", fill = "lightblue") + 
  #geom_point(color = "red", size = 1) +   
  labs(title = "Trend Over Time",
       x = "",
       y = "") +
  theme_minimal()



# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_tot ~ poly(Max_Irrigation_mm, 2), data = df_2018)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_df <- data.frame(
  Max_Irrigation_mm = seq(100, 500, by = 20)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_df$mv_pred <- predict(poly_fit, newdata = predicted_df)

# Plot with manually computed polynomial regression line
ggplot(df_2018, aes(x = Max_Irrigation_mm, y = mv_tot)) +
  geom_line(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
            color = "blue", linewidth = 0.5, linetype = "dashed") +  # Use a manually fitted line instead of geom_smooth
  geom_point(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
             color = "red", size = 1) +  # Add predicted points
  labs(title = "2018",
       x = "Irrigation Level (mm)",
       y = expression("Marginal Value ($ m"^-3*")")) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 0.06, by = 0.01),
    limits = c(0.0, 0.06)
  ) +
  scale_x_continuous(
    breaks = seq(120, 500, by = 20),
    
    limits = c(120, 500)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines  # Flip x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.line = element_line(color = "black"))  # Keep main x and y axis lines


#### 2019 #### 2019 #### 2019 #### 2019 #### 2019 #### 2019 #### 2019 #### 2019
wheat2019 <-wheat2019 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)%>%
  left_join(wheat_weight)%>%
  mutate(wmv_wheat =wheat_w*mv_wheat)


canola2019 <-canola2019 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)%>%
  left_join(canola_weight)%>%
  mutate(wmv_canola =canola_w*mv_canola)%>%
  select(-year)

df_2019 <- wheat2019%>%
  left_join(canola2019, by = c("Site", "Max_Irrigation_mm"))%>%
  mutate(mv_tot = wmv_canola+wmv_wheat)%>%
  select(year, Site,Max_Irrigation_mm,mv_tot )


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_tot ~ poly(Max_Irrigation_mm, 2), data = df_2019)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_df <- data.frame(
  Max_Irrigation_mm = seq(100, 500, by = 20)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_df$mv_pred <- predict(poly_fit, newdata = predicted_df)

# Plot with manually computed polynomial regression line
ggplot(df_2018, aes(x = Max_Irrigation_mm, y = mv_tot)) +
  geom_line(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
            color = "blue", linewidth = 0.5, linetype = "dashed") +  # Use a manually fitted line instead of geom_smooth
  geom_point(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
             color = "red", size = 1) +  # Add predicted points
  labs(title = "2019",
       x = "Irrigation Level (mm)",
       y = expression("Marginal Value ($ m"^-3*")")) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 0.06, by = 0.01),
    limits = c(0.0, 0.06)
  ) +
  scale_x_continuous(
    breaks = seq(120, 500, by = 20),
    
    limits = c(120, 500)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines  # Flip x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.line = element_line(color = "black"))  # Keep main x and y axis lines







#### 2020 #### 2020 #### 2020 #### 2020 #### 2020 #### 2020 #### 2020 #### 2020
wheat2020 <-wheat2020 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)%>%
  left_join(wheat_weight)%>%
  mutate(wmv_wheat =wheat_w*mv_wheat)


canola2020 <-canola2020 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)%>%
  left_join(canola_weight)%>%
  mutate(wmv_canola =canola_w*mv_canola)%>%
  select(-year)

df_2020 <- wheat2020%>%
  left_join(canola2020, by = c("Site", "Max_Irrigation_mm"))%>%
  mutate(mv_tot = wmv_canola+wmv_wheat)%>%
  select(year, Site,Max_Irrigation_mm,mv_tot )



# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_tot ~ poly(Max_Irrigation_mm, 2), data = df_2020)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_df <- data.frame(
  Max_Irrigation_mm = seq(100, 500, by = 20)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_df$mv_pred <- predict(poly_fit, newdata = predicted_df)

# Plot with manually computed polynomial regression line
ggplot(df_2020, aes(x = Max_Irrigation_mm, y = mv_tot)) +
  geom_line(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
            color = "blue", linewidth = 0.5, linetype = "dashed") +  # Use a manually fitted line instead of geom_smooth
  geom_point(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
             color = "red", size = 1) +  # Add predicted points
  labs(title = "2020",
       x = "Irrigation Level (mm)",
       y = expression("Marginal Value ($ m"^-3*")")) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 0.09, by = 0.01),
    limits = c(0.0, 0.09)
  ) +
  scale_x_continuous(
    breaks = seq(120, 500, by = 20),
    
    limits = c(120, 500)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines  # Flip x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.line = element_line(color = "black"))  # Keep main x and y axis lines





#### 2021 #### 2021 #### 2021 #### 2021 #### 2021 #### 2021 #### 2021 #### 2021
wheat2021 <-wheat2021 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)%>%
  left_join(wheat_weight)%>%
  mutate(wmv_wheat =wheat_w*mv_wheat)


canola2021 <-canola2021 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)%>%
  left_join(canola_weight)%>%
  mutate(wmv_canola =canola_w*mv_canola)%>%
  select(-year)

df_2021 <- wheat2021%>%
  left_join(canola2021, by = c("Site", "Max_Irrigation_mm"))%>%
  mutate(mv_tot = wmv_canola+wmv_wheat)%>%
  select(year, Site,Max_Irrigation_mm,mv_tot )



# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_tot ~ poly(Max_Irrigation_mm, 2), data = df_2021)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_df <- data.frame(
  Max_Irrigation_mm = seq(100, 500, by = 20)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_df$mv_pred <- predict(poly_fit, newdata = predicted_df)

# Plot with manually computed polynomial regression line
ggplot(df_2021, aes(x = Max_Irrigation_mm, y = mv_tot)) +
  geom_line(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
            color = "blue", linewidth = 0.5, linetype = "dashed") +  # Use a manually fitted line instead of geom_smooth
  geom_point(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
             color = "red", size = 1) +  # Add predicted points
  labs(title = "2021",
       x = "Irrigation Level (mm)",
       y = expression("Marginal Value ($ m"^-3*")")) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 0.1, by = 0.01),
    limits = c(0.0, 0.1)
  ) +
  scale_x_continuous(
    breaks = seq(120, 500, by = 20),
    
    limits = c(120, 500)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines  # Flip x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.line = element_line(color = "black"))  # Keep main x and y axis lines




#### 2022 #### 2022 #### 2022 #### 2022 #### 2022 #### 2022 #### 2022 #### 2022
wheat2022 <-wheat2022 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)%>%
  left_join(wheat_weight)%>%
  mutate(wmv_wheat =wheat_w*mv_wheat)


canola2022 <-canola2022 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)%>%
  left_join(canola_weight)%>%
  mutate(wmv_canola =canola_w*mv_canola)%>%
  select(-year)

df_2022 <- wheat2022%>%
  left_join(canola2022, by = c("Site", "Max_Irrigation_mm"))%>%
  mutate(mv_tot = wmv_canola+wmv_wheat)%>%
  select(year, Site,Max_Irrigation_mm,mv_tot )


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_tot ~ poly(Max_Irrigation_mm, 2), data = df_2022)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_df <- data.frame(
  Max_Irrigation_mm = seq(100, 500, by = 20)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_df$mv_pred <- predict(poly_fit, newdata = predicted_df)

# Plot with manually computed polynomial regression line
ggplot(df_2022, aes(x = Max_Irrigation_mm, y = mv_tot)) +
  geom_line(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
            color = "blue", linewidth = 0.5, linetype = "dashed") +  # Use a manually fitted line instead of geom_smooth
  geom_point(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
             color = "red", size = 1) +  # Add predicted points
  labs(title = "2022",
       x = "Irrigation Level (mm)",
       y = expression("Marginal Value ($ m"^-3*")")) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 0.14, by = 0.01),
    limits = c(0.0, 0.14)
  ) +
  scale_x_continuous(
    breaks = seq(120, 500, by = 20),
    
    limits = c(120, 500)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines  # Flip x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.line = element_line(color = "black"))  # Keep main x and y axis lines




#### 2023 #### 2023 #### 2023 #### 2023 #### 2023 #### 2023 #### 2023 #### 2023
wheat2023 <-wheat2023 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)%>%
  left_join(wheat_weight)%>%
  mutate(wmv_wheat =wheat_w*mv_wheat)


canola2023 <-canola2023 %>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)%>%
  left_join(canola_weight)%>%
  mutate(wmv_canola =canola_w*mv_canola)%>%
  select(-year)

df_2023 <- wheat2023%>%
  left_join(canola2023, by = c("Site", "Max_Irrigation_mm"))%>%
  mutate(mv_tot = wmv_canola+wmv_wheat)%>%
  select(year, Site,Max_Irrigation_mm,mv_tot )




# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_tot ~ poly(Max_Irrigation_mm, 2), data = df_2023)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_df <- data.frame(
  Max_Irrigation_mm = seq(100, 500, by = 20)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_df$mv_pred <- predict(poly_fit, newdata = predicted_df)

# Plot with manually computed polynomial regression line
ggplot(df_2023, aes(x = Max_Irrigation_mm, y = mv_tot)) +
  geom_line(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
            color = "blue", linewidth = 0.5, linetype = "dashed") +  # Use a manually fitted line instead of geom_smooth
  geom_point(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
             color = "red", size = 1) +  # Add predicted points
  labs(title = "2023",
       x = "Irrigation Level (mm)",
       y = expression("Marginal Value ($ m"^-3*")")) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 0.07, by = 0.01),
    limits = c(0.0, 0.07)
  ) +
  scale_x_continuous(
    breaks = seq(120, 500, by = 20),
    
    limits = c(120, 500)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines  # Flip x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.line = element_line(color = "black"))  # Keep main x and y axis lines




df <- rbind(df_2018,df_2019,df_2020,df_2021, df_2022, df_2023)




# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_tot ~ poly(Max_Irrigation_mm, 2), data = df)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_df <- data.frame(
  Max_Irrigation_mm = seq(100, 500, by = 20)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_df$mv_pred <- predict(poly_fit, newdata = predicted_df)

# Plot with manually computed polynomial regression line
ggplot(df, aes(x = Max_Irrigation_mm, y = mv_tot)) +
  geom_line(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
            color = "blue", linewidth = 0.5, linetype = "dashed") +  # Use a manually fitted line instead of geom_smooth
  geom_point(data = predicted_df, aes(x = Max_Irrigation_mm, y = mv_pred), 
             color = "red", size = 1) +  # Add predicted points
  labs(title = "",
       x = "Irrigation Level (mm)",
       y = expression("Marginal Value ($ m"^-3*")")) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 0.09, by = 0.01),
    limits = c(0.0, 0.09)
  ) +
  scale_x_continuous(
    breaks = seq(120, 500, by = 20),
    
    limits = c(120, 500)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines  # Flip x-axis labels
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.line = element_line(color = "black"))  # Keep main x and y axis lines




