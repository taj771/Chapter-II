#clear memory
rm(list = ls())

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fixest)
library(priceR)
library(pracma)
library(patchwork)
library(mgcv)
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

wheat_marginal <- wheat_marginal%>%
  left_join(return_wheat)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)


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

canola_marginal <- canola_marginal%>%
  left_join(return_canola)%>%
  mutate(return_rf = `Dry yield rain (bu/ac)`*price.bu)%>%
  mutate(profit_rf = return_rf - dry_cost_ac)%>%
  mutate(return_ir = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit_ir = return_ir - irri_cost_ac)%>%
  mutate(prof_dif = profit_ir - profit_rf )%>%
  mutate(val_mm = prof_dif/irrq_m3)


### potato ### potato ### potato ### potato ### potato ### potato ### potato
# Set the directory where your CSV files are located
file_path <- "./AquaCropOPSyData/PotatoMarginal"

# Get a list of all files that match the pattern
file_list <- list.files(path = file_path, pattern = "merged_simulation_results_potato_marginal_\\d{4}\\.csv", full.names = TRUE)

# Load each file and add a "year" column
potato_marginal <- map_dfr(file_list, ~ {
  year <- gsub(".*_(\\d{4})\\.csv$", "\\1", .x)  # Extract the year from the file name
  read_csv(.x) %>% 
    mutate(year = as.numeric(year))              # Add the year column as a numeric type
})%>%
  filter(`Total_Irrigation_mm`!=0)%>%
  mutate(`Dry yield irri (ton/ac)`= `Yield_tonne_per_ha`*0.4047)%>%
  rename(Site=Site_ID)%>%
  mutate(irrq_m3 = 4046.86*(`Total_Irrigation_mm`*0.001))

potato_marginal <- potato_marginal%>%
  filter(year %in% c(2018,2019,2020,2021,2022,2023))

# crop retun
return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv")


irri_cost <- return$irri_cost
price.ton <- return$price.ton
years <- return$year

return$irri_cost <- adjust_for_inflation(irri_cost, years, "CA", to_date = 2023)
return$price.ton <- adjust_for_inflation(price.ton, years, "CA", to_date = 2023)

potato_marginal <- potato_marginal%>%
  select(Site,year,Max_Irrigation_mm,`Dry yield irri (ton/ac)`,irrq_m3,Total_Irrigation_mm)

return_potato <- return%>%
  filter(crop =="potato")%>%
  select(crop,price.ton,year,irri_cost)

potato_marginal <- potato_marginal%>%
  left_join(return_potato)%>%
  mutate(revenue = `Dry yield irri (ton/ac)`*price.ton)%>%
  mutate(profit = revenue-irri_cost)%>%
  mutate(val_mm = profit/irrq_m3)

###############################################################################

wheat <- wheat_marginal%>%
  filter(year==2018)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  mutate(mar_prof_dif = prof_dif - lag(prof_dif))%>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,`Dry yield rain (bu/ac)`,`Dry yield irri (bu/ac)`,prof_dif,irrq_m3,val_mm,mv_wheat,mar_prof_dif)

wheat <- wheat%>%
  na.omit()

total_value_trapz <- trapz(wheat$irrq_m3, wheat$mv_wheat)



ggplot(wheat, aes(x = Max_Irrigation_mm, y = mv_wheat)) +
  #geom_point(color = "blue", size = 1) +  # Blue dots with size 3
  geom_smooth(method = "gam", formula = y ~ s(x), color = "yellow") + 
  geom_smooth(method = "loess", color = "red") +  # Linear model smoothing
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "green") +  # Polynomial of degree 2
  labs(x = "Irrigation (mm)", y = "Value ($)", title = "Irrigation vs. Value") +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 0.08, by = 0.02),
    limits = c(0,0.08)
  ) +
  theme_minimal()




#fit <- loess(mv_wheat ~ Max_Irrigation_mm, data = wheat, span = 0.75)  # Adjust 'span' for smoothness

#fit <- gam(mv_wheat ~ s(Max_Irrigation_mm, bs = "cr"), data = wheat)

fit <- lm(mv_wheat ~ poly(irrq_m3, 3), data = wheat)


# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(100, 500, by = 1)  # Generate sequence from 100 to 500 mm
)%>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  select(irrq_m3)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(fit, newdata = predicted_wheat)


predicted_wheat <- predicted_wheat %>%
  mutate(Max_Irrigation_mm = irrq_m3 / 4046.86 * 1000)

predicted_wheat <- predicted_wheat%>%
  na.omit()

total_value_trapz <- trapz(predicted_wheat$irrq_m3, predicted_wheat$mv_pred_wheat )




canola <- canola_marginal%>%
  filter(year==2018)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,irrq_m3,mv_canola)



#loess_fit <- loess(mv_canola ~ Max_Irrigation_mm, data = canola, span = 0.75)  # Adjust 'span' for smoothness

fit <- lm(mv_canola ~ poly(irrq_m3, 4), data = canola)


# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)%>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  select(irrq_m3)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(fit, newdata = predicted_canola)

predicted_canola <- predicted_canola %>%
  mutate(Max_Irrigation_mm = irrq_m3 / 4046.86 * 1000)



# wheat canola
wheat2018 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(irri_wheat,irri_canola,mv_pred_wheat)


canola2018 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)


df <- wheat2018%>%
  left_join(canola2018)%>%
  na.omit()



p1 <- ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    breaks = seq(0,600, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      transform = ~ max(df$irri_wheat) - (. - min(df$irri_wheat)),  # Reverse transformation
      name = "Irrigation for Canola (mm)",
      breaks = rev(seq(0,600, by = 100))  # Reverse breaks
    )
  ) +
  scale_y_continuous(
    breaks = seq(-0.3, 0.2, by = 0.02)
  )+
  theme_minimal()


# Find intersection points
crossing_points2018 <- df %>%
  mutate(diff = mv_pred_canola - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2018)

# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)%>%
  na.omit()

df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)%>%
  na.omit()

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))



library(pracma)

# Compute total economic value
total_value_trapz <- trapz(df_wheat$irrq_m3, df_wheat$mv_pred_wheat)

print(total_value_trapz)



df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(-irrq_m3)


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_canola)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_canola)

################################################################################


wheat <- wheat_marginal%>%
  filter(year==2018)%>%
  select(year,Site, Max_Irrigation_mm,irrq_m3,prof_dif)


#loess_fit <- loess(prof_dif ~ Max_Irrigation_mm, data = wheat, span = 0.75)  # Adjust 'span' for smoothness

#gam_fit <- gam(mv_wheat ~ s(Max_Irrigation_mm, bs = "cr"), data = wheat)

fit <- lm(prof_dif ~ poly(irrq_m3, 1), data = wheat)


# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(120, 600, by = 1)  # Generate sequence from 100 to 500 mm
)%>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  select(irrq_m3)

# Predict values using the polynomial model
predicted_wheat$prof_dif_wheat <- predict(fit, newdata = predicted_wheat)


predicted_wheat <- predicted_wheat %>%
  mutate(Max_Irrigation_mm = irrq_m3 / 4046.86 * 1000)



  
canola <- canola_marginal%>%
  filter(year==2018)%>%
  select(year,Site, Max_Irrigation_mm,irrq_m3,prof_dif)


#fit <- loess(prof_dif ~ Max_Irrigation_mm, data = canola, span = 0.75)  # Adjust 'span' for smoothness

fit <- lm(prof_dif ~ poly(irrq_m3, 1), data = canola)


# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(120, 600, by = 1)  # Generate sequence from 100 to 500 mm
)%>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  select(irrq_m3)

# Predict values using the polynomial model
predicted_canola$prof_dif_canola <- predict(fit, newdata = predicted_canola)

predicted_canola <- predicted_canola %>%
  mutate(Max_Irrigation_mm = irrq_m3 / 4046.86 * 1000)




# wheat canola
wheat2018 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(irri_wheat,irri_canola,prof_dif_wheat)


canola2018 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)


df <- wheat2018%>%
  left_join(canola2018)%>%
  mutate(tot_prof = prof_dif_wheat + prof_dif_canola)%>%
  na.omit()




ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = prof_dif_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = prof_dif_canola, color = "Canola"), size = 1) +
  geom_line(aes(y = tot_prof, color = "Total"), size = 1) +
  
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "", linetype = "") +
  
  scale_x_continuous(
    breaks = seq(0, 600, by = 100),  
    sec.axis = sec_axis(
      transform = ~ max(df$irri_wheat) - (. - min(df$irri_wheat)),  
      name = "Irrigation for Canola (mm)",
      breaks = rev(seq(0, 600, by = 100))  
    )
  ) +
  scale_y_continuous(
    breaks = seq(-500, 1000, by = 100)
  ) +
  
  scale_linetype_manual(values = c("Wheat" = "solid", "Canola" = "solid", "Total" = "solid")) +  # Ensures all lines are solid
  theme_minimal()



# Find intersection points
crossing_points2018 <- df %>%
  mutate(diff = prof_dif_canola - prof_dif_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2018)



ggplot() +
  geom_smooth(data = wheat, aes(x = Max_Irrigation_mm, y = profit_ir), 
              method = "loess", color = "blue", se = FALSE) +  # Loess smoothing for wheat
  geom_smooth(data = canola, aes(x = Max_Irrigation_mm, y = profit_ir), 
              method = "loess", color = "red", se = FALSE) +  # Loess smoothing for canola
  labs(x = "Irrigation Water (m³)", y = "Marginal Value ($)", 
       title = "Smoothed Marginal Value for Wheat & Canola") +
  theme_minimal()








################################################################################
wheat <- wheat_marginal%>%
  filter(year==2019)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


canola <- canola_marginal%>%
  filter(year==2019)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)




# wheat canola
wheat2019 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(irri_wheat,irri_canola,mv_pred_wheat)


canola2019 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)


df <- wheat2019%>%
  left_join(canola2019)



p2 <-ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    breaks = seq(0,600, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      transform = ~ max(df$irri_wheat) - (. - min(df$irri_wheat)),  # Reverse transformation
      name = "Irrigation for Canola (mm)",
      breaks = rev(seq(0,600, by = 100))  # Reverse breaks
    )
  ) +
  scale_y_continuous(
    breaks = seq(-0.3, 0.9, by = 0.04)
  )+
  theme_minimal()



# Find intersection points
crossing_points2019 <- df %>%
  mutate(diff = mv_pred_canola - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2019)



# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(-irrq_m3)


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_canola)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_canola)








wheat <- wheat_marginal%>%
  filter(year==2020)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


canola <- canola_marginal%>%
  filter(year==2020)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)




# wheat canola
wheat2020 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(irri_wheat,irri_canola,mv_pred_wheat)


canola2020 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)


df <- wheat2020%>%
  left_join(canola2020)




p3 <-ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    breaks = seq(0,600, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      transform = ~ max(df$irri_wheat) - (. - min(df$irri_wheat)),  # Reverse transformation
      name = "Irrigation for Canola (mm)",
      breaks = rev(seq(0,600, by = 100))  # Reverse breaks
    )
  ) +
  scale_y_continuous(
    breaks = seq(-0.3, 0.9, by = 0.04)
  )+
  theme_minimal()



# Find intersection points
crossing_points2020 <- df %>%
  mutate(diff = mv_pred_canola - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2020)



# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(-irrq_m3)


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_canola)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_canola)




wheat <- wheat_marginal%>%
  filter(year==2021)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


canola <- canola_marginal%>%
  filter(year==2021)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)




# wheat canola
wheat2021 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(irri_wheat,irri_canola,mv_pred_wheat)


canola2021 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)


df <- wheat2021%>%
  left_join(canola2021)




p4 <- ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    breaks = seq(0,600, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      transform = ~ max(df$irri_wheat) - (. - min(df$irri_wheat)),  # Reverse transformation
      name = "Irrigation for Canola (mm)",
      breaks = rev(seq(0,600, by = 100))  # Reverse breaks
    )
  ) +
  scale_y_continuous(
    breaks = seq(-0.3, 0.9, by = 0.04)
  )+
  theme_minimal()

# Find intersection points
crossing_points2021 <- df %>%
  mutate(diff = mv_pred_canola - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2021)



# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(-irrq_m3)


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_canola)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_canola)





wheat <- wheat_marginal%>%
  filter(year==2022)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


canola <- canola_marginal%>%
  filter(year==2022)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)




# wheat canola
wheat2022 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(irri_wheat,irri_canola,mv_pred_wheat)


canola2022 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)


df <- wheat2022%>%
  left_join(canola2022)



p5 <- ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    breaks = seq(0,600, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      transform = ~ max(df$irri_wheat) - (. - min(df$irri_wheat)),  # Reverse transformation
      name = "Irrigation for Canola (mm)",
      breaks = rev(seq(0,600, by = 100))  # Reverse breaks
    )
  ) +
  scale_y_continuous(
    breaks = seq(-0.3, 0.9, by = 0.04)
  )+
  theme_minimal()

# Find intersection points
crossing_points2022 <- df %>%
  mutate(diff = mv_pred_canola - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2022)



# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(-irrq_m3)


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_canola)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_canola)







wheat <- wheat_marginal%>%
  filter(year==2023)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


canola <- canola_marginal%>%
  filter(year==2023)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)




# wheat canola
wheat2023 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(irri_wheat,irri_canola,mv_pred_wheat)


canola2023 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)


df <- wheat2023%>%
  left_join(canola2023)



p6 <- ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    breaks = seq(0,600, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      transform = ~ max(df$irri_wheat) - (. - min(df$irri_wheat)),  # Reverse transformation
      name = "Irrigation for Canola (mm)",
      breaks = rev(seq(0,600, by = 100))  # Reverse breaks
    )
  ) +
  scale_y_continuous(
    breaks = seq(-0.3, 0.9, by = 0.04)
  )+
  theme_minimal()


# Find intersection points
crossing_points2023 <- df %>%
  mutate(diff = mv_pred_canola - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2023)


# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_canola=600-irri_wheat)%>%
  select(-irrq_m3)


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_canola)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_canola)




combined_plot <- (p1 + p2) / (p3 + p4) / (p5 + p6)
combined_plot


#df_wheat_canola <- rbind(crossing_points2018,crossing_points2019,crossing_points2020,crossing_points2021,crossing_points2022,crossing_points2023)


### wheat to potato ### wheat to potato ### wheat to potato  ### wheat to potato 

wheat <- wheat_marginal%>%
  filter(year==2018)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 500, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


potato <- potato_marginal%>%
  filter(year==2018)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 500, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





wheat2018 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=500-irri_wheat)%>%
  select(irri_wheat,irri_potato,mv_pred_wheat)


potato2018 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- wheat2018%>%
  left_join(potato2018)

# Find intersection points
crossing_points2018 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2018)

df_wheat <- df%>%
  select(irri_wheat,mv_pred_wheat)%>%
  filter(irri_wheat <= 300) 

df_potato <- df%>%
  select(irri_potato,mv_pred_potato)%>%
  filter(irri_potato >= 300)


# Plot with two different x-axes
p1 <- ggplot() +
  geom_line(data = df_wheat, aes(x = irri_wheat, y = mv_pred_wheat, color = "Wheat"), size = 1) +
  geom_line(data = df_potato, aes(x = 500 - irri_potato, y = mv_pred_potato, color = "Canola"), size = 1) +
  labs(x = "Irrigation Level (mm)", 
       y = expression("Marginal Value ($m"^{-3}*")"),  
       title = "")+
  scale_x_continuous(
    limits = c(0, 200),
    breaks = seq(0, 200, by = 50),
    name = "Irrigation for Wheat (mm)",
    sec.axis = sec_axis(
      trans = ~ 500 - .,  # Reverse transformation for Canola
      name = "Irrigation for Canola (mm)",
      breaks = seq(200, 500, by = 50)
    )
  ) +
  
  # Y-axis scale
  scale_y_continuous(
    name = "Marginal Value ($)",
    limits = c(0, 0.6),
    breaks = seq(0, 6, by = 0.2)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.3, by = 0.05),  # Specify breaks on the y-axis
    limits = c(0, 0.3)  # Set both lower and upper limits
  ) +
  theme_minimal()


# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=500-irri_wheat)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_potato)


#################################################################################

wheat <- wheat_marginal%>%
  filter(year==2019)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 400, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


potato <- potato_marginal%>%
  filter(year==2019)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 500, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





wheat2019 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=500-irri_wheat)%>%
  select(irri_wheat,irri_potato,mv_pred_wheat)


potato2019 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- wheat2019%>%
  left_join(potato2019)

# Find intersection points
crossing_points2019 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2019)

df_wheat <- df%>%
  select(irri_wheat,mv_pred_wheat)%>%
  filter(irri_wheat <= 300) 

df_potato <- df%>%
  select(irri_potato,mv_pred_potato)%>%
  filter(irri_potato >= 300)


# Plot with two different x-axes
p2 <- ggplot() +
  geom_line(data = df_wheat, aes(x = irri_wheat, y = mv_pred_wheat, color = "Wheat"), size = 1) +
  geom_line(data = df_potato, aes(x = 500 - irri_potato, y = mv_pred_potato, color = "Canola"), size = 1) +
  labs(x = "Irrigation Level (mm)", 
       y = expression("Marginal Value ($m"^{-3}*")"),  
       title = "")+
  scale_x_continuous(
    limits = c(0, 200),
    breaks = seq(0, 200, by = 50),
    name = "Irrigation for Wheat (mm)",
    sec.axis = sec_axis(
      trans = ~ 500 - .,  # Reverse transformation for Canola
      name = "Irrigation for Canola (mm)",
      breaks = seq(200, 500, by = 50)
    )
  ) +
  
  # Y-axis scale
  scale_y_continuous(
    name = "Marginal Value ($)",
    limits = c(0, 0.6),
    breaks = seq(0, 6, by = 0.2)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.3, by = 0.05),  # Specify breaks on the y-axis
    limits = c(0, 0.3)  # Set both lower and upper limits
  ) +
  theme_minimal()


p2 <- ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 400),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 400, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 400 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 400, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.2),  # Specify breaks on the y-axis
    limits = c(0, 2)  # Set both lower and upper limits
  ) +
  theme_minimal()







# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_wheat)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_potato)







wheat <- wheat_marginal%>%
  filter(year==2020)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


potato <- potato_marginal%>%
  filter(year==2020)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





wheat2020 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_wheat)%>%
  select(irri_wheat,irri_potato,mv_pred_wheat)


potato2020 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- wheat2020%>%
  left_join(potato2020)



p3 <- ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()


# Find intersection points
crossing_points2020 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2020)



# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_wheat)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_potato)





wheat <- wheat_marginal%>%
  filter(year==2021)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


potato <- potato_marginal%>%
  filter(year==2021)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





wheat2021 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_wheat)%>%
  select(irri_wheat,irri_potato,mv_pred_wheat)


potato2021 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- wheat2021%>%
  left_join(potato2021)



p4 <- ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()



# Find intersection points
crossing_points2021 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2021)


# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_wheat)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_potato)





wheat <- wheat_marginal%>%
  filter(year==2022)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


potato <- potato_marginal%>%
  filter(year==2022)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





wheat2022 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_wheat)%>%
  select(irri_wheat,irri_potato,mv_pred_wheat)


potato2022 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- wheat2022%>%
  left_join(potato2022)




p5 <- ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()



# Find intersection points
crossing_points2022 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2022)



# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_wheat)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_potato)




wheat <- wheat_marginal%>%
  filter(year==2023)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_wheat = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_wheat)



# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_wheat ~ poly(Max_Irrigation_mm, 2), data = wheat)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_wheat <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_wheat$mv_pred_wheat <- predict(poly_fit, newdata = predicted_wheat)


potato <- potato_marginal%>%
  filter(year==2023)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





wheat2023 <- predicted_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_wheat)%>%
  select(irri_wheat,irri_potato,mv_pred_wheat)


potato2023 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- wheat2023%>%
  left_join(potato2023)


p6 <- ggplot(df, aes(x = irri_wheat)) +
  geom_line(aes(y = mv_pred_wheat, color = "Wheat"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Wheat (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()

# Find intersection points
crossing_points2023 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_wheat) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2023)




# Ensure data is sorted by irrigation levels
df_wheat <- predicted_wheat %>%
  select(Max_Irrigation_mm, mv_pred_wheat) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_wheat$cumulative_auc_wheat <- cumsum(c(0, diff(df_wheat$irrq_m3) * (head(df_wheat$mv_pred_wheat, -1) + tail(df_wheat$mv_pred_wheat, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_wheat <- df_wheat%>%
  rename(irri_wheat=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_wheat)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_wheat%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_wheat+cumulative_auc_potato)



combined_plot2 <- (p1 + p2) / (p3 + p4) / (p5 + p6)
combined_plot2

### canola to potato ### canola to potato ### canola to potato  ### canola to potato 

canola <- canola_marginal%>%
  filter(year==2018)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)


potato <- potato_marginal%>%
  filter(year==2018)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





canola2018 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(irri_canola,irri_potato,mv_pred_canola)


potato2018 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- canola2018%>%
  left_join(potato2018)



p1 <- ggplot(df, aes(x = irri_canola)) +
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Canola (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()


# Find intersection points
crossing_points2018 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_canola) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2018)


# Ensure data is sorted by irrigation levels
df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_canola%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_canola+cumulative_auc_potato)




canola <- canola_marginal%>%
  filter(year==2019)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)


potato <- potato_marginal%>%
  filter(year==2019)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





canola2019 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(irri_canola,irri_potato,mv_pred_canola)


potato2019 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- canola2019%>%
  left_join(potato2019)



p2 <- ggplot(df, aes(x = irri_canola)) +
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Canola (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()



# Find intersection points
crossing_points2019 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_canola) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2019)


# Ensure data is sorted by irrigation levels
df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_canola%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_canola+cumulative_auc_potato)



canola <- canola_marginal%>%
  filter(year==2020)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)


potato <- potato_marginal%>%
  filter(year==2020)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





canola2020 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(irri_canola,irri_potato,mv_pred_canola)


potato2020 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- canola2020%>%
  left_join(potato2020)



p3 <- ggplot(df, aes(x = irri_canola)) +
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Canola (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()



# Find intersection points
crossing_points2020 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_canola) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2020)


# Ensure data is sorted by irrigation levels
df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_canola%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_canola+cumulative_auc_potato)



canola <- canola_marginal%>%
  filter(year==2021)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)


potato <- potato_marginal%>%
  filter(year==2021)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





canola2021 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(irri_canola,irri_potato,mv_pred_canola)


potato2021 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- canola2021%>%
  left_join(potato2021)



p4 <- ggplot(df, aes(x = irri_canola)) +
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Canola (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()



# Find intersection points
crossing_points2021 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_canola) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2021)


# Ensure data is sorted by irrigation levels
df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_canola%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_canola+cumulative_auc_potato)



canola <- canola_marginal%>%
  filter(year==2022)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)


potato <- potato_marginal%>%
  filter(year==2022)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





canola2022 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(irri_canola,irri_potato,mv_pred_canola)


potato2022 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- canola2022%>%
  left_join(potato2022)



p5 <- ggplot(df, aes(x = irri_canola)) +
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Canola (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()



# Find intersection points
crossing_points2022 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_canola) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2022)


# Ensure data is sorted by irrigation levels
df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_canola%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_canola+cumulative_auc_potato)


### canola to potato ### canola to potato ### canola to potato  ### canola to potato 

canola <- canola_marginal%>%
  filter(year==2023)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_canola = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_canola)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_canola ~ poly(Max_Irrigation_mm, 2), data = canola)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_canola <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_canola$mv_pred_canola <- predict(poly_fit, newdata = predicted_canola)


potato <- potato_marginal%>%
  filter(year==2023)%>%
  arrange(Site, Max_Irrigation_mm) %>%  # Ensure correct order within each Site
  group_by(Site) %>%  # Compute differences within each site
  mutate(mv_potato = val_mm - lag(val_mm)) %>%
  ungroup()%>%
  select(year,Site, Max_Irrigation_mm,mv_potato)


# Fit a quadratic polynomial regression (degree 2)
poly_fit <- lm(mv_potato ~ poly(Max_Irrigation_mm, 2), data = potato)

# Create a dataframe for predicted values at specified intervals (100 to 500 mm, step = 20 mm)
predicted_potato <- data.frame(
  Max_Irrigation_mm = seq(1, 600, by = 1)  # Generate sequence from 100 to 500 mm
)

# Predict values using the polynomial model
predicted_potato$mv_pred_potato <- predict(poly_fit, newdata = predicted_potato)





canola2023 <- predicted_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(irri_canola,irri_potato,mv_pred_canola)


potato2023 <- predicted_potato%>%
  rename(irri_potato=Max_Irrigation_mm)


df <- canola2023%>%
  left_join(potato2023)



p6 <- ggplot(df, aes(x = irri_canola)) +
  geom_line(aes(y = mv_pred_canola, color = "Canola"), size = 1) + 
  geom_line(aes(y = mv_pred_potato, color = "Potato"), size = 1) +
  labs(y = "Marginal Value ($)", x = "Irrigation for Canola (mm)", color = "") +
  scale_x_continuous(
    limits = c(0, 300),  # Ensure both axes go from 0 to 300
    breaks = seq(0, 300, by = 100),  # Breaks for primary x-axis
    sec.axis = sec_axis(
      trans = ~ 300 - .,  # Reverse transformation (but keeping the same range)
      name = "Irrigation for Potato (mm)",
      breaks = seq(0, 300, by = 100)  # Ensure breaks match
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1),  # Specify breaks on the y-axis
    limits = c(0, 0.5)  # Set both lower and upper limits
  ) +
  theme_minimal()



# Find intersection points
crossing_points2023 <- df %>%
  mutate(diff = mv_pred_potato - mv_pred_canola) %>%                          # Difference between lines
  filter(lead(diff) * diff < 0)%>%
  mutate(year=2023)


# Ensure data is sorted by irrigation levels
df_canola <- predicted_canola %>%
  select(Max_Irrigation_mm, mv_pred_canola) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

df_potato <- predicted_potato %>%
  select(Max_Irrigation_mm, mv_pred_potato) %>%
  mutate(irrq_m3 = 4046.86*(Max_Irrigation_mm*0.001))%>%
  arrange(Max_Irrigation_mm)

# Compute cumulative AUC using the trapezoidal rule
df_canola$cumulative_auc_canola <- cumsum(c(0, diff(df_canola$irrq_m3) * (head(df_canola$mv_pred_canola, -1) + tail(df_canola$mv_pred_canola, -1)) / 2))
df_potato$cumulative_auc_potato <- cumsum(c(0, diff(df_potato$irrq_m3) * (head(df_potato$mv_pred_potato, -1) + tail(df_potato$mv_pred_potato, -1)) / 2))


df_canola <- df_canola%>%
  rename(irri_canola=Max_Irrigation_mm)%>%
  mutate(irri_potato=600-irri_canola)%>%
  select(-irrq_m3)


df_potato <- df_potato%>%
  rename(irri_potato=Max_Irrigation_mm)%>%
  select(-irrq_m3)


df <- df_canola%>%
  left_join(df_potato)%>%
  mutate(tot = cumulative_auc_canola+cumulative_auc_potato)



combined_plot3 <- (p1 + p2) / (p3 + p4) / (p5 + p6)

combined_plot3


