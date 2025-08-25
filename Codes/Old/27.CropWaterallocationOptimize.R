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


wheat_marginal <- wheat_marginal%>%
  select(Site,year,Max_Irrigation_mm,`Dry yield irri (bu/ac)`)


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
  filter(crop =="wheat")%>%
  select(crop,price.bu,year,irri_cost_ac)


wheat <- wheat_marginal%>%
  left_join(return_wheat)%>%
  mutate(revenue = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit = revenue-irri_cost_ac)

ggplot(wheat, aes(x = as.numeric(Max_Irrigation_mm), y = revenue)) +
  #geom_point(size = 1.5, color = "red") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = TRUE) +
  labs(x = "Irrigation Level (mm)", 
       y = expression("Marginal Value ($m"^{-3}*")"),  
       title = "") +
  scale_x_continuous(breaks = unique(as.numeric(wheat$Max_Irrigation_mm)),
                     labels = unique(wheat$Max_Irrigation_mm)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    legend.position = "bottom"
  )


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


canola_marginal <- canola_marginal%>%
  filter(year %in% c(2018,2019,2020,2021,2022,2023))


canola_marginal <- canola_marginal%>%
  select(Site,year,Max_Irrigation_mm,`Dry yield irri (bu/ac)`)

return_canola <- return%>%
  filter(crop =="canola")%>%
  select(crop,price.bu,year,irri_cost_ac)


canola <- canola_marginal%>%
  left_join(return_canola)%>%
  mutate(revenue = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit = revenue-irri_cost_ac)


wheat <- wheat%>%
  select(year,Site,Max_Irrigation_mm,revenue,profit)%>%
  #filter(Max_Irrigation_mm > 100)%>%
  mutate(irri_alloc =Max_Irrigation_mm )%>%
  rename(rev_wheat = revenue,
         profit_wheat = profit,
         wheat_iiri = Max_Irrigation_mm)
  


canola <- canola%>%
  select(year,Site,Max_Irrigation_mm,revenue,profit)%>%
  #filter(Max_Irrigation_mm > 100)%>%
  mutate(irri_alloc = 500-Max_Irrigation_mm)%>%
  rename(rev_canola = revenue,
         profit_canola = profit,
         canola_iiri = Max_Irrigation_mm)


#df <- wheat1%>%
  #left_join(canola1, by = c("year","Site","irri_alloc"))



df <- wheat%>%
  left_join(canola)%>%
  mutate(revenue_premium = rev_canola + rev_wheat,
         profit_premium = profit_canola + profit_wheat)%>%
  drop_na()


#t <- df%>%
#group_by(irri_alloc)%>%
#mutate(ave_revenue_premium = mean(revenue_premium))%>%
#mutate(ave_profit_premium = mean(profit_premium))%>%
#distinct(irri_alloc, .keep_all = T)



# Plotting with ggplot - wheat vs canola - total revenue
ggplot(df, aes(x = factor(irri_alloc), y = revenue_premium)) +
  stat_summary(
    fun.data = function(x) {
      data.frame(
        ymin = min(x), 
        ymax = max(x)
      )
    },
    geom = "errorbar", 
    width = 0.2, 
    color = "black"  # Color of the error bars
  ) +
  geom_boxplot(
    fill = "grey", 
    color = "black", 
    alpha = 1, 
    width = 0.3,  # Adjust width to reduce spacing between boxes
    outlier.shape = NA  # This removes the outlier dots
  ) +
  
  scale_x_discrete(
    name = "Irrigation Level in Wheat (mm)",
    breaks = seq(100, 400, 20),  # Bottom axis from 100 to 400
    labels = seq(100, 400, 20)   # Labels from 100 to 400
  ) +
  scale_y_continuous(
    name = "Total Revenue ($/acre)", 
    breaks = seq(0, 1800, 100),  # Set breaks at 500 intervals on the y-axis
    labels = seq(0, 1800, 100)   # Set labels corresponding to the breaks
  ) +
  #coord_cartesian(ylim = c(0, 7500)) +  # Limit y-axis without removing data points
  theme_minimal() +
  theme(
    axis.title.x.bottom = element_text(color = "black", size = 12),
    axis.title.x.top = element_text(color = "black", size = 12),
    axis.text.x.bottom = element_text(color = "black"),
    axis.text.x.top = element_text(color = "black"),
    panel.grid = element_blank(),  # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Set black border around the graph
    axis.line = element_line(color = "black"),  # Keep main axis lines
    axis.ticks.x.top = element_line(color = "black"),  # Add ticks on the top axis
    axis.ticks.x.bottom = element_line(color = "black")  # Add ticks on the bottom axis
  ) +
  # Annotate the second axis on top with reversed labels
  annotate("text", 
           x = seq_along(unique(df$irri_alloc)), 
           y = rep(max(df$revenue_premium) + 300, length(unique(df$irri_alloc))),  # Adjust y-position for top
           label = as.character(seq(400, 100, -20)[1:length(unique(df$irri_alloc))]),  # Reverse the values for second axis
           size = 3, color = "black", hjust = 1) +
  annotate("text", 
           x = length(unique(df$irri_alloc)) / 2, 
           y = max(df$revenue_premium) + 400, 
           label = "Irrigation Level in Canola (mm)", 
           size = 4, color = "black", hjust = 0.5)



########## Potato ########## Potato ########## Potato ########## Potato ########## Potato

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
  select(Site,year,Max_Irrigation_mm,`Dry yield irri (ton/ac)`)

return_potato <- return%>%
  filter(crop =="potato")%>%
  select(crop,price.ton,year,irri_cost)

potato <- potato_marginal%>%
  left_join(return_potato)%>%
  mutate(revenue = `Dry yield irri (ton/ac)`*price.ton)%>%
  mutate(profit = revenue-irri_cost)
  #filter(Max_Irrigation_mm > 100)


potato <- potato%>%
  select(year,Site,Max_Irrigation_mm,revenue,profit)%>%
  mutate(irri_alloc = 500-Max_Irrigation_mm)%>%
  rename(rev_potato = revenue,
         profit_potato = profit,
         potato_iiri = Max_Irrigation_mm)


df <- wheat%>%
  left_join(potato)%>%
  mutate(revenue_premium = rev_potato + rev_wheat,
         profit_premium = profit_potato + profit_wheat)%>%
  drop_na()


t <- df%>%
  group_by(irri_alloc)%>%
  mutate(ave_revenue_premium = mean(revenue_premium))%>%
  mutate(ave_profit_premium = mean(profit_premium))%>%
  distinct(irri_alloc, .keep_all = T)



# Plotting with ggplot - potato vs wheat
ggplot(df, aes(x = factor(irri_alloc), y = revenue_premium)) +
  stat_summary(
    fun.data = function(x) {
      data.frame(
        ymin = min(x), 
        ymax = max(x)
      )
    },
    geom = "errorbar", 
    width = 0.2, 
    color = "black"  # Color of the error bars
  ) +
  geom_boxplot(
    fill = "grey", 
    color = "black", 
    alpha = 1, 
    width = 0.3,  # Adjust width to reduce spacing between boxes
    outlier.shape = NA  # This removes the outlier dots
  ) +
  
  scale_x_discrete(
    name = "Irrigation Level in Wheat (mm)",
    breaks = seq(100, 400, 20),  # Bottom axis from 100 to 400
    labels = seq(100, 400, 20)   # Labels from 100 to 400
  ) +
  scale_y_continuous(
    name = "Total Revenue ($/acre)", 
    breaks = seq(0, 7500, 500),  # Set breaks at 500 intervals on the y-axis
    labels = seq(0, 7500, 500)   # Set labels corresponding to the breaks
  ) +
  #coord_cartesian(ylim = c(0, 7500)) +  # Limit y-axis without removing data points
  theme_minimal() +
  theme(
    axis.title.x.bottom = element_text(color = "black", size = 12),
    axis.title.x.top = element_text(color = "black", size = 12),
    axis.text.x.bottom = element_text(color = "black"),
    axis.text.x.top = element_text(color = "black"),
    panel.grid = element_blank(),  # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Set black border around the graph
    axis.line = element_line(color = "black"),  # Keep main axis lines
    axis.ticks.x.top = element_line(color = "black"),  # Add ticks on the top axis
    axis.ticks.x.bottom = element_line(color = "black")  # Add ticks on the bottom axis
  ) +
  # Annotate the second axis on top with reversed labels
  annotate("text", 
           x = seq_along(unique(df$irri_alloc)), 
           y = rep(max(df$revenue_premium) + 1000, length(unique(df$irri_alloc))),  # Adjust y-position for top
           label = as.character(seq(400, 100, -20)[1:length(unique(df$irri_alloc))]),  # Reverse the values for second axis
           size = 3, color = "black", hjust = 0.5) +
  annotate("text", 
           x = length(unique(df$irri_alloc)) / 2, 
           y = max(df$revenue_premium) + 1500, 
           label = "Irrigation Level in Potato (mm)", 
           size = 4, color = "black", hjust = 0.5)




canola <- canola_marginal%>%
  left_join(return_canola)%>%
  mutate(revenue = `Dry yield irri (bu/ac)`*price.bu)%>%
  mutate(profit = revenue-irri_cost_ac)%>%
  select(year,Site,Max_Irrigation_mm,revenue,profit)%>%
  #filter(Max_Irrigation_mm > 100)%>%
  mutate(irri_alloc =Max_Irrigation_mm )%>%
  rename(rev_canola = revenue,
         profit_canola = profit,
         wheat_iiri = Max_Irrigation_mm)





df <- canola%>%
  left_join(potato)%>%
  mutate(revenue_premium = rev_potato + rev_canola,
         profit_premium = profit_potato + profit_canola)%>%
  drop_na()


# Plotting with ggplot - potato vs wheat
ggplot(df, aes(x = factor(irri_alloc), y = revenue_premium)) +
  stat_summary(
    fun.data = function(x) {
      data.frame(
        ymin = min(x), 
        ymax = max(x)
      )
    },
    geom = "errorbar", 
    width = 0.2, 
    color = "black"  # Color of the error bars
  ) +
  geom_boxplot(
    fill = "grey", 
    color = "black", 
    alpha = 1, 
    width = 0.3,  # Adjust width to reduce spacing between boxes
    outlier.shape = NA  # This removes the outlier dots
  ) +
  
  scale_x_discrete(
    name = "Irrigation Level in Canola (mm)",
    breaks = seq(100, 400, 20),  # Bottom axis from 100 to 400
    labels = seq(100, 400, 20)   # Labels from 100 to 400
  ) +
  scale_y_continuous(
    name = "Total Revenue ($/acre)", 
    breaks = seq(0, 7500, 500),  # Set breaks at 500 intervals on the y-axis
    labels = seq(0, 7500, 500)   # Set labels corresponding to the breaks
  ) +
  #coord_cartesian(ylim = c(0, 7500)) +  # Limit y-axis without removing data points
  theme_minimal() +
  theme(
    axis.title.x.bottom = element_text(color = "black", size = 12),
    axis.title.x.top = element_text(color = "black", size = 12),
    axis.text.x.bottom = element_text(color = "black"),
    axis.text.x.top = element_text(color = "black"),
    panel.grid = element_blank(),  # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Set black border around the graph
    axis.line = element_line(color = "black"),  # Keep main axis lines
    axis.ticks.x.top = element_line(color = "black"),  # Add ticks on the top axis
    axis.ticks.x.bottom = element_line(color = "black")  # Add ticks on the bottom axis
  ) +
  # Annotate the second axis on top with reversed labels
  annotate("text", 
           x = seq_along(unique(df$irri_alloc)), 
           y = rep(max(df$revenue_premium) + 1000, length(unique(df$irri_alloc))),  # Adjust y-position for top
           label = as.character(seq(400, 100, -20)[1:length(unique(df$irri_alloc))]),  # Reverse the values for second axis
           size = 3, color = "black", hjust = 0.5) +
  annotate("text", 
           x = length(unique(df$irri_alloc)) / 2, 
           y = max(df$revenue_premium) + 1500, 
           label = "Irrigation Level in Potato (mm)", 
           size = 4, color = "black", hjust = 0.5)



