# Clear memory
rm(list = ls())



library(dplyr)
library(purrr)
library(readr)
library(priceR)
library(tidyverse)
library(priceR)
library(furrr)
library(progressr)


# Run analysis for a given year

process_year <- function(year_target) {
  
  # --- Load and adjust crop return data ---
  return <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnDarkBrown.csv")
  
  dry_cost_ac <- return$dry_cost_ac
  irri_cost_fix_ac <- return$irri_cost_fix_ac
  irri_cost_var_ac <- return$irri_cost_var_ac
  price.bu <- return$price.bu
  years <- return$year
  
  return$dry_cost_ac       <- adjust_for_inflation(dry_cost_ac, years, "CA", to_date = 2023)
  return$irri_cost_fix_ac  <- adjust_for_inflation(irri_cost_fix_ac, years, "CA", to_date = 2023)
  return$irri_cost_var_ac  <- adjust_for_inflation(irri_cost_var_ac, years, "CA", to_date = 2023)
  return$price.bu          <- adjust_for_inflation(price.bu, years, "CA", to_date = 2023)
  
  # WHEAT SECTION

  return_wheat <- return %>%
    filter(crop == "wheat", year == 2023) %>%
    mutate(year = year_target)
  
  wheat_file <- sprintf("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp%d.csv", year_target)
  
  df_wheat_ir <- read_csv(wheat_file) %>%
    mutate(year = year_target) %>%
    mutate(
      `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74,
      `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47,
      irrq_m3 = 4046.86 * (Total_Irrigation_mm * 0.001)
    ) %>%
    select(year, Max_Irrigation_mm, `Dry yield (bu/ac)`, irrq_m3, Site_ID) %>%
    mutate(
      irr_level_mm  = irrq_m3 / (0.001 * 4046.86),
      irr_level_inc = irr_level_mm * 0.03937
    ) %>%
    left_join(return_wheat) %>%
    mutate(
      irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
      irri_cost_ac     = irri_cost_var_ac + irri_cost_fix_ac,
      return_ir        = `Dry yield (bu/ac)` * price.bu,
      profit_ir        = return_ir - irri_cost_ac
    ) %>%
    select(Site_ID, Max_Irrigation_mm, irr_level_mm, irrq_m3, return_ir, profit_ir)
  
  wheat <- df_wheat_ir %>%
    arrange(Site_ID, irrq_m3) %>%
    group_by(Site_ID) %>%
    mutate(
      reve_incre_wheat = return_ir - lag(return_ir),
      prof_incre_wheat = profit_ir - lag(profit_ir),
      irrq_m3_incre    = irrq_m3 - lag(irrq_m3),
      reve_mv_wheat    = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, reve_incre_wheat / irrq_m3_incre),
      prof_mv_wheat    = ifelse(irrq_m3_incre == 0 | is.na(irrq_m3_incre), 0, prof_incre_wheat / irrq_m3_incre)
    ) %>% ungroup()
  
  wheat_interp <- wheat %>%
    group_by(Site_ID) %>%
    summarise(interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))) %>%
    unnest_wider(interp) %>%
    unnest(cols = c(x, y)) %>%
    rename(Max_Irrigation_mm = x, profit_ir = y) %>%
    mutate(crop = "wheat")
  
  # CANOLA SECTION

  return_canola <- return %>%
    filter(crop == "canola", year == 2023) %>%
    mutate(year = year_target)
  
  canola_file <- sprintf("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp%d.csv", year_target)
  
  df_canola_ir <- read_csv(canola_file) %>%
    mutate(year = year_target) %>%
    mutate(
      `Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74,
      `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47,
      irrq_m3 = 4046.86 * (Total_Irrigation_mm * 0.001)
    ) %>%
    select(year, Max_Irrigation_mm, `Dry yield (bu/ac)`, irrq_m3, Site_ID) %>%
    mutate(
      irr_level_mm  = irrq_m3 / (0.001 * 4046.86),
      irr_level_inc = irr_level_mm * 0.03937
    ) %>%
    left_join(return_canola) %>%
    mutate(
      irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
      irri_cost_ac     = irri_cost_var_ac + irri_cost_fix_ac,
      return_ir        = `Dry yield (bu/ac)` * price.bu,
      profit_ir        = return_ir - irri_cost_ac
    ) %>%
    select(Site_ID, Max_Irrigation_mm, irr_level_mm, irrq_m3, return_ir, profit_ir)
  
  canola_interp <- df_canola_ir %>%
    arrange(Site_ID, irrq_m3) %>%
    group_by(Site_ID) %>%
    summarise(interp = list(approx(
      x = Max_Irrigation_mm,
      y = profit_ir,
      xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)
    ))) %>%
    unnest_wider(interp) %>%
    unnest(cols = c(x, y)) %>%
    rename(Max_Irrigation_mm = x, profit_ir = y) %>%
    mutate(crop = "canola")
  
  # Two-crop allocation optimization

  site_ids <- 1:139
  allocations <- expand.grid(
    wheat_irrigation = seq(0, 200, 1),
    canola_irrigation = seq(0, 200, 1),
    Site_ID = site_ids
  )
  
  wheat_sub <- wheat_interp %>%
    rename(wheat_irrigation = Max_Irrigation_mm,
           wheat_profit_ir  = profit_ir) %>%
    select(-crop)
  
  canola_sub <- canola_interp %>%
    rename(canola_irrigation = Max_Irrigation_mm,
           canola_profit_ir  = profit_ir) %>%
    select(-crop)
  
  df_result <- allocations %>%
    left_join(wheat_sub, by = c("Site_ID", "wheat_irrigation")) %>%
    left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
    mutate(
      Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir), na.rm = TRUE),
      total_irrigation = canola_irrigation + wheat_irrigation
    ) %>%
    filter(total_irrigation <= 300) %>%
    group_by(Site_ID) %>%
    mutate(
      prof_wheat_150mm  = wheat_profit_ir[wheat_irrigation == 150][1],
      prof_canola_150mm = canola_profit_ir[canola_irrigation == 150][1]
    ) %>%
    ungroup() %>%
    group_by(Site_ID) %>%
    slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE),
      net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100,
      year = year_target
    ) %>%
    mutate(
      net_benefit_percent = ifelse(net_benefit_percent > 100, log(net_benefit_percent + 1), net_benefit_percent)
    ) %>%
    drop_na()
  
  return(df_result)
}

years <- 2030:2050
all_results <- map_dfr(years, process_year)

# Save combined results
write_csv(all_results, "Data/Processed/NetBenefits_canola_wheat_2030_2050.csv")


#### Three crop rotations


# Parallel setup
plan(multisession, workers = parallel::detectCores() - 1)
handlers(global = TRUE)
handlers("progress")

# Define irrigation ranges
wheat_seq  <- seq(0, 200, 1)
canola_seq <- seq(0, 200, 1)
potato_seq <- seq(0, 260, 1)

# Define site IDs
site_ids <- 1:139

# Output folder
output_dir <- "Data/Processed/site_results"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Define year range
years_to_run <- 2030:2050

# Function: process one year
run_year_simulation <- function(YEAR) {
  
  message(paste(" Processing year:", YEAR))
  
  ## ---------------- Wheat ---------------- ##
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
  
  return_wheat <- return %>%
    filter(crop == "wheat") %>%
    mutate(year = YEAR)
  
  wheat_ir <- read_csv(paste0("AquaCropOPSyData/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp", YEAR, ".csv")) %>%
    mutate(year = YEAR)
  
  df_wheat_ir <- wheat_ir %>%
    mutate(`Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74,
           `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47,
           irrq_m3 = 4046.86 * (Total_Irrigation_mm * 0.001)) %>%
    select(year, Max_Irrigation_mm, `Dry yield (bu/ac)`, irrq_m3, Site_ID) %>%
    mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
           irr_level_inc = irr_level_mm * 0.03937) %>%
    left_join(return_wheat) %>%
    mutate(irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
           irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac,
           return_ir = `Dry yield (bu/ac)` * price.bu,
           profit_ir = return_ir - irri_cost_ac) %>%
    select(Site_ID, Max_Irrigation_mm, irr_level_mm, irrq_m3, return_ir, profit_ir)
  
  wheat_interp_profit <- df_wheat_ir %>%
    arrange(Site_ID, irrq_m3) %>%
    group_by(Site_ID) %>%
    summarise(interp = list(approx(Max_Irrigation_mm, profit_ir, xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)))) %>%
    unnest_wider(interp) %>%
    unnest(cols = c(x, y)) %>%
    rename(Max_Irrigation_mm = x, profit_ir = y) %>%
    mutate(crop = "wheat")
  
  wheat_sub <- wheat_interp_profit %>%
    rename(wheat_irrigation = Max_Irrigation_mm,
           wheat_profit_ir = profit_ir) %>%
    select(-crop)
  
  ## ---------------- Canola ---------------- ##
  return_canola <- return %>%
    filter(crop == "canola") %>%
    mutate(year = YEAR)
  
  canola_ir <- read_csv(paste0("AquaCropOPSyData/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp", YEAR, ".csv")) %>%
    mutate(year = YEAR)
  
  df_canola_ir <- canola_ir %>%
    mutate(`Dry yield (bu/ha)` = `Yield_tonne_per_ha` * 36.74,
           `Dry yield (bu/ac)` = `Dry yield (bu/ha)` / 2.47,
           irrq_m3 = 4046.86 * (Total_Irrigation_mm * 0.001)) %>%
    select(year, Max_Irrigation_mm, `Dry yield (bu/ac)`, irrq_m3, Site_ID) %>%
    mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
           irr_level_inc = irr_level_mm * 0.03937) %>%
    left_join(return_canola) %>%
    mutate(irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
           irri_cost_ac = irri_cost_var_ac + irri_cost_fix_ac,
           return_ir = `Dry yield (bu/ac)` * price.bu,
           profit_ir = return_ir - irri_cost_ac) %>%
    select(Site_ID, Max_Irrigation_mm, irr_level_mm, irrq_m3, return_ir, profit_ir)
  
  canola_interp_profit <- df_canola_ir %>%
    arrange(Site_ID, irrq_m3) %>%
    group_by(Site_ID) %>%
    summarise(interp = list(approx(Max_Irrigation_mm, profit_ir, xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)))) %>%
    unnest_wider(interp) %>%
    unnest(cols = c(x, y)) %>%
    rename(Max_Irrigation_mm = x, profit_ir = y) %>%
    mutate(crop = "canola")
  
  canola_sub <- canola_interp_profit %>%
    rename(canola_irrigation = Max_Irrigation_mm,
           canola_profit_ir = profit_ir) %>%
    select(-crop)
  
  ## ---------------- Potato ---------------- ##
  potato_ir <- read_csv(paste0("AquaCropOPSyData/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp", YEAR, ".csv")) %>%
    mutate(year = YEAR)
  
  df_potato_ir <- potato_ir %>%
    mutate(`Dry yield (ton/ac)` = `Yield_tonne_per_ha` / 2.47,
           irrq_m3 = 4046.86 * (Total_Irrigation_mm * 0.001)) %>%
    select(year, Max_Irrigation_mm, `Dry yield (ton/ac)`, irrq_m3, Site_ID)
  
  return_potato <- read.csv("./AquaCropOPSyData/CropReturn/CropReturnPotato.csv") %>%
    mutate(across(c(irri_cost_fix_ac, irri_cost_var_ac, price.ton), ~adjust_for_inflation(., year, "CA", to_date = 2023))) %>%
    filter(year == 2023) %>%
    mutate(year = YEAR)
  
  potato_profit_interp <- df_potato_ir %>%
    left_join(return_potato) %>%
    mutate(irr_level_mm = irrq_m3 / (0.001 * 4046.86),
           irr_level_inc = irr_level_mm * 0.03937,
           irri_cost_var_ac = irri_cost_var_ac * irr_level_inc,
           irri_cost = irri_cost_var_ac + irri_cost_fix_ac,
           return_ir = `Dry yield (ton/ac)` * price.ton,
           profit_ir = return_ir - irri_cost) %>%
    arrange(Site_ID, irrq_m3) %>%
    group_by(Site_ID) %>%
    summarise(interp = list(approx(Max_Irrigation_mm, profit_ir, xout = seq(min(Max_Irrigation_mm), max(Max_Irrigation_mm), by = 1)))) %>%
    unnest_wider(interp) %>%
    unnest(cols = c(x, y)) %>%
    rename(Max_Irrigation_mm = x, profit_ir = y) %>%
    mutate(crop = "potato")
  
  potato_sub <- potato_profit_interp %>%
    rename(potato_irrigation = Max_Irrigation_mm,
           potato_profit_ir = profit_ir) %>%
    select(-crop)
  
  ## ---------------- Optimization ---------------- ##
  
  process_site <- function(site_id, wheat_sub, canola_sub, potato_sub) {
    alloc_site <- expand.grid(
      wheat_irrigation  = wheat_seq,
      canola_irrigation = canola_seq,
      potato_irrigation = potato_seq
    ) %>% mutate(Site_ID = site_id)
    
    result_site <- alloc_site %>%
      left_join(wheat_sub,  by = c("Site_ID", "wheat_irrigation")) %>%
      left_join(canola_sub, by = c("Site_ID", "canola_irrigation")) %>%
      left_join(potato_sub, by = c("Site_ID", "potato_irrigation")) %>%
      mutate(total_irrigation = wheat_irrigation + canola_irrigation + potato_irrigation,
             Tot_prof_scenario2 = rowSums(select(., wheat_profit_ir, canola_profit_ir, potato_profit_ir), na.rm = TRUE)) %>%
      filter(total_irrigation <= 450)
    
    best_allocation <- result_site %>% slice_max(order_by = Tot_prof_scenario2, n = 1, with_ties = FALSE)
    
    # Scenario 1: 150 mm each
    prof_wheat_150mm  <- result_site %>% filter(wheat_irrigation == 150) %>% pull(wheat_profit_ir) %>% first()
    prof_canola_150mm <- result_site %>% filter(canola_irrigation == 150) %>% pull(canola_profit_ir) %>% first()
    prof_potato_150mm <- result_site %>% filter(potato_irrigation == 150) %>% pull(potato_profit_ir) %>% first()
    
    Tot_prof_scenario1 <- sum(prof_wheat_150mm, prof_canola_150mm, prof_potato_150mm, na.rm = TRUE)
    
    best_allocation %>%
      mutate(
        prof_wheat_150mm = prof_wheat_150mm,
        prof_canola_150mm = prof_canola_150mm,
        prof_potato_150mm = prof_potato_150mm,
        Tot_prof_scenario1 = Tot_prof_scenario1,
             net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario2)) * 100,
             year = YEAR)
  }
  
  best_all_sites <- with_progress({
    p <- progressor(along = site_ids)
    future_map_dfr(site_ids, function(sid) {
      result <- process_site(sid, wheat_sub, canola_sub, potato_sub)
      p(message = sprintf("Processed site %d / %d", sid, length(site_ids)))
      return(result)
    })
  }) %>% drop_na()
  
  # Save combined results
  write_csv(best_all_sites, paste0("Data/ProcessedNetBenefit_wheat_canola_potato_", YEAR, ".csv"))
}

# Run for all years 2030–2050
walk(years_to_run, run_year_simulation)

# Reset parallel plan
plan(sequential)

#################################################################################################


df_wheat_canola <- read_csv("Data/Processed/NetBenefits_canola_wheat_2030_2050.csv")%>%
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0))%>%
  mutate(type = "Crop Rotation: Wheat & Canola")%>%
  select(Site_ID, year,type, net_benefit_percent)%>%
  mutate(
    net_benefit_percent = ifelse(net_benefit_percent > 100, log(net_benefit_percent + 1), net_benefit_percent)
  )


# Define the year range and path pattern
years <- 2030:2050
file_paths <- paste0("Data/ProcessedNetBenefit_wheat_canola_potato_", years, ".csv")

# Read and combine all files into one dataframe
df_wheat_canola_potato <- file_paths %>%
  map_dfr(read_csv, .id = "file_id") %>% 
  mutate(year = years[as.integer(file_id)]) %>%
  select(-file_id)%>%
  
  mutate(prof_wheat_150mm = ifelse(wheat_irrigation <= 150,wheat_profit_ir,prof_wheat_150mm))%>%
  mutate(prof_canola_150mm = ifelse(canola_irrigation <= 150,canola_profit_ir,prof_canola_150mm))%>%
  mutate(prof_potato_150mm = ifelse(potato_irrigation <= 150,potato_profit_ir,prof_potato_150mm))%>%
  mutate(Tot_prof_scenario1 = rowSums(select(., prof_wheat_150mm, prof_canola_150mm,prof_potato_150mm), na.rm = TRUE))%>%
  mutate(net_benefit_percent = ((Tot_prof_scenario2 - Tot_prof_scenario1) / abs(Tot_prof_scenario1)) * 100)%>%
  mutate(Flex_allocation_2_wheat = ifelse(wheat_irrigation > 150, wheat_irrigation - 150, 0),
         Flex_allocation_2_canola = ifelse(canola_irrigation > 150, canola_irrigation - 150, 0),
         Flex_allocation_2_potato = ifelse(potato_irrigation > 150, potato_irrigation - 150, 0))%>%
  mutate(type = "Crop Rotation: Wheat, Canola & Potato")%>%
  select(Site_ID, year,type, net_benefit_percent)%>%
  mutate(
    net_benefit_percent = ifelse(net_benefit_percent > 100, log(net_benefit_percent + 1), net_benefit_percent)
  )

  

df_all <- df_wheat_canola%>%
  rbind(df_wheat_canola_potato )%>%
  mutate(net_benefit_percent = ifelse(net_benefit_percent > 100, 100, net_benefit_percent))



p <- ggplot(df_all, aes(x = type, y = net_benefit_percent, fill = type, color = type)) +
  geom_violin(trim = FALSE, alpha = 0.2, color = "black", width = 0.6) +   # main violin layer
  geom_boxplot(width = 0.05, color = "black", alpha = 0.8, outlier.shape = NA) +  # optional boxplot inside violin
  #geom_jitter(width = 0.07, alpha = 0.3, size = 1.2) +  # jitter points
  labs(
    title = "",
    x = "",
    y = "Net Benefit (%)"
  ) +
  scale_fill_manual(
    values = c(
      "Crop Rotation: Wheat & Canola" = "darkgreen",
      "Crop Rotation: Wheat, Canola & Potato" = "darkred"
    ),
    labels = c("Wheat & Canola", "Wheat, Canola & Potato"),
    name = "Crop Rotation:"
  ) +
  scale_color_manual(
    values = c(
      "Crop Rotation: Wheat & Canola" = "darkgreen",
      "Crop Rotation: Wheat, Canola & Potato" = "darkred"
    ),
    labels = c("Wheat & Canola", "Wheat, Canola & Potato"),
    name = "Crop Rotation:"
  ) +
  scale_x_discrete(labels = NULL) +  # Hide x-axis labels
  scale_y_continuous(
    breaks = seq(-100, 100, by = 10),
    limits = c(-3, 80),
    expand = c(0, 0)
  ) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "white", fill = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.8),
    legend.key.size = unit(0.3, "cm")
  )

ggsave("./results/images/reallocationBenefitsclimatechnage_option2.png", plot = p, width = 10, height = 7, dpi = 300)


t <- df_all%>%
  filter(type == "Crop Rotation: Wheat & Canola" )

tt <- t%>%
  group_by(year)%>%
  mutate(ave_year_net_ben = mean(net_benefit_percent))%>%
  distinct(year, .keep_all = T)

mean(t$net_benefit_percent)

t <- df_all%>%
  filter(type ==  "Crop Rotation: Wheat, Canola & Potato" )


mean(t$net_benefit_percent)



tt <- t%>%
  group_by(year)%>%
  mutate(ave_year_net_ben = mean(net_benefit_percent))%>%
  distinct(year, .keep_all = T)

