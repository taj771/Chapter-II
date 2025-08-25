# clear memory
rm(list = ls())


library(tidyverse)
library(fs)
library(readxl)
library(stringr)
library(janitor)
library(dplyr)
library(readr)
library(modelsummary)

############################################################################
# Load processed data

Irrigated_crop_23 <- read.csv("./data/Processed/IrrigatecropProd2024.csv")%>%
  dplyr::select(-avg_value)
dry_crop_23 <- read.csv("./data/Processed/DrylandcropProd2024.csv")

# select crop types that are in both data set

dry_crop <- filter(dry_crop_23, Crop %in% Irrigated_crop_23$Crop)%>%
  dplyr::select(-X)%>%
  mutate(var = case_when(Cost.Expen.Reve == "Total Variable Expenses (D)" ~ "varcost",
                         Cost.Expen.Reve == "Total Other Expenses (E)"~"fixcost",
                         Cost.Expen.Reve == "Return over Variable Expenses (C-D)" ~ "retovervarcost",
                         Cost.Expen.Reve == "Return over Total Expenses (C-G)" ~ "retovertotcost"
  ))%>%
  dplyr::select(-Cost.Expen.Reve)

Irrig_crop <- filter(Irrigated_crop_23, Crop %in% dry_crop_23$Crop)



df <- dry_crop%>%
  left_join(Irrig_crop, by = c("Crop","var"))%>%
  relocate(var, Brown)%>%
  rowwise()%>%
  mutate(value_dry = mean(c(Brown,Black,Dark.Brown),na.rm = T))%>% # average across the cost/revenue in soil regions (i.e. Brown, Dark.Brown, Black) in dry crop
  rename("value_irri" = "Value")


df_return <- subset(df, var %in% c("retovervarcost","retovertotcost"))%>%
  mutate(value_water = value_irri - value_dry)

df_summary <- df_return%>%
  dplyr::select(var,Crop, value_water)%>%
  mutate(var = case_when(var == "retovervarcost" ~ "Return_Over_Variable_Cost",
                         var == "retovertotcost" ~ "Return_Over_Total_Cost"
  ))%>%
  rename("IrrigationPremium" = "value_water")

# irrigation requirement (including water from percipitation)
canola_avg = 371
fababean = NA
feedbarley = 271
flax = 372
hybridfallrye = 400
maltbarley = 271
quinoa = 375
soybean = NA
winterwheat = 400


data_wide <- spread(df_summary, var, IrrigationPremium)%>%
  mutate(acre_sqm = 4046.86)%>%
  mutate(irri_req = case_when(Crop == "Canola" ~ canola_avg/1000,
                         Crop == "FabaBean"~ fababean/1000,
                         Crop == "FeedBarley"~ feedbarley/1000,
                         Crop == "Flax"~ flax/1000,
                         Crop == "HybridFallRye"~ hybridfallrye/1000,
                         Crop == "MaltBarley"~ maltbarley/1000,
                         Crop == "Quinoa"~ quinoa/1000,
                         Crop == "Soybean"~ soybean/1000,
                         Crop == "WinterWheat"~ winterwheat/1000))%>%
  mutate(value_cubmeter_vari_cost = Return_Over_Variable_Cost/acre_sqm*irri_req)%>%
  mutate(value_cubmeter_tot_cost = Return_Over_Total_Cost/acre_sqm*irri_req)%>%
  select(-acre_sqm, - irri_req)
  

data_wide


datasummary_df(data_wide, output = "./summary_table.tex")



###################################################################################
# value calculation per unit of water use

# The values above are for acre, so the dimension of acre 

#acre = 4,046.86 sqm

# if water use of particular crop is 350mm, then the total water requirement is:


#canola_avg = 371mm
# fababean = NA
# feedbarley = 271mm
# flax = 372mm
# hybridfallrye = 400mm
# maltbarley = 271mm
# quinoa = 375mm
# soybean = NA
# winterwheat = 400mm



irri_req_canola <- seq(from = 0, to = 0.397, by = 0.01)
irri_req_feedbarley <- seq(from = 0, to = 0.231, by = 0.01)
irri_req_flax <- seq(from = 0, to = 0.372, by = 0.01)
irri_req_hybridfallrye <- seq(from = 0, to = 0.400, by = 0.01) # not average available so maximum of range used 
irri_req_maltbarley <- seq(from = 0, to = 0.231, by = 0.01)
irri_req_quinoa <- seq(from = 0, to = 0.375, by = 0.01)
irri_req_winterwheat <- seq(from = 0, to = 0.400, by = 0.01)



df_canola <- data_frame(irri_req_canola)%>%
  mutate(prcp = rev(irri_req_canola))%>%
  mutate(waterreq_m = 372/1000)%>%
  mutate(acre_sqm = 4046.86)%>% # area covered by 1 acre is 4046.86 square meters
  mutate(wtrreqcm3 =waterreq_m*acre_sqm)%>% #water quantity for an acre in cubic meters
  mutate(ip_vc = 847)%>% # irrigation premium over variable cost for acre
  mutate(ip_tc = 76.91)%>% # irrigation premium over total cost for acre
  mutate(return_vc_cm3 =ip_vc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(return_tc_cm3 =ip_tc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(irri_req = waterreq_m-prcp)%>% # irrigation requirement 
  mutate(irri_req= if_else(irri_req< 0, 0, irri_req))%>%
  mutate(req_irri_cm3 = irri_req*acre_sqm)%>%  #water quantity required by irrigation for an acre in cubic meters
  mutate(val_irri_vc = return_vc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  mutate(val_irri_tc = return_tc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  rename("irrig" = "irri_req_canola")%>%
  mutate(crop = "Canola")

ggplot( aes(x = prcp, y = val_irri_vc), data = df_canola) +
  geom_line() +
  #geom_point()+
  theme_bw()+
  scale_x_continuous(breaks=seq(0, 0.4, 0.05), expand=c(0, 0), limits=c(0, 0.41), name = "Percipitation (m)",
                     sec.axis = sec_axis(~rev(.),breaks=seq(0, 0.4, 0.05), name = 'Irrigation Requirement (m)'))+
  scale_y_continuous(breaks=seq(0, 900, 100), expand=c(0, 0), limits=c(0, 900))+
  labs(y = "$ per cubic meters")+
  labs(color='Crop')+
  theme(legend.position = "bottom")



df_feedbarley <- data_frame(irri_req_feedbarley)%>%
  mutate(prcp = rev(irri_req_feedbarley))%>%
  mutate(waterreq_m = 231/1000)%>%
  mutate(acre_sqm = 4046.86)%>% # area covered by 1 acre is 4046.86 square meters
  mutate(wtrreqcm3 =waterreq_m*acre_sqm)%>% #water quantity for an acre in cubic meters
  mutate(ip_vc = 585.89)%>% # irrigation premium over variable cost for acre
  mutate(ip_tc = -1.22)%>% # irrigation premium over total cost for acre
  mutate(return_vc_cm3 =ip_vc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(return_tc_cm3 =ip_tc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(irri_req = waterreq_m-prcp)%>% # irrigation requirement 
  mutate(irri_req= if_else(irri_req< 0, 0, irri_req))%>%
  mutate(req_irri_cm3 = irri_req*acre_sqm)%>%  #water quantity required by irrigation for an acre in cubic meters
  mutate(val_irri_vc = return_vc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  mutate(val_irri_tc = return_tc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  rename("irrig" = "irri_req_feedbarley")%>%
  mutate(crop = "Feed Barley")

df_flax <- data_frame(irri_req_flax)%>%
  mutate(prcp = rev(irri_req_flax))%>%
  mutate(waterreq_m = 372/1000)%>%
  mutate(acre_sqm = 4046.86)%>% # area covered by 1 acre is 4046.86 square meters
  mutate(wtrreqcm3 =waterreq_m*acre_sqm)%>% #water quantity for an acre in cubic meters
  mutate(ip_vc = 585.60)%>% # irrigation premium over variable cost for acre
  mutate(ip_tc = -90.52)%>% # irrigation premium over total cost for acre
  mutate(return_vc_cm3 =ip_vc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(return_tc_cm3 =ip_tc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(irri_req = waterreq_m-prcp)%>% # irrigation requirement 
  mutate(irri_req= if_else(irri_req< 0, 0, irri_req))%>%
  mutate(req_irri_cm3 = irri_req*acre_sqm)%>%  #water quantity required by irrigation for an acre in cubic meters
  mutate(val_irri_vc = return_vc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  mutate(val_irri_tc = return_tc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  rename("irrig" = "irri_req_flax")%>%
  mutate(crop = "Flax")

df_hybridfallrye <- data_frame(irri_req_hybridfallrye)%>%
  mutate(prcp = rev(irri_req_hybridfallrye))%>%
  mutate(waterreq_m = 400/1000)%>%
  mutate(acre_sqm = 4046.86)%>% # area covered by 1 acre is 4046.86 square meters
  mutate(wtrreqcm3 =waterreq_m*acre_sqm)%>% #water quantity for an acre in cubic meters
  mutate(ip_vc = 660.38)%>% # irrigation premium over variable cost for acre
  mutate(ip_tc = 281.26)%>% # irrigation premium over total cost for acre
  mutate(return_vc_cm3 =ip_vc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(return_tc_cm3 =ip_tc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(irri_req = waterreq_m-prcp)%>% # irrigation requirement 
  mutate(irri_req= if_else(irri_req< 0, 0, irri_req))%>%
  mutate(req_irri_cm3 = irri_req*acre_sqm)%>%  #water quantity required by irrigation for an acre in cubic meters
  mutate(val_irri_vc = return_vc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  mutate(val_irri_tc = return_tc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  rename("irrig" = "irri_req_hybridfallrye")%>%
  mutate(crop = "Hybrid Fall Rye")

df_maltbarley <- data_frame(irri_req_maltbarley)%>%
  mutate(prcp = rev(irri_req_maltbarley))%>%
  mutate(waterreq_m = 231/1000)%>%
  mutate(acre_sqm = 4046.86)%>% # area covered by 1 acre is 4046.86 square meters
  mutate(wtrreqcm3 =waterreq_m*acre_sqm)%>% #water quantity for an acre in cubic meters
  mutate(ip_vc = 704.13)%>% # irrigation premium over variable cost for acre
  mutate(ip_tc = 154.00)%>% # irrigation premium over total cost for acre
  mutate(return_vc_cm3 =ip_vc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(return_tc_cm3 =ip_tc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(irri_req = waterreq_m-prcp)%>% # irrigation requirement 
  mutate(irri_req= if_else(irri_req< 0, 0, irri_req))%>%
  mutate(req_irri_cm3 = irri_req*acre_sqm)%>%  #water quantity required by irrigation for an acre in cubic meters
  mutate(val_irri_vc = return_vc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  mutate(val_irri_tc = return_tc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  rename("irrig" = "irri_req_maltbarley")%>%
  mutate(crop = "Malt Barley")

df_quinoa <- data_frame(irri_req_quinoa)%>%
  mutate(prcp = rev(irri_req_quinoa))%>%
  mutate(waterreq_m = 375/1000)%>%
  mutate(acre_sqm = 4046.86)%>% # area covered by 1 acre is 4046.86 square meters
  mutate(wtrreqcm3 =waterreq_m*acre_sqm)%>% #water quantity for an acre in cubic meters
  mutate(ip_vc = 1127.06)%>% # irrigation premium over variable cost for acre
  mutate(ip_tc = 560.48)%>% # irrigation premium over total cost for acre
  mutate(return_vc_cm3 =ip_vc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(return_tc_cm3 =ip_tc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(irri_req = waterreq_m-prcp)%>% # irrigation requirement 
  mutate(irri_req= if_else(irri_req< 0, 0, irri_req))%>%
  mutate(req_irri_cm3 = irri_req*acre_sqm)%>%  #water quantity required by irrigation for an acre in cubic meters
  mutate(val_irri_vc = return_vc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  mutate(val_irri_tc = return_tc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  rename("irrig" = "irri_req_quinoa")%>%
  mutate(crop = "Quinoa")

df_winterwheat <- data_frame(irri_req_winterwheat)%>%
  mutate(prcp = rev(irri_req_winterwheat))%>%
  mutate(waterreq_m = 400/1000)%>%
  mutate(acre_sqm = 4046.86)%>% # area covered by 1 acre is 4046.86 square meters
  mutate(wtrreqcm3 =waterreq_m*acre_sqm)%>% #water quantity for an acre in cubic meters
  mutate(ip_vc = 688.67)%>% # irrigation premium over variable cost for acre
  mutate(ip_tc = 66.54)%>% # irrigation premium over total cost for acre
  mutate(return_vc_cm3 =ip_vc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(return_tc_cm3 =ip_tc/wtrreqcm3)%>% # irrigation premium for an acre in cubic meter over variable cost for acre
  mutate(irri_req = waterreq_m-prcp)%>% # irrigation requirement 
  mutate(irri_req= if_else(irri_req< 0, 0, irri_req))%>%
  mutate(req_irri_cm3 = irri_req*acre_sqm)%>%  #water quantity required by irrigation for an acre in cubic meters
  mutate(val_irri_vc = return_vc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  mutate(val_irri_tc = return_tc_cm3*req_irri_cm3)%>% # price of cubic meter in acre multiplied by total requred water 
  rename("irrig" = "irri_req_winterwheat")%>%
  mutate(crop = "Winter Wheat")




df <- rbind(df_canola, df_feedbarley,df_flax,df_hybridfallrye,df_maltbarley,df_quinoa,df_winterwheat)

df <- df%>%
  filter(val_irri_vc!=Inf)%>%
  filter(val_irri_tc!=Inf)
  


ggplot(df, aes(x = irri_req, y = value_cubmeter)) + 
  geom_smooth(method ="lm",span = 0.07) + theme_bw() + 
  scale_x_continuous(breaks=seq(0, 2000, 250), expand=c(0, 0), limits=c(0, 2000))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.5))+
  scale_colour_discrete("")+
  labs(x = "Distance (Meters)", y = "Elasticity")+
  labs(color='Distance Specification')+
  theme(legend.position = "bottom")



ggplot( aes(x = prcp, y = val_irri_vc, colour = crop), data = df) +
  geom_line() +
  #geom_point()+
  theme_bw()+
  scale_x_continuous(breaks=seq(0, 0.4, 0.05), expand=c(0, 0), limits=c(0, 0.41), name = "Percipitation (m)",
                     sec.axis = sec_axis(~rev(.),breaks=seq(0, 0.4, 0.05), name = 'Irrigation Requirement (m)'))+
  scale_y_continuous(breaks=seq(0, 1200, 100), expand=c(0, 0), limits=c(0, 1200))+
  labs(y = "$ per cubic meters for an acre")+
  labs(color='Crop')+
  theme(legend.position = "bottom")

ggplot( aes(x = prcp, y = val_irri_tc, colour = crop), data = df) +
  geom_line() +
  #geom_point()+
  theme_bw()+
  scale_x_continuous(breaks=seq(0, 0.4, 0.05), expand=c(0, 0), limits=c(0, 0.41), name = "Percipitation (m)",
                     sec.axis = sec_axis(~rev(.),breaks=seq(0, 0.4, 0.05), name = 'Irrigation Requirement (m)'))+
  scale_y_continuous(breaks=seq(0, 600, 100), expand=c(0, 0), limits=c(0, 600))+
  labs(y = "$ per cubic meters for an acre")+
  labs(color='Crop')+
  theme(legend.position = "bottom")





ggplot(df, aes(x = irri_req, y = value_cubmeter)) + 
  geom_smooth(method ="lm",span = 0.07) + theme_bw() + 
  scale_x_continuous(breaks=seq(0, 0.4, 0.05), expand=c(0, 0), limits=c(0, 0.41))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.2))+
  scale_colour_discrete("")+
  labs(x = "Irrigation Requirement (m)", y = "$ per cubic meters")+
  labs(color='')+
  theme(legend.position = "bottom")



