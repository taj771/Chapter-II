library(rgdal)
library(dplyr)
library(sf)
library(terra)
library(sp)
library(SPEI)
library(dplyr)
library(tidyr)
library(tidyverse)
library(daymetr)



# clear memory
rm(list = ls())

df1 <- read.csv("./Data/sask_dry_wheatCrop.csv")%>%
  select(DAP, WPet, Y.dry.)%>%
  rename("WPet_dry" = "WPet",
         "Y.yld_dry" = "Y.dry.")

df2 <- read.csv("./Data/sask_irri_wheatCrop.csv")%>%
  select(DAP, WPet,Y.dry.)%>%
  rename("WPet_irri" = "WPet",
         "Y.yld_irri" = "Y.dry.")

df3 <- read.csv("./Data/sask_irri_wheatInet.csv")%>%
  select(DAP.S,Inet, ET, Rain)%>%
  rename(DAP=DAP.S)%>%
  #mutate(wat_irri_sys = (Inet+Rain))%>%
  #mutate(csum_irri = cumsum(wat_irri_sys))%>%
  mutate(csum_irri = cumsum(Inet))%>%
  mutate(csum_rain = cumsum(Rain))%>%
  mutate(csum_irri_rain = csum_rain+csum_irri)%>%
  select(-Inet)

df4 <- read.csv("./Data/sask_dry_wheatWabal.csv")%>%
  select(DAP, ET)%>%
  rename("ET_dry" = "ET")


df <- df1%>%
  left_join(df2)%>%
  left_join(df3)%>%
  left_join(df4)%>%
  mutate(yld_dry_kg_ha = Y.yld_dry*1000)%>%
  mutate(yld_irri_kg_ha = Y.yld_irri*1000)%>%
  mutate(yld_dry_kg_acr = yld_dry_kg_ha/2.47105)%>%
  mutate(yld_irri_kg_acr = yld_irri_kg_ha/2.47105)%>%
  mutate(yld_dry_bu_acr = yld_dry_kg_acr*0.0272155)%>%
  mutate(yld_irri_bu_acr = yld_irri_kg_acr*0.0272155)%>%
  mutate(acre_sqm = 4046.86)%>% # square meter for an acre
  mutate(ET_m = ET/1000)%>% # evapotranspirartion in meters
  mutate(ET_m_dry = ET_dry/1000)%>% # evapotranspirartion in meters - dryland
  mutate(ET_dry = cumsum(ET_m_dry))%>%
  mutate(ET_irri = cumsum(ET_m ))%>%
  mutate(dry_wpet_yld = WPet_dry*ET_dry*acre_sqm*0.0272155)%>% #bu per acre 
  mutate(irri_wpet_yld = WPet_irri*ET_irri*acre_sqm*0.0272155)%>% # bu per acre
  mutate(yld_diff = (irri_wpet_yld -dry_wpet_yld)/dry_wpet_yld*100 )%>%
  mutate(dry_price_bu = 8.44)%>% # price of hard wheat 2023 in dry lands 
  mutate(irri_price_bu = 8.44)%>% #price of hard wheat 2023 in irrigated lands
  mutate(dry_var_cost = 294.17)%>% # this is brown soil region per acre 
  mutate(dry_tot_cost = 429.58)%>% # this is brown soil region per acre
  mutate(irri_var_cost = 429.20)%>% # this per acre (no discrimination soil region)
  mutate(irri_tot_cost = 736.70)%>% # this per acre (no discrimination soil region)
  mutate(dry_return = dry_price_bu*dry_wpet_yld)%>% # yiled bu/ac*price
  mutate(irri_return = irri_price_bu*irri_wpet_yld)%>% # yiled bu/ac*price
  mutate(dry_profit_var = dry_return-dry_var_cost)%>% # yiled bu/ac*price
  mutate(dry_profit_tot = dry_return-dry_tot_cost)%>% # yiled bu/ac*price
  mutate(irri_profit_var = irri_return-irri_var_cost)%>% # yiled bu/ac*price
  mutate(irri_profit_tot = irri_return-irri_tot_cost) # yiled bu/ac*price
  
 
  
  
#1

df_sub <- df%>%
  select(DAP,csum_irri,yld_diff)

p <-ggplot(df, aes(x = csum_irri, y = yld_diff)) + 
  geom_smooth()+
  scale_x_continuous(breaks=seq(200, 450, 50), expand=c(0, 0), limits=c(200, 450))+
  scale_y_continuous(breaks=seq(0,100, 10), limits=c(0, 100))+
  scale_colour_discrete("")+
  labs(x = "Cumilative Irrigation Days after Planting", y = "Yield increase in %")+
  labs(color='')+
  theme(legend.position = "bottom")+
  theme_bw()
p 

#2
df1 <- df%>%
  select(DAP, dry_wpet_yld,ET_dry, acre_sqm)%>%
  rename("yld" = "dry_wpet_yld")%>%
  mutate(type = "Dry land")%>%
  mutate(wtr =ET_dry*acre_sqm )

df2 <- df%>%
  select(DAP, irri_wpet_yld,ET_dry, acre_sqm)%>%
  rename("yld" = "irri_wpet_yld")%>%
  mutate(type = "Irrigate land")%>%
  mutate(wtr =ET_dry*acre_sqm)


df_sub <- rbind(df1, df2)

p <-ggplot(df_sub, aes(x = wtr, y = yld, color = type)) + 
  geom_smooth()+
  scale_x_continuous(breaks=seq(0, 1800, 100), expand=c(0, 0), limits=c(0, 1800))+
  scale_y_continuous(breaks=seq(0,100, 10), limits=c(0, 100))+
  scale_colour_discrete("")+
  labs(x = "Days after planting", y = "Wheat yield increase in bu/ac")+
  labs(color='')+
  theme(legend.position = "bottom")+
  theme_bw()
p 




#3
df1 <- df%>%
  select(DAP, dry_profit_var,ET_dry, acre_sqm)%>%
  rename("profit" = "dry_profit_var")%>%
  mutate(type = "Dry land profit over variable cost")%>%
  mutate(wtr =ET_dry*acre_sqm )%>%
  select(-ET_dry)

df2 <- df%>%
  select(DAP, dry_profit_tot,ET_dry, acre_sqm)%>%
  rename("profit" = "dry_profit_tot")%>%
  mutate(type = "Dry land profit over total cost")%>%
  mutate(wtr =ET_dry*acre_sqm )%>%
  select(-ET_dry)


df3 <- df%>%
  select(DAP, irri_profit_var,ET_irri, acre_sqm)%>%
  rename("profit" = "irri_profit_var")%>%
  mutate(type = "Irrigated land profit over variable cost")%>%
  mutate(wtr =ET_irri*acre_sqm )%>%
  select(-ET_irri)

df4 <- df%>%
  select(DAP, irri_profit_tot,ET_irri, acre_sqm)%>%
  rename("profit" = "irri_profit_tot")%>%
  mutate(type = "Irrigated land profit over total cost")%>%
  mutate(wtr =ET_irri*acre_sqm )%>%
  select(-ET_irri)


df_sub <- rbind(df1, df2, df3, df4)


p <-ggplot(df_sub, aes(x = wtr, y = profit, color = type)) + 
  #geom_smooth()+
  geom_line()+
  geom_point(size = 0.5)+
  scale_x_continuous(breaks=seq(0, 2800, 250), expand=c(0, 0), limits=c(0, 2800))+
  scale_y_continuous(breaks=seq(-800,120, 50), limits=c(-800, 120))+
  scale_colour_discrete("")+
  labs(x = "Water in cubic meter", y = "Proifit over variable/total cost per acre")+
  labs(color='')+
  theme(legend.position = "bottom")+
  theme_bw()
p 


df1 <- df%>%
  select(DAP,dry_profit_var,irri_profit_var,dry_profit_tot, irri_profit_tot, csum_rain,csum_irri, csum_irri_rain,ET_dry,ET_irri, acre_sqm)%>%
  mutate(wtr_dry =ET_dry*acre_sqm)%>%
  mutate(wtr_irri =ET_irri*acre_sqm )%>%
  mutate(prof = irri_profit_var-dry_profit_var )%>%
  mutate(type = "profit over variable cost")%>%
  mutate(irri_wtr_dif =wtr_irri- wtr_dry)

df2 <- df%>%
  select(DAP,csum_rain,csum_irri,dry_profit_var,irri_profit_var, dry_profit_tot, irri_profit_tot, csum_irri_rain,ET_dry,ET_irri, acre_sqm)%>%
  mutate(wtr_dry =ET_dry*acre_sqm)%>%
  mutate(wtr_irri =ET_irri*acre_sqm )%>%
  mutate(prof = irri_profit_tot-dry_profit_tot )%>%
  mutate(type = "profit over total cost")%>%
  mutate(irri_wtr_dif =wtr_irri- wtr_dry)


df_sub <- rbind(df1,df2)



p <-ggplot(df_sub, aes(x = irri_wtr_dif , y = prof, color = type)) + 
  geom_point()+
  #geom_line()+
  geom_smooth()+
  scale_x_continuous(breaks=seq(0, 1200, 100), expand=c(0, 0), limits=c(0, 1200))+
  scale_y_continuous(breaks=seq(-320,250, 50), limits=c(-320, 250))+
  scale_colour_discrete("")+
  labs(x = "Irrigated water in cubic meter", y = "Profit over variable/total cost per acre")+
  labs(color='')+
  theme(legend.position = "bottom")+
  theme_bw()
p 




m <- loess(prof~irri_wtr_dif, df1)

newvals <- data.frame(irri_wtr_dif =seq(from=1, to=2600, by=20))

df_prof_var <- cbind(newvals, value=predict(m, newvals))%>%
  drop_na()%>%
  mutate(prof_cubm = value/irri_wtr_dif)%>%
  filter(value>0)%>%
  select(-value)%>%
  round(digits = 2)%>%
  rename("Profit per cubic meter" ="prof_cubm",
         "Net irrigated water in m3/acre"="irri_wtr_dif" )


m <- loess(prof~irri_wtr_dif, df2)

newvals <- data.frame(irri_wtr_dif =seq(from=1, to=2600, by=10))

df_prof_tot <- cbind(newvals, value=predict(m, newvals))%>%
  drop_na()%>%
  mutate(prof_cubm = value/irri_wtr_dif)%>%
  filter(value>0)%>%
  select(-value)%>%
  round(digits = 2)%>%
  rename("Profit per cubic meter" ="prof_cubm",
         "Net irrigated water in m3/acre"="irri_wtr_dif" )

#initial
day <- c(1:12)
kc <- c(0.35)
c_ini <- data.frame(day,kc)
#crop development
day <- c(12,60)
kc <- c(0.35, 1.15)
data <- data.frame(day,kc)
ggplot(data, aes(x = day, y = kc)) +
  geom_point() +
  stat_smooth(method = "lm")
m <- lm(kc~day, data)
newvals <- data.frame(day =seq(from=12, to=60, by=1))
c_develp <- cbind(newvals, kc=predict(m, newvals))
ggplot(c_develp, aes(x = day, y = kc)) +
  geom_line()
#mid season
day <- c(60:120)
kc <- c(1.15)
c_mid <- data.frame(day,kc)
#end season
day <- c(120,150)
kc <- c(1.15, 0.65)
data <- data.frame(day,kc)
ggplot(data, aes(x = day, y = kc)) +
  geom_point() +
  stat_smooth(method = "lm")
m <- lm(kc~day, data)
newvals <- data.frame(day =seq(from=120, to=150, by=1))
c_end <- cbind(newvals, kc=predict(m, newvals))
ggplot(c_end, aes(x = day, y = kc)) +
  geom_line()


data <- rbind(c_ini,c_develp,c_mid,c_end)


ggplot(data, aes(x = day, y = kc)) +
  geom_line()+
  scale_x_continuous(breaks=seq(0, 153, 10), expand=c(0, 0), limits=c(0, 153))+
  scale_y_continuous(breaks=seq(0,1.2, 0.1), limits=c(0, 1.2))+
  xlab("Days after planting")+
  theme_bw()



