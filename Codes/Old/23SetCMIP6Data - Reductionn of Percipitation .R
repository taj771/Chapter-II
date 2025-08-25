#clear memory
rm(list = ls())

# wheat
library(dplyr)
library(tidyverse)

# 20%
CMIP126_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126.csv") %>%
  #filter(
    #Year >= 2030 & Year <= 2050,  # Filter year range
    #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.2)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP126_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_20redPrcp.csv")


# 40%
CMIP126_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126.csv") %>%
  #filter(
  #Year >= 2030 & Year <= 2050,  # Filter year range
  #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.4)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP126_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_40redPrcp.csv")



# 60%
CMIP126_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126.csv") %>%
  #filter(
  #Year >= 2030 & Year <= 2050,  # Filter year range
  #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.6)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP126_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_60redPrcp.csv")

# 80%
CMIP126_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126.csv") %>%
  #filter(
  #Year >= 2030 & Year <= 2050,  # Filter year range
  #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.8)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP126_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_80redPrcp.csv")



##### 245 ##### 245 ##### 245 ##### 245 ##### 245 ##### 245 ##### 245

# 20%
CMIP245_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245.csv") %>%
  #filter(
    #Year >= 2030 & Year <= 2050,  # Filter year range
    #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.2)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP245_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_20redPrcp.csv")


# 40%
CMIP245_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245.csv") %>%
  #filter(
  #Year >= 2030 & Year <= 2050,  # Filter year range
  #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.4)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP245_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_40redPrcp.csv")


# 60%
CMIP245_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245.csv") %>%
  #filter(
  #Year >= 2030 & Year <= 2050,  # Filter year range
  #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.6)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP245_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_60redPrcp.csv")

# 80%
CMIP245_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245.csv") %>%
  #filter(
  #Year >= 2030 & Year <= 2050,  # Filter year range
  #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.8)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP245_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_80redPrcp.csv")




##### 585 ##### 585 ##### 585 ##### 585 ##### 585 ##### 585 

# 20%
CMIP585_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585.csv") %>%
  #filter(
    #Year >= 2030 & Year <= 2050,  # Filter year range
    #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.2)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP245_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_20redPrcp.csv")

# 40%
CMIP585_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585.csv") %>%
  #filter(
  #Year >= 2030 & Year <= 2050,  # Filter year range
  #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.4)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP245_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_40redPrcp.csv")

# 60%
CMIP585_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585.csv") %>%
  #filter(
  #Year >= 2030 & Year <= 2050,  # Filter year range
  #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.6)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP245_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_60redPrcp.csv")


# 80%
CMIP585_prcp <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585.csv") %>%
  #filter(
  #Year >= 2030 & Year <= 2050,  # Filter year range
  #Month >= 5 & Month <= 10 )%>%
  mutate(PrcpRed = Precipitation*0.8)%>%
  mutate(Precipitation_new = Precipitation-PrcpRed)%>%
  select(-Precipitation)%>%
  rename(Precipitation=Precipitation_new)%>%
  select(site,MinTemp,MaxTemp,Precipitation,daylength,Date,Year,Month,Day)

write.csv(CMIP245_prcp,"/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP585_80redPrcp.csv")



