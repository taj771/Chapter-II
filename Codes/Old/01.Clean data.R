# clear memory
rm(list = ls())


library(tidyverse)
library(fs)
library(readxl)
library(stringr)
library(janitor)
library(dplyr)
library(readr)




#Dry crop/wo irrigation

MaltBarley <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 6)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="MALT BARLEY PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "MaltBarley")

MaltBarley <- MaltBarley[MaltBarley$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                           'Total Other Expenses (E)',
                                                           'Return over Variable Expenses (C-D)',
                                                           'Return over Total Expenses (C-G)'),]
  
  
  
FeedBarley <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 5)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="FEED BARLEY PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "FeedBarley")

FeedBarley <- FeedBarley[FeedBarley$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                           'Total Other Expenses (E)',
                                                           'Return over Variable Expenses (C-D)',
                                                           'Return over Total Expenses (C-G)'),]



Corn <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 7)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="CORN PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "Corn")
Corn <- Corn[Corn$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                           'Total Other Expenses (E)',
                                                           'Return over Variable Expenses (C-D)',
                                                           'Return over Total Expenses (C-G)'),]


HybridFallRye <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 8)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="HYBRID FALL RYE PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "HybridFallRye")
HybridFallRye <- HybridFallRye[HybridFallRye$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                         'Total Other Expenses (E)',
                                         'Return over Variable Expenses (C-D)',
                                         'Return over Total Expenses (C-G)'),]


Oats <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 9)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="OATS PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "Oats")
Oats <- Oats[Oats$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                         'Total Other Expenses (E)',
                                         'Return over Variable Expenses (C-D)',
                                         'Return over Total Expenses (C-G)'),]



DurumWheat <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 10)%>%
  select(1:4)%>%
  rename("Cost/Expen/Reve"="DURUM PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Black = NA)%>%
  mutate(Crop = "DurumWheat")
DurumWheat <- DurumWheat[DurumWheat$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                         'Total Other Expenses (E)',
                                         'Return over Variable Expenses (C-D)',
                                         'Return over Total Expenses (C-G)'),]


HardRedSpringWheat <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 11)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="HARD RED SPRING WHEAT PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "HardRedSpringWheat")
HardRedSpringWheat <- HardRedSpringWheat[HardRedSpringWheat$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                           'Total Other Expenses (E)',
                                                           'Return over Variable Expenses (C-D)',
                                                           'Return over Total Expenses (C-G)'),]

  

WinterWheat <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 12)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="WINTER WHEAT PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "WinterWheat")
WinterWheat <- WinterWheat[WinterWheat$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                               'Total Other Expenses (E)',
                                                               'Return over Variable Expenses (C-D)',
                                                               'Return over Total Expenses (C-G)'),]


Canola <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 13)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="CANOLA PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "Canola")
Canola <- Canola[Canola$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                           'Total Other Expenses (E)',
                                                           'Return over Variable Expenses (C-D)',
                                                           'Return over Total Expenses (C-G)'),]

Flax <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 14)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="FLAX PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "Flax")
Flax <- Flax[Flax$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                         'Total Other Expenses (E)',
                                         'Return over Variable Expenses (C-D)',
                                         'Return over Total Expenses (C-G)'),]



BrownMustard <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 15)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="BROWN MUSTARD PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Dark.Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "BrownMustard")

BrownMustard <- BrownMustard[BrownMustard$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                         'Total Other Expenses (E)',
                                         'Return over Variable Expenses (C-D)',
                                         'Return over Total Expenses (C-G)'),]




OrientalMustard <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 17)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="ORIENTAL MUSTARD PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Dark.Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "OrientalMustard")
OrientalMustard <- OrientalMustard[OrientalMustard $Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                                 'Total Other Expenses (E)',
                                                                 'Return over Variable Expenses (C-D)',
                                                                 'Return over Total Expenses (C-G)'),]


YellowMustard <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 18)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="YELLOW MUSTARD PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Dark.Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "Yellow Mustard")
YellowMustard <- YellowMustard[YellowMustard$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                                           'Total Other Expenses (E)',
                                                                           'Return over Variable Expenses (C-D)',
                                                                           'Return over Total Expenses (C-G)'),]


SunflowerOilseed <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 19)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="SUNFLOWER PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Dark.Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "SunflowerOilseed")
SunflowerOilseed <- SunflowerOilseed[SunflowerOilseed$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                                    'Total Other Expenses (E)',
                                                                    'Return over Variable Expenses (C-D)',
                                                                    'Return over Total Expenses (C-G)'),]


Soybean <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 20)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="SOYBEAN PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "Soybean")
Soybean <- Soybean[Soybean$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                  'Total Other Expenses (E)',
                                                  'Return over Variable Expenses (C-D)',
                                                  'Return over Total Expenses (C-G)'),]



DesiChickpea <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 21)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="CHICKPEAS PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Dark.Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "DesiChickpea")
DesiChickpea <- DesiChickpea[DesiChickpea$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                  'Total Other Expenses (E)',
                                                  'Return over Variable Expenses (C-D)',
                                                  'Return over Total Expenses (C-G)'),]



KabuliChickpealarge <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 22)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="CHICKPEAS PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Dark.Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "KabuliChickpeaLarge")
KabuliChickpealarge  <- KabuliChickpealarge [KabuliChickpealarge $Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                                 'Total Other Expenses (E)',
                                                                 'Return over Variable Expenses (C-D)',
                                                                 'Return over Total Expenses (C-G)'),]

KabuliChickpeasmall <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 23)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="CHICKPEAS PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  transform(Brown = as.numeric(Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Dark.Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "KabuliChickpeaSmall")
KabuliChickpeasmall  <- KabuliChickpeasmall[KabuliChickpeasmall$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                                                       'Total Other Expenses (E)',
                                                                                       'Return over Variable Expenses (C-D)',
                                                                                       'Return over Total Expenses (C-G)'),]



LargeGreenLentils <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 24)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="LARGE GREEN LENTILS PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "LargeGreenLentils")
LargeGreenLentils <- LargeGreenLentils[LargeGreenLentils$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                                 'Total Other Expenses (E)',
                                                                 'Return over Variable Expenses (C-D)',
                                                                 'Return over Total Expenses (C-G)'),]


RedLentils <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 25)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="RED LENTILS PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "RedLentils")
RedLentils <- RedLentils[RedLentils$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                           'Total Other Expenses (E)',
                                                           'Return over Variable Expenses (C-D)',
                                                           'Return over Total Expenses (C-G)'),]


EdibleGreenPeas <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 26)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="GREEN PEAS PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "EdibleGreenPeas")
EdibleGreenPeas <- EdibleGreenPeas[EdibleGreenPeas$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                           'Total Other Expenses (E)',
                                                           'Return over Variable Expenses (C-D)',
                                                           'Return over Total Expenses (C-G)'),]


EdibleYellowPeas <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 27)%>%
  select(1:5)%>%
  rename("Cost/Expen/Reve"="YELLOW PEAS PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3",
         "Dark Brown" = "...4",
         "Black" = "...5")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Crop = "EdibleYellowPeas ")
EdibleYellowPeas <- EdibleYellowPeas[EdibleYellowPeas$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                                          'Total Other Expenses (E)',
                                                                          'Return over Variable Expenses (C-D)',
                                                                          'Return over Total Expenses (C-G)'),]


BlackBean <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 28)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="BLACK BEAN PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Dark.Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "Black Bean")
BlackBean <- BlackBean[BlackBean$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                        'Total Other Expenses (E)',
                                                        'Return over Variable Expenses (C-D)',
                                                        'Return over Total Expenses (C-G)'),]

  

FabaBean <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 29)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="FABABEANS PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Black" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Brown = NA,
         Dark.Brown = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "FabaBean")
FabaBean  <- FabaBean[FabaBean$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                        'Total Other Expenses (E)',
                                                        'Return over Variable Expenses (C-D)',
                                                        'Return over Total Expenses (C-G)'),]

  

Camelina <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 30)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="CAMELINA PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Black = NA,
         Dark.Brown = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "Camelina")
Camelina <- Camelina[Camelina$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                      'Total Other Expenses (E)',
                                                      'Return over Variable Expenses (C-D)',
                                                      'Return over Total Expenses (C-G)'),]



CanarySeed <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 31)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="CANARYSEED PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Dark.Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Black = NA,
         Brown = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "Canary Seed")
CanarySeed <- CanarySeed[CanarySeed$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                     'Total Other Expenses (E)',
                                                     'Return over Variable Expenses (C-D)',
                                                     'Return over Total Expenses (C-G)'),]



Caraway <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 32)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="CARAWAY PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Black" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Dark.Brown = NA,
         Brown = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "Caraway")
Caraway <- Caraway[Caraway$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                  'Total Other Expenses *',
                                                  'Return over Variable Expenses (C-D)',
                                                  'Return over Total Expenses (C-G)'),]



Coriander <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 33)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="CORIANDER PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Dark.Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Dark.Brown = as.numeric(Dark.Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "Coriander")
Coriander <- Coriander[Coriander$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                  'Total Other Expenses (E)',
                                                  'Return over Variable Expenses (C-D)',
                                                  'Return over Total Expenses (C-G)'),]


Fenugreek <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 34)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="FENUGREEK PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Brown" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Brown = as.numeric(Brown))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Dark.Brown = NA,
         Black = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "Fenugreek")
Fenugreek <- Fenugreek[Fenugreek$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                        'Total Other Expenses (E)',
                                                        'Return over Variable Expenses (C-D)',
                                                        'Return over Total Expenses (C-G)'),]


Quinoa <- read_excel("./Data/DrylandcropProd/CropProdGuide2024.xlsx", 35)%>%
  select(1:3)%>%
  rename("Cost/Expen/Reve"="QUINOA PRODUCTION COSTS ($/ACRE) FOR SASKATCHEWAN 2024",
         "Black" = "...3")%>%
  subset(select = -c(...2))%>%
  na.omit()%>%
  slice(-c(1))%>%
  transform(Black = as.numeric(Black))%>%
  mutate_if(is.numeric, round, digits = 2)%>%
  mutate(Dark.Brown = NA,
         Brown = NA)%>%
  relocate(Cost.Expen.Reve, Brown, Dark.Brown, Black)%>%
  mutate(Crop = "Quinoa")
Quinoa <- Quinoa[Quinoa$Cost.Expen.Reve %in% c('Total Variable Expenses (D)',
                                                        'Total Other Expenses (E)',
                                                        'Return over Variable Expenses (C-D)',
                                                        'Return over Total Expenses (C-G)'),]

dry_crop_24 <- rbind(BlackBean, BrownMustard, Camelina, CanarySeed, Canola, Caraway,
                     Coriander, Corn, DesiChickpea, DurumWheat, EdibleGreenPeas,
                     EdibleYellowPeas, FabaBean,FeedBarley, Fenugreek, Flax,
                     HardRedSpringWheat, HybridFallRye, KabuliChickpealarge,
                     KabuliChickpeasmall,LargeGreenLentils, MaltBarley, Oats,
                     OrientalMustard, Quinoa, Soybean, SunflowerOilseed, WinterWheat,
                     YellowMustard)

write.csv(dry_crop_24,"./data/Processed/DrylandcropProd2024.csv")

###############################################################################
# Irrigation revenue

#read in .pdf format news article
article_path <- list.files("data/IrrigationProd/", pattern = "pdf$")

pdf_file = paste0("data/IrrigationProd//",article_path[1])

out <- extract_tables(pdf_file, pages = 7)
out_temp = out[[1]][,1:5] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V5[23] = out_temp$V3[23]
out_temp$V1[10] = "Equipment Fuel" 
out_temp <- out_temp%>%
  select(V1,V5)%>%
  mutate(Value = parse_number(out_temp$V5))%>%
  rename("Cost/Return" = V1)%>%
  select(-V5)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                      'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "HardWheat")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
HardWheat <- out_temp

out <- extract_tables(pdf_file, pages = 8)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair" 
out_temp$V1[31] = "Price $/bu (2-row select) " 
out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Durum")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
Durum <- out_temp


out <- extract_tables(pdf_file, pages = 9)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[31] = "Price $/bu (2-row select) " 
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "MaltBarley")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
MaltBarley <- out_temp



out <- extract_tables(pdf_file, pages = 10)
out_temp = out[[1]][,1:5] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[31] = "Price $/bu (2-row select) " 
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V5)%>%
  mutate(Value = parse_number(out_temp$V5))%>%
  rename("Cost/Return" = V1)%>%
  select(-V5)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "FeedBarley")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
FeedBarley <- out_temp



out <- extract_tables(pdf_file, pages = 11)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[31] = "Price $/bu (2-row select) " 
out_temp$V1[26] = "Land" 
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "MillingOats")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
MillingOats <- out_temp


out <- extract_tables(pdf_file, pages = 12)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[31] = "Price $/bu (2-row select) " 
out_temp$V1[26] = "Land" 
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp[-35,]
out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Canola")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
Canola <- out_temp


out <- extract_tables(pdf_file, pages = 13)
out_temp = out[[1]][,1:6] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[30] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V6)%>%
  mutate(Value = parse_number(out_temp$V6))%>%
  rename("Cost/Return" = V1)%>%
  select(-V6)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Soybean")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
Soybean <- out_temp


out <- extract_tables(pdf_file, pages = 14)
out_temp = out[[1]][,1:11] %>%
  as_tibble()%>%
  filter(V2 != "" | V11 != "")
out_temp <- out_temp[-1:-3,]
out_temp$V2[4] = "Fertilizer:N"
out_temp$V2[5] = "Fertilizer:P2O5"
out_temp$V2[6] = "Fertilizer:K2O"
out_temp$V11[24] = 455.90
out_temp$V2[24] = "Total Cash Costs"
out_temp$V11[25] = 272.50
out_temp$V2[25] = "Total Non-Cash Costs"
out_temp$V2[26] = "Gross Return" 
out_temp$V11[26] = 925
out_temp$V2[26] = "Gross Return" 
out_temp$V11[27] = 345
out_temp$V2[27] = "Net Return"
out_temp <- out_temp%>%
  select(V2,V11)%>%
  mutate(Value = parse_number(out_temp$V11))%>%
  rename("Cost/Return" = V2)%>%
  select(-V11)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Flax")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
Flax <- out_temp



out <- extract_tables(pdf_file, pages = 15)
out_temp = out[[1]][,1:3] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V3)%>%
  mutate(Value = parse_number(out_temp$V3))%>%
  rename("Cost/Return" = V1)%>%
  select(-V3)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Pea")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
Pea <- out_temp

out <- extract_tables(pdf_file, pages = 16)
out_temp = out[[1]][,1:3] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V3)%>%
  mutate(Value = parse_number(out_temp$V3))%>%
  rename("Cost/Return" = V1)%>%
  select(-V3)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "FabaBean")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
FabaBean <- out_temp


out <- extract_tables(pdf_file, pages = 17)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "RedLentil")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
RedLentil <- out_temp


out <- extract_tables(pdf_file, pages = 18)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return" 

out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "DryBeanPinto")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
DryBeanPinto <- out_temp


out <- extract_tables(pdf_file, pages = 19)
out_temp = out[[1]][,1:3] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V3)%>%
  mutate(Value = parse_number(out_temp$V3))%>%
  rename("Cost/Return" = V1)%>%
  select(-V3)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "DryBeanBlack")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
DryBeanBlack <- out_temp


out <- extract_tables(pdf_file, pages = 20)
out_temp = out[[1]][,1:3] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V3)%>%
  mutate(Value = parse_number(out_temp$V3))%>%
  rename("Cost/Return" = V1)%>%
  select(-V3)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "DryBeanSolidSeeded")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
DryBeanSolidSeeded <- out_temp


out <- extract_tables(pdf_file, pages = 21)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "WinterWheat")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
WinterWheat <- out_temp


out <- extract_tables(pdf_file, pages = 22)
out_temp = out[[1]][,1] %>%
  as_tibble()
out_temp <- out_temp%>%
  mutate(Value = parse_number(out_temp$value))%>%
  filter(Value != "" | Value != "")
out_temp$value[4] = "Fertilizer:N"
out_temp$value[5] = "Fertilizer:P2O5"
out_temp$value[6] = "Fertilizer:K2O"
out_temp$value[23] = "Farm Equipment & Building"
out_temp$value[10] = "Equipment Fuel" 
out_temp$value[14] = "Irrigation Repair"
out_temp$value[15] = "Total Non-Cash Costs"
out_temp$Value[15] = 542.09
out_temp$value[16] = "Total Non-Cash Costs"
out_temp$Value[16] = 272.50
out_temp$value[30] = "Gross Return"
out_temp$Value[30] = 950
out_temp$value[31] = "Net Return"
out_temp$Value[31] = 136
out_temp <- out_temp%>%
  rename("Cost/Return" = "value")
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "HybridFallRye")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
HybridFallRye <- out_temp


out <- extract_tables(pdf_file, pages = 23)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Quinoa")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
Quinoa<- out_temp

out <- extract_tables(pdf_file, pages = 24)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Hemp")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
Hemp <- out_temp

out <- extract_tables(pdf_file, pages = 25)
out_temp = out[[1]][,1:5] %>%
  as_tibble()%>%
  filter(V1 != "" | V5 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V5[4] = out_temp$V4[4]
out_temp$V5[21] = out_temp$V4[21]
out_temp$V5[25] = out_temp$V4[25]
out_temp$V5[26] = out_temp$V4[26]
out_temp$V5[27] = out_temp$V4[27]
out_temp$V5[31] = out_temp$V4[31]
out_temp$V1[31] = "Gross Return" 
out_temp <- out_temp%>%
  select(V1,V5)%>%
  mutate(Value = parse_number(out_temp$V5))%>%
  rename("Cost/Return" = V1)%>%
  select(-V5)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "CornGrain")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
CornGrain <- out_temp


out <- extract_tables(pdf_file, pages = 26)
out_temp = out[[1]][,1:5] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp <- out_temp%>%
  select(V1,V5)%>%
  mutate(Value = parse_number(out_temp$V5))%>%
  rename("Cost/Return" = V1)%>%
  select(-V5)
#CornGrazing <- out_temp

out <- extract_tables(pdf_file, pages = 27)
out_temp = out[[1]][,1:5] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[23] = "Total Cash Costs"
out_temp$V1[33] = "Gross Return"
out_temp$V5[34] = -295
out_temp <- out_temp%>%
  select(V1,V5)%>%
  mutate(Value = parse_number(out_temp$V5))%>%
  rename("Cost/Return" = V1)%>%
  select(-V5)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "CornSilage")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
CornSilage <- out_temp



out <- extract_tables(pdf_file, pages = 28)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[27] = "Total Non-Cash Costs"
out_temp$V4[33] = -295

out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Cereal Silage")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
CerealSilage <- out_temp



out <- extract_tables(pdf_file, pages = 29)
out_temp = out[[1]][,1:5] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[32] = "Gross Return"
out_temp$V5[33] = -143
out_temp <- out_temp%>%
  select(V1,V5)%>%
  mutate(Value = parse_number(out_temp$V5))%>%
  rename("Cost/Return" = V1)%>%
  select(-V5)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Seedling Alfalfa")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
SeedlingAlfalfa <- out_temp


out <- extract_tables(pdf_file, pages = 30)
out_temp = out[[1]][,1:3] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
#out_temp$V1[32] = "Gross Return"
out_temp$V3[33] = -94
out_temp <- out_temp%>%
  select(V1,V3)%>%
  mutate(Value = parse_number(out_temp$V3))%>%
  rename("Cost/Return" = V1)%>%
  select(-V3)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Established Alfalfa")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
EstablishedAlfalfa <- out_temp


out <- extract_tables(pdf_file, pages = 31)
out_temp = out[[1]][,1:3] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
#out_temp$V1[32] = "Gross Return"
out_temp$V3[33] = -54
out_temp <- out_temp%>%
  select(V1,V3)%>%
  mutate(Value = parse_number(out_temp$V3))%>%
  rename("Cost/Return" = V1)%>%
  select(-V3)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "EstablishedAlfalfa_3")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
EstablishedAlfalfa_3 <- out_temp



out <- extract_tables(pdf_file, pages = 32)
out_temp = out[[1]][,1:3] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[23] = "Total Cash Costs"
#out_temp$V3[33] = -54
out_temp <- out_temp%>%
  select(V1,V3)%>%
  mutate(Value = parse_number(out_temp$V3))%>%
  rename("Cost/Return" = V1)%>%
  select(-V3)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "SeedPotato")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
SeedPotato <- out_temp



out <- extract_tables(pdf_file, pages = 33)
out_temp = out[[1]][,1:4] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[23] = "Total Cash Costs"
#out_temp$V3[33] = -54
out_temp <- out_temp%>%
  select(V1,V4)%>%
  mutate(Value = parse_number(out_temp$V4))%>%
  rename("Cost/Return" = V1)%>%
  select(-V4)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return','Net Return',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "TablePotato")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
TablePotato <- out_temp


out <- extract_tables(pdf_file, pages = 34)
out_temp = out[[1]][,1:3] %>%
  as_tibble()%>%
  filter(V1 != "" | V3 != "")
out_temp$V1[4] = "Fertilizer:N"
out_temp$V1[5] = "Fertilizer:P2O5"
out_temp$V1[6] = "Fertilizer:K2O"
out_temp$V1[23] = "Farm Equipment & Building"
out_temp$V1[10] = "Equipment Fuel" 
out_temp$V1[14] = "Irrigation Repair"
out_temp$V1[23] = "Total Cash Costs"
out_temp <- out_temp%>%
  select(V1,V3)%>%
  mutate(Value = parse_number(out_temp$V3))%>%
  rename("Cost/Return" = V1)%>%
  select(-V3)
out_temp <- out_temp[out_temp$`Cost/Return` %in% c('Gross Return†','Net Return†',
                                                   'Total Cash Costs', 'Total Non-Cash Costs'),]%>%
  mutate(Crop = "Carrot")
out_temp$`Cost/Return`[1] = "Total Variable Expenses (D)"
out_temp$`Cost/Return`[2] = "Total Other Expenses (E)"
out_temp$`Cost/Return`[3] = "Return Over Variable Expenses (C-D)"
out_temp$`Cost/Return`[4] = "Return Over Total Expenses (C-G)"
Carrot <- out_temp

Irrigated_crop_23 <- rbind(Canola,Carrot,CerealSilage,CornGrain,CornSilage,DryBeanBlack,
                     DryBeanPinto,DryBeanSolidSeeded,Durum,EstablishedAlfalfa,EstablishedAlfalfa_3,
                     FabaBean,FeedBarley,Flax,HardWheat,Hemp,HybridFallRye,MaltBarley,
                     MillingOats,Pea,Quinoa,RedLentil,SeedlingAlfalfa,SeedPotato,
                     Soybean,TablePotato,WinterWheat)

write.csv(Irrigated_crop_23,"./data/Processed/IrrigatecropProd2023.csv")





