### LAND USE ###

library(tidyverse)
library(stringr)
library(dplyr)

### 2006

Landuse_2006 <- read.csv("Landuse_2006.csv") # adjust code to match your working directory

sum(Landuse_2006$HECTARES)

LU_2006 <- Landuse_2006 %>% 
  group_by(LU_DESC) %>% 
  summarise(
  area = sum(HECTARES)
  )
LU_2006

HouAcc <- LU_2006 %>% 
  filter(LU_DESC %in% c("Detached Home", "Residential Rural / Rural Lifestyle (0.4 to 20 Ha)", 
                        "Vacant Residential Home Site / Surveyed Lot", 
                        "Vacant Residential Rural / Rural Lifestyle (0.4 to 20ha)"))

bush <- LU_2006 %>% 
  filter(LU_DESC %in% c("Creek Reserve (Fresh Water)", "Forest Reserves - Public", "Reserved Land",
                       "Softwood Plantation", "Nature Reserve Area", "Water Supply",
                       "Unclassified Private Land", "Native Hardwood (standing timber)"))

agri <- LU_2006 %>% 
  filter(LU_DESC %in% c("Mixed farming and grazing  (generally more than 20ha)", 
                        "Livestock Production (Dairy Cattle)", "Livestock Production (Beef Cattle)", 
                        "Market Garden - Vegetables (generally less than 20ha plantings)"))

protected <- LU_2006 %>% 
  filter(LU_DESC %in% c("National Park - Land", "Conservation Area - Public"))

sum(HouAcc$area)
sum(bush$area)
sum(agri$area)
sum(protected$area)
sum(LU_2006$area)
mean(agri$area)
mean(HouAcc$area)
sum(HouAcc$area, bush$area, agri$area, protected$area)

### 2008

Landuse_2008 <- read.csv("Landuse_2008.csv") # adjust code to match your working directory

sum(Landuse_2008$HECTARES)

LU_2008 <- Landuse_2008 %>% 
  group_by(LU_DESCS) %>% 
  summarise(
    area = sum(HECTARES)
  )
LU_2008

HouAcc <- LU_2008 %>% 
  filter(LU_DESCS %in% c("Detached Home", "Vacant Residential Home Site / Surveyed Lot", 
                        "Vacant Residential Rural / Rural Lifestyle (0.4 to 20ha)"))

bush <- LU_2008 %>% 
  filter(LU_DESCS %in% c("Forestry (Commercial Timber Production)", "Creek Reserve (fresh Water)",
                         "Forest Reserves - Public", "Nature Reserve Area", "Reserved Land", "Water Supply",
                         "Unclassified Private Land", "Native Hardwood (standing timber)"))

agri <- LU_2008 %>% 
  filter(LU_DESCS %in% c("Mixed farming and grazing  (generally more than 20ha)", 
                        "Livestock Production (Dairy Cattle)", "Livestock Production (Beef Cattle)", 
                        "Market Garden - Vegetables (generally less than 20ha plantings)"))

protected <- LU_2008 %>% 
  filter(LU_DESCS %in% c("National Park - Land", "Conservation Area - Public"))

sum(HouAcc$area)
sum(bush$area)
sum(agri$area)
sum(protected$area)
sum(LU_2008$area)
mean(agri$area)
mean(HouAcc$area)
sum(HouAcc$area, bush$area, agri$area, protected$area)

ag_protec <- Landuse_2008 %>% 
  filter(LU_DESCS %in% c("Mixed farming and grazing  (generally more than 20ha)", 
                         "Livestock Production (Dairy Cattle)", "Livestock Production (Beef Cattle)", 
                         "Market Garden - Vegetables (generally less than 20ha plantings)"))
ag_protec_area <- ag_protec %>% 
  filter(HECTARES > 40)
ag_protec_area
sum(ag_protec_area$HECTARES)

### 2010

Landuse_2010 <- read.csv("Landuse_2010.csv") # adjust code to match your working directory

sum(Landuse_2010$HECTARES)

LU_2010 <- Landuse_2010 %>% 
  group_by(LU_DESC) %>% 
  summarise(
    area = sum(HECTARES)
  )
LU_2010

HouAcc <- LU_2010 %>% 
  filter(LU_DESC %in% c("Detached Home", "Vacant Residential Home Site / Surveyed Lot", 
                        "Residential Rural / Rural Lifestyle (0.4 to 20 Ha)",
                         "Vacant Residential Rural / Rural Lifestyle (0.4 to 20ha)"))

bush <- LU_2010 %>% 
  filter(LU_DESC %in% c("Softwood Plantation", "Forest Reserves - Public", "Reserved Land", "Water Supply",
                        "Nature Reserve", "Unclassified Private Land", "Native Hardwood (standing timber)"))

agri <- LU_2010 %>% 
  filter(LU_DESC %in% c("Mixed farming and grazing  (generally more than 20ha)", 
                         "Livestock Production (Dairy Cattle)", "Livestock Production (Beef Cattle)", 
                         "Market Garden - Vegetables (generally less than 20ha plantings)"))

protected <- LU_2010 %>% 
  filter(LU_DESC %in% c("National Park - Land"))

sum(HouAcc$area)
sum(bush$area)
sum(agri$area)
sum(protected$area)
sum(LU_2010$area)
mean(agri$area)
mean(HouAcc$area)
sum(HouAcc$area, bush$area, agri$area, protected$area)

### 2012

Landuse_2012 <- read.csv("Landuse_2012.csv") # adjust code to match your working directory

sum(Landuse_2012$HECTARES)

LU_2012 <- Landuse_2012 %>% 
  group_by(LU_DESC) %>% 
  summarise(
    area = sum(HECTARES)
  )
LU_2012

HouAcc <- LU_2012 %>% 
  filter(LU_DESC %in% c("Detached Home", "Vacant Residential Home Site / Surveyed Lot", 
                        "Residential Rural / Rural Lifestyle (0.4 to 20 Ha)",
                        "Vacant Residential Rural / Rural Lifestyle (0.4 to 20ha)",
                        "Miscellaneous Buildings on Residential Land", 
                        "Short Term Holiday Accommodation"))

bush <- LU_2012 %>% 
  filter(LU_DESC %in% c("Softwood Plantation", "Forest Reserves - Public", "Reserved Land", "Water Supply",
                        "Nature Reserve", "Native Hardwood (standing timber)", "Unclassified Private Land",
                        "Forestry (Commercial Timber Production)"))

agri <- LU_2012 %>% 
  filter(LU_DESC %in% c("Mixed farming and grazing  (generally more than 20ha)", 
                        "Livestock Production (Dairy Cattle)", "Livestock Production (Beef Cattle)"))

protected <- LU_2012 %>% 
  filter(LU_DESC %in% c("National Park - Land"))

sum(HouAcc$area)
sum(bush$area)
sum(agri$area)
sum(protected$area)
sum(LU_2012$area)
mean(agri$area)
mean(HouAcc$area)
sum(HouAcc$area, bush$area, agri$area, protected$area)

### 2014

Landuse_2014 <- read.csv("Landuse_2014.csv") # adjust code to match your working directory

sum(Landuse_2014$HECTARES)

LU_2014 <- Landuse_2014 %>% 
  group_by(LU_DESC) %>% 
  summarise(
    area = sum(HECTARES)
  )
LU_2014

HouAcc <- LU_2014 %>% 
  filter(LU_DESC %in% c("Detached Home", "Separate House and Curtilage",
                        "Vacant Residential Rural / Rural Lifestyle (0.4 to 20ha)",
                        "Miscellaneous Building on Residential Rural Land (>1 and<20ha)", 
                        "Short Term Holiday Accommodation", "Miscellaneous Buildings on Residential Land",
                        "Vacant Residential Home Site / Surveyed Lot"))

bush <- LU_2014 %>% 
  filter(LU_DESC %in% c("Softwood Plantation", "Forest Reserves - Public", "Reserved Land", "Water Supply",
                        "Nature Reserve", "Native Hardwood (standing timber)", "Unclassified Private Land",
                        "Native Vegetation / Bushland Without Covenant / Agreement"))

agri <- LU_2014 %>% 
  filter(LU_DESC %in% c("Mixed farming and grazing  (generally more than 20ha)", 
                        "Livestock Production (Dairy Cattle)", "Livestock Production (Beef Cattle)"))

protected <- LU_2014 %>% 
  filter(LU_DESC %in% c("National Park - Land"))

sum(HouAcc$area)
sum(bush$area)
sum(agri$area)
sum(protected$area)
sum(LU_2014$area)
mean(agri$area)
mean(HouAcc$area)
sum(HouAcc$area, bush$area, agri$area, protected$area)

### 2016

Landuse_2016 <- read.csv("Landuse_2016.csv") # adjust code to match your working directory

sum(Landuse_2016$HECTARES)

LU_2016 <- Landuse_2016 %>% 
  group_by(LU_DESC) %>% 
  summarise(
    area = sum(HECTARES)
  )
LU_2016

HouAcc <- LU_2016 %>% 
  filter(LU_DESC %in% c("Detached Home", "Separate House and Curtilage",
                        "Vacant Residential Rural / Rural Lifestyle (0.4 to 20ha)",
                        "Miscellaneous Building on Residential Rural Land (>1 and<20ha)", 
                        "Short Term Holiday Accommodation", "Miscellaneous Buildings on Residential Land",
                        "Vacant Residential Home Site / Surveyed Lot"))

bush <- LU_2016 %>% 
  filter(LU_DESC %in% c("Softwood Plantation", "Forest Reserves - Public", "Reserved Land", "Water Supply",
                        "Nature Reserve", "Native Hardwood (standing timber)", "Unclassified Private Land",
                        "Native Vegetation / Bushland Without Covenant / Agreement"))

agri <- LU_2016 %>% 
  filter(LU_DESC %in% c("Mixed farming and grazing  (generally more than 20ha)", 
                        "Livestock Production (Dairy Cattle)", "Livestock Production (Beef Cattle)"))

protected <- LU_2016 %>% 
  filter(LU_DESC %in% c("National Park - Land"))

sum(HouAcc$area)
sum(bush$area)
sum(agri$area)
sum(protected$area)
sum(LU_2016$area)
mean(agri$area)
mean(HouAcc$area)
sum(HouAcc$area, bush$area, agri$area, protected$area)

### 2017

Landuse_2017 <- read.csv("Landuse_2017.csv") # adjust code to match your working directory

sum(Landuse_2017$HECTARES)

LU_2017 <- Landuse_2017 %>% 
  group_by(LU_DESC) %>% 
  summarise(
    area = sum(HECTARES)
  )
LU_2017

HouAcc <- LU_2017 %>% 
  filter(LU_DESC %in% c("Residential Rural / Rural Lifestyle (0.4 to 20 Hectares)", 
                        "Vacant Residential Rural / Rural Lifestyle (0.4 to 20 Hectares)",
                        "Detached Home", "Single Residential Accommodation",
                        "Vacant Residential Home Site/Surveyed Lot", 
                        "Short Term Holiday Accommodation", "Separate House and Curtilage",
                        "Miscellaneous Improvements on Residential Rural Land (0.4 to 20 Hectares)",
                        "Miscellaneous Improvements on Residential Land"))

bush <- LU_2017 %>% 
  filter(LU_DESC %in% c("Softwood Plantation", "Forest Reserves - Public", "Hardwood Plantation",
                        "Nature Reserve", "Native Hardwood (standing timber)", "Reserved Land", "Water Supply",
                        "Native Vegetation / Bushland Without Covenant/Agreement"))

agri <- LU_2017 %>% 
  filter(LU_DESC %in% c("Mixed Farming and Grazing", "Livestock Production - Beef Cattle", 
                        "Livestock Production - Dairy Cattle", 
                        "Mixed farming and grazing  (generally more than 20 ha)"))

protected <- LU_2017 %>% 
  filter(LU_DESC %in% c("National Park - Land"))

sum(HouAcc$area)
sum(bush$area)
sum(agri$area)
sum(protected$area)
sum(LU_2017$area)
mean(agri$area)
mean(HouAcc$area)
sum(HouAcc$area, bush$area, agri$area, protected$area)

