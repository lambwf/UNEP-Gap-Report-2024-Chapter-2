
rm(list = ls())
library(openxlsx)
library(tidyverse)
library(countrycode)

source("https://raw.githubusercontent.com/lambwf/Codebase/main/load_edgar_v9.R")
source("https://raw.githubusercontent.com/lambwf/Codebase/main/load_gcb_countries_v2023.R")


## load EDGAR data (https://edgar.jrc.ec.europa.eu/dataset_ghg80)

edgar_co2 <- read.xlsx("sources/not public/EDGAR_2024_CO2_1970_2023_v2.xlsx",sheet=2,startRow=9)
edgar_ch4 <- read.xlsx("sources/not public/EDGAR_2024_CH4_1970_2023_v2.xlsx",sheet=2,startRow = 9)
edgar_n2o <- read.xlsx("sources/not public/EDGAR_2024_N2O_1970_2023_v2.xlsx",sheet=2,startRow = 9)
edgar_fgas <- read.xlsx("sources/not public/EDGAR_2024_F-gases_1970_2023_v2.xlsx",sheet=2,startRow = 9)
data_ghg <- load_edgar(edgar_co2,edgar_ch4,edgar_n2o,edgar_fgas)


## join GWPs and calculate CO2e

gwps <- read.csv("https://raw.githubusercontent.com/openclimatedata/globalwarmingpotentials/main/globalwarmingpotentials.csv",skip = 10)

data_ghg$gas <- gsub("-","",data_ghg$gas)
data_ghg <- left_join(data_ghg,gwps %>% select(gas=Species,AR6GWP100)) %>% 
  mutate(AR6GWP100=ifelse(gas=="CO2",1,AR6GWP100))

## remove biogenic CO2

data_ghg <- data_ghg %>% 
  filter(gas!="CO2bio")


## check all gwps joined

check <- data_ghg %>% filter(is.na(AR6GWP100))
check <- data_ghg %>% select(gas,AR6GWP100) %>% distinct()

data_ghg <- data_ghg %>% mutate(value_gwp=value*AR6GWP100)


list_fgases <- data_ghg %>% 
  select(gas) %>% 
  distinct() %>% 
  mutate(group=ifelse(gas=="CO2","CO2 Fossil",NA)) %>% 
  mutate(group=ifelse(gas=="CH4","CH4",group)) %>% 
  mutate(group=ifelse(gas=="N2O","N2O",group)) %>% 
  mutate(group=ifelse(is.na(group),"F-gases",group))

data_ghg <- left_join(data_ghg,list_fgases,by="gas")
data_ghg <- data_ghg %>% 
  mutate(gas=group) %>% 
  group_by(iso,country,code,code_description,gas,year) %>% 
  summarise(value=sum(value_gwp,na.rm=TRUE))

## Gg to tons

data_ghg$value = data_ghg$value*1000


## Grassi et al. LULUCF data (https://zenodo.org/records/7190601)
# 
# data_lulucf <- read.xlsx('sources/not public/Grassi_et_al_LULUCF.xlsx')
# data_lulucf <- gather(data_lulucf,year,value,-country,-iso)
# data_lulucf <- data_lulucf %>% 
#   mutate(year=as.numeric(year)) %>% 
#   filter(!is.na(year)) %>% 
#   mutate(value=value*1e6) %>% 
#   mutate(code="3B") %>% 
#   mutate(code_description="Land use change") %>% 
#   mutate(gas="CO2 LULUCF") %>% 
#   select(iso,country,code,code_description,gas,year,value) %>% 
#   filter(year<=2021)
# 
# data_lulucf_v2 <- read.xlsx('sources/not public/LULUCF NGHGI data for UNEP 2024.xlsx',startRow = 3) %>% 
#   rename(category=LAND.CATEGORY,country=UNFCCC.country,iso=country.code)
# data_lulucf_v2 <- gather(data_lulucf_v2,year,value,`2000`:`2023`) %>% 
#   filter(category=="LULUCF net") %>% 
#   mutate(value=value*1e6) %>%
#   mutate(year=as.numeric(year)) %>% 
#   mutate(gas="CO2 LULUCF") %>% 
#   mutate(code="3B") %>% 
#   mutate(code_description="Land use change") %>% 
#   filter(!is.na(year)) %>% 
#   filter(!is.na(iso)) %>% 
#   select(iso,country,code,code_description,gas,year,value) %>% 
#   filter(year<=2021)
# 
# data_lulucf <- rbind(data_lulucf %>% filter(year<2000),data_lulucf_v2)
# data_lulucf <- data_lulucf %>% 
#   arrange(country,iso,year)
# 
# data_ghg <- rbind(data_ghg,data_lulucf)


### consistent names

newnames <- data_ghg %>% 
  ungroup() %>% 
  select(iso) %>% 
  distinct() %>% 
  mutate(newname=countrycode(iso,"iso3c","country.name")) %>% 
  mutate(newname=ifelse(iso=="AIR","Intl. Aviation",newname)) %>% 
  mutate(newname=ifelse(iso=="SEA","Intl. Shipping",newname)) %>% 
  mutate(newname=ifelse(iso=="SCG","Serbia and Montenegro",newname)) %>% 
  mutate(newname=ifelse(iso=="ANT","Netherlands Antilles",newname)) %>% 
  mutate(newname=ifelse(iso=="WLD","World",newname))

data_ghg <- left_join(data_ghg,newnames,by="iso") %>% 
  mutate(country=newname) %>% 
  select(-newname) %>% 
  filter(!is.na(iso))

## aggregate sectors

data_ghg <- left_join(data_ghg,read.xlsx("sources/cc_sectors.xlsx"),by=join_by(code, code_description))

data_ghg$gas <- as.factor(data_ghg$gas)
data_ghg$gas <- fct_relevel(data_ghg$gas,"CO2 Fossil","CH4","N2O","F-gases")

data_ghg$sector_lv1 <- as.factor(data_ghg$sector_lv1)
data_ghg$sector_lv1 <- fct_reorder(data_ghg$sector_lv1,data_ghg$sector_lv1_order)

data_ghg$sector_lv2 <- as.factor(data_ghg$sector_lv2)
data_ghg$sector_lv2 <- fct_reorder(data_ghg$sector_lv2,data_ghg$sector_lv2_order)

data_ghg$sector_lv2 <- as.factor(data_ghg$sector_lv2)
data_ghg$sector_lv2 <- fct_reorder(data_ghg$sector_lv2,data_ghg$sector_lv2_order)

data_ghg$sector_lv3 <- as.factor(data_ghg$sector_lv3)
data_ghg$sector_lv3 <- fct_reorder(data_ghg$sector_lv3,data_ghg$sector_lv3_order)


data_edgar <- data_ghg %>% 
  group_by(across(c(-code,-code_description,-sector_lv2_codes,-sector_lv3_codes))) %>% 
  summarise(value=sum(value,na.rm=TRUE))

save(data_edgar,file="data/data_edgar.RData")



##########



# Load Global Carbon Budget (GCB) CO2 land use change (LUC) emissions (https://globalcarbonbudget.org/carbonbudget2023/)

data_gcb_luc <- load_gcb_countries_luc(
  readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx',range="A8:GT182",sheet=2),
  readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx',range="A8:GT182",sheet=3),
  readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx',range="A8:GT182",sheet=4))

data_gcb_luc <- data_gcb_luc %>% 
  filter(year>=1970) %>% 
  #filter(country=="Global") %>% 
  select(iso,country,year,value=mean) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(value=value*1e9)


save(data_gcb_luc,file="data/data_gcb_luc.RData")











