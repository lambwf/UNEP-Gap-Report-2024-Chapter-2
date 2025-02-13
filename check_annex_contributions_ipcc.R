### check the contributions of AI vs non-AI countries using the IPCC AR6 dataset


rm(list = ls())
library(tidyverse)
library(countrycode)
source("https://raw.githubusercontent.com/lambwf/Codebase/main/load_gcb_countries_v2023.R")

load('C:/Users/lamw/ownCloud/Projects/AR6-Emissions-trends-and-drivers/Data/data_edgar_ghg.RData')
#load('C:/Users/lamw/ownCloud/Projects/AR6-Emissions-trends-and-drivers/Data/data_land_co2.RData')



# Load Global Carbon Budget (GCB) CO2 land use change (LUC) emissions (https://globalcarbonbudget.org/carbonbudget2023/)

data_gcb_co2_luc <- load_gcb_countries_luc(
  readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx',range="A8:GT182",sheet=2),
  readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx',range="A8:GT182",sheet=3),
  readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx',range="A8:GT182",sheet=4))

data_egr_2024 <- data_ghg %>% 
  filter(gas=="CO2 Fossil") %>% 
  group_by(iso,country,year) %>% 
  summarise(value=sum(value,na.rm=TRUE))

data_egr_2024 <- left_join(data_egr_2024,data_gcb_co2_luc %>% select(iso,year,CO2_LUC=mean) %>% mutate(year=as.numeric(year)))
data_egr_2024 <- data_egr_2024 %>% 
  mutate(value=value/1e9) %>% 
  mutate(value=value+CO2_LUC)


cc_ai <- read.csv("https://raw.githubusercontent.com/openclimatedata/countrygroups/refs/heads/main/data/annex-one.csv")

data_egr_2024 <- left_join(data_egr_2024,cc_ai %>% select(iso=Code) %>% mutate(group="ai"))
data_egr_2024 <- data_egr_2024 %>% 
  mutate(group=ifelse(is.na(group),"n-ai",group))

data_egr_2024 <- data_egr_2024 %>% 
  group_by(group,year) %>% 
  summarise(value=sum(value,na.rm=TRUE))

data_egr_2024 <- data_egr_2024 %>% 
  group_by(year) %>% 
  mutate(total=sum(value,na.rm=TRUE))

data_egr_2024 <- data_egr_2024 %>% 
  mutate(ratio=value/total)



#########################

data <- edgar_ghg %>%
  group_by(ISO,country,year) %>% 
  summarise(GHG=sum(GHG,na.rm=TRUE))

data <- left_join(data,data_gcb_co2_luc %>% select(ISO=iso,year,CO2_LUC=mean) %>% mutate(year=as.numeric(year)))
data <- data %>% 
  mutate(GHG=GHG/1e9)

data <- data %>% 
  mutate(GHG_incl_luc=GHG+CO2_LUC)


cc_ai <- read.csv("https://raw.githubusercontent.com/openclimatedata/countrygroups/refs/heads/main/data/annex-one.csv")

data <- left_join(data,cc_ai %>% select(ISO=Code) %>% mutate(group="ai"))
data <- data %>% 
  mutate(group=ifelse(is.na(group),"n-ai",group))

data <- data %>% 
  group_by(group,year) %>% 
  summarise(GHG=sum(GHG,na.rm=TRUE),GHG_incl_luc=sum(GHG_incl_luc,na.rm=TRUE))

data <- data %>% 
  group_by(year) %>% 
  mutate(total_GHG_incl_luc=sum(GHG_incl_luc,na.rm=TRUE)) %>% 
  filter(year<2020)

data <- data %>% 
  mutate(ratio=GHG_incl_luc/total_GHG_incl_luc)
