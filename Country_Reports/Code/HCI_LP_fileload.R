library(tidyverse)
library(haven)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(stringr)
library(scales)
library(readxl)
library(glue)
library(httr)
library(jsonlite)
library(wbstats)
library(ggrepel)
library(here)

#########################
# Launch Code
########################

#########################
# Learning poverty
########################

##########
# Pull Ed Stats Data using World Bank API
##########

#get WDI metadata infor
cache_list<-wbstats::wbcache()
country_list <- wbstats::wbcountries()
country_list_hci <- country_list %>%
  rename(country_name = country) %>%
  select(iso3c, country_name, region, regionID, admin, adminID, income, incomeID)

# make request to World Bank API and get a list of all EdStats indicators (including learning poverty)
EdStatsRequest <- GET(url = "http://api.worldbank.org/v2/indicator?per_page=20000&format=json&source=12")
EdStatsResponse <- content(EdStatsRequest, as = "text", encoding = "UTF-8")

# Parse the JSON content and convert it to a data frame.
EdStatsJSON <- fromJSON(EdStatsResponse, flatten = TRUE) %>%
  data.frame()

# Create a list of indicators to pull from edstats. More can be added as needed
learning_pov_indicators<- 'SE.LPV.PRIM' # Pupils below minimum reading proficiency at end of primary (%). Low GAML threshold

#set reference year to 2019 (Year of survey)
reference_year<-as.numeric(year)

# Now use wbstats package in R to pull from World Bank API

learning_poverty_df <-wbstats::wb(country="all", #by specifying all, we can get a list of all aggregates (such as region or income level)
                                  indicator=learning_pov_indicators,
                                  startdate=reference_year-10,
                                  enddate=reference_year,
                                  return_wide = T,
                                  removeNA=TRUE,
                                  cache = cache_list
) %>%
  filter(((reference_year-as.numeric(date))<=10) & (reference_year>=as.numeric(date))) %>%  #filter out years outside reference window of 10 years 
  mutate_if(is.character, str_trim) %>% 
  group_by(iso3c, country) %>%   #group by country to create one observation per country which is the latest value
  filter(as.numeric(date)==max(as.numeric(date))) %>%
  rename(date_lp = date,
         country_name = country)

#########################
# Learning adjusted years in schooling
########################
##########
# Pull  Data using World Bank API
##########

#Pulll data for the Human Capital INdex - Learning Adjusted Years of Schooling:
hci_df <- GET(url = "http://api.worldbank.org/v2/country/all/indicator/HD.HCI.LAYS?per_page=500&format=json") %>%
  content( as = "text", encoding = "UTF-8") %>%
  fromJSON( flatten = TRUE) %>%
  data.frame() %>%
  rename(iso3c = countryiso3code,
         date_hci = date,
         country_name = country.value,
         HD.HCI.LAYS = value)

#add region, admin and income markers for countries
hci_df <- hci_df %>%
  left_join(country_list_hci, by = c("iso3c", "country_name"))

hci_region <- hci_df%>%
  filter(!is.na(region)) %>%
  group_by(region, regionID) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(iso3c = regionID,
         country_name = region) %>%
  ungroup() %>%
  select(iso3c, date_hci, country_name, HD.HCI.LAYS)

hci_admin <- hci_df%>%
  filter(!is.na(admin)) %>%
  group_by(admin, adminID) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(iso3c = adminID,
         country_name = admin) %>%
  ungroup() %>%
  select(iso3c, date_hci, country_name, HD.HCI.LAYS)

hci_income <- hci_df%>%
  filter(!is.na(income)) %>%
  group_by(income, incomeID) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(iso3c = incomeID,
         country_name = income) %>%
  ungroup() %>%
  select(iso3c, date_hci, country_name, HD.HCI.LAYS)

hci_main <- hci_df %>%
  select(iso3c, date_hci, country_name, HD.HCI.LAYS)

hci_final <- rbind(hci_main, hci_region, hci_admin, hci_income)
hci_final <- hci_final %>%
  mutate_if(is.character, str_trim)

#Creating file with both learning poverty and LAYS values
#de-deplicated list of LP and LAYS countries
iso3c_lp <- learning_poverty_df %>%
  ungroup() %>%
  select(iso3c, country_name)
iso3c_hci <- hci_final %>%
  select(iso3c, country_name)

hci_lp_list <- rbind(as.data.frame(iso3c_lp),as.data.frame(iso3c_hci))
hci_lp_list <- hci_lp_list %>%
  distinct() %>%
  arrange(country_name)
#Joining LP and LAYS values
hci_lp_final <- hci_lp_list %>%
  left_join(learning_poverty_df) %>%
  left_join(hci_final) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  left_join(country_list_hci) %>%
  mutate(region =   if_else(is.na(regionID), country_name, region),
         regionID = if_else(is.na(regionID), iso3c, regionID)) %>%
  mutate(admin =   case_when(!is.na(admin) ~ admin,
                             is.na(admin) ~ region),
         adminID = case_when(!is.na(adminID) ~ adminID,
                             is.na(adminID) ~ regionID),
         tab = 0,
         SE.LPV.PRIM = round(as.numeric(SE.LPV.PRIM), digits =0),
         HD.HCI.LAYS = round(as.numeric(HD.HCI.LAYS), digits =1),
         LPV_decimal = SE.LPV.PRIM/100
  )