########################################
# Input raw data files/Excel files used in analysis
# Brian Stacy
# March 27, 2020
########################################

# Load Packages
library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(wbstats)

#Set the country name here
country <- "Jordan"
year <- "2019"

##########
# Pull Ed Stats Data using World Bank API
##########

# make request to World Bank API and get a list of all EdStats indicators (including learning poverty)
EdStatsRequest <- GET(url = "http://api.worldbank.org/v2/indicator?per_page=20000&format=json&source=12")
EdStatsResponse <- content(EdStatsRequest, as = "text", encoding = "UTF-8")

# Parse the JSON content and convert it to a data frame.
EdStatsJSON <- fromJSON(EdStatsResponse, flatten = TRUE) %>%
  data.frame()

# Create a list of indicators to pull from edstats.  these are the overall learning poverty indicator, out of school children, 
# and below minimum proficiency children.  More can be added as needed
learning_pov_indicators<- c(
  'SE.LPV.PRIM'	, # Pupils below minimum reading proficiency at end of primary (%). Low GAML threshold
  'SE.LPV.PRIM.OOS'		, # Primary school age children out-of-school (%)	
  'SE.LPV.PRIM.BMP'		 # Pupils below minimum reading proficiency at end of primary (%). Low GAML threshold	
 )


#get WDI metadata infor
cache_list<-wbstats::wbcache()
country_list <- wbstats::wbcountries()

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
    filter(((reference_year-as.numeric(date))<=10) & (reference_year>=as.numeric(date))) %>% #filter out years outside reference window of 10 years     
    group_by(iso3c, country) %>%
    filter(as.numeric(date)==max(as.numeric(date))) #group by country to create one observation per country which is the latest value     
  
  
#Also we can pull data for the Human Capital INdex - Learning Adjusted Years of Schooling:
hci_df <- GET(url = "http://api.worldbank.org/v2/country/all/indicator/HD.HCI.LAYS?per_page=300&format=json") %>%
  content( as = "text", encoding = "UTF-8") %>%
  fromJSON( flatten = TRUE) %>%
  data.frame()
  
