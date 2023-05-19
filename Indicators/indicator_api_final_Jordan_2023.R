#########################
# Description: Produce final indicators for upload to EdStats
# Author: Brian Stacy
# Created: 1/9/2020
##########################

# Load libraries
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(skimr)
library(readxl)
library(readr)
library(WDI)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

########################
# Do file organization
########################
# 1. Read in latest indicator metadata 
# 2. Use api_data function to read in collected survey data
# 3. Merge this survey data with metadata to produce final data for EdStats upload


##########################
#read in api_data function
##########################


# 
# This function takes five arguments: a directory for the cleaned school data, a directory for the cleaned survey of public officials data, and a directory for the exper data, a country code, and a year.
# The directory refers to a location of the final cleaned data containing the file "final_complete_school_data.dta" produced in the school_data_cleaner.R code
# The directory refers to a location of the final cleaned data containing the file "public_officials_survey_data.dta" produced in the school_data_cleaner.R code
# The directory refers to a location of the final cleaned data containing the file "expert_dta_final.dta" produced in the school_data_cleaner.R code
# The function then reads in this dataset and produces a formatted data frame that is compatible with EdStats containing our indicator information
# 
# Example:
# data_dir1 <- "C:\\Documents\Peru\2019\Data\clean\School"
# data_dir2 <- "C:\\Documents\Peru\2019\Data\clean\Public_officials"
# data_dir3 <- "C:\\Documents\Peru\2019\Data\clean\Expert_Survey"

# PER_data_2019 <- api_data(data_dir1, data_dir2, data_dir3, 'PER', 2019)


source('R/api_template_fun.R', echo=TRUE)
api_template <- api_template_fun()




##########################
#use api_data function to pull in data collected
##########################

###########
# Jordan
###########

# Example:

#specify path to data
data_dir <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/CNT/JOR/JOR_2023_GEPD/JOR_2023_GEPD_v01_M/Data/"




#pull data for learning poverty from wbopendata
#list of indicators
ind_list <- c( "SE.LPV.PRIM", "SE.LPV.PRIM.FE", "SE.LPV.PRIM.MA", "SE.LPV.PRIM.OOS",  "SE.LPV.PRIM.OOS.FE", "SE.LPV.PRIM.OOS.MA",
               "SE.LPV.PRIM.BMP", "SE.LPV.PRIM.BMP.FE", "SE.LPV.PRIM.BMP.MA", "SE.PRM.TENR", "SE.PRM.TENR.FE", "SE.PRM.TENR.MA")
#read in data from wbopendata
#get WDI metadata infor
# cache_list<-wbstats::wbcache()
# wbopendat<-wbstats::wb(country="JOR", 
#             indicator=ind_list,
#             startdate=2015,
#             enddate=2015,
#             return_wide = T,
#             removeNA=FALSE)

wbopendat<-WDI(country="JO", indicator=ind_list, start=2000, end=2022, extra=T) %>%
  fill(starts_with("SE.")) %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  filter(row_number()==n())

#read in databases for indicators

load(paste(data_dir, "School/school_indicators_data_anon.RData", sep="/"))
load(paste(data_dir, "Public_Officials/public_officials_indicators_data_anon.RData", sep="/"))
# 
# 
expert_df <- read_stata(paste(data_dir, 'Policy_Survey/expert_dta_final.dta', sep="/" ))

#score expert data (this requires a lot of hard coding and transcribing)
#read in data
defacto_dta_learners <- readxl::read_xlsx(path=paste(data_dir, 'Other_Indicators/Learners_defacto_indicators.xlsx', sep="/"),  .name_repair = 'universal')
defacto_dta_learners_shaped<-data.frame(t(defacto_dta_learners[-1]), stringsAsFactors = FALSE)
colnames(defacto_dta_learners_shaped) <- defacto_dta_learners$Question

#create indicators
defacto_dta_learners_final <- defacto_dta_learners_shaped %>%
  rownames_to_column() %>%
  filter(rowname=='Scoring') %>%
  select(-rowname)
# 
# 
# 
# #financing
finance_df <- readxl::read_xlsx(path=paste(data_dir, 'Other_Indicators/Finance_scoring.xlsx', sep="/"),  .name_repair = 'universal')
finance_df_shaped<-data.frame(t(finance_df[-1]), stringsAsFactors = FALSE)
colnames(finance_df_shaped) <- finance_df$Question

#create indicatorsTS
finance_df_final <- finance_df_shaped %>%
  rownames_to_column() %>%
  filter(rowname=='Scores') %>%
  select(-rowname)


source('R/api_data_fun_JOR_2023.R')

#Tags
practice_tags <- "SE.PRM.PROE|SE.LPV.PRIM|SE.PRM.LERN|SE.PRM.TENR|SE.PRM.EFFT|SE.PRM.CONT|SE.PRM.ATTD|SE.PRM.LCAP|SE.PRM.PEDG|SE.LPV"

#function to create score data for a specified country and year
api_metadata_fn <- function(cntry, yr) {
  api_metadata_fn_p <- api_final %>%
    rename(Indicator.Name='Indicator Name') %>%
    filter(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name)) %>%
    rename(  'Indicator Name'=Indicator.Name) %>%
    select(Series, `Indicator Name`, value) %>%
    mutate(value=if_else(value==-999,as.numeric(NA),as.numeric(value))) %>%
    mutate(
      value_metadata=case_when(
        grepl("SE.LPV.PRIM$|SE.LPV.PRIM.1", Series) & value >15 ~ "Needs Improvement",
        grepl("SE.LPV.PRIM$|SE.LPV.PRIM.1", Series) & value <=15 & value>10 ~ "Caution",
        grepl("SE.LPV.PRIM$|SE.LPV.PRIM.1", Series) & value <=10 ~ "On Target",               
        value <85 ~ "Needs Improvement",
        value >=85 & value<90 ~ "Caution",
        value >=90 ~ "On Target",
        TRUE ~ "N/A"
      ))
  
  
  api_metadata_fn_c <- api_final %>%
    rename(Indicator.Name='Indicator Name') %>%
    filter(!(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name))) %>%
    rename(  'Indicator Name'=Indicator.Name) %>%
    select(Series, `Indicator Name`, value) %>%
    mutate(value=if_else(value==-999,as.numeric(NA),as.numeric(value))) %>%
    mutate(
      value_metadata=case_when(
        value <3 ~ "Needs Improvement",
        value >=3 & value<4 ~ "Caution",
        value >=4 ~ "On Target",
        TRUE ~ "N/A"
      ))
  
  api_metadata_fn_p %>%
    bind_rows(api_metadata_fn_c) %>%
    arrange(Series) %>%
    mutate(year=yr,
           cty_or_agg="cty",
           countrycode=cntry,
           value=round(value,1),
           Series=str_replace_all(Series, "SE.LPV","SE.GEPD"))
}


JOR_data_2023 <- api_metadata_fn('JOR', 2023)


#export Indicators_metatdata section
write_excel_csv(JOR_data_2023, paste( 'GEPD_Indicators_API_JOR_2023.csv',sep=""))

write_excel_csv(JOR_data_2023, paste(data_dir,'Indicators/', 'GEPD_Indicators_API_JOR_2023.csv',sep=""))



