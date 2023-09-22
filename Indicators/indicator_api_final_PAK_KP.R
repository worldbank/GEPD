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

#specify path to data
#Country name and year of survey
country <-'PAK'
iso3 <- 'PAK'
country_name <- "Pakistan"
province <- "KP"
year <- '2022'

strata <- c('Gender', 'Location')

#set directory to bring in data
if (str_to_lower(Sys.getenv("USERNAME")) == "wb469649"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  data_dir <- file.path("C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/CNT",country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_M", sep="_"),paste0("Data/",province))
  gen_dir <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/General/"
  project_folder  <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT/"
  download_folder <-file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/raw/", sep="/"))
  confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/confidential/", sep="/"))
  save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/anonymized/", sep="/"))
  backup_onedrive="no"
  save_folder_onedrive <- file.path(paste("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/",country_name,year,"Data/clean/", sep="/"))
  anonymized_dir <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/"
  
} else if (str_to_lower(Sys.getenv("USERNAME")) == "wb577189") {
  
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT"
  data_dir <- file.path("C:/Users/wb577189/OneDrive - WBG/GEPD/CNT",country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_M"),paste0("Data/",province))
  download_folder <-file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/raw/", sep="/"))
  confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/confidential/", sep="/"))
  save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/anonymized/", sep="/"))
  backup_onedrive="no"
  save_folder_onedrive <- file.path(paste("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work",country_name,year,"Data/clean/", sep="/"))
} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")
  anonymized_dir <- "C:/Users/wb577189/OneDrive - WBG/GEPD/"
}








#pull data for learning poverty from wbopendata
#list of indicators
ind_list <- c( "SE.LPV.PRIM", "SE.LPV.PRIM.FE", "SE.LPV.PRIM.MA", "SE.LPV.PRIM.OOS",  "SE.LPV.PRIM.OOS.FE", "SE.LPV.PRIM.OOS.MA",
               "SE.LPV.PRIM.BMP", "SE.LPV.PRIM.BMP.FE", "SE.LPV.PRIM.BMP.MA", "SE.PRM.TENR", "SE.PRM.TENR.FE", "SE.PRM.TENR.MA")
#read in data from wbopendata
#get WDI metadata infor
# cache_list<-wbstats::wbcache()
# wbopendat<-wbstats::wb(country="NER", 
#             indicator=ind_list,
#             startdate=2015,
#             enddate=2015,
#             return_wide = T,
#             removeNA=FALSE)

wbopendat<-WDI(country='PK', indicator=ind_list, start=2000, end=2023, extra=T) %>%
  fill(starts_with("SE.")) %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  filter(row_number()==n())

#add new learning poverty number
g4_prof <- read_excel(path=paste0(gen_dir,'/lpv_edstats_1205.xls'), sheet='WDI_indicators') %>%
  filter(countrycode==iso3) %>%
  group_by(indicator) %>%
  arrange(as.numeric(year)) %>%
  filter(row_number()==n()) %>%
  select(countrycode, indicator, year, value) %>%
  ungroup() %>%
  pivot_wider(
    names_from = indicator,
    values_from=value
  )


#read in UIS data on 4.1.1a
uis_df <- read_csv(file='https://geo.uis.unesco.org/data/sdg-benchmarks.csv') %>%
  filter(country_id==iso3) %>%
  group_by(ind_nber) %>%
  arrange(as.numeric(year)) %>%
  filter(row_number()==n()) %>%
  select(country_id, ind_nber, year, latest_value) %>%
  ungroup() %>%
  pivot_wider(
    names_from = ind_nber,
    values_from=latest_value,
    values_fill=as.numeric(NA),
    names_prefix = 'SDG'
  ) %>%
  mutate(across(starts_with('SDG'), as.numeric))

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


source('R/api_data_fun_PAK.R')

#Tags
practice_tags <- "SE.PRM.PROE|SE.LPV.PRIM|SE.LPV.PRIM.BMP|SE.PRM.LERN|SE.PRM.TENR|SE.PRM.EFFT|SE.PRM.CONT|SE.PRM.ATTD|SE.PRM.LCAP|SE.PRM.PEDG|SE.LPV"

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
        grepl("SE.LPV.PRIM$", Series) & value >15 ~ "Needs Improvement",
        grepl("SE.LPV.PRIM$", Series) & value <=15 & value>10 ~ "Caution",
        grepl("SE.LPV.PRIM$", Series) & value <=10 ~ "On Target",               
        grepl("SE.LPV.PRIM.1$", Series) & value >15 ~ "Needs Improvement",
        grepl("SE.LPV.PRIM.1$", Series) & value <=15 & value>10 ~ "Caution",
        grepl("SE.LPV.PRIM.1$", Series) & value <=10 ~ "On Target",                 
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


PAK_KP_data_2022 <- api_metadata_fn('PAK-KP', 2022)


#export Indicators_metatdata section
write_excel_csv(PAK_KP_data_2022, paste( 'GEPD_Indicators_API_PAK_KP.csv',sep=""))

write_excel_csv(PAK_KP_data_2022, paste(data_dir,'/Indicators/', 'GEPD_Indicators_API_PAK_KP.csv',sep=""))



