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
library(naniar)
library(vtable)
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



############################
#Read in indicators.md file
###########################
#Read in list of indicators
indicators <- read_csv(here::here('Indicators','indicators.csv'))
indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)


indicators <- indicators %>%
  select(-c('X1', 'X8'))

indicator_names <-  indicators$indicator_tag
indicator_names <- sapply(indicator_names, tolower)

names(indicators)<-make.names(names(indicators), unique=TRUE)


#get metadata on indicators
#Read in list of indicators
indicator_choices <- read_delim(here::here('Indicators','indicators_choices.md'), delim="|", trim_ws=TRUE)
indicator_choices <- indicator_choices %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)


indicator_choices <- indicator_choices %>%
  select(-c('X1', 'X6')) %>%
  rename("Source Note"="How is the indicator scored?" ) 


names(indicator_choices)<-make.names(names(indicator_choices), unique=TRUE)


#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- indicators$indicator_tag



#Read in Sergio's excel with subquestions to include
subquestions<-read_excel('GEPD_Indicators_Info_v5.xlsx', sheet='SubQuestions') 

df<-indicators %>%
  left_join(subquestions) %>%
  select(Series, indicator_tag, Indicator.Name,  starts_with('Column_'), starts_with('Sub')) 



df_overall <- df %>%
  select(Series, Indicator.Name, indicator_tag ) 
  

df_defacto_dejure <- df %>%
  filter(grepl("Policy Lever", Indicator.Name )) %>%
  select(Series, Indicator.Name ) %>%
  mutate('De Facto' = "DF",
         'De Jure' = "DJ") %>%
  pivot_longer(cols=c('De Facto', 'De Jure'),
               names_to="type",
               values_to="type_val") %>%
  mutate(Series=paste(Series, type_val, sep="."),
         Indicator.Name=paste("(",type,") ",Indicator.Name, sep="")) %>%
  select(Series, Indicator.Name)
  
# Prepare a data frame, on which we can add values

##########################
# Now create database for upload with correct format for EdStats
###########################
#Pivot longer
df_longer<-df %>%
  pivot_longer(cols=c(
    'Subquestion_1', 'Subquestion_2', 'Subquestion_3',
    'Subquestion_4', 'Subquestion_5', 'Subquestion_6',
    'Subquestion_7', 'Subquestion_8','Subquestion_9',
    'Subquestion_10', 'Subquestion_11', 'Subquestion_12',
    'Subquestion_13', 'Subquestion_14', 'Subquestion_15',
    'Subquestion_16', 'Subquestion_17', 'Subquestion_18',
    'Subquestion_19', 'Subquestion_20'),
    values_to='short_desc') %>%
  filter(short_desc!="") %>%
  filter(short_desc!="Overall") %>%
  pivot_longer(cols=c(    "Column_2", "Column_3", "Column_4","Column_5", "Column_6"),
               values_to='urban_rural_gender',
               names_to = 'urban_rural_gender_name')  %>%
  select(-urban_rural_gender_name) %>%
  filter(urban_rural_gender!="") 

#break up name into two components
# (type=="Column" & num!="1") ~ paste(Series, substr(short_desc,1,1), sep="."),
# (type=="Column" & num!="1") ~ paste(Indicator.Name, short_desc, sep=" - "),

#now modify API IDs
df_sub<-df_longer %>%
  separate(name, c("type", "num"), "_") %>%
  mutate(Series=paste(Series, num, sep="."))  %>% #add tag for subindicators
  mutate(Series=case_when( #add tag for urban/rural gender
    ( urban_rural_gender=="Overall") ~ Series,
    ( urban_rural_gender!="Overall") ~ paste(Series, substr(urban_rural_gender,1,1), sep="."),
    TRUE ~ Series  )) %>%
  mutate(Indicator.Name= short_desc) %>%
  mutate(Indicator.Name=case_when( #add tag for urban/rural gender for indicator name
    (urban_rural_gender=="Overall") ~ Indicator.Name,
    (urban_rural_gender!="Overall") ~ paste(Indicator.Name, urban_rural_gender, sep=" - "),
    TRUE ~ Indicator.Name  )) %>%
  select(-Column_1, -type, -num, -urban_rural_gender) 

api_template  <- df_overall %>%
  bind_rows(df_defacto_dejure) %>%
  bind_rows(df_sub) %>%
  arrange(Series) %>%
  select(Series, Indicator.Name)

indicator_match  <- df_overall %>%
  bind_rows(df_defacto_dejure) %>%
  bind_rows(df_sub) %>%
  arrange(Series) %>%
  select(Series, Indicator.Name, indicator_tag)

#add extra metadata
api_template <- api_template %>%
  mutate(Source="Global Education Policy Dashboard",
         'Source Organization'="World Bank") %>%
  left_join(indicator_choices) %>%
  mutate(Source.Note = gsub("(\n|<br/>)"," ",Source.Note)) %>%
  mutate(Source.Note = str_replace(Source.Note, "-", ",")) %>%
  rename('Source Note'=Source.Note,
         'Indicator Name'=Indicator.Name) %>%
  select(-c(indicator_tag, Value))



##########################
#use api_data function to pull in data collected
##########################

###########
# Rwanda
###########

# Example:

#specify path to data
data_dir <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/CNT/RWA/RWA_2020_GEPD/RWA_2020_GEPD_v01_M/Data/"




#pull data for learning poverty from wbopendata
#list of indicators
ind_list <- c( "SE.LPV.PRIM", "SE.LPV.PRIM.FE", "SE.LPV.PRIM.MA", "SE.LPV.PRIM.OOS",  "SE.LPV.PRIM.OOS.FE", "SE.LPV.PRIM.OOS.MA",
               "SE.LPV.PRIM.BMP", "SE.LPV.PRIM.BMP.FE", "SE.LPV.PRIM.BMP.MA", "SE.PRM.TENR", "SE.PRM.TENR.FE", "SE.PRM.TENR.MA")
#read in data from wbopendata
#get WDI metadata infor
cache_list<-wbstats::wbcache()
wbopendat<-wbstats::wb(country="RWA", 
            indicator=ind_list,
            startdate=2000,
            enddate=2020,
            return_wide = T,
            removeNA=FALSE)

wbopendat<-WDI(country="RW", indicator=ind_list, start=2000, end=2020, extra=T) %>%
  filter(!is.na(SE.PRM.TENR) &!is.na(country)) %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  filter(row_number()==n())

#read in databases for indicators

load(paste(data_dir, "School/school_indicators_data_anon.RData", sep="/"))
load(paste(data_dir, "Public_Officials/public_officials_indicators_data_anon.RData", sep="/"))
# 
# 
expert_df <- read_stata(paste(data_dir, 'Expert_Survey/expert_dta_final.dta', sep="/" ))

#score expert data (this requires a lot of hard coding and transcribing)
#read in data
defacto_dta_learners <- readxl::read_xlsx(path=paste(data_dir, 'Other_Indicators/Learners_defacto_indicators.xlsx', sep="/"),  .name_repair = 'universal') %>%
  filter(!is.na(indicator))
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


source('R/api_data_fun_RWA.R')

RWA_data_2019 <- api_final


#export Indicators_metatdata section
write_excel_csv(api_final, 'GEPD_Indicators_API_RWA.csv')

write_excel_csv(api_final, paste(data_dir,'Indicators/', 'GEPD_Indicators_API_RWA.csv',sep=""))



