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
library(srvyr)
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



iso3='GAB'


##########################
#use api_data function to pull in data collected
##########################

###########
# Sierra Leone
###########

# Example:

if (Sys.getenv("USERNAME") == "WB469649" | Sys.getenv("USERNAME") == "wb469649"){
  
  data_dir <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_M/Data"
  gen_dir <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/General/"
  
}else if (Sys.getenv("USERNAME") == "wb577189"){
  
  data_dir <- "C:/Users/wb577189/OneDrive - WBG/GEPD/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_M/Data"
}


options(survey.lonely.psu="adjust")
item <- 'ecd_literacy_student_knowledge'


stat_df <- ecd_dta_anon


#turn into a function of item
tab_calc <- function(item) {
  
  tab_break <- stat_df %>%
    #create column named indicator that evaluates expression in indicator argument
    mutate(
      VALUE=eval(parse(text=item))
      ) %>%
        filter(!is.na(strata_prob)
        ) %>%
        mutate(school_weight=1/strata_prob) %>%
        select(VALUE, c('Province', 'private', 'rural'), ecd_student_male, school_weight, hashed_school_code, ecd_assessment__id ) %>%
        pivot_longer(cols='VALUE',
                     names_to = 'indicators',
                     values_to='value') %>%
        as_survey_design(
          id=c(hashed_school_code, ecd_assessment__id),
          strata=c('Province', 'private', 'rural'),
          weight=school_weight) %>%
        group_by(ecd_student_male) %>%
        summarise(mean=survey_mean(value, na.rm=T, vartype=c('se', 'ci','var')),
                  N=sum(!(is.na(value))))
      
      
      tab_ovl <- stat_df %>%
        #create column named indicator that evaluates expression in indicator argument
        mutate(
          VALUE=eval(parse(text=item))
          ) %>%
            filter(!is.na(ipw)
            ) %>%
            mutate(school_weight=1/strata_prob) %>%
            select(VALUE, c('Province', 'private', 'rural'), ecd_student_male, school_weight, hashed_school_code, ecd_assessment__id ) %>%
            pivot_longer(cols='VALUE',
                         names_to = 'indicators',
                         values_to='value') %>%
            as_survey_design(
              id=c(hashed_school_code, ecd_assessment__id),
              strata=c('Province', 'private', 'rural'),
              weight=c('school_weight')) %>%
            ungroup() %>%
            summarise(mean=survey_mean(value, na.rm=T, vartype=c('se', 'ci','var')),
                      N=sum(!(is.na(value))))
          
      #bind tables
      tab <- bind_rows(tab_break, tab_ovl)
      tab
          
}

# do for ecd_student_knowledge, ecd_student_math_knowledge, ecd_student_literacy_knowledge

tab_calc('ecd_student_knowledge')
tab_calc('ecd_math_student_knowledge')
tab_calc('ecd_literacy_student_knowledge')
tab_calc('ecd_exec_student_knowledge')
tab_calc('ecd_soc_student_knowledge')

