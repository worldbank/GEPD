#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(skimr)

library(vtable)
#NOTE:  The R script to pull the data from the API should be run before this file

#move working directory to github main folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')

#Create function to save metadata for each question in each module
#The attr function retrieves metadata imported by haven. E.g. attr(school_dta$m1s0q2_code, "label")
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}




#read in public officials interview file
public_officials_dta<-read_dta(file.path(download_folder, "public_officials.dta"))
public_officials_metadata<-makeVlist(public_officials_dta)

vtable(public_officials_dta)

#Create a function which will generate new binary variable using case_when, but 
#if value is misisng it will generate binary variable to be missing
#This is done a lot so will create function for it.
#e.g. school_absent=case_when(
#         m2sbq6_efft==6  ~ 1,
#         m2sbq6_efft!=6   ~ 0,
#         is.na(m2sbq6_efft) ~ as.numeric(NA))
bin_var <- function(var, val) {
  case_when(
    var==val  ~ 1,
    var!=val   ~ 0,
    is.na(var) ~ as.numeric(NA))
}

#rename a few key variables up front
public_officials_dta<- public_officials_dta %>%
  mutate(enumerator_name=m1s0q1_name_other  ,
         enumerator_number=if_else(!is.na(m1s0q1_name),m1s0q1_name, as.double(m1s0q1_number_other)) ,
         school_district=if_else(!is.na(school_district_preload),school_district_preload, district_other),
         school_province=if_else(!is.na(school_province_preload),school_province_preload, region_other),
         survey_time=m1s0q8,
         lat=m1s0q9__Latitude,
         lon=m1s0q9__Longitude,
         govt_tier=factor(m1s0q2_name, levels=c(1,2,3), labels=c('Ministry of Education (or equivalent)', 
                                                                 'Regional office (or equivalent)',
                                                                 'District office (or equivalent)')),
         consent=m1s2q2,
         occupational_category=DEM1q1,
         professional_service=bin_var(DEM1q1,1),
         sub_professional_service=bin_var(DEM1q1,2),
         admin=bin_var(DEM1q1,3),
         position=DEM1q2,
         director=bin_var(DEM1q12n,1), 
         responsible_finance_planning=bin_var(DEM1q5__1,1),
         responsible_hiring_teachers=bin_var(DEM1q5__2,1),
         responsible_monitoring_performance=bin_var(DEM1q5__3,1),
         responsible_none=bin_var(DEM1q5__4,1),
         education=factor(DEM1q11, levels=c(1,2,3,4,5,6,7, 97, 900, 998), labels=c('Primary school',
                                                                                   'Middle school',
                                                                                   'Secondary school',
                                                                                   'Diploma / Other post-high-school certificate',
                                                                                   'Undergraduate degree',
                                                                                   'Masters degree',
                                                                                   'PhD',
                                                                                   'Other (dont specify)',
                                                                                   'Dont know',
                                                                                   'Refused to answer' )),
         gender=DEM1q15,
         salary_differential=DEM1q13,
         private_sector_two_years=bin_var(DEM1q12,1)
                  )

#list info that will be useful to keep in each indicator dataframe
preamble_info <- c('interview__id', 'school_district', 'school_province','location', 'govt_tier',
                   'enumerator_name', 'enumerator_number', 'survey_time', 'lat', 'lon', 'consent',
                   'occupational_category', 'professional_service', 'sub_professional_service', 'admin', 'position',
                   'responsible_finance_planning', 'responsible_hiring_teachers', 'responsible_monitoring_performance','responsible_none',
                   'education','gender')




#use dplyr select(contains()) to search for variables with select tags and create separate databases by indicator
#This will make the information for each indicator contained in an independent database
#Will need to join the school level information with teacher level questionnaire information for some indicators.  This will be done later.

public_officials_dta_clean <-public_officials_dta %>%
  dplyr::select(preamble_info, starts_with('DEM'), starts_with('NLG'), starts_with('ACM'), starts_with('QB'), starts_with('IDM'), starts_with('ORG'), starts_with('ENUM')) %>%
  dplyr::select(-starts_with("enumerators_preload"))



#######################################
# Score Public Officials Data
#######################################

######
#clean up missing values, etc
######

public_officials_dta_clean <-public_officials_dta_clean %>%
  mutate_at(vars(starts_with('NLG'), starts_with('ACM'), starts_with('QB'), starts_with('IDM'), starts_with('ORG')), ~case_when(.x==900 ~ as.numeric(NA),
                                                                                                                                .x==998 ~ as.numeric(NA),
                                                                                                                                .x>=1 & .x<=5 ~ as.numeric(.x),
                                                                                                                                is.na(.x) ~ as.numeric(NA)))

########
# National Learning Goals
########
public_officials_dta_clean$nlg_length<-length(grep(x=colnames(public_officials_dta_clean), pattern="NLG"))


#calculate item scores
public_officials_dta_clean <- public_officials_dta_clean %>%
  mutate(national_learning_goals=rowSums(.[grep(x=colnames(public_officials_dta_clean), pattern="NLG")], na.rm=TRUE)/(nlg_length))


########
# Mandates and Accountability
########
public_officials_dta_clean$acm_length<-length(grep(x=colnames(public_officials_dta_clean), pattern="ACM"))


#calculate item scores
public_officials_dta_clean <- public_officials_dta_clean %>%
  mutate(mandates_accountability=rowSums(.[grep(x=colnames(public_officials_dta_clean), pattern="ACM")], na.rm=TRUE)/(acm_length))


########
# Quality of Bureaucracy
########
public_officials_dta_clean$qb_length<-length(grep(x=colnames(public_officials_dta_clean), pattern="QB"))


#calculate item scores
public_officials_dta_clean <- public_officials_dta_clean %>%
  mutate(quality_bureaucracy=rowSums(.[grep(x=colnames(public_officials_dta_clean), pattern="QB")], na.rm=TRUE)/(qb_length))


########
# Impartial Decision Making
########
public_officials_dta_clean$idm_length<-length(grep(x=colnames(public_officials_dta_clean), pattern="IDM"))


#calculate item scores
public_officials_dta_clean <- public_officials_dta_clean %>%
  mutate(impartial_decision_making=rowSums(.[grep(x=colnames(public_officials_dta_clean), pattern="IDM")], na.rm=TRUE)/(idm_length))


#list of Bureaucracy indicators
bureau_ind<-c( 'national_learning_goals','mandates_accountability' ,'quality_bureaucracy', 'impartial_decision_making')

public_officials_dta_clean <-public_officials_dta_clean %>%
  dplyr::select(preamble_info, bureau_ind, starts_with('DEM'), starts_with('NLG'), starts_with('ACM'), starts_with('QB'), starts_with('IDM'), starts_with('ORG'), starts_with('ENUM')) 


write.csv(public_officials_dta_clean, file = file.path(save_folder, "public_officials_survey_data.csv"))
write_dta(public_officials_dta_clean, path = file.path(save_folder, "public_officials_survey_data.dta"), version = 14)


################################
#Store Key Created Datasets
################################

#saves the following in R and stata format

data_list <- c( 'public_officials_dta_clean')

save(public_officials_dta_clean, file = file.path(save_folder, "public_officials_survey_data.RData"))
#loop and produce list of data tables



