#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(skimr)
library(spatstat)

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


#Read in list of indicators
indicators <- read_delim(here::here('Indicators','indicators.md'), delim="|", trim_ws=TRUE)
indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)

#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- indicators$indicator_tag


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
         district_code=district,
         region_code=region,
         district=if_else(!is.na(school_district_preload),school_district_preload, district_other),
         province=if_else(!is.na(school_province_preload),school_province_preload, region_other),
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


###############################
# Read in School Data for comparison to public officials answers
###############################

school_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/School", sep="/"))

load(file=paste(school_folder, "school_indicators_data.RData", sep="/"))

currentDate<-c("2019-07-22")
sample_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/sampling/", sep="/"))
sample_frame_name <- paste(sample_folder,"/school_sample_",currentDate,".RData", sep="")

load(sample_frame_name)

#compare data collected to original sample
school_dta_short <- school_dta_short %>%
  mutate(codigo.modular=as.numeric(school_code_preload)) %>%
  left_join(data_set_updated) %>%
  mutate(longitude=as.character(longitude)) %>%
  mutate(latitude=as.character(latitude)) %>%
  mutate(lat=if_else(is.na(lat), as.numeric(latitude), lat),
         lon=if_else(is.na(lon), as.numeric(longitude), lon),
         school_ipw=weights) %>%
  mutate(school_ipw=if_else(is.na(school_ipw), median(school_ipw, na.rm=T), school_ipw)*total_4th) %>%
  mutate(school_ipw=school_ipw/sum(school_ipw, na.rm = T))

weights<-school_dta_short %>%
  group_by(school_code) %>%
  summarise(school_ipw=mean(school_ipw))

final_indicator_data_INPT <- final_indicator_data_INPT %>%
  left_join(weights) %>%
  filter(!is.na(school_ipw))

class_size <- weighted.mean(final_indicator_data_INPT$m4scq4_inpt, w=final_indicator_data_INPT$school_ipw, na.rm=TRUE)

school_absence <- school_dta_short %>%
  filter(!is.na(school_ipw))

absence <- weighted.mean(school_absence$absence_rate, w=school_absence$school_ipw, na.rm=TRUE)
############################
#Clean up idiosyncratic variables
#############################

# Clean up some variables that need to be reversed coded, so that 5 is best and 1 is worst

attitude_fun_rev  <- function(x) {
  case_when(
    x==99 ~ as.numeric(NA),
    x==1 ~ 5,
    x==2 ~ 4,
    x==3 ~ 3,
    x==4 ~ 2,
    x==5 ~ 1
  )
}

#create list of these variables
var_rev_list<-c('QB2q2',  'QB4q4a', 'QB4q4b', 'QB4q4c', 'QB4q4d', 'QB4q4e', 'QB4q4f', 'QB4q4g',
                'IDM1q1', 'IDM1q2' )

public_officials_dta <- public_officials_dta %>%
  mutate_at(var_rev_list, attitude_fun_rev)

#scale some variables that ask integers as 1-5 (e.g. motivation)
public_officials_dta <- public_officials_dta %>%
  mutate(avg_class_size_guess=QB1q2,
         avg_absence_guess=QB1q1,
         motivation_relative_start=QB4q2, 
         proportion_reported_underperformance=IDM1q3,
         proportion_broke_rules=IDM3q1,
         proportion_contracts_political=IDM3q2,
         proportion_producement_political=IDM3q3,
         DEM1q13=as.numeric(DEM1q13)) %>%
  mutate(QB1q2= case_when(
            between(abs(QB1q2-class_size)/class_size,0,10) ~ 5, #between 0-10% of actual value gets 5 points
            between(abs(QB1q2-class_size)/class_size,10,20) ~ 4, #between 10-20% of actual value gets 4 points
            between(abs(QB1q2-class_size)/class_size,20,30) ~ 3, #between 20-30% of actual value gets 3 points
            between(abs(QB1q2-class_size)/class_size,30,40) ~ 2, #between 30-40% of actual value gets 2 points
            between(abs(QB1q2-class_size)/class_size,40,100) ~ 1 #between 40-10% of actual value gets 1 points
  ),
         QB1q1= case_when(
            between(abs(QB1q1-absence)/absence,0,10) ~ 5, #between 0-10% of actual value gets 5 points
            between(abs(QB1q1-absence)/absence,10,20) ~ 4, #between 10-20% of actual value gets 4 points
            between(abs(QB1q1-absence)/absence,20,30) ~ 3, #between 20-30% of actual value gets 3 points
            between(abs(QB1q1-absence)/absence,30,40) ~ 3, #between 30-40% of actual value gets 3 points
            between(abs(QB1q1-absence)/absence,40,100) ~ 1 #between 40-10% of actual value gets 1 points
  ),
         QB4q2= case_when(
           QB4q2>=120 ~ 5,
           QB4q2>=110 ~ 4,
           QB4q2>=100 ~ 3,
           QB4q2>=90 ~ 2,
           QB4q2>=80 ~ 1,
           TRUE ~ 1),
         IDM1q3=case_when(
           (IDM1q3>=0 & IDM1q3<=5) ~ 5,
           (IDM1q3>5 & IDM1q3<=10) ~ 4,
           (IDM1q3>10 & IDM1q3<=15) ~ 3,
           (IDM1q3>15 & IDM1q3<=20) ~ 2,
           TRUE ~ 1),
         IDM3q1=case_when(
           (IDM3q1>=0 & IDM3q1<=5) ~ 5,
           (IDM3q1>5 & IDM3q1<=10) ~ 4,
           (IDM3q1>10 & IDM3q1<=15) ~ 3,
           (IDM3q1>15 & IDM3q1<=20) ~ 2,
           TRUE ~ 1),
         IDM3q2=case_when(
           (IDM3q2>=0 & IDM3q2<=5) ~ 5,
           (IDM3q2>5 & IDM3q2<=10) ~ 4,
           (IDM3q2>10 & IDM3q2<=15) ~ 3,
           (IDM3q2>15 & IDM3q2<=20) ~ 2,
           TRUE ~ 1),
         IDM3q3=case_when(
           (IDM3q3>=0 & IDM3q3<=5) ~ 5,
           (IDM3q3>5 & IDM3q3<=10) ~ 4,
           (IDM3q3>10 & IDM3q3<=15) ~ 3,
           (IDM3q3>15 & IDM3q3<=20) ~ 2,
           TRUE ~ 1)
  )
    


#list info that will be useful to keep in each indicator dataframe
preamble_info <- c('interview__id', 'school_district_preload', 'school_province_preload',
                   'region_code', 'district_code', 'district', 'province','location', 'govt_tier',
                   'enumerator_name', 'enumerator_number', 'survey_time', 'lat', 'lon', 'consent',
                   'occupational_category', 'professional_service', 'sub_professional_service', 'admin', 'position',
                   'responsible_finance_planning', 'responsible_hiring_teachers', 'responsible_monitoring_performance','responsible_none',
                   'education','gender', 'director_hr')


#include list of constructed variables
constr_list <- c('avg_class_size_guess', 'avg_absence_guess', 'motivation_relative_start', 'proportion_reported_underperformance', 
                 'proportion_broke_rules', 'proportion_contracts_political', 'proportion_producement_political'
)

#use dplyr select(contains()) to search for variables with select tags and create separate databases by indicator
#This will make the information for each indicator contained in an independent database
#Will need to join the school level information with teacher level questionnaire information for some indicators.  This will be done later.

public_officials_dta_clean <-public_officials_dta %>%
  dplyr::select(preamble_info,constr_list, starts_with('DEM'), starts_with('NLG'), starts_with('ACM'), starts_with('QB'), starts_with('IDM'), starts_with('ORG'), starts_with('ENUM')) %>%
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
  mutate(national_learning_goals=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="NLG")], na.rm=TRUE),
         targeting=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="NLG1")], na.rm=T),
         monitoring=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="NLG2")], na.rm=T),
         incentives=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="NLG3")], na.rm=T),
         community_engagement=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="NLG4")], na.rm=T))


########
# Mandates and Accountability
########
public_officials_dta_clean$acm_length<-length(grep(x=colnames(public_officials_dta_clean), pattern="ACM"))


#calculate item scores
public_officials_dta_clean <- public_officials_dta_clean %>%
  mutate(mandates_accountability=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="ACM")], na.rm=TRUE),
         coherence=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="ACM2")], na.rm=T),
         transparency=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="ACM3")], na.rm=T),
         accountability=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="ACM4")], na.rm=T))


########
# Quality of Bureaucracy
########
public_officials_dta_clean$qb_length<-length(grep(x=colnames(public_officials_dta_clean), pattern="QB"))


#calculate item scores
public_officials_dta_clean <- public_officials_dta_clean %>%
  mutate(quality_bureaucracy=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="QB")], na.rm=TRUE),
         knowledge_skills=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="QB1")], na.rm=T),
         work_environment=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="QB2")], na.rm=T),
         merit=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="QB3")], na.rm=T),
         motivation_attitudes=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="QB4")], na.rm=T))


########
# Impartial Decision Making
########
public_officials_dta_clean$idm_length<-length(grep(x=colnames(public_officials_dta_clean), pattern="IDM"))


#calculate item scores
public_officials_dta_clean <- public_officials_dta_clean %>%
  mutate(impartial_decision_making=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="IDM")], na.rm=TRUE),
         politicized_personnel_management=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="IDM1")], na.rm=T),
         politicized_policy_making=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="IDM2")], na.rm=T),
         politicized_policy_implementation=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="IDM3")], na.rm=T),
         employee_unions_as_facilitators=rowMeans(.[grep(x=colnames(public_officials_dta_clean), pattern="IDM4")], na.rm=T))


#list of Bureaucracy indicators
bureau_ind_nlg  <-c( 'national_learning_goals', 'targeting', 'monitoring', 'incentives', 'community_engagement')
bureau_ind_acm  <-c('mandates_accountability' , 'coherence', 'transparency', 'accountability') 
bureau_ind_qb   <-c('quality_bureaucracy', 'knowledge_skills', 'work_environment', 'merit', 'motivation_attitudes')
bureau_ind_idm  <-c('impartial_decision_making','politicized_personnel_management', 'politicized_policy_making', 'politicized_policy_implementation', 'employee_unions_as_facilitators')


public_officials_dta_clean <-public_officials_dta_clean %>%
  dplyr::select(preamble_info, bureau_ind_nlg, bureau_ind_acm , bureau_ind_qb, bureau_ind_idm,
                constr_list, starts_with('DEM'), starts_with('NLG'), starts_with('ACM'), starts_with('QB'), starts_with('IDM'), starts_with('ORG'), starts_with('ENUM')) 

public_officials_dta_short <-public_officials_dta_clean %>%
  dplyr::select(preamble_info, bureau_ind_nlg, bureau_ind_acm , bureau_ind_qb, bureau_ind_idm
                , constr_list, starts_with('NLG'), starts_with('ACM'), starts_with('QB'), starts_with('IDM'), starts_with('ORG')) 


#filter out the director of HR, which isn't specifically asked about indicator questions

public_officials_dta_hr <- public_officials_dta_clean %>%
  filter(director_hr==1)

public_officials_dta_clean <- public_officials_dta_clean %>%
  filter(director_hr==0)

if (backup_onedrive=="yes") {
  write.csv(public_officials_dta_clean, file = file.path(confidential_folder_onedrive, "public_officials_survey_data.csv"))
  write_dta(public_officials_dta_short, path = file.path(confidential_folder_onedrive, "public_officials_survey_data.dta"), version = 14)
}


write.csv(public_officials_dta_clean, file = file.path(confidential_folder, "public_officials_survey_data.csv"))


public_officials_dta_clean2 <- public_officials_dta_clean %>%
  mutate(pol_personnel_management=politicized_personnel_management ,
         pol_policy_making=politicized_policy_making ,
         pol_policy_implementation=politicized_policy_implementation)
write_dta(public_officials_dta_clean2, path = file.path(confidential_folder, "public_officials_survey_data.dta"), version = 14)


keep_info <- c('interview__id', 'school_district_preload', 'school_province_preload','region_code', 'district_code', 'district', 'province','location', 'govt_tier',
                   'enumerator_name', 'enumerator_number', 'survey_time', 'lat', 'lon')

###############
#Aggregate to office level
#################

public_officials_office_level<- public_officials_dta_clean %>%
  group_by(region_code, district_code, govt_tier) %>%
  select(keep_info,bureau_ind_nlg, bureau_ind_acm , bureau_ind_qb, bureau_ind_idm, 
         starts_with('DEM'), starts_with('NLG'), starts_with('ACM'), starts_with('QB'), starts_with('IDM'), starts_with('ORG'), starts_with('ENUM'), motivation_relative_start ) %>%
  mutate(count=n() ) %>% 
  summarise_all(list(~if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
  
#convert variables to factor
# public_officials_dta_clean<- public_officials_dta_clean %>%
#   left_join(public_officials_metadata)
#   mutate_at(vars(starts_with('NLG'), starts_with('ACM'), starts_with('QB'), starts_with('IDM')), factor(., labels=vallables) ) 

# label_df<-public_officials_metadata %>%
#   filter(grepl('NLG|ACM|QB|IDM', name))
# 
# #add in value labels to questions using apply
# 
# #  factor function to create factor variable with value labels attached
# ff = function(x){ 
#   
#   #get string containing value labels
#   rownum<-which(grepl(x, label_df$name))
#   lab <- as.character(label_df[rownum,3])
#   #access column of same name as unit in the apply loop
#   public_officials_dta_clean[,x] <-    factor(public_officials_dta_clean$x,labels=lab)
# 
# }
# 
# 
# public_officials_dta_clean2 <- data.frame(sapply(label_df$name, ff))



################################
#Store Key Created Datasets
################################

#saves the following in R and stata format

data_list <- c( 'public_officials_dta_clean','public_officials_office_level')

save(data_list, file = file.path(confidential_folder, "public_officials_survey_data.RData"))


#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- c("NLG", "ACM", "QB", "IDM", "ORG")

#Create indicator level databases

ind_dta_list<-c()



for (i in indicator_names ) {
  
  j <- case_when(
    i=="NLG" ~ "BNLG",
    i=="ACM" ~ "BMAC",
    i=="QB" ~ "BQBR",
    i=="IDM" ~ "BIMP",
    TRUE ~ i
  )
  
  if (i!="ORG") {
  temp_df<-public_officials_dta_clean 
    if (ncol(temp_df) > 0) {
      temp_df<-temp_df %>%
        select(keep_info, get(paste('bureau_ind', tolower(i), sep="_")), starts_with('DEM'), starts_with(i))
     assign(paste("final_indicator_data_",j, sep=""), temp_df )
     
     ind_dta_list<-c(ind_dta_list, paste("final_indicator_data_",j, sep=""))
    
    }
  } else if (i=="ORG") {
    temp_df<-public_officials_dta_hr
    if (ncol(temp_df) > 0) {
      temp_df<-temp_df %>%
        select(keep_info, starts_with('DEM'), starts_with(i))
      assign(paste("final_indicator_data_",j, sep=""), temp_df )
      
      ind_dta_list<-c(ind_dta_list, paste("final_indicator_data_",j, sep=""))
      
    }
  }
  

}

save(list=c(ind_dta_list, "public_officials_dta_clean", 'public_officials_metadata', 'public_officials_dta_hr' ), file = file.path(confidential_folder, "public_officials_indicators_data.RData"))


#loop and produce list of data tables

if (backup_onedrive=="yes") {
  save(data_list, file = file.path(confidential_folder_onedrive, "public_officials_survey_data.RData"))
}




