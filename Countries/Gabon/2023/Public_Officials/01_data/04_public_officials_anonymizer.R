#Use of Survey Solutions API system
#Brian Stacy 6/11/2019

library(httr)
library(haven)
library(tidyverse)
library(here)
library(digest)
#library(sdcMicro)

##################
# Load the data
##################

load(file = file.path(confidential_folder, "public_officials_indicators_data.RData"))


#generate list of datasets to anonnymize
#Read in list of indicators
indicators <- read_delim(here::here('Indicators','indicators.md'), delim="|", trim_ws=TRUE)

indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)

#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- c("NLG", "ACM", "QB", "IDM", "ORG")

ind_dta_list<-c("final_indicator_data_BNLG" , "final_indicator_data_BMAC" , "final_indicator_data_BQBR" ,
                "final_indicator_data_BIMP" , "final_indicator_data_ORG"  , "public_officials_dta_clean",
                 "public_officials_dta")

anon_dta_list<-c("public_officials_metadata")





data_list<-ind_dta_list


#define function to create weights for summary statistics


####################
# Code to anonymize
####################
#create hashed school code
public_officials_dta_short <- public_officials_dta_short 

public_officials_dta_short$hashed_position <-as.character(lapply(public_officials_dta_short$position, function(x) {digest(x, algo="xxhash64", seed=531254, serialize = T)}))
public_officials_dta_short$hashed_office <-as.character(lapply(public_officials_dta_short$office_preload, function(x) {digest(x, algo="xxhash64", seed=531254, serialize = F)}))
public_officials_dta_short$hashed_name <-as.character(lapply(public_officials_dta_short$inter_officials, function(x) {digest(x, algo="xxhash64", seed=531254, serialize = F)}))

#save a hashed version of the dataset, to produce a link file
key<-public_officials_dta_short %>%
  select(id_code, position, office_preload, inter_officials, hashed_position,  hashed_office, hashed_name) 

write_excel_csv(key, file.path(confidential_folder, "public_official_linkfile_hashed.csv"))

#######################################
#loop through databases and remove PII
#######################################



for (i in data_list ) {
  if (exists(i)) {
    #form temp data frame with each schools data
    temp<-get(i) 
    
    #add hashed school code if needed
    if ("position" %in% colnames(temp)) {
      temp <- temp %>%
        left_join(key) %>%
        select(id_code, hashed_position, hashed_office,  everything())
    }
    
    
    
    #Scrub names, geocodes
    temp <- temp %>%
      select(-starts_with('position'), -starts_with('office'), contains('location')) %>% # drop position names and address
      select(-one_of('survey_time', 'lat','lon')) %>% #drop geo-codes
      select(-contains('office')) %>%
      select(-contains('ENUMq8')) %>% #drop enumerator notes
      select(-contains('m1s0q9')) %>%
      select(-contains('inter_officials')) %>%
      select(-contains('DEM1q2')) %>% #drop DEM1q2 questions, because they are too sensitive
      select(-starts_with('enumerators_preload'), -contains('m1s0q1_name_other')) %>%  #get rid of enumerator names
      select(-contains('name')) %>%
      select(-starts_with('ORG1'))  #drop ORG1 questions, which could lead to reidentification
    
    #Drop any "other" responses to questions
    temp <- temp %>%
      select(-contains('_other'))
    
    
    
    #convert some sensitive questions to categorical
    if ("DEM1q6" %in% colnames(temp)) { #age
      temp <- temp %>%
        mutate(age=case_when(
          DEM1q6<25 ~ 1,
          DEM1q6>=25 &  DEM1q6<35 ~ 2,
          DEM1q6>=35 &  DEM1q6<40 ~ 3,
          DEM1q6>=40 &  DEM1q6<45 ~ 4,
          DEM1q6>=45 &  DEM1q6<50 ~ 5,
          DEM1q6>=50 &  DEM1q6<55 ~ 6,
          DEM1q6>=55 &  DEM1q6<65 ~ 7,
          DEM1q6>=65  ~ 8
        )) %>%
        mutate(age=factor(age, levels=c(1,2,3,4,5,6,7,8), labels = c("Under 25", "25-30", "35-40", "40-45", "45-50", "50-55", "55-65", "Over 65"))) %>%
        select(-DEM1q6)
    }
    
    if ("DEM1q7" %in% colnames(temp)) { #how many years in current position
      temp <- temp %>%
        mutate(years_current_position=case_when(
          DEM1q7<5 ~ 1,
          DEM1q7>=5 &  DEM1q7<10 ~ 2,
          DEM1q7>=10 &  DEM1q7<15 ~ 3,
          DEM1q7>=15 &  DEM1q7<20 ~ 4,
          DEM1q7>=20 &  DEM1q7<25 ~ 5,
          DEM1q7>=25  ~ 6
        )) %>%
        mutate(years_current_position=factor(years_current_position, levels=c(1,2,3,4,5,6), labels = c("Under 5", "5-10", "10-15", "15-20", "20-25", "Over 25"))) %>%
        select(-DEM1q7)
    }
    
    if ("DEM1q8" %in% colnames(temp)) { #how many years in current organization
      temp <- temp %>%
        mutate(years_current_org=case_when(
          DEM1q8<5 ~ 1,
          DEM1q8>=5 &  DEM1q8<10 ~ 2,
          DEM1q8>=10 &  DEM1q8<15 ~ 3,
          DEM1q8>=15 &  DEM1q8<20 ~ 4,
          DEM1q8>=20 &  DEM1q8<25 ~ 5,
          DEM1q8>=25  ~ 6
        )) %>%
        mutate(years_current_org=factor(years_current_org, levels=c(1,2,3,4,5,6), labels = c("Under 5", "5-10", "10-15", "15-20", "20-25", "Over 25"))) %>%
        select(-DEM1q8)
    }
    if ("DEM1q9" %in% colnames(temp)) { #how many years in civil service
      temp <- temp %>%
        mutate(years_civil_service=case_when(
          DEM1q9<5 ~ 1,
          DEM1q9>=5 &  DEM1q9<10 ~ 2,
          DEM1q9>=10 &  DEM1q9<15 ~ 3,
          DEM1q9>=15 &  DEM1q9<20 ~ 4,
          DEM1q9>=20 &  DEM1q9<25 ~ 5,
          DEM1q9>=25  ~ 6
        )) %>%
        mutate(years_civil_service=factor(years_civil_service, levels=c(1,2,3,4,5,6), labels = c("Under 5", "5-10", "10-15", "15-20", "20-25", "Over 25"))) %>%
        select(-DEM1q9)
    }
    if ("DEM1q10" %in% colnames(temp)) { #How many organizations have you worked in in the civil service
      temp <- temp %>%
        mutate(num_pub_sector_org=case_when(
          DEM1q10==1 ~ 1,
          DEM1q10==2 ~ 2,
          DEM1q10==3 ~ 3,
          DEM1q10>3 ~ 4
        )) %>%
        mutate(num_pub_sector_org=factor(num_pub_sector_org, levels=c(1,2,3,4), labels = c("1", "2", "3", "Over 3"))) %>%
        select(-DEM1q10)
    }
    
    if ("DEM1q10" %in% colnames(temp)) { #How many full-time staff members that you manage directly report to you?
      temp <- temp %>%
        mutate(num_staff_report=case_when(
          DEM1q13n==0 ~ 0,
          DEM1q13n<5 & DEM1q13n!=0~ 1,
          DEM1q13n>=5 &  DEM1q13n<10 ~ 2,
          DEM1q13n>=10 &  DEM1q13n<15 ~ 3,
          DEM1q13n>=15 &  DEM1q13n<20 ~ 4,
          DEM1q13n>=20 ~ 5
        )) %>%
        mutate(num_staff_report=factor(num_staff_report, levels=c(0, 1,2,3,4, 5), labels = c("0", "1-4", "5-9", "10-14", "15-19", "20 or more"))) %>%
        select(-DEM1q13n)
    }
    
    #do noise addition (10% of std dev) using sdcMicro and addNoise for income
    if ("DEM1q14n" %in% colnames(temp)) { #What is your monthly net salary?
      
      temp$net_monthly_salary<-as.numeric(addNoise(temp, variables=c('DEM1q14n'), noise=110)$xm)
      temp <- temp %>%
        select(-DEM1q14n)
      
    }
    
    #add element to list
    anon_dta_list<-c(anon_dta_list, paste(i,"_anon", sep=""))
    
    assign(quo_name(paste(i,"_anon", sep="")), temp, envir=.GlobalEnv)
    
    print(i)
    
    print(i)
    write_excel_csv(temp,  file.path(paste(save_folder,"/data", sep=""), paste(i,"_anon.csv", sep="")))
    
    # temp %>%
    #   rename_with(~str_trunc(.,32)) %>%
    #   write_dta(temp, path = file.path(paste(save_folder,"/data", sep=""), paste(i,"_anon.dta", sep="")), version = 14)
    
    
  }
}

save(list=anon_dta_list, file = file.path(save_folder, "public_officials_indicators_data_anon.RData"))
