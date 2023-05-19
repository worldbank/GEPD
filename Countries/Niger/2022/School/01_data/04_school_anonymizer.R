#Use of Survey Solutions API system
#Brian Stacy 6/11/2019

library(httr)
library(haven)
library(tidyverse)
library(here)
library(digest)
##################
# Load the data
##################

load(file = file.path(confidential_folder, "school_survey_data.RData"))

#add some disability questions
school_dis <- school_dta %>%
  group_by(school_code) %>%
  summarise(across(starts_with('m4scq'),mean, na.rm=TRUE))


final_school_data <- final_school_data %>%
  left_join(school_dis, by='school_code',suffix=c("",".y")) %>%
  select(-ends_with(".y"))

#generate list of datasets to anonnymize
#Read in list of indicators
indicators <- read_delim(here::here('Indicators','indicators.md'), delim="|", trim_ws=TRUE)

indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)

#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- indicators$indicator_tag

ind_dta_list<-c()
anon_dta_list<-c()


for (i in indicator_names ) {

    #add element to list
    ind_dta_list<-c(ind_dta_list, paste("final_indicator_data_",i, sep=""))
    
  
}
  
ind_dta_list<-c(ind_dta_list, c("final_indicator_data_ATTD_M", "final_indicator_data_ATTD_F", 
                                "final_indicator_data_CONT_M", "final_indicator_data_CONT_F", 
                                "final_indicator_data_EFFT_M", "final_indicator_data_EFFT_F", 
                                "final_indicator_data_LCAP_M", "final_indicator_data_LCAP_F", 
                                "final_indicator_data_LERN_M", "final_indicator_data_LERN_F",
                                "final_indicator_data_OPMN_M", "final_indicator_data_OPMN_F",
                                "final_indicator_data_ILDR_M", "final_indicator_data_ILDR_F",
                                "final_indicator_data_PKNW_M", "final_indicator_data_PKNW_F",
                                "final_indicator_data_PMAN_M", "final_indicator_data_PMAN_F"))


data_list<-c(ind_dta_list,'school_dta', 'school_dta_short', 'school_dta_short_imp', 'school_data_preamble', 'final_school_data', 'teacher_questionnaire','teacher_absence_final', 'ecd_dta', 'teacher_assessment_dta', 'teacher_roster', 
             'school_gdp', 'assess_4th_grade_anon', 'ecd_dta_anon', 'school_weights',
             'school_dta_raw', 'ecd_dta_raw', 'assess_4th_grade_dta_raw', 'teacher_assessment_dta_raw', 'teacher_questionnaire_raw'
             )

#define function to create weights for summary statistics


#Load original sample of schools
currentDate<-c("2022-10-24")
sample_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/sampling/", sep="/"))
data_set_updated <- read_csv(paste(sample_folder, '/school_weights_revised_', currentDate,  '.csv', sep="")
) %>%
  mutate(ipw=case_when(
    is.na(ipw) ~ median(ipw, na.rm=TRUE),
    is.infinite(ipw) ~ median(ipw, na.rm=TRUE),
    TRUE ~ ipw)
  ) %>%
  ungroup() %>%
  mutate(school_code=CODE_ETABLISSEMENT
         #private=if_else(sch_owner %in% c("Government", "Community"), "Public", "Private")
         ) %>%
  select(school_code, REGION,urban_rural,LIRE,ownership, public,
         ipw) 






df_weights_function <- function(dataset,scode, snumber, prov) {
  scode<-enquo(scode)  
  snumber<-enquo(snumber)
  prov<-enquo(prov)
  
  dataset %>%
    left_join(data_set_updated)  %>%
    mutate(province=REGION           ) 
}


####################
# Code to anonymize
####################
#create hashed school code
school_dta_short$hashed_school_code <-as.character(lapply(school_dta_short$school_code, function(x) {digest(x, algo="xxhash64", seed=531254, serialize = T)}))
school_dta_short$hashed_school_province <-as.character(lapply(school_dta_short$school_province_preload, function(x) {digest(x, algo="xxhash64", seed=531254, serialize = F)}))
school_dta_short$hashed_school_district <-as.character(lapply(school_dta_short$school_district_preload, function(x) {digest(x, algo="xxhash64", seed=531254, serialize = F)}))

#save a hashed version of the dataset, to produce a link file
key<-school_dta_short %>%
  select(school_code, school_province_preload, school_district_preload, hashed_school_code, hashed_school_province, hashed_school_district) 

write_excel_csv(key, file.path(confidential_folder, "EPDash_linkfile_hashed.csv"))

#######################################
#loop through databases and remove PII
#######################################



for (i in data_list ) {
  if (exists(i)) {
    #form temp data frame with each schools data
    temp<-get(i) 
    
    #add hashed school code if needed
    if ("school_code" %in% colnames(temp)) {
      temp <- temp %>%
        left_join(key) %>%
        select(hashed_school_code, hashed_school_province, hashed_school_district, everything())
    }
    
    
    #add on weights
    if ("school_code" %in% colnames(temp)) {
      temp <- df_weights_function(temp, Code_School, grd4_total, Region)
    }
    
    #Scrub names, geocodes
    temp <- temp %>%
      select(-starts_with('school_'), -one_of('m1s0q2_name', 'm1s0q2_code','m1s0q2_emis')) %>% # drop school names and address
      select(-starts_with('m1s0q9')) %>%
      select(-one_of('survey_time', 'lat','lon')) %>% #drop geo-codes
      select(-one_of('total_enrolled', 'm7saq8','m7saq10')) %>% 
      select(-one_of('m6_class_count')) %>% 
      select(-starts_with('enumerators_preload'), -one_of('m1s0q1_name', 'm1s0q1_name_other','m1s0q1_comments')) %>%  #get rid of enumerator names
      select(-one_of('m1saq1_first','m1saq1_last', 'm1saq2', 'm1saq2b')) %>% #drop principal names and phone numbers
      select(-contains('troster')) %>%
      select(-contains('name')) %>%
      select(-contains('_response')) %>%
      select(-contains('m2saq2')) %>%
      select(-contains('m6s1q1')) %>%
      select(-contains('m8s1q1')) 
    
    #Drop any "other" responses to questions
    temp <- temp %>%
      select(-contains('_other'))
    
    
    
    #convert # of students in school to categorical
    if ("m1saq7" %in% colnames(temp)) {
      temp <- temp %>%
        mutate(students_enrolled=case_when(
          m1saq7<25 ~ 1,
          m1saq7>=25 &  m1saq7<50 ~ 2,
          m1saq7>=50 &  m1saq7<75 ~ 3,
          m1saq7>=75 &  m1saq7<100 ~ 4,
          m1saq7>=100 &  m1saq7<150 ~ 5,
          m1saq7>=150 &  m1saq7<300 ~ 6,
          m1saq7>=300 &  m1saq7<500 ~ 7,
          m1saq7>=500  ~ 8
        )) %>%
        mutate(students_enrolled=factor(students_enrolled, levels=c(1,2,3,4,5,6,7,8), labels = c("Under 25", "25-50", "50-75", "75-100", "100-150", "150-300", "300-500", "Over 500"))) %>%
        select(-m1saq7)
    }
    
    if ("m1saq8" %in% colnames(temp)) {
      temp <- temp %>%
        mutate(boy_students_enrolled=case_when(
          m1saq8<25 ~ 1,
          m1saq8>=25 &  m1saq8<50 ~ 2,
          m1saq8>=50 &  m1saq8<75 ~ 3,
          m1saq8>=75 &  m1saq8<100 ~ 4,
          m1saq8>=100 &  m1saq8<150 ~ 5,
          m1saq8>=150 &  m1saq8<300 ~ 6,
          m1saq8>=300 &  m1saq8<500 ~ 7,
          m1saq8>=500  ~ 8
        ))  %>%
        mutate(boy_students_enrolled=factor(students_enrolled, levels=c(1,2,3,4,5,6,7,8), labels = c("Under 25", "25-50", "50-75", "75-100", "100-150", "150-300", "300-500", "Over 500"))) %>%
        select( -m1saq8)      
    }
    
    #add element to list
    anon_dta_list<-c(anon_dta_list, paste(i,"_anon", sep=""))
    
    assign(quo_name(paste(i,"_anon", sep="")), temp, envir=.GlobalEnv)
    
    print(i)
    
    #final_school_data<-temp
    print(i)
    
    temp %>%
      janitor::clean_names() %>%
      write_csv(  file.path(paste(save_folder,"/data", sep=""), paste(i,"_anon.csv", sep="")))
    
    temp %>%
      rename_with(~str_trunc(.,32)) %>%
      janitor::clean_names() %>%
      write_dta( path= file.path(paste(save_folder,"/data", sep=""), paste(i,"_anon.dta", sep="")))
    
    
  }
}

save(list=c(anon_dta_list,'metadta','indicators'), file = file.path(save_folder, "school_indicators_data_anon.RData"))
