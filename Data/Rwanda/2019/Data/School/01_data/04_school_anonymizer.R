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
#Load original sample of schools
currentDate<-c("2019-08-30")

sample_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/sampling/", sep="/"))
sample_frame_name <- paste(sample_folder,"/school_sample_",currentDate,".RData", sep="")

load(sample_frame_name)
#load some auxiliary data to help do sampling weights, because we lacked total enrollment for the districts
# auxillary_frame_info<-read_csv(file.path(paste(download_folder,"/pupil_counts_district/primary_pupil_enrollment.csv",sep=""))) %>%
#   mutate(district=str_to_upper(district))



df_weights_function <- function(dataset,scode, snumber, prov) {
  scode<-enquo(scode)
  snumber<-enquo(snumber)
  prov<-enquo(prov)

  data_set_updated <- school_weights %>% left_join(sample_frame %>% select(1:8) %>% rename(school_code= Inst_ID,
                                                                                           district = District,
                                                                                           province = province,
                                                                                           urban_rural=Location) %>% 
                                                     mutate(school_code=as.numeric(school_code))) %>% filter(!is.na(school_code)) %>% 
    
    group_by(district, urban_rural) %>%
    mutate(total_students=if_else(is.na(total_students), as.numeric(median(total_students, na.rm=T)), as.numeric(total_students))) %>%
    mutate(weights=n()/total_students) %>%
    mutate(unity=1) %>%
    ungroup()

    # data_set_updated %>%
    # left_join(auxillary_frame_info) %>%
    # mutate(province=district) %>%
    # group_by(district_code, urban_rural) %>%
    # mutate(allocate=if_else(is.na(allocate), as.numeric(median(allocate, na.rm=T)), as.numeric(allocate))) %>%
    # mutate(weights=n()/allocate) %>%
    # mutate(unity=1) %>%
    # ungroup()


  dataset %>%
    mutate(!! scode := as.numeric(.data$school_code)) %>%
    left_join(data_set_updated) %>%
    mutate(rural=urban_rural=="Rural") %>%
    mutate(ipw=if_else(is.na(.data$weights), as.numeric(median(.data$weights, na.rm=T)), as.numeric(.data$weights))*!! snumber ) %>%
    select(-one_of(colnames(data_set_updated[, -which(names(data_set_updated) == "urban_rural" | names(data_set_updated) == "district" | names(data_set_updated) == "province"
                                                      | names(data_set_updated) == "totalstudents" | names(data_set_updated) == "Tehsil" | names(data_set_updated) == "weights"  )])))
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
  select(school_code, school_province_preload, school_district_preload, hashed_school_code, hashed_school_province, hashed_school_district, total_enrolled,
         g4_stud_weight_component, abs_weight_component, teacher_weight_component, g1_stud_weight_component) 

write_excel_csv(key, file.path(confidential_folder, "EPDash_linkfile_hashed.csv"))

#######################################
#loop through databases and remove PII
#######################################



for (i in data_list ) {
  if (exists(i)) {
    #form temp data frame with each schools data
    temp<-get(i) 
    print(i)
    #add hashed school code if needed
    if ("school_code" %in% colnames(temp)) {
      temp <- temp %>%
        left_join(key) %>%
        select(hashed_school_code, hashed_school_province, hashed_school_district, everything())
    }
    
    
    #add on weights
    if ("school_code" %in% colnames(temp)) {
      
      # G4 students
      if (i %in% c("final_indicator_data_LERN","final_indicator_data_LERN_M", "final_indicator_data_LERN_F",
                  "final_indicator_data_ATTD",  "final_indicator_data_ATTD_M", "final_indicator_data_ATTD_F")) {
        temp <- df_weights_function(temp, school_code, g4_stud_weight_component, district)
      # Teacher Questionnaire and content knowledge  
      } else if (i %in% c("final_indicator_data_TATT",   "final_indicator_data_TSDP", "final_indicator_data_TSUP",
                          "final_indicator_data_TEVL",   "final_indicator_data_TMNA",   "final_indicator_data_TINM",
                          "final_indicator_data_CONT","final_indicator_data_CONT_M","final_indicator_data_CONT_F")) {
        temp <- df_weights_function(temp, school_code, teacher_weight_component, district)
      # Teacher Absence  
      } 
        else if (i %in% c("final_indicator_data_EFFT","final_indicator_data_EFFT_M", "final_indicator_data_EFFT_F" )) {
        temp <- df_weights_function(temp, school_code, abs_weight_component, district)
      # G1 Assessment  
      } else if (i %in% c("final_indicator_data_LCAP","final_indicator_data_LCAP_M", "final_indicator_data_LCAP_F" )) {
        temp <- df_weights_function(temp, school_code, g1_stud_weight_component, district)
      # school level  
      } else {
        temp <- df_weights_function(temp, school_code, unity, district)
      }
    }
    
    #Scrub names, geocodes
    temp <- temp %>%
      select(-starts_with('school_'), -one_of('m1s0q2_name', 'm1s0q2_code','m1s0q2_emis')) %>% # drop school names and address
      select(-starts_with('m1s0q9')) %>%
      select(-one_of('survey_time', 'lat','lon')) %>% #drop geo-codes
      select(-one_of('total_enrolled', 'm7saq8','m7saq10')) %>% 
      #select(-one_of('m6_class_count')) %>% 
      select(-starts_with('enumerators_preload'), -one_of('m1s0q1_name', 'm1s0q1_name_other','m1s0q1_comments')) %>%  #get rid of enumerator names
      select(-one_of('m1saq1_first','m1saq1_last', 'm1saq2', 'm1saq2b')) %>% #drop principal names and phone numbers
      select(-contains('troster')) %>%
      select(-contains('name')) %>%
      select(-contains('p4_class_')) %>%
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
      write_csv(temp, file = file.path(paste(save_folder,"/data", sep=""), paste(i,"_anon.csv", sep="")))

      

  }
}

save(list=c(anon_dta_list, 'metadta', 'indicators'), file = file.path(save_folder, "school_indicators_data_anon.RData"))

