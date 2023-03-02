library(tidyverse)
library(here)


#directories


if (str_to_lower(Sys.info()["user"]) == "wb469649") {
  
  dir <- here()
  shared_loc <- 'C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Main_Documents/GEPD_FCDO_report/'
} else if (str_to_lower(Sys.info()["user"]) == "wb577189") {
  
  dir <- "C:/Users/wb577189/OneDrive - WBG/Documents/GitHub/GEPD"
  shared_loc <- 'C:/Users/wb577189/WBG/Ezequiel Molina - Dashboard (Team Folder)/Main_Documents/GEPD_FCDO_report/'
}


indicators_dir <- paste(dir, 'Indicators', sep="/")
out_dir <- paste(dir, 'Output', sep="/")

#path to confidential directory

if (str_to_lower(Sys.info()["user"]) == "wb469649") {
  #directories
  confidential_dir<- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/"
  anonymized_dir <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/"
  
} else if (str_to_lower(Sys.info()["user"]) == "wb577189") {
  #directories
  confidential_dir<- "C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/"
  anonymized_dir <- "C:/Users/wb577189/OneDrive - WBG/GEPD/"
  
}



bur_indicators <- c(
  #"responsible_finance_planning"   ,      
  #"responsible_hiring_teachers"       ,   
  #"responsible_monitoring_performance"   ,
  #"responsible_none"                   , 
  #"education"                           ,
  "gender"                               ,
  #"director_hr"    ,
  "national_learning_goals",              
  "targeting"               ,            
  "monitoring"               ,       
  "incentives"                ,     
  "community_engagement"       ,      
  "mandates_accountability"     ,      
  "coherence"     ,
  "transparency"   ,               
  "accountability"  ,              
  "quality_bureaucracy",           
  "knowledge_skills"    ,           
  "work_environment"     ,               
  "merit"                 ,          
  "motivation_attitudes"   ,          
  "impartial_decision_making",        
  "politicized_personnel_management", 
  "politicized_policy_making"        ,   
  "politicized_policy_implementation" ,   
  "employee_unions_as_facilitators"    , 
  "avg_class_size_guess"                ,
  "avg_absence_guess"                   ,
  "motivation_relative_start"           
  #"proportion_reported_underperformance",
  #"proportion_broke_rules"            ,
  #"proportion_contracts_political"     , 
  #"proportion_producement_political" 
)


########
# Peru
#######
#read in anonymized school data
country <- "PER"
year <- "2019"
PER_bur_df <- haven::read_dta(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//confidential/Public_Officials/public_officials_survey_data.dta", sep="/")) %>%
  mutate(iso3c=country) %>%
  select(iso3c,govt_tier, bur_indicators, everything()) %>%
  select(-one_of('survey_time')) %>% 
  select(-starts_with("ENUM")) %>%
  select(-contains('ENUMq8')) %>% #drop enumerator notes
  select(-starts_with('enumerators_preload'), -contains('m1s0q1_name_other')) %>%  #get rid of enumerator names
  select(-contains('name')) %>%
  mutate(govt_tier=factor(govt_tier, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" )),
         education=factor(DEM1q11, levels=c(1,2,3,4,5,6,7, 97, 900, 998), labels=c('Primary school',
                                                                                   'Middle school',
                                                                                   'Secondary school',
                                                                                   'Diploma / Other post-high-school certificate',
                                                                                   'Undergraduate degree',
                                                                                   'Masters degree',
                                                                                   'PhD',
                                                                                   'Other (dont specify)',
                                                                                   'Dont know',
                                                                                   'Refused to answer' ))) %>%
  select(-starts_with('pol_policy_implementation1')) %>%
  select(-starts_with('res_monitoring_perform1')) %>%
  select(-starts_with('prop_reported_underperform1'))

########
# Jordan
#######
#read in anonymized school data
country <- "JOR"
year <- "2019"
JOR_bur_df <- haven::read_dta(file=paste(confidential_dir,"CNT", paste0(country,""),paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//confidential/Public_Officials/public_officials_survey_data.dta", sep="/")) %>%
  mutate(iso3c=country) %>%
  select(iso3c,govt_tier, bur_indicators, everything()) %>%
  select(-one_of('survey_time')) %>% 
  select(-starts_with("ENUM")) %>%
  select(-contains('ENUMq8')) %>% #drop enumerator notes
  select(-starts_with('enumerators_preload'), -contains('m1s0q1_name_other')) %>%  #get rid of enumerator names
  select(-contains('name'))  %>%
  mutate(govt_tier=factor(govt_tier, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" )),
         education=factor(DEM1q11, levels=c(1,2,3,4,5,6,7, 97, 900, 998), labels=c('Primary school',
                                                                                   'Middle school',
                                                                                   'Secondary school',
                                                                                   'Diploma / Other post-high-school certificate',
                                                                                   'Undergraduate degree',
                                                                                   'Masters degree',
                                                                                   'PhD',
                                                                                   'Other (dont specify)',
                                                                                   'Dont know',
                                                                                   'Refused to answer' ))) %>%
  select(-starts_with('pol_policy_implementation1')) %>%
  select(-starts_with('res_monitoring_perform1')) %>%
  select(-starts_with('prop_reported_underperform1'))


########
# Rwanda
#######
#read in anonymized school data
country <- "RWA"
year <- "2020"
RWA_bur_df <- haven::read_dta(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//confidential/Public_Officials/public_officials_survey_data.dta", sep="/")) %>%
  mutate(iso3c=country) %>%
  select(iso3c,govt_tier, bur_indicators, everything()) %>%
  select(-one_of('survey_time')) %>% 
  select(-starts_with("ENUM")) %>%
  select(-contains('ENUMq8')) %>% #drop enumerator notes
  select(-starts_with('enumerators_preload'), -contains('m1s0q1_name_other')) %>%  #get rid of enumerator names
  select(-contains('name')) %>%
  mutate(govt_tier=factor(govt_tier, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" )),
         education=factor(DEM1q11, levels=c(1,2,3,4,5,6,7, 97, 900, 998), labels=c('Primary school',
                                                                                   'Middle school',
                                                                                   'Secondary school',
                                                                                   'Diploma / Other post-high-school certificate',
                                                                                   'Undergraduate degree',
                                                                                   'Masters degree',
                                                                                   'PhD',
                                                                                   'Other (dont specify)',
                                                                                   'Dont know',
                                                                                   'Refused to answer' )))  %>%
  select(-starts_with('pol_policy_implementation1')) %>%
  select(-starts_with('res_monitoring_perform1')) %>%
  select(-starts_with('prop_reported_underperform1'))


########
# Ethiopia
#######
#read in anonymized school data
country <- "ETH"
year <- "2020_2021"
ETH_bur_df <- read_csv(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//confidential/Public_Officials/public_officials_survey_data.csv", sep="/")) %>%
  mutate(iso3c=country) %>%
  select(iso3c, govt_tier, bur_indicators, everything()) %>%
  select(-one_of('survey_time')) %>% 
  select(-starts_with("ENUM")) %>%
  select(-contains('ENUMq8')) %>% #drop enumerator notes
  select(-starts_with('enumerators_preload'), -contains('m1s0q1_name_other')) %>%  #get rid of enumerator names
  select(-contains('name'))  %>%
  mutate(govt_tier=factor(govt_tier,  levels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" )),
         education=factor(DEM1q11, levels=c(1,2,3,4,5,6,7, 97, 900, 998), labels=c('Primary school',
                                                                                   'Middle school',
                                                                                   'Secondary school',
                                                                                   'Diploma / Other post-high-school certificate',
                                                                                   'Undergraduate degree',
                                                                                   'Masters degree',
                                                                                   'PhD',
                                                                                   'Other (dont specify)',
                                                                                   'Dont know',
                                                                                   'Refused to answer' )))  %>%
  select(-starts_with('pol_policy_implementation1')) %>%
  select(-starts_with('res_monitoring_perform1')) %>%
  select(-starts_with('prop_reported_underperform1'))

########
# Madagascar
#######
#read in anonymized school data
country <- "MDG"
year <- "2021"
MDG_bur_df <- read_csv(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//confidential/Public_Officials/public_officials_survey_data.csv", sep="/")) %>%
  mutate(iso3c=country) %>%
  select(iso3c,govt_tier, bur_indicators, everything()) %>%
  select(-one_of('survey_time')) %>% 
  select(-starts_with("ENUM")) %>%
  select(-contains('ENUMq8')) %>% #drop enumerator notes
  select(-starts_with('enumerators_preload'), -contains('m1s0q1_name_other')) %>%  #get rid of enumerator names
  select(-contains('name')) %>%
  mutate(govt_tier=factor(govt_tier, levels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" )),
         education=factor(DEM1q11, levels=c(1,2,3,4,5,6,7, 97, 900, 998), labels=c('Primary school',
                                                                                   'Middle school',
                                                                                   'Secondary school',
                                                                                   'Diploma / Other post-high-school certificate',
                                                                                   'Undergraduate degree',
                                                                                   'Masters degree',
                                                                                   'PhD',
                                                                                   'Other (dont specify)',
                                                                                   'Dont know',
                                                                                   'Refused to answer' ))) %>%
  select(-starts_with('pol_policy_implementation1')) %>%
  select(-starts_with('res_monitoring_perform1')) %>%
  select(-starts_with('prop_reported_underperform1'))

########
# Madagascar
#######
#read in anonymized school data
country <- "SLE"
year <- "2022"
SLE_bur_df <- read_csv(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//confidential/Public_Officials/public_officials_survey_data.csv", sep="/")) %>%
  mutate(iso3c=country) %>%
  select(iso3c,govt_tier, bur_indicators, everything()) %>%
  select(-one_of('survey_time')) %>% 
  select(-starts_with("ENUM")) %>%
  select(-contains('ENUMq8')) %>% #drop enumerator notes
  select(-starts_with('enumerators_preload'), -contains('m1s0q1_name_other')) %>%  #get rid of enumerator names
  select(-contains('name')) %>%
  mutate(govt_tier=factor(govt_tier, levels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" )),
         education=factor(DEM1q11, levels=c(1,2,3,4,5,6,7, 97, 900, 998), labels=c('Primary school',
                                                                                   'Middle school',
                                                                                   'Secondary school',
                                                                                   'Diploma / Other post-high-school certificate',
                                                                                   'Undergraduate degree',
                                                                                   'Masters degree',
                                                                                   'PhD',
                                                                                   'Other (dont specify)',
                                                                                   'Dont know',
                                                                                   'Refused to answer' ))) %>%
  select(-starts_with('pol_policy_implementation1')) %>%
  select(-starts_with('res_monitoring_perform1')) %>%
  select(-starts_with('prop_reported_underperform1'))
########
# Madagascar
#######
#read in anonymized school data
country <- "NER"
year <- "2022"
NER_bur_df <- read_csv(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//confidential/Public_Officials/public_officials_survey_data.csv", sep="/")) %>%
  mutate(iso3c=country) %>%
  select(iso3c,govt_tier, bur_indicators, everything()) %>%
  select(-one_of('survey_time')) %>% 
  select(-starts_with("ENUM")) %>%
  select(-contains('ENUMq8')) %>% #drop enumerator notes
  select(-starts_with('enumerators_preload'), -contains('m1s0q1_name_other')) %>%  #get rid of enumerator names
  select(-contains('name')) %>%
  mutate(govt_tier=factor(govt_tier, levels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" )),
         education=factor(DEM1q11, levels=c(1,2,3,4,5,6,7, 97, 900, 998), labels=c('Primary school',
                                                                                   'Middle school',
                                                                                   'Secondary school',
                                                                                   'Diploma / Other post-high-school certificate',
                                                                                   'Undergraduate degree',
                                                                                   'Masters degree',
                                                                                   'PhD',
                                                                                   'Other (dont specify)',
                                                                                   'Dont know',
                                                                                   'Refused to answer' )))  %>%
  select(-starts_with('pol_policy_implementation1')) %>%
  select(-starts_with('res_monitoring_perform1')) %>%
  select(-starts_with('prop_reported_underperform1')) 
########
#combined
########
combined_bur_df <- bind_rows(PER_bur_df, JOR_bur_df, RWA_bur_df, ETH_bur_df, MDG_bur_df,SLE_bur_df,NER_bur_df) %>%
  mutate(iso3c=factor(iso3c, levels=c("NER","SLE","MDG", "ETH", "RWA", "JOR", "PER")))


combined_bur_df %>%
  rename(pol_policy_implementation1=politicized_policy_implementation,
         res_monitoring_perform1=responsible_monitoring_performance,
         prop_reported_underperform1=proportion_reported_underperformance
  ) %>%
  janitor::clean_names() %>%
  haven::write_dta(  paste0(confidential_dir, "General/combined_public_officials_confidential.dta")) 
