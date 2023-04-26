library(tidyverse)
library(here)
library(geosphere)
library(fuzzyjoin)
library(stringdist)

set.seed(123)

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
  select(-starts_with("ENUMq")) %>%
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
  select(-starts_with('prop_reported_underperform1')) %>%
  select(-starts_with("ORG")) %>%
  mutate(interview_code=floor(10^9*runif(nrow(.))),
         office_name=str_to_lower(paste(school_district_preload,school_province_preload,govt_tier, sep=" - ")),
         office_id=row_number()) %>%
  group_by(office_name) %>%
  fill(lat,lon, .direction = 'downup') %>%
  ungroup() 
  

#read in data from office administrators
PER_po_admin_df <- haven::read_dta(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//raw/Public_Officials/public_officials.dta", sep="/"))  %>%
  filter(director_hr==1) %>%
  mutate(govt_tier=factor(m1s0q2_name, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" ))) %>%
  select(school_district_preload, school_province_preload, govt_tier, m1s0q9__Latitude, m1s0q9__Longitude, starts_with("ORG")) %>%
  mutate(admin_id=row_number(),
         admin_lat=as.numeric(m1s0q9__Latitude),
         admin_lon=as.numeric(m1s0q9__Longitude),
         admin_office_name=str_to_lower(paste(school_district_preload,school_province_preload, govt_tier, sep=" - "))) %>%
  select(-school_district_preload, -school_province_preload) %>%
  group_by(admin_office_name) %>%
  slice(1)



PER_bur_df$admin_office_name <- sapply(PER_bur_df$office_name, function(x) {
  match_index <- amatch(x, unique(PER_po_admin_df$admin_office_name), maxDist = 2)
  if (match_index == 0 | is.na(match_index)) {
    return(NA)
  } else {
    return(unique(PER_po_admin_df$admin_office_name)[match_index])
  }
})

PER_bur_df <- PER_bur_df %>%
  left_join(PER_po_admin_df)

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
  select(-starts_with("ENUMq")) %>%
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
  select(-starts_with("ORG")) %>%
  select(-starts_with('prop_reported_underperform1')) %>%
  mutate(office_name=office_preload) 

#read in data from office administrators
JOR_po_admin_df <- haven::read_dta(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//raw/Public_Officials/public_officials.dta", sep="/"))  %>%
  filter(director_hr==1) %>%
  mutate(govt_tier=factor(m1s0q2_name, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" ))) %>%
  select(office_preload, m1s0q9__Latitude, m1s0q9__Longitude, starts_with("ORG")) %>%
  mutate(admin_id=row_number(),
         admin_lat=as.numeric(m1s0q9__Latitude),
         admin_lon=as.numeric(m1s0q9__Longitude))  %>%
  filter(!is.na(office_preload)) %>%
  filter(office_preload!="") %>%
  mutate(admin_office_name=office_preload) %>%
  select(-office_preload)


JOR_bur_df$admin_office_name <- sapply(JOR_bur_df$office_name, function(x) {
  match_index <- amatch(x, unique(JOR_po_admin_df$admin_office_name), maxDist = 2)
  if (match_index == 0 | is.na(match_index)) {
    return(NA)
  } else {
    return(unique(JOR_po_admin_df$admin_office_name)[match_index])
  }
})

JOR_bur_df <- JOR_bur_df %>%
  left_join(JOR_po_admin_df)

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
  select(-starts_with("ENUMq")) %>%
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
  select(-starts_with("ORG")) %>%
  select(-starts_with('prop_reported_underperform1')) %>%
  mutate(office_name=paste0(office_preload,"-",govt_tier)) 

#read in data from office administrators
RWA_po_admin_df <- haven::read_dta(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//raw/Public_Officials/public_officials_RWA.dta", sep="/"))  %>%
  filter(director_hr==1) %>%
  mutate(govt_tier=factor(m1s0q2_name, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" ))) %>%
  select(office_preload, govt_tier, m1s0q9__Latitude, m1s0q9__Longitude, starts_with("ORG")) %>%
  mutate(admin_id=row_number(),
         admin_lat=as.numeric(m1s0q9__Latitude),
         admin_lon=as.numeric(m1s0q9__Longitude))  %>%
  filter(!is.na(office_preload)) %>%
  filter(office_preload!="") %>%
  mutate(admin_office_name=paste0(office_preload,"-",govt_tier)) %>%
  select(-office_preload) %>%
  group_by(admin_office_name) %>% slice(1)


RWA_bur_df$admin_office_name <- sapply(RWA_bur_df$office_name, function(x) {
  match_index <- amatch(x, unique(RWA_po_admin_df$admin_office_name), maxDist = 2)
  if (match_index == 0 | is.na(match_index)) {
    return(NA)
  } else {
    return(unique(RWA_po_admin_df$admin_office_name)[match_index])
  }
})

RWA_bur_df <- RWA_bur_df %>%
  left_join(RWA_po_admin_df)


########
# Ethiopia
#######
#read in anonymized school data
country <- "ETH"
year <- "2020_2021"
ETH_bur_df <- read_csv(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//confidential/Public_Officials/public_officials_survey_data.csv", sep="/")) %>%
  mutate(iso3c=country) %>%
  select(iso3c, govt_tier, Region, Zone, Woreda, bur_indicators, everything()) %>%
  select(-one_of('survey_time')) %>% 
  #select(-starts_with("ENUM")) %>%
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
  select(-starts_with("ORG")) %>%
  select(-starts_with('prop_reported_underperform1')) %>%
  mutate(office_name=paste(Region, Zone, Woreda, sep=" - ")) 

#read in data from office administrators
ETH_po_admin_df <- haven::read_dta(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//raw/Public_Officials/public_officials_final_combined.dta", sep="/"))  %>%
  filter(director_hr==1) %>%
  mutate(govt_tier=factor(m1s0q2_name, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" ))) %>%
  select(office_preload,Region, Zone, Woreda, govt_tier, m1s0q9__Latitude, m1s0q9__Longitude, starts_with("ORG")) %>%
  mutate(admin_id=row_number(),
         admin_lat=as.numeric(m1s0q9__Latitude),
         admin_lon=as.numeric(m1s0q9__Longitude))  %>%
  filter(!is.na(office_preload)) %>%
  filter(office_preload!="") %>%
  mutate(admin_office_name=paste(Region, Zone, Woreda, sep=" - ")) %>%
  select(-office_preload) %>%
  group_by(admin_office_name) %>% slice(1)


ETH_bur_df$admin_office_name <- sapply(ETH_bur_df$office_name, function(x) {
  match_index <- amatch(x, unique(ETH_po_admin_df$admin_office_name), maxDist = 2)
  if (match_index == 0 | is.na(match_index)) {
    return(NA)
  } else {
    return(unique(ETH_po_admin_df$admin_office_name)[match_index])
  }
})

ETH_bur_df <- ETH_bur_df %>%
  left_join(ETH_po_admin_df)


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
  select(-starts_with("ENUMq")) %>%
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
  select(-starts_with("ORG")) %>%
  select(-starts_with('prop_reported_underperform1')) %>%
  mutate(office_name=location) 


#read in data from office administrators
MDG_po_admin_df <- haven::read_dta(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//raw/Public_Officials/public_officials.dta", sep="/"))  %>%
  filter(director_hr==1) %>%
  mutate(govt_tier=factor(m1s0q2_name, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" ))) %>%
  select(location, m1s0q9__Latitude, m1s0q9__Longitude, starts_with("ORG")) %>%
  mutate(admin_id=row_number(),
         admin_lat=as.numeric(m1s0q9__Latitude),
         admin_lon=as.numeric(m1s0q9__Longitude))  %>%
  filter(!is.na(location)) %>%
  filter(location!="") %>%
  mutate(admin_office_name=location) %>%
  select(-location)


MDG_bur_df$admin_office_name <- sapply(MDG_bur_df$office_name, function(x) {
  match_index <- amatch(x, unique(MDG_po_admin_df$admin_office_name), maxDist = 2)
  if (match_index == 0 | is.na(match_index)) {
    return(NA)
  } else {
    return(unique(MDG_po_admin_df$admin_office_name)[match_index])
  }
})

MDG_bur_df <- MDG_bur_df %>%
  left_join(MDG_po_admin_df)


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
  select(-starts_with("ENUMq")) %>%
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
  select(-starts_with("ORG")) %>%
  select(-starts_with('prop_reported_underperform1')) %>%
  mutate(office_name=paste(office_preload, location, sep="-")) 

#read in data from office administrators
SLE_po_admin_df <- haven::read_dta(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//raw/Public_Officials/public_officials.dta", sep="/"))  %>%
  filter(director_hr==1) %>%
  mutate(govt_tier=factor(m1s0q2_name, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" ))) %>%
  select(location, office_preload, m1s0q9__Latitude, m1s0q9__Longitude, starts_with("ORG")) %>%
  mutate(admin_id=row_number(),
         admin_lat=as.numeric(m1s0q9__Latitude),
         admin_lon=as.numeric(m1s0q9__Longitude))  %>%
  filter(!is.na(location)) %>%
  filter(location!="") %>%
  filter(location!="Blank interview") %>%
  filter(location!="ENTRETIEN BLANC") %>%
  mutate(admin_office_name=paste(office_preload, location, sep="-")) %>%
  select(-location, office_preload)


SLE_bur_df$admin_office_name <- sapply(SLE_bur_df$office_name, function(x) {
  match_index <- amatch(x, unique(SLE_po_admin_df$admin_office_name), maxDist = 2)
  if (match_index == 0 | is.na(match_index)) {
    return(NA)
  } else {
    return(unique(SLE_po_admin_df$admin_office_name)[match_index])
  }
})

SLE_bur_df <- SLE_bur_df 


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
  select(-starts_with("ENUMq")) %>%
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
  select(-starts_with("ORG")) %>%
  select(-starts_with('prop_reported_underperform1'))  %>%
  mutate(office_name=paste(office_preload, location, sep="-")) 


#read in data from office administrators
NER_po_admin_df <- haven::read_dta(file=paste(confidential_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_RAW", sep="_"),"Data//raw/Public_Officials/public_officials.dta", sep="/"))  %>%
  filter(director_hr==1) %>%
  mutate(govt_tier=factor(m1s0q2_name, levels=c(1,2,3), labels=c("Ministry of Education (or equivalent)","Regional office (or equivalent)", "District office (or equivalent)" ))) %>%
  select(office_preload, location, m1s0q9__Latitude, m1s0q9__Longitude, starts_with("ORG")) %>%
  mutate(admin_id=row_number(),
         admin_lat=as.numeric(m1s0q9__Latitude),
         admin_lon=as.numeric(m1s0q9__Longitude))  %>%
  filter(!is.na(location)) %>%
  filter(location!="") %>%
  filter(location!="Blank interview") %>%
  filter(location!="ENTRETIEN BLANC") %>%
  mutate(admin_office_name=paste(office_preload, location, sep="-")) %>%
  select(-location, office_preload)

NER_bur_df$admin_office_name <- sapply(NER_bur_df$office_name, function(x) {
  match_index <- amatch(x, unique(NER_po_admin_df$admin_office_name), maxDist = 2)
  if (match_index == 0 | is.na(match_index)) {
    return(NA)
  } else {
    return(unique(NER_po_admin_df$admin_office_name)[match_index])
  }
})

NER_bur_df <- NER_bur_df %>%
  left_join(NER_po_admin_df)

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




###################
# Combined Data ###
###################


#specify some key indicators to be imputed with mean value when necessary
practices <- c('presence_rate', 'content_knowledge', 'teach_score', 'inputs', 'infrastructure', 'ecd_student_knowledge', 'student_attendance', 'operational_management', 'instructional_leadership', 'principal_knowledge_score', 'principal_management')
policies <- c('teacher_attraction', 'teacher_selection_deployment', 'teacher_support', 'teaching_evaluation', 'teacher_monitoring', 'intrinsic_motivation', 'standards_monitoring', 'sch_monitoring', 'sch_management_clarity', 'sch_management_attraction', 'sch_selection_deployment', 'sch_support', 'principal_evaluation')
politics <- c('quality_bureaucracy', 'national_learning_goals', 'impartial_decision_making', 'mandates_accountability')


# read in the microdata
########
## Peru
########
#read in anonymized school data
country <- "PER"
year <- "2019"
confidential_folder <- file.path(paste(confidential_dir,"CNT",country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/", sep="/"))
PER_school_anon_df <- read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_short_anon.csv", sep="/"))
PER_school_office_linkages <- read_csv(file=paste0(confidential_folder, "/School/linked_po_school_data_",country,".csv"))
#load gdp info
PER_school_gdp <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_gdp_anon.csv", sep="/"))
PER_school_size <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_anon.csv", sep="/")) %>%
  select(hashed_school_code, students_enrolled) %>%
  filter(!is.na(students_enrolled)) %>%
  group_by(hashed_school_code) %>%
  slice(1) %>%
  ungroup() 

PER_school_office_link_df <- PER_school_anon_df %>%
  left_join(PER_school_gdp) %>%
  left_join(PER_school_office_linkages) %>%
  group_by(hashed_school_code) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(PER_school_size) %>%
  mutate(country="Peru") %>%
  mutate(dist=distHaversine(.[,c('school_lon', 'school_lat')], .[c('office_lon', 'office_lat')]),
         dist_km=dist/1000,
         dist_log=log(dist_km),
         bureau_index=(quality_bureaucracy + national_learning_goals + impartial_decision_making + mandates_accountability )/4,
         # rural2=if_else(urban_rural=="Rural",1,0),
         # rural=if_else(is.na(rural),rural2,rural ),
         students_enrolled=case_when(
           students_enrolled=="Under 25" ~1, 
           students_enrolled=="25-50" ~2 , 
           students_enrolled=="50-75" ~3, 
           students_enrolled=="75-100" ~4, 
           students_enrolled=="100-150" ~5, 
           students_enrolled=="150-300" ~6, 
           students_enrolled=="300-500" ~7, 
           students_enrolled=="Over 500" ~8),
         students_enrolled=factor(students_enrolled, 
                                  levels=c(1:8),
                                  labels= c("Under 25", "25-50", "50-75", "75-100", "100-150", "150-300", "300-500", "Over 500"))) %>%
  group_by(country) %>%
  #impute missing values with the mean within country
  mutate(across(practices, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  mutate(across(policies, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  ungroup() 
########
########
## Jordan
########
#read in anonymized school data
country <- "JOR"
year <- "2019"
confidential_folder <- file.path(paste(confidential_dir,"CNT",country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/", sep="/"))
JOR_school_anon_df <- haven::read_dta(file=paste(anonymized_dir,"CNT", 'JOR-19',paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_short_anon.dta", sep="/"))
JOR_school_office_linkages <- read_csv(file=paste0(confidential_folder, "/School/linked_po_school_data_",country,".csv"))
#load gdp info
JOR_school_gdp <-haven::read_dta(file=paste(anonymized_dir,"CNT", 'JOR-19',paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_gdp_anon.dta", sep="/")) %>%
  select(hashed_school_code, GDP)
#load school size info
JOR_school_size <-haven::read_dta(file=paste(anonymized_dir,"CNT", 'JOR-19',paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_anon.dta", sep="/")) %>%
  select(hashed_school_code, students_enrolled) %>%
  mutate(students_enrolled=factor(students_enrolled, levels=c(1,2,3,4,5,6,7,8), labels = c("Under 25", "25-50", "50-75", "75-100", "100-150", "150-300", "300-500", "Over 500")),
         students_enrolled=as.character(students_enrolled)) %>%
  filter(!is.na(students_enrolled))
JOR_school_office_link_df <- JOR_school_anon_df %>%
  left_join(JOR_school_gdp) %>%
  left_join(JOR_school_office_linkages) %>%
  left_join(JOR_school_size) %>%
  mutate(country="Jordan") %>%
  mutate(dist=distHaversine(.[,c('school_lon', 'school_lat')], .[c('office_lon', 'office_lat')]),
         dist_km=dist/1000,
         dist_log=log(dist_km),
         rural=if_else(rural==1, TRUE,FALSE),
         bureau_index=(quality_bureaucracy + national_learning_goals + impartial_decision_making + mandates_accountability )/4,
         # rural2=if_else(urban_rural=="Rural",1,0),
         # rural=if_else(is.na(rural),rural2,rural ),
         students_enrolled=case_when(
           students_enrolled=="Under 25" ~1, 
           students_enrolled=="25-50" ~2 , 
           students_enrolled=="50-75" ~3, 
           students_enrolled=="75-100" ~4, 
           students_enrolled=="100-150" ~5, 
           students_enrolled=="150-300" ~6, 
           students_enrolled=="300-500" ~7, 
           students_enrolled=="Over 500" ~8),
         students_enrolled=factor(students_enrolled, 
                                  levels=c(1:8),
                                  labels= c("Under 25", "25-50", "50-75", "75-100", "100-150", "150-300", "300-500", "Over 500"))) %>%
  group_by(country) %>%
  #impute missing values with the mean within country
  mutate(across(practices, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  mutate(across(policies, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  ungroup() 
########
########
## Rwanda
########
#read in anonymized school data
country <- "RWA"
year <- "2020"
confidential_folder <- file.path(paste(confidential_dir,"CNT",country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/", sep="/"))
RWA_school_anon_df <- read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_short_anon.csv", sep="/"))
RWA_school_office_linkages <- read_csv(file=paste0(confidential_folder, "/School/linked_po_school_data_",country,".csv"))
#load gdp info
RWA_school_gdp <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_gdp_anon.csv", sep="/"))
#load school size info
RWA_school_size <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_anon.csv", sep="/")) %>%
  select(hashed_school_code, students_enrolled) %>%
  filter(!is.na(students_enrolled))
RWA_school_office_link_df <- RWA_school_anon_df %>%
  left_join(RWA_school_gdp) %>%
  left_join(RWA_school_office_linkages) %>%
  left_join(RWA_school_size) %>%
  mutate(country="Rwanda") %>%
  mutate(dist=distHaversine(.[,c('school_lon', 'school_lat')], .[c('office_lon', 'office_lat')]),
         dist_km=dist/1000,
         dist_log=log(dist_km),
         bureau_index=(quality_bureaucracy + national_learning_goals + impartial_decision_making + mandates_accountability )/4,
         # rural2=if_else(urban_rural=="Rural",1,0),
         # rural=if_else(is.na(rural),rural2,rural ),
         students_enrolled=case_when(
           students_enrolled=="Under 25" ~1, 
           students_enrolled=="25-50" ~2 , 
           students_enrolled=="50-75" ~3, 
           students_enrolled=="75-100" ~4, 
           students_enrolled=="100-150" ~5, 
           students_enrolled=="150-300" ~6, 
           students_enrolled=="300-500" ~7, 
           students_enrolled=="Over 500" ~8),
         students_enrolled=factor(students_enrolled, 
                                  levels=c(1:8),
                                  labels= c("Under 25", "25-50", "50-75", "75-100", "100-150", "150-300", "300-500", "Over 500")))  %>%
  group_by(country) %>%
  #impute missing values with the mean within country
  mutate(across(practices, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  mutate(across(policies, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  ungroup() 
########
########
########
## Ethiopia
########
#read in anonymized school data
country <- "ETH"
year <- "2020_2021"
confidential_folder <- file.path(paste(confidential_dir,"CNT",country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/", sep="/"))
ETH_school_anon_df <- read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_short_anon.csv", sep="/"))
ETH_school_office_linkages <- read_csv(file=paste0(confidential_folder, "/School/linked_po_school_data_",country,".csv"))
#load gdp info
ETH_school_gdp <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_gdp_anon.csv", sep="/"))
#load school size info
ETH_school_size <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_anon.csv", sep="/")) %>%
  select(hashed_school_code, students_enrolled) %>%
  filter(!is.na(students_enrolled)) %>%
  group_by(hashed_school_code) %>%
  slice(1) %>%
  ungroup() 
  
ETH_school_office_link_df <- ETH_school_anon_df %>%
  left_join(ETH_school_gdp) %>%
  left_join(ETH_school_office_linkages) %>%
  group_by(hashed_school_code) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(ETH_school_size) %>%
  mutate(country="Ethiopia")  %>%
  mutate(dist=distHaversine(.[,c('school_lon', 'school_lat')], .[c('office_lon', 'office_lat')]),
         dist_km=dist/1000,
         dist_log=log(dist_km),
         bureau_index=(quality_bureaucracy + national_learning_goals + impartial_decision_making + mandates_accountability )/4,
         rural=if_else(urban_rural=="Rural",TRUE,FALSE),
         students_enrolled=case_when(
           students_enrolled=="Under 25" ~1, 
           students_enrolled=="25-50" ~2 , 
           students_enrolled=="50-75" ~3, 
           students_enrolled=="75-100" ~4, 
           students_enrolled=="100-150" ~5, 
           students_enrolled=="150-300" ~6, 
           students_enrolled=="300-500" ~7, 
           students_enrolled=="Over 500" ~8),
         students_enrolled=factor(students_enrolled, 
                                  levels=c(1:8),
                                  labels= c("Under 25", "25-50", "50-75", "75-100", "100-150", "150-300", "300-500", "Over 500"))) %>%
  group_by(country) %>%
  #impute missing values with the mean within country
  mutate(across(practices, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  mutate(across(policies, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  ungroup() 
########
########
## Madagascar
########
#read in anonymized school data
country <- "MDG"
year <- "2021"
confidential_folder <- file.path(paste(confidential_dir,"CNT",country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/", sep="/"))
MDG_school_anon_df <- read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_short_anon.csv", sep="/"))
MDG_school_office_linkages <- read_csv(file=paste0(confidential_folder, "/School/linked_po_school_data_",country,".csv")) %>%
  select(hashed_school_code, national_learning_goals, targeting,monitoring, incentives, community_engagement, mandates_accountability, coherence, transparency, accountability, quality_bureaucracy, knowledge_skills, work_environment, merit, motivation_attitudes, motivation_relative_start, impartial_decision_making, politicized_policy_implementation, employee_unions_as_facilitators, school_lat, school_lon, office_lat, office_lon)
#load gdp info

MDG_school_gdp <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_gdp_anon.csv", sep="/"))
MDG_school_office_link_df <- MDG_school_anon_df %>%
  left_join(MDG_school_gdp) %>%
  left_join(MDG_school_office_linkages) %>%  
  select(-district) %>%
  mutate(country="Madagascar") %>%
  mutate(
    presence_rate=100-absence_rate,
    dist=distHaversine(.[,c('school_lon', 'school_lat')], .[c('office_lon', 'office_lat')]),
    dist_km=dist/1000,
    dist_log=log(dist_km),
    bureau_index=(quality_bureaucracy + national_learning_goals + impartial_decision_making + mandates_accountability )/4,
    rural=if_else(urban_rural=="Rural",TRUE,FALSE)
  ) %>%
  group_by(country) %>%
  #impute missing values with the mean within country
  mutate(across(c('presence_rate', 'content_knowledge', 'inputs', 'infrastructure', 'ecd_student_knowledge',  'operational_management', 'instructional_leadership', 'principal_knowledge_score', 'principal_management'), ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  mutate(across(policies, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  ungroup() 


########
########
## Sierra Leone
########
#read in anonymized school data
country <- "SLE"
year <- "2022"
confidential_folder <- file.path(paste(confidential_dir,"CNT",country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/", sep="/"))
SLE_school_anon_df <- read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_short_anon.csv", sep="/"))
SLE_school_office_linkages <- read_csv(file=paste0(confidential_folder, "/School/linked_po_school_data_",country,".csv"))
#load gdp info
SLE_school_gdp <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_gdp_anon.csv", sep="/"))
#load school size info
SLE_school_size <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_anon.csv", sep="/")) %>%
  select(hashed_school_code, students_enrolled) %>%
  filter(!is.na(students_enrolled)) %>%
  group_by(hashed_school_code) %>%
  slice(1) %>%
  ungroup() 


SLE_school_office_link_df <- SLE_school_anon_df %>%
  left_join(SLE_school_gdp) %>%
  left_join(SLE_school_office_linkages) %>%
  group_by(hashed_school_code) %>%
  slice(1) %>%
  ungroup()  %>%
  left_join(SLE_school_size) %>%
  mutate(country="Sierra Leone") %>%
  mutate(dist=distHaversine(.[,c('school_lon', 'school_lat')], .[c('office_lon', 'office_lat')]),
         dist_km=dist/1000,
         dist_log=log(dist_km),
         bureau_index=(quality_bureaucracy + national_learning_goals + impartial_decision_making + mandates_accountability )/4,
         # rural2=if_else(urban_rural=="Rural",1,0),
         # rural=if_else(is.na(rural),rural2,rural ),
         students_enrolled=case_when(
           students_enrolled=="Under 25" ~1, 
           students_enrolled=="25-50" ~2 , 
           students_enrolled=="50-75" ~3, 
           students_enrolled=="75-100" ~4, 
           students_enrolled=="100-150" ~5, 
           students_enrolled=="150-300" ~6, 
           students_enrolled=="300-500" ~7, 
           students_enrolled=="Over 500" ~8),
         students_enrolled=factor(students_enrolled, 
                                  levels=c(1:8),
                                  labels= c("Under 25", "25-50", "50-75", "75-100", "100-150", "150-300", "300-500", "Over 500")))  %>%
  group_by(country) %>%
  #impute missing values with the mean within country
  mutate(across(practices, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  mutate(across(policies, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  ungroup() 


########
########
## Niger
########
#read in anonymized school data
country <- "NER"
year <- "2022"
confidential_folder <- file.path(paste(confidential_dir,"CNT",country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/", sep="/"))
NER_school_anon_df <- read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_short_anon.csv", sep="/"))
NER_school_office_linkages <- read_csv(file=paste0(confidential_folder, "/School/linked_po_school_data_",country,".csv"))
#load gdp info
NER_school_gdp <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_gdp_anon.csv", sep="/"))
#load school size info
NER_school_size <-read_csv(file=paste(anonymized_dir,"CNT", country,paste(country,year,"GEPD", sep="_"), paste(country,year,"GEPD_v01_M", sep="_"),"Data/School/data/school_dta_anon.csv", sep="/")) %>%
  select(hashed_school_code, students_enrolled) %>%
  filter(!is.na(students_enrolled))
NER_school_office_link_df <- NER_school_anon_df %>%
  left_join(NER_school_gdp) %>%
  left_join(NER_school_office_linkages) %>%
  left_join(NER_school_size) %>%
  mutate(country="Niger") %>%
  mutate(dist=distHaversine(.[,c('school_lon', 'school_lat')], .[c('office_lon', 'office_lat')]),
         dist_km=dist/1000,
         dist_log=log(dist_km),
         bureau_index=(quality_bureaucracy + national_learning_goals + impartial_decision_making + mandates_accountability )/4,
         # rural2=if_else(urban_rural=="Rural",1,0),
         # rural=if_else(is.na(rural),rural2,rural ),
         students_enrolled=case_when(
           students_enrolled=="Under 25" ~1, 
           students_enrolled=="25-50" ~2 , 
           students_enrolled=="50-75" ~3, 
           students_enrolled=="75-100" ~4, 
           students_enrolled=="100-150" ~5, 
           students_enrolled=="150-300" ~6, 
           students_enrolled=="300-500" ~7, 
           students_enrolled=="Over 500" ~8),
         students_enrolled=factor(students_enrolled, 
                                  levels=c(1:8),
                                  labels= c("Under 25", "25-50", "50-75", "75-100", "100-150", "150-300", "300-500", "Over 500")))  %>%
  group_by(country) %>%
  #impute missing values with the mean within country
  mutate(across(practices, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  mutate(across(policies, ~if_else(is.na(.), mean(., na.rm=TRUE),.))) %>%
  ungroup() 

###########
# Combined
##########



combined_school_office_df <- bind_rows(PER_school_office_link_df, JOR_school_office_link_df, RWA_school_office_link_df, ETH_school_office_link_df, MDG_school_office_link_df, SLE_school_office_link_df, NER_school_office_link_df) 


combined_school_office_df %>%
  rename(pol_policy_implementation=politicized_policy_implementation) %>%
  haven::write_dta( paste0(confidential_dir, "General/combined_school_office_df.dta")) 

