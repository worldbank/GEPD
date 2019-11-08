#File to clean questions specific to peru for governement


#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(skimr)
library(naniar)
library(vtable)
library(digest)
#NOTE:  The R script to pull the data from the API should be run before this file



#Create function to save metadata for each question in each module
#The attr function retrieves metadata imported by haven. E.g. attr(school_dta$m1s0q2_code, "label")
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}



#Country name and year of survey
country <-'PER'
country_name <- "Peru"
year <- '2019'

#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

backup_onedrive="no"

#Add your UPI here and set the directory paths of your choice.
if (Sys.getenv("USERNAME") == "wb469649"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/"
  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School", sep="/"))
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School", sep="/"))
  
} else if (Sys.getenv("USERNAME") == "wb550666"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb550666/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/"
  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School", sep="/"))
  
  # This is experimental and not currently in use.
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb550666/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School", sep="/"))
  
} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")
  save_folder_onedrive <- choose.dir(default = "", caption = "Select folder to save backed up data to onedrive")
  
}





###########################
#read in school level file
###########################
school_dta<-read_dta(file.path(download_folder, "EPDash.dta"))
vtable(school_dta)
#rename a few key variables up front
school_dta<- school_dta %>%
  mutate(enumerator_name_other= m1s0q1_name_other  ,
         enumerator_number=if_else(!is.na(m1s0q1_name),m1s0q1_name, as.double(m1s0q1_number_other)) ,
         survey_time=m1s0q8,
         lat=m1s0q9__Latitude,
         lon=m1s0q9__Longitude,
         school_code=if_else(!is.na(school_code_preload),as.double(school_code_preload), as.double(m1s0q2_code)),
         m7_teach_count_pknw=m7_teach_count #this variable was mistakenly not tagged as pknw
  )

#create school metadata frame
school_metadta<-makeVlist(school_dta)


#list additional info that will be useful to keep in each indicator dataframe
preamble_info <- c( 'school_code')

teacher_preamble <- c('teacher_number')

school_dta_anon <- school_dta %>%
  select(preamble_info, contains("peru")) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.)))




#########################################
#read in teacher questionnaire level file
#########################################
teacher_questionnaire<-read_dta(file.path(download_folder ,"questionnaire_roster.dta"))
teacher_questionnaire_metadta<-makeVlist(teacher_questionnaire)

#Add school preamble info
teacher_questionnaire_anon <- teacher_questionnaire %>%
  left_join(school_dta) %>% 
  select(school_code, colnames(teacher_questionnaire)) %>%
  select(-interview__key, -interview__id) %>%
  left_join(school_dta_anon) 
  


#filter out teachers who did not consent to interview

teacher_questionnaire_anon <- teacher_questionnaire_anon %>%
  filter(m3s0q1==1)


teacher_questionnaire_anon$teacher_number <-lapply(teacher_questionnaire_anon$m3sb_tnumber, function(x) {digest(x, algo="xxhash64", seed=531254, )})

teacher_questionnaire_anon <- teacher_questionnaire_anon %>%
  mutate(teacher_number=as.character(teacher_number)) %>%
  select(preamble_info, teacher_preamble, contains("peru")) 


write_dta(teacher_questionnaire_anon, file.path(save_folder ,"peru_items.dta"))
write_excel_csv(teacher_questionnaire_anon, file.path(save_folder ,"peru_items.csv"))

write_dta(school_dta_anon, file.path(save_folder ,"peru_items_principal.dta"))
write_excel_csv(school_dta_anon, file.path(save_folder ,"peru_items_principal.csv"))
