#Use of Survey Solutions API system
#Brian Stacy 6/11/2019

library(httr)
library(haven)
library(dplyr)
library(Hmisc)
library(tidyr)
library(here)
######################################
# User Inputs for API #
######################################
# Here you need to indicate the path where you replicated the folder structures on your own computer
here::here() #"C:/Users/wb469649/Documents/Github/GEPD"

#user credentials
#Check whether password.R file is in Github repo
# pw_file<-here::here("password.R")
# if (file.exists(pw_file)) {
#   source(pw_file)
# } else {
#   #these credentials may need to be entered
#   user<-rstudioapi::askForPassword(prompt = 'Please enter API username:')
#   password <- rstudioapi::askForPassword(prompt = 'Please enter API password:')
# }
# 
# #Survey Solutions Server address
# #e.g. server_add<-"https://gepd.mysurvey.solutions"
# server_add<- svDialogs::dlgInput("Please Enter Server http Address:", 'http://ec2-3-86-122-22.compute-1.amazonaws.com/')$res
# 
# #questionnaire version
# #e.g. quest_version<-8
# quest_version<-svDialogs::dlgInput("Please enter Questionnaire Version:", 'Enter integer')$res 
#   
# #path and folder where the .zip file will be stored
# #this needs to be entered
# #Please note that the following directory path may need to be created
#   
# 
# currentDate<-Sys.Date()
# 
# tounzip <- paste("mydata-",currentDate, ".zip" ,sep="")
# 
# 
# 
# ######################################
# # Interactions with API
# ######################################
# 
# #Get list of questionnaires available
# #the server address may need to be modified
# # q<-GET(paste(server_add,"/primary/api/v1/questionnaires", sep=""),
# #        authenticate(user, password))
# # 
# # str(content(q))
# 
# #pull data from version of our Education Policy Dashboard Questionnaire
# # POST(paste(server_add,"/primary/api/v2/export/STATA/3a2c5f18-6019-4181-9e0f-1231a7dd45f8$",quest_version, sep=""),
# #          authenticate(user, password))
# 
# #sleep for 10 seconds to wait for stata file to compile
# #Sys.sleep(10)
# 
# dataDownload <- GET(paste(server_add,"primary/api/v2/export/", quest_version,"/file",sep=""),
#                     authenticate(user, password))
# 
# 
# #Now save zip to computer
# filecon <- file(file.path(download_folder, tounzip), "wb")
# 
# writeBin(dataDownload$content, filecon) 
# #close the connection
# close(filecon)
# 
# 
# unzip(file.path(download_folder, tounzip), exdir=download_folder)


#Create function to save metadata for each question in each module
#The attr function retrieves metadata imported by haven. E.g. attr(school_dta$m1s0q2_code, "label")
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}


#Read in list of indicators
indicators <- readr::read_delim(here::here('Indicators','indicators.md'), delim="|", trim_ws=TRUE)
indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)

indicators <- indicators %>%
  select(-c('X1', 'X8'))

indicator_names <-  indicators$indicator_tag
indicator_names <- sapply(indicator_names, tolower)

#############################
#Because we switched versions of our survey in the middle, have to append version 17 databases
############################


#read in school level file
school_dta <- read_dta(file.path(download_folder, school_file))



#Add in school metadata
school_metadta<-makeVlist(school_dta) %>%
  mutate(indicator_tag=as.character(NA)) 
           


for (i in indicator_names ) {
  school_metadta<-school_metadta %>%
    mutate(indicator_tag=if_else(grepl(i,name ),toupper(i),indicator_tag, as.character(NA)) )
  
}

school_metadta<-school_metadta %>%
  left_join(indicators)

#bind version 18 and 17

label(school_dta) = as.list(as.character(school_metadta$varlabel))

school_dta %>%
  write_dta(file.path(download_folder,school_file))



#read in ecd level file
ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))

#Add in ecd metadata
ecd_metadta<-makeVlist(ecd_dta) %>%
  mutate(indicator_tag='LCAP' )



ecd_metadta<-ecd_metadta %>%
  left_join(indicators)

#bind version 18 and 17

label(ecd_dta) = as.list(as.character(ecd_metadta$varlabel))

ecd_dta %>%
  write_dta(file.path(download_folder, "ecd_assessment.dta"))



#read in 4th grade assessment level file
assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))


#Add in assessment metadata
assess_4th_grade_metadta<-makeVlist(assess_4th_grade_dta) %>%
  mutate(indicator_tag='LERN' )


assess_4th_grade_metadta<-assess_4th_grade_metadta %>%
  left_join(indicators)

#bind version 18 and 17

label(assess_4th_grade_dta) = as.list(as.character(assess_4th_grade_metadta$varlabel))


assess_4th_grade_dta %>%
  write_dta(file.path(download_folder, "fourth_grade_assessment.dta"))



#read in teacher questionnaire level file
teacher_questionnaire<-read_dta(file.path(download_folder, "questionnaire_roster.dta"))

#Add in questionnaire metadata
teacher_questionnaire_metadta<-makeVlist(teacher_questionnaire) %>%
  mutate(indicator_tag=as.character(NA)) 



for (i in indicator_names ) {
  teacher_questionnaire_metadta<-teacher_questionnaire_metadta %>%
    mutate(indicator_tag=if_else(grepl(i,name ),toupper(i),indicator_tag, as.character(NA)) )
  
}

teacher_questionnaire_metadta<-teacher_questionnaire_metadta %>%
  left_join(indicators)

#bind version 18 and 17


label(teacher_questionnaire) = as.list(as.character(teacher_questionnaire_metadta$varlabel))


teacher_questionnaire %>%
  write_dta(file.path(download_folder, "questionnaire_roster.dta"))



#read in teacher absence file
teacher_absence_dta<-read_dta(file.path(download_folder, "TEACHERS.dta"))

#Add in absemce metadata
teacher_absence_metadta<-makeVlist(teacher_absence_dta) %>%
  mutate(indicator_tag=as.character(NA)) 



for (i in indicator_names ) {
  teacher_absence_metadta<-teacher_absence_metadta %>%
    mutate(indicator_tag=if_else(grepl(i,name ),toupper(i),indicator_tag, as.character(NA)) )
  
}

teacher_absence_metadta<-teacher_absence_metadta %>%
  left_join(indicators)

#bind version 18 and 17

label(teacher_absence_dta) = as.list(as.character(teacher_absence_metadta$varlabel))


teacher_absence_dta %>%
  write_dta(file.path(download_folder, "TEACHERS.dta"))



#read in teacher assessment file
teacher_assessment_dta<-read_dta(file.path(download_folder, "teacher_assessment_answers.dta"))

#Add in assessment metadata
teacher_assessment_metadta<-makeVlist(teacher_assessment_dta) %>%
  mutate(indicator_tag='CONT') 




teacher_assessment_metadta<-teacher_assessment_metadta %>%
  left_join(indicators)

#bind version 18 and 17

label(teacher_assessment_dta) = as.list(as.character(teacher_assessment_metadta$varlabel))


teacher_assessment_dta %>%
  write_dta(file.path(download_folder, "teacher_assessment_answers.dta"))

school_metadta$varlabel<-as.character(school_metadta$varlabel)
metadta<-bind_rows(school_metadta,  ecd_metadta, assess_4th_grade_metadta, teacher_questionnaire_metadta, teacher_assessment_metadta)

metadta %>%
  writexl::write_xlsx( path=file.path(download_folder, "metadata.xlsx"))



