#Test of Survey Solutions API system
#Brian Stacy 6/11/2019

library(httr)
library(haven)
library(dplyr)
library(tidyr)
source("C:/Users/WB469649/OneDrive - WBG/Education Policy Dashboard/Survey Solutions/API/SuSoAPI-master/dl_one.R")

#user credentials
user<-"bstacy_api"
password<-"GEPD_bstacy6"


#path and folder where the .zip file will be stored
download_folder <- file.path("C:/Users/WB469649/OneDrive - WBG/Education Policy Dashboard/Survey Solutions/Peru/Data")
tounzip <- "mydata.zip" 


#Get list of questionnaires available
q<-GET("https://gepd.mysurvey.solutions/api/v1/questionnaires",
       authenticate(user, password))

str(content(q))



user<-"bstacy_api"
password<-"GEPD_bstacy6"

#Get list of questionnaires available
q<-GET("https://gepd.mysurvey.solutions/api/v1/questionnaires",
       authenticate(user, password))

str(content(q))


#pull data from version 8 of our Education Policy Dashboard Questionnaire
POST("https://gepd.mysurvey.solutions/api/v1/export/stata/06756cace6d24cc996ffccbfc26a2264$4/start",
         authenticate(user, password))


GET("https://gepd.mysurvey.solutions/api/v1/export/stata/06756cace6d24cc996ffccbfc26a2264$4/details",
    authenticate(user, password))


GET("https://gepd.mysurvey.solutions/api/v1/export/stata/06756cace6d24cc996ffccbfc26a2264$4/",
    authenticate(user, password))

dataDownload <- GET("https://gepd.mysurvey.solutions/api/v1/export/stata/06756cace6d24cc996ffccbfc26a2264$4/",
           authenticate(user, password))

redirectURL <- dataDownload$url 
RawData <- GET(redirectURL) #Sucess!!


#Now save zip to computer
filecon <- file(file.path(download_folder, tounzip), "wb")

writeBin(RawData$content, filecon) 
#close the connection
close(filecon)

#unzip
unzip(file.path(download_folder, tounzip), exdir=download_folder)


#Create function to save metadata for each question in each module
#The attr function retrieves metadata imported by haven. E.g. attr(school_dta$m1s0q2_code, "label")
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}




#read in school level file
school_dta<-read_dta(file.path(download_folder, "EPDash.dta"))
school_metadta<-makeVlist(school_dta)

#read in ecd level file
ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))
ecd_metadta<-makeVlist(ecd_dta)

#read in 4th grade assessment level file
assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))
assess_4th_grade_metadta<-makeVlist(assess_4th_grade_dta)

#read in teacher questionnaire level file
teacher_questionnaire<-read_dta(file.path(download_folder, "questionnaire_roster.dta"))
teacher_questionnaire_metadta<-makeVlist(teacher_questionnaire_dta)

#read in teacher absence file
teacher_absence_dta<-read_dta(file.path(download_folder, "absence_roster2.dta"))
teacher_absence_metadta<-makeVlist(teacher_absence_dta)

#read in principal absence file
principal_absence_dta<-read_dta(file.path(download_folder, "principal_absence_roster2.dta"))
principal_absence_metadta<-makeVlist(principal_absence_dta)

#read in teacher assessment file
teacher_assessment_dta<-read_dta(file.path(download_folder, "teacher_assessment.dta"))
teacher_assessment_metadta<-makeVlist(teacher_assessment_dta)



