#Test of Survey Solutions API system
#Brian Stacy 6/11/2019

library(httr)
library(haven)
library(dplyr)
library(tidyr)

######################################
# User Inputs for API #
######################################

#user credentials
#these credentials may need to be modified
user<-rstudioapi::askForPassword(prompt = 'Please enter API username:')
password <- rstudioapi::askForPassword(prompt = 'Please enter API password:')

#Survey Solutions Server address
#e.g. server_add<-"https://gepd.mysurvey.solutions"
server_add<-"https://gepdmoz.mysurvey.solutions"

#questionnaire version
#e.g. quest_version<-8
quest_version<-8
  
  #path and folder where the .zip file will be stored
  #this needs to be entered
  download_folder <- file.path()
tounzip <- "mydata.zip" 


######################################
# Interactions with API
######################################

#Get list of questionnaires available
#the server address may need to be modified
q<-GET(paste(server_add,"/api/v1/questionnaires", sep=""),
       authenticate(user, password))

str(content(q))


#pull data from version of our Education Policy Dashboard Questionnaire
POST(paste(server_add,"/api/v1/export/stata/06756cace6d24cc996ffccbfc26a2264$",quest_version,"/start", sep=""),
     authenticate(user, password))


dataDownload <- GET(paste(server_add,"/api/v1/export/stata/06756cace6d24cc996ffccbfc26a2264$", quest_version,"/",sep=""),
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



