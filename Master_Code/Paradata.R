#File produced by Brian Stacy
# 8/19/2019

#This file will download and clean paradata from the Survey Solutions API

library(httr)
library(haven)
library(dplyr)
library(tidyr)
library(Hmisc)
library(lubridate)
library(stringr)
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
quest_version<-"3"
  
#path and folder where the .zip file will be stored
#this needs to be entered
download_folder <- choose.dir(default = "", caption = "Select folder to save data downloaded from API")
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
POST(paste(server_add,"/api/v1/export/paradata/06756cace6d24cc996ffccbfc26a2264$",quest_version,"/start", sep=""),
     authenticate(user, password))


dataDownload <- GET(paste(server_add,"/api/v1/export/paradata/06756cace6d24cc996ffccbfc26a2264$", quest_version,"/",sep=""),
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


#########################################
# Read in paradata and do basic cleaning
#########################################

#read in data
para_df<-read.delim(paste(download_folder, "paradata.tab", sep="/"), sep="\t")

#label variables
var.labels=c(
            interview__id = "Unique 32-character long identifier of the interview",
            order = "Sequential event number within each interview",
            event = "Type of event happened",
            responsible = "Login name of the person who initiated the event",
            role = "System role of the person who initiated the event",
            timestamp = "Date and time when the event happened",
            offset = "Timezone offset relative to UTC",
            parameters = "Event-specific parameters"
            )
# Label data
label(para_df) = as.list(var.labels[match(names(para_df), names(var.labels))])


######################################
# Clean Paradata files
######################################
#clean up timestamp
para_df <- para_df %>% 
  mutate(timestamp= ymd_hms(timestamp))

#Only keep paradata on actual questions for indicators. These are tagged with m******
para_df <- para_df %>% 
  filter(str_detect(parameters, "m1s|m2s|m3s|m4s|m5s|m6s|m7s|m8s"))


#Split up question name to identify tags for each indicator
para_df <- para_df %>% 
  separate(parameters, c("question", NA, "response", NA, "other"), remove=FALSE, sep = "[|]")

  