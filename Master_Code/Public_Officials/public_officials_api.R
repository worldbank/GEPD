#Test of Survey Solutions API system
#Brian Stacy 6/11/2019

library(httr)
library(haven)
library(dplyr)
library(tidyr)
library(here)
######################################
# User Inputs for API #
######################################
# Here you need to indicate the path where you replicated the folder structures on your own computer
here() #"C:/Users/wb469649/Documents/Github/GEPD"

#user credentials
#Check whether password.R file is in Github repo
pw_file<-here::here("password.R")
if (file.exists(pw_file)) {
  source(pw_file)
} else {
  #these credentials may need to be entered
  user<-rstudioapi::askForPassword(prompt = 'Please enter API username:')
  password <- rstudioapi::askForPassword(prompt = 'Please enter API password:')
}

#Survey Solutions Server address
#e.g. server_add<-"https://gepd.mysurvey.solutions"
server_add<- svDialogs::dlgInput("Please Enter Server http Address:", 'https://{enter here}.mysurvey.solutions')$res

#questionnaire version
#e.g. quest_version<-8
quest_version<-svDialogs::dlgInput("Please enter Questionnaire Version:", 'Enter integer')$res 
  
#path and folder where the .zip file will be stored
#this needs to be entered
#Please note that the following directory path may need to be created
  
currentDate<-Sys.Date()

tounzip <- paste("mydata-",currentDate, ".zip" ,sep="")



######################################
# Interactions with API
######################################

#Get list of questionnaires available
#the server address may need to be modified
q<-GET(paste(server_add,"/api/v1/questionnaires", sep=""),
       authenticate(user, password))

str(content(q))


#pull data from version of our Education Policy Dashboard Questionnaire
POST(paste(server_add,"/api/v1/export/stata/25534a374fa8434bb7d6f5133cdebab2$",quest_version,"/start", sep=""),
         authenticate(user, password))

#sleep for 10 seconds to wait for stata file to compile
Sys.sleep(10)

dataDownload <- GET(paste(server_add,"/api/v1/export/stata/25534a374fa8434bb7d6f5133cdebab2$", quest_version,"/",sep=""),
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





#read in public officials interview file
public_officials_dta<-read_dta(file.path(download_folder, "public_officials.dta"))
public_officials_metadata<-makeVlist(public_officials_dta)
