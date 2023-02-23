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
if (need_api==1) {
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
  server_add<- svDialogs::dlgInput("Please Enter Server http Address:", 'http://etri.gepd.solutions/gepdpak')$res
  
  #questionnaire version
  #e.g. quest_version<-8
  quest_version<-svDialogs::dlgInput("Please enter Questionnaire Version:", 'Enter integer')$res 
  
  #path and folder where the .zip file will be stored
  #this needs to be entered
  #Please note that the following directory path may need to be created
  
  
  currentDate<-Sys.Date()
  
  tounzip <- paste("mydata-",currentDate, ".zip" ,sep="")
  


######################################
# Interactions with API - GEPD school
######################################

#Get list of questionnaires available
#the server address may need to be modified


#pull data from version of our Education Policy Dashboard Questionnaire

json_body <- jsonlite::toJSON(list(
  ExportType= "Stata",
  QuestionnaireId= paste0("feeef8fb3b3049dbbf945c1768b788f3$", quest_version),
  From="",
  To="",
  AccessToken="",
  RefreshToken="",
  StorageType="",
  TranslationId="",
  InterviewStatus= "All",
  IncludeMeta= TRUE
), auto_unbox = TRUE)

pst <- POST(paste(server_add,"/api/v2/export", sep=""),
            body=json_body,
            authenticate(user, password),
            accept_json(),
            content_type_json())

#get new export id
id <-  unlist(content(pst)[1])

#sleep for 10 seconds to wait for stata file to compile
Sys.sleep(10)




dataDownload <- GET(paste(server_add,"/api/v2/export/",id,sep=""),
                    authenticate(user, password))

#download file
redirectURL <- paste0(dataDownload$url, "/file")

file_name <- paste0("survey_solutions_",Sys.Date(),".zip")

#Now save zip to computer
response <- httr::GET(
  url = redirectURL,
  authenticate(user = user, password = password),
  accept_json(),
  content_type_json(),
  write_disk(fs::path(download_folder, file_name), overwrite = TRUE)
)



######################################
# Interactions with API - AIM ECD
######################################

#Get list of questionnaires available
#the server address may need to be modified

download_folder_aimecd <- "C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/ICT/raw/aim_ecd"

#pull data from version of our Education Policy Dashboard Questionnaire

json_body <- jsonlite::toJSON(list(
  ExportType= "Stata",
  QuestionnaireId= paste0("285a2e9343894a3fbf39c214d2296864$4"),
  From="",
  To="",
  AccessToken="",
  RefreshToken="",
  StorageType="",
  TranslationId="",
  InterviewStatus= "All",
  IncludeMeta= TRUE
), auto_unbox = TRUE)

pst <- POST(paste(server_add,"/api/v2/export", sep=""),
            body=json_body,
            authenticate(user, password),
            accept_json(),
            content_type_json())

#get new export id
id <-  unlist(content(pst)[1])

#sleep for 10 seconds to wait for stata file to compile
Sys.sleep(10)




dataDownload <- GET(paste(server_add,"/api/v2/export/",id,sep=""),
                    authenticate(user, password))

#download file
redirectURL <- paste0(dataDownload$url, "/file")

file_name_aimecd <- paste0("survey_solutions_aimecd_",Sys.Date(),".zip")

#Now save zip to computer
response <- httr::GET(
  url = redirectURL,
  authenticate(user = user, password = password),
  accept_json(),
  content_type_json(),
  write_disk(fs::path(download_folder_aimecd, file_name_aimecd), overwrite = TRUE)
)


######################################
# Interactions with API - TEACH ECE
######################################

#Get list of questionnaires available
#the server address may need to be modified

download_folder_teach <- "C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/ICT/raw/teach_ece"

#pull data from version of our Education Policy Dashboard Questionnaire

json_body <- jsonlite::toJSON(list(
  ExportType= "Stata",
  QuestionnaireId= paste0("0b5398fcfb8c44928e61008bd66174f2$7"),
  From="",
  To="",
  AccessToken="",
  RefreshToken="",
  StorageType="",
  TranslationId="",
  InterviewStatus= "All",
  IncludeMeta= TRUE
), auto_unbox = TRUE)

pst <- POST(paste(server_add,"/api/v2/export", sep=""),
            body=json_body,
            authenticate(user, password),
            accept_json(),
            content_type_json())

#get new export id
id <-  unlist(content(pst)[1])

#sleep for 10 seconds to wait for stata file to compile
Sys.sleep(10)




dataDownload <- GET(paste(server_add,"/api/v2/export/",id,sep=""),
                    authenticate(user, password))

#download file
redirectURL <- paste0(dataDownload$url, "/file")

file_name_teach <- paste0("survey_solutions_teach_",Sys.Date(),".zip")

#Now save zip to computer
response <- httr::GET(
  url = redirectURL,
  authenticate(user = user, password = password),
  accept_json(),
  content_type_json(),
  write_disk(fs::path(download_folder_teach, file_name_teach), overwrite = TRUE)
)


}


##Unzing in appropriate folder the school survey data

if (quest_version==8) {
  unzip(file.path(download_folder, file_name), exdir=paste(download_folder,'version_8', sep="/"))
  unzip(file.path(download_folder, file_name), exdir=paste(download_folder))
  
}  else if (quest_version==7) {
  unzip(file.path(download_folder, file_name), exdir=paste(download_folder,'version_7', sep="/"))
  
} else {
  unzip(file.path(download_folder, file_name), exdir=download_folder)
}

##Unzing in appropriate folder the aim ecd

  unzip(file.path(download_folder_aimecd, file_name_aimecd), exdir=paste(download_folder_aimecd))


  ##Unzing in appropriate folder the aim ecd
  
  unzip(file.path(download_folder_teach, file_name_teach), exdir=paste(download_folder_teach))


