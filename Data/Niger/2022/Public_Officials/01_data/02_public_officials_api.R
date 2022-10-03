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
here::here() #"C:/Users/wb469649/Documents/Github/GEPD"
if (need_api==1) {
  pw_file<-here::here("GitHub/GEPD/password.R")
  if (file.exists(pw_file)) {
    source(pw_file)
  } else {
    #these credentials may need to be entered
    user<-rstudioapi::askForPassword(prompt = 'Please enter API username:')
    password <- rstudioapi::askForPassword(prompt = 'Please enter API password:')
  }
  
  #Survey Solutions Server address
  #e.g. server_add<-"https://gepd.mysurvey.solutions"
  server_add<- svDialogs::dlgInput("Please Enter Server http Address:", 'http://etri.gepd.solutions/gepdngr')$res
  
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
  
  
  #pull data from version of our Education Policy Dashboard Questionnaire
  
  json_body <- jsonlite::toJSON(list(
    ExportType= "Stata",
    QuestionnaireId= paste0("db3624a7890b4382b294a9a6a12a8a37$", quest_version),
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
  
  
}

##Unzing in appropriate folder

if (quest_version==3) {
  unzip(file.path(download_folder, file_name), exdir=paste(download_folder,'version_3', sep="/"))
  unzip(file.path(download_folder, file_name), exdir=paste(download_folder))
  
}  else if (quest_version==2) {
  unzip(file.path(download_folder, file_name), exdir=paste(download_folder,'version_2', sep="/"))
  
} else {
  unzip(file.path(download_folder, file_name), exdir=download_folder)
}

#Create function to save metadata for each question in each module
#The attr function retrieves metadata imported by haven. E.g. attr(school_dta$m1s0q2_code, "label")
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}





#read in public officials interview file
#read in public officials interview file

public_officials_dta<-read_dta(file.path(download_folder, po_file)) 

public_officials_metadata<-makeVlist(public_officials_dta)

#bind version 7
public_officials_dta_2<-read_dta(file.path(download_folder,'version_2', po_file)) 

public_officials_dta <- public_officials_dta %>%
  bind_rows(public_officials_dta_2)


public_officials_dta <- public_officials_dta %>%
  mutate(m1s0q1_number_other=as.character(m1s0q1_number_other)) 

write_dta(public_officials_dta, file.path(download_folder, po_file))

public_officials_metadata<-makeVlist(public_officials_dta)
