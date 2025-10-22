#Use of Survey Solutions API system
#Brian Stacy 6/11/2019

library(httr)
library(haven)
library(dplyr)
library(Hmisc)
library(tidyr)
library(here)

need_api=1
######################################
# User Inputs for API #
######################################
# Here you need to indicate the path where you replicated the folder structures on your own computer
here::here() #"C:/Users/wb469649/Documents/Github/GEPD"
if (need_api==1) {
  pw_file<-here::here("password.R")
  if (file.exists(pw_file)) {
    source(pw_file)
    user <- api_user
  } else {
    #these credentials may need to be entered
    user<-rstudioapi::askForPassword(prompt = 'Please enter API username:')
    password <- rstudioapi::askForPassword(prompt = 'Please enter API password:')
  }
  
  #Survey Solutions Server address
  #e.g. server_add<-"https://gepd.mysurvey.solutions"
  server_add<- svDialogs::dlgInput("Please Enter Server http Address:", 'http://etri.gepd.solutions/gepdnig')$res
  
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
    QuestionnaireId= paste0("e14337d75bad4210b1ea87a60b084203$", quest_version),
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
  Sys.sleep(20)
  
  
  
  
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
  
  unzip(file.path(download_folder, file_name), exdir=paste0(download_folder, "/version_",quest_version))
  
}

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
  select(-c('...1', '...8'))

indicator_names <-  indicators$indicator_tag
indicator_names <- sapply(indicator_names, tolower)

#############################
#Because we switched versions of our survey in the middle, have to append version 17 databases
############################

# get list of main files

main_files <- c(
  'EPDash',
  'questionnaire_roster',
  'teacher_assessment_answers',
  'TEACHERS',
  'etri_roster',
  'ecd_assessment',
  'fourth_grade_assessment',
  'random_list',
  'before_after_closure',
  'climatebeliefs',
  'teacherimpact',
  'direct_instruction_etri',
  'planning_lesson_etri',
  'ability_to_use_etri',
  'digital_use_inschool_etri',
  'use_outsideschool_etri',
  'proficiency_ict_etri',
  'schoolcovid_roster'
)

combine_main_files <- function(file_names) {
  
  #read in  file
  file_list <- list.files(download_folder, pattern = paste0(file_names,"\\.dta$"), full.names = TRUE, recursive = TRUE)
  # Initialize an empty data frame
  combined_dta <- data.frame()
  
  # Read and bind the rows from each .dta file
  for (file in file_list) {
    data <- read_dta(file) %>%
      mutate(across(starts_with('teacher_phone_number'), as.character))
    
    combined_dta <- bind_rows(combined_dta, data)
  }
  
  combined_dta %>%
    write_dta(file.path(download_folder, paste0(file_names,"_combined.dta")))
  
}

lapply(main_files, combine_main_files)
