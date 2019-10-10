#Use of Survey Solutions API system
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
POST(paste(server_add,"/api/v1/export/stata/06756cace6d24cc996ffccbfc26a2264$",quest_version,"/start", sep=""),
         authenticate(user, password))

#sleep for 10 seconds to wait for stata file to compile
Sys.sleep(10)

dataDownload <- GET(paste(server_add,"/api/v1/export/stata/06756cace6d24cc996ffccbfc26a2264$", quest_version,"/",sep=""),
                    authenticate(user, password))

redirectURL <- dataDownload$url 
RawData <- GET(redirectURL) #Sucess!!


#Now save zip to computer
filecon <- file(file.path(download_folder, tounzip), "wb")

writeBin(RawData$content, filecon) 
#close the connection
close(filecon)

#unzip.  Because we switched versions of our survey in the middle, have to append version 17 databases
if (quest_version==17) {
unzip(file.path(download_folder, tounzip), exdir=paste(download_folder,'version_17', sep="/"))
  
} else if (quest_version==15) {
  unzip(file.path(download_folder, tounzip), exdir=paste(download_folder,'version_15', sep="/"))
  
} else {
unzip(file.path(download_folder, tounzip), exdir=download_folder)
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
  select(-c('X1', 'X8'))

indicator_names <-  indicators$indicator_tag
indicator_names <- sapply(indicator_names, tolower)

#############################
#Because we switched versions of our survey in the middle, have to append version 17 databases
############################

if (quest_version!=17) {
#read in school level file
school_dta<-read_dta(file.path(download_folder, "EPDash.dta"))


school_dta_17<-read_dta(file.path(paste(download_folder,'version_17', sep="/"), "EPDash.dta"))

school_dta_15<-read_dta(file.path(paste(download_folder,'version_15', sep="/"), "EPDash.dta"))


#Add in school metadata
school_metadta<-makeVlist(school_dta_17) %>%
  mutate(indicator_tag=as.character(NA)) 
           


for (i in indicator_names ) {
  school_metadta<-school_metadta %>%
    mutate(indicator_tag=if_else(grepl(i,name ),toupper(i),indicator_tag, as.character(NA)) )
  
}

school_metadta<-school_metadta %>%
  left_join(indicators)

#bind version 18 and 17
school_dta <- school_dta %>%
  bind_rows(school_dta_17) %>%
  bind_rows(school_dta_15) 

school_dta %>%
  write_dta(file.path(download_folder, "EPDash.dta"))



#read in ecd level file
ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))
ecd_dta_17<-read_dta(file.path(paste(download_folder,'version_17', sep="/"), "ecd_assessment.dta"))
ecd_dta_15<-read_dta(file.path(paste(download_folder,'version_15', sep="/"), "ecd_assessment.dta"))

#Add in ecd metadata
ecd_metadta<-makeVlist(ecd_dta_17) %>%
  mutate(indicator_tag='LCAP' )



ecd_metadta<-ecd_metadta %>%
  left_join(indicators)

#bind version 18 and 17

ecd_dta <- ecd_dta %>%
  bind_rows(ecd_dta_17) %>%
  bind_rows(ecd_dta_15)


ecd_dta %>%
  write_dta(file.path(download_folder, "ecd_assessment.dta"))



#read in 4th grade assessment level file
assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))
assess_4th_grade_dta_17<-read_dta(file.path(paste(download_folder,'version_17', sep="/"), "fourth_grade_assessment.dta"))
assess_4th_grade_dta_15<-read_dta(file.path(paste(download_folder,'version_15', sep="/"), "fourth_grade_assessment.dta"))


#Add in assessment metadata
assess_4th_grade_metadta<-makeVlist(assess_4th_grade_dta_17) %>%
  mutate(indicator_tag='LERN' )


assess_4th_grade_metadta<-assess_4th_grade_metadta %>%
  left_join(indicators)

#bind version 18 and 17

assess_4th_grade_dta <- assess_4th_grade_dta %>%
  bind_rows(assess_4th_grade_dta_17) %>%
  bind_rows(assess_4th_grade_dta_15)


assess_4th_grade_dta %>%
  write_dta(file.path(download_folder, "fourth_grade_assessment.dta"))



#read in teacher questionnaire level file
teacher_questionnaire<-read_dta(file.path(download_folder, "questionnaire_roster.dta"))
teacher_questionnaire_17<-read_dta(file.path(paste(download_folder,'version_17', sep="/"), "questionnaire_roster.dta"))
teacher_questionnaire_15<-read_dta(file.path(paste(download_folder,'version_15', sep="/"), "questionnaire_roster.dta"))

#Add in questionnaire metadata
teacher_questionnaire_metadta<-makeVlist(teacher_questionnaire_17) %>%
  mutate(indicator_tag=as.character(NA)) 



for (i in indicator_names ) {
  teacher_questionnaire_metadta<-teacher_questionnaire_metadta %>%
    mutate(indicator_tag=if_else(grepl(i,name ),toupper(i),indicator_tag, as.character(NA)) )
  
}

teacher_questionnaire_metadta<-teacher_questionnaire_metadta %>%
  left_join(indicators)

#bind version 18 and 17


teacher_questionnaire <- teacher_questionnaire %>%
  bind_rows(teacher_questionnaire_17) %>%
  bind_rows(teacher_questionnaire_15)


teacher_questionnaire %>%
  write_dta(file.path(download_folder, "questionnaire_roster.dta"))



#read in teacher absence file
teacher_absence_dta<-read_dta(file.path(download_folder, "questionnaire_selected.dta"))
teacher_absence_dta_17<-read_dta(file.path(paste(download_folder,'version_17', sep="/"), "questionnaire_selected.dta"))
teacher_absence_dta_15<-read_dta(file.path(paste(download_folder,'version_15', sep="/"), "questionnaire_selected.dta"))

#Add in absemce metadata
teacher_absence_metadta<-makeVlist(teacher_absence_dta_17) %>%
  mutate(indicator_tag=as.character(NA)) 



for (i in indicator_names ) {
  teacher_absence_metadta<-teacher_absence_metadta %>%
    mutate(indicator_tag=if_else(grepl(i,name ),toupper(i),indicator_tag, as.character(NA)) )
  
}

teacher_absence_metadta<-teacher_absence_metadta %>%
  left_join(indicators)

#bind version 18 and 17

teacher_absence_dta <- teacher_absence_dta %>%
  bind_rows(teacher_absence_dta_17) %>%
  bind_rows(teacher_absence_dta_15)


teacher_absence_dta %>%
  write_dta(file.path(download_folder, "questionnaire_selected.dta"))



#read in teacher assessment file
teacher_assessment_dta<-read_dta(file.path(download_folder, "teacher_assessment_answers.dta"))
teacher_assessment_dta_17<-read_dta(file.path(paste(download_folder,'version_17', sep="/"), "teacher_assessment_answers.dta"))
teacher_assessment_dta_15<-read_dta(file.path(paste(download_folder,'version_15', sep="/"), "teacher_assessment_answers.dta"))

#Add in assessment metadata
teacher_assessment_metadta<-makeVlist(teacher_assessment_dta_17) %>%
  mutate(indicator_tag='CONT') 




teacher_assessment_metadta<-teacher_assessment_metadta %>%
  left_join(indicators)

#bind version 18 and 17

teacher_assessment_dta <- teacher_assessment_dta %>%
  bind_rows(teacher_assessment_dta_17) %>%
  bind_rows(teacher_assessment_dta_15)


teacher_assessment_dta %>%
  write_dta(file.path(download_folder, "teacher_assessment_answers.dta"))

school_metadta$varlabel<-as.character(school_metadta$varlabel)
metadta<-bind_rows(school_metadta,  ecd_metadta, assess_4th_grade_metadta, teacher_questionnaire_metadta, teacher_assessment_metadta)

metadta %>%
  writexl::write_xlsx( path=file.path(download_folder, "metadata.xlsx"))

}


#########################################################################
# Get DDI #
##########################################################################



tounzip <- paste("myddi-",currentDate, ".zip" ,sep="")

######################################
# Interactions with API
######################################

#Get list of questionnaires available
#the server address may need to be modified
q<-GET(paste(server_add,"/api/v1/questionnaires", sep=""),
       authenticate(user, password))

str(content(q))


#pull data from version of our Education Policy Dashboard Questionnaire
POST(paste(server_add,"/api/v1/export/DDI/06756cace6d24cc996ffccbfc26a2264$",quest_version,"/start", sep=""),
     authenticate(user, password))


#sleep for 10 seconds to wait for stata file to compile
Sys.sleep(10)

dataDownload <- GET(paste(server_add,"/api/v1/export/DDI/06756cace6d24cc996ffccbfc26a2264$", quest_version,"/",sep=""),
                    authenticate(user, password))



#Now save zip to computer
filecon <- file(file.path(download_folder, tounzip), "wb")

writeBin(dataDownload$content, filecon) 
#close the connection
close(filecon)



#unzip
if (quest_version==17) {
  unzip(file.path(download_folder, tounzip), exdir=paste(download_folder,'version_17', sep="/"))
  
} else {
  unzip(file.path(download_folder, tounzip), exdir=download_folder)
}





#########################################################################
# Get Paradata #
##########################################################################



tounzip <- paste("myparadata-",currentDate, ".zip" ,sep="")

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


#sleep for 10 seconds to wait for stata file to compile
Sys.sleep(10)

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
if (quest_version==17) {
  unzip(file.path(download_folder, tounzip), exdir=paste(download_folder,'version_17', sep="/"))
  
} else {
  unzip(file.path(download_folder, tounzip), exdir=download_folder)
}


#########################################
# Read in paradata and do basic cleaning
#########################################

#read in data

if (quest_version!=17) {
  para_df<-read.delim(paste(download_folder, "paradata.tab", sep="/"), sep="\t")
  para_df_17<-read.delim(paste(paste(download_folder,'version_17', sep="/"), "paradata.tab", sep="/"), sep="\t")
  
  para_df <- para_df %>%
    bind_rows(para_df_17)
  
} else if (quest_version==17) {
  para_df<-read.delim(paste(download_folder, "paradata.tab", sep="/"), sep="\t")
  
}


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

#Generate length of time for each question, by calculating gap in time between when question was entered and previous question
para_df <- para_df %>% 
  arrange(ï..interview__id, order) %>% 
  mutate(timelength=lag(timestamp) %--% timestamp) %>% 
  mutate(timelength_sec=int_length(timelength)) %>%
  mutate(date=date(timestamp),
         hour=hour(timestamp),
         am_pm=am(timestamp))

#Only keep paradata on actual questions for indicators. These are tagged with m******
para_df <- para_df %>% 
  filter(str_detect(parameters, "m1s|m2s|m3s|m4s|m5s|m6s|m7s|m8s")) %>%
  filter(responsible!="")


#Split up question name to identify tags for each indicator
para_df <- para_df %>% 
  separate(parameters, c("question", NA, "response", NA, "other"), remove=FALSE, sep = "[|]")

#Add tags for module, section, and indicator
para_df <- para_df %>% 
  mutate(module = str_to_upper(substr(question, start=1, stop=2))) %>%
  mutate(section = str_to_upper(substr(question, start=1, stop=4))) %>%
  separate(question, c(NA, "indicator"), sep="_", remove=FALSE) %>%
  mutate(indicator = str_to_upper(indicator))

#Read in list of indicators
indicators <- read_delim(here::here("Indicators","indicators.md"), delim="|", trim_ws=TRUE)
indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)

#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- indicators$indicator_tag


#merge on question text
#Create function to save metadata for each question in each module
#The attr function retrieves metadata imported by haven. E.g. attr(school_dta$m1s0q2_code, "label")
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}


#create school metadata frame
raw_school_dta<-read_dta(file.path(download_folder, "EPDash.dta"))
school_metadta<-makeVlist(raw_school_dta)

#create teacher questionnaire metadata frame
raw_teacher_questionnaire<-read_dta(file.path(download_folder, "questionnaire_roster.dta"))
teacher_questionnaire_metadta<-makeVlist(raw_teacher_questionnaire)

#create teacher absence metadata frame
raw_teacher_absence_dta<-read_dta(file.path(download_folder, "questionnaire_selected.dta"))
teacher_absence_metadta<-makeVlist(raw_teacher_absence_dta)

#create ecd metadata frame
raw_ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))
ecd_metadta<-makeVlist(raw_ecd_dta)

#create 4th grade assessment metadata frame
raw_assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))
assess_4th_grade_metadta<-makeVlist(raw_assess_4th_grade_dta)

metadata <- rbind(school_metadta, teacher_questionnaire_metadta, ecd_metadta, assess_4th_grade_metadta)
metadata <- rbind(school_metadta, teacher_questionnaire_metadta, teacher_absence_metadta, ecd_metadta, assess_4th_grade_metadta)

metadata <- metadata %>% 
  mutate(question=name)

para_df <- para_df %>% 
  left_join(metadata )

save(para_df, file=paste(download_folder, "paradata.RData", sep="/"))

para_dta<- para_df %>% 
  mutate(interview_id=ï..interview__id) %>%
  select(-vallabel, -varlabel, -ï..interview__id)
write_dta(para_dta, path=paste(download_folder, "paradata.dta", sep="/"))

para_df_section <- para_df %>% 
  group_by(ï..interview__id, section) %>% 
  summarise(responsible=first(responsible), date=first(date), module=first(module), timelength_sec=sum(timelength_sec))

para_df_tab <- para_df %>%
  select( responsible, date, timestamp, module, section, indicator, question, varlabel, timelength_sec, ï..interview__id) %>%
  group_by(ï..interview__id, section) %>% 
  summarise(responsible=first(responsible), date=first(date), module=first(module), timestamp=last(timestamp))


save(para_df_section, para_df_tab, file=paste(download_folder, "paradata_light.RData", sep="/"))


