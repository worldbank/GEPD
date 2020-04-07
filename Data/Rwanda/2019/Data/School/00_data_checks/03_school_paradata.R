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
library(crosstalk)
library(DT)
library(plotly)
library(ggplot2)
library(haven)
library(readr)
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
server_add<- svDialogs::dlgInput("Please Enter Server http Address:", 'https://gepdrwa.mysurvey.solutions')$res

#questionnaire version
#e.g. quest_version<-8
quest_version<-svDialogs::dlgInput("Please enter Questionnaire Version:", 'Enter integer')$res 

#path and folder where the .zip file will be stored
#this needs to be entered
#Please note that the following directory path may need to be created


currentDate<-Sys.Date()

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
POST(paste(server_add,"/api/v1/export/paradata/f5366202a24043d59d85dbf14fb0a32d$",quest_version,"/start", sep=""),
     authenticate(user, password))


#sleep for 10 seconds to wait for stata file to compile
Sys.sleep(10)

dataDownload <- GET(paste(server_add,"/api/v1/export/paradata/f5366202a24043d59d85dbf14fb0a32d$", quest_version,"/",sep=""),
                    authenticate(user, password))

redirectURL <- dataDownload$url 
RawData <- GET(redirectURL) #Sucess!!


#Now save zip to computer
filecon <- file(file.path(download_folder, tounzip), "wb")

writeBin(RawData$content, filecon) 
#close the connection
close(filecon)



#unzip

#unzip
if (quest_version==17) {
  unzip(file.path(download_folder, tounzip), exdir=paste(download_folder,'version_17', sep="/"))
  
} else if  (quest_version==16) {
  unzip(file.path(download_folder, tounzip), exdir=paste(download_folder,'version_16', sep="/"))
  
} else if  (quest_version==18) {
  unzip(file.path(download_folder, tounzip), exdir=paste(download_folder,'version_16', sep="/"))
  
} else {
  unzip(file.path(download_folder, tounzip), exdir=download_folder)
}


#########################################
# Read in paradata and do basic cleaning
#########################################

#read in data


  para_df<-read.delim(paste(download_folder, "paradata.tab", sep="/"), sep="\t")
  para_df_18<-read.delim(paste(paste(download_folder,'version_17', sep="/"), "paradata.tab", sep="/"), sep="\t")

  para_df_17<-read.delim(paste(paste(download_folder,'version_17', sep="/"), "paradata.tab", sep="/"), sep="\t")
  para_df_16<-read.delim(paste(paste(download_folder,'version_16', sep="/"), "paradata.tab", sep="/"), sep="\t")
  
  #para_df <-  bind_rows(para_df,para_df_17, para_df_16)
  


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
raw_school_dta<-read_dta(file.path(download_folder, school_file))
school_metadta<-makeVlist(raw_school_dta)

#create teacher questionnaire metadata frame
raw_teacher_questionnaire<-read_dta(file.path(download_folder, "questionnaire_roster.dta"))
teacher_questionnaire_metadta<-makeVlist(raw_teacher_questionnaire)

#create teacher absence metadata frame
raw_teacher_absence_dta<-read_dta(file.path(download_folder, "TEACHERS.dta"))
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

save(para_df, file=paste(save_folder, "paradata.RData", sep="/"))

para_dta<- para_df %>% 
  mutate(interview_id=ï..interview__id) %>%
  select(-vallabel, -varlabel, -ï..interview__id)
write_dta(para_dta, path=paste(save_folder, "paradata.dta", sep="/"))

para_df_section <- para_df %>% 
  group_by(ï..interview__id, section) %>% 
  summarise(responsible=first(responsible), date=first(date), module=first(module), timelength_sec=sum(timelength_sec))

para_df_tab <- para_df %>%
  select( responsible, date, timestamp, module, section, indicator, question, varlabel, timelength_sec, ï..interview__id) %>%
  group_by(ï..interview__id, section) %>% 
  summarise(responsible=first(responsible), date=first(date), module=first(module), timestamp=last(timestamp))

#create a database based on paradata on the number of modules completed per school
para_df_module <- para_df %>% 
  group_by(ï..interview__id, module) %>% 
  summarise(responsible=first(responsible), date=first(date), timelength_sec=sum(timelength_sec))

school_dta_preamble_id <- school_dta %>%
  select(interview__id, interview__key, school_code, school_name_preload, school_address_preload, school_province_preload, school_district_preload, lat, lon )

school_modules_complete <- para_df_module %>%
  mutate(interview__id=ï..interview__id) %>%
  ungroup() %>%
  select(-c('ï..interview__id', 'timelength_sec')) %>%
  left_join(school_dta_preamble_id) %>%
  mutate(value='Yes') %>%
  pivot_wider(names_from=module, values_from = value) %>%
  group_by(school_code) %>%
  mutate(interview__key1=first(interview__key),
         interview__key2=nth(interview__key,2),
         interview__key3=nth(interview__key,3),
         interview__key4=nth(interview__key,4),
         interview__key5=nth(interview__key,5),
         
         responsible1=first(responsible),
         responsible2=nth(responsible,2),
         responsible3=nth(responsible,3),
         responsible4=nth(responsible,4),
         responsible5=nth(responsible,5)) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(school_code, M1, M2, M3, M4, M5, M6, M7, M8,  
         responsible1, responsible2, responsible3, responsible4, responsible5,  
         interview__key1, interview__key2, interview__key3, interview__key4, interview__key5,
         school_name_preload, school_address_preload, school_province_preload, school_district_preload, lat, lon) %>%
  arrange(school_code)
  

save(para_df_section, para_df_tab, school_modules_complete, file=paste(save_folder, "paradata_light.RData", sep="/"))


# 
# ######################################
# # Length of each question by Enumerator
# #######################################
# 
# para_df_tab <- para_df %>%
#   select( responsible, date, module, section, indicator, question, varlabel, timelength_sec, ï..interview__id,)
# 
# linked_df<-SharedData$new(para_df_tab)
# 
# 
# bscols(widths=c(3,NA),
#        list(
#          filter_slider("time", "Length in Seconds", linked_df, ~timelength_sec),
#          filter_select("enumerator", "Enumerator", linked_df, ~responsible),
#          filter_checkbox("date", "Date of Survey", linked_df, ~as.character(date), inline=FALSE),
#          filter_checkbox("section", "Section", linked_df, ~section, inline=FALSE),
#          filter_checkbox("module", "Module", linked_df, ~module, inline=FALSE)
#          
#        ),
#       
#        list (
#          plot_ly(linked_df, x=~question, y=~timelength_sec, type='scatter', mode='markers', color=~responsible) %>%
#            layout(title='Question Length by Enumerator',yaxis=list(title='Length in Seconds'), xaxis=list(title='Question ID')),
#          datatable(linked_df, 
#                    colnames=c('Interview Code'='ï..interview__id', 'Enumerator' = 'responsible', 'Date' = 'date', 'Module' = 'module', 'Section' = 'section',
#                             'Indicator' = 'indicator', 'Question ID' = 'question', 'Question'='varlabel', 'Length in Seconds' = 'timelength_sec'),
#                    extensions="Scroller", style="bootstrap", class="compact", width="100%",
#                    options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
#          
#        )
#        
# )
# 
# 
# 
# ######################################
# # Length of each section by Enumerator
# #######################################
# 
# para_df_section <- para_df %>% 
#   group_by(ï..interview__id, section) %>% 
#   summarise(responsible=first(responsible), date=first(date), module=first(module), timelength_sec=sum(timelength_sec))
# 
# linked_df<-SharedData$new(para_df_section)
# 
# 
# bscols(widths=c(3,NA),
#        list(
#          filter_slider("time", "Length in Seconds", linked_df, ~timelength_sec),
#          filter_checkbox("enumerator", "Enumerator", linked_df, ~responsible),
#          filter_checkbox("date", "Date of Survey", linked_df, ~as.character(date), inline=FALSE),
#          filter_checkbox("module", "Module", linked_df, ~module, inline=FALSE)
#        ),
#        list (
#        plot_ly(linked_df, x=~section, y=~timelength_sec, type='scatter', mode='markers', color=~responsible) %>%
#          layout(title='Section Length by Enumerator',yaxis=list(title='Length of Question in Seconds'), xaxis=list(title='Question Name')),
#       
#         datatable(linked_df, 
#                  colnames=c('Enumerator' = 'responsible', 'Date' = 'date', 'Module' = 'module', 'Section' = 'section',
#                             'Length in Seconds' = 'timelength_sec'),
#                  extensions="Scroller", style="bootstrap", class="compact", width="100%",
#                  options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
#       
#        )
#        
# )
# 
# ######################################
# # Time of Day of each question by Enumerator
# #######################################
# 
# para_df_tab <- para_df %>%
#   select( responsible, date, timestamp, module, section, indicator, question, varlabel, timelength_sec, ï..interview__id,)
# 
# linked_df<-SharedData$new(para_df_tab)
# 
# 
# bscols(widths=c(3,NA),
#        list(
#          filter_slider("Time", "Time Question Completed", linked_df, ~timestamp),
#          filter_checkbox("enumerator", "Enumerator", linked_df, ~responsible),
#          filter_checkbox("date", "Date of Survey", linked_df, ~as.character(date), inline=FALSE),
#          filter_checkbox("section", "Section", linked_df, ~section, inline=FALSE),
#          filter_checkbox("module", "Module", linked_df, ~module, inline=FALSE)
#          
#        ),
#        
#        list (
#          plot_ly(linked_df, x=~question, y=~timestamp, type='scatter', mode='markers', color=~responsible) %>%
#            layout(title='Time Question Completed by Enumerator',yaxis=list(title='Time'), xaxis=list(title='Question ID')),
#          datatable(linked_df, 
#                    colnames=c('Interview Code'='ï..interview__id', 'Enumerator' = 'responsible', 'Date' = 'date', 'Module' = 'module', 'Section' = 'section',
#                               'Indicator' = 'indicator', 'Question ID' = 'question', 'Question'='varlabel', 'Time' = 'timestamp'),
#                    extensions="Scroller", style="bootstrap", class="compact", width="100%",
#                    options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
#          
#        )
#        
# )  