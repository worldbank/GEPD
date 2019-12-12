#Use of DHS API system
#Brian Stacy 12/12/2019

library(httr)
library(haven)
library(dplyr)
library(Hmisc)
library(tidyr)
library(here)
library(jsonlite)
library(xml2)

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

#DHSaddress
#e.g. server_add<-'https://api.dhsprogram.com/rest/dhs/data/2010'
server_add<- 'https://api.dhsprogram.com/rest/dhs'
server_datasets <- 'http://apps.who.int/gho/athena/api/GHO.json'

currentDate<-Sys.Date()

tounzip <- paste("mydata-",currentDate, ".zip" ,sep="")



######################################
# Interactions with API
######################################

#Get list of datsets


datasets_avail<-content(GET(paste(server_datasets, sep="")), as='text')
datasets_avail_df<-fromJSON(datasets_avail)


#get dhs data
q<-GET(paste(server_add,"/data/PE?selectSurveys=latest", sep="")
       # authenticate(user, password)
)

http_type(q)

#convert json to parsed, for forming database
json_parsed<-fromJSON(content(q, as='text'))

#
json_dta<-json_parsed$Data 



