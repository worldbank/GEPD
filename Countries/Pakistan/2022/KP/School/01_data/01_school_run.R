# Purpose:  Run file to pull data from Survey Solutions Server and save it to GEPD-Confidential Drive
# Author: Brian Stacy
# This file will run four R scripts in order.
# Each file can be run independently, but you will be prompted for certain paths that may not be specified
# This file will sequency the R scripts the correct way to produce an
# R markdown html file with high frequency and data quality checks for the school survey.
# 1. school_api.R                       #This file will access the Survey Solutions API and pull rawdata and paradata  
# 2. school_data_cleaner.R              #This file opens the raw data and cleans it to produce our indicators for the Dashboard

# 4. school_data_quality_checks.Rmd     #This file produces an R Markdown report containing several quality checks.

######################################
# Load Required Packages#
######################################
library(tidyverse)
library(here)
library(knitr)
library(markdown)
library(rmarkdown)
######################################
# User Inputs for Run File #
######################################
# Here you need to indicate the path where you replicated the folder structures on your own computer
here() #"C:/Users/wb469649/Documents/Github/GEPD"




#Country name and year of survey
country <-'PAK'
country_name <- "Pakistan"
province <- "KP"
year <- '2022'

#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored



if (Sys.getenv("USERNAME") == "WB469649" | Sys.getenv("USERNAME") == "wb469649"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT/"
  download_folder <-file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/raw/School/", sep="/"))
  confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/confidential/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/anonymized/School", sep="/"))

}else if (Sys.getenv("USERNAME") == "wb577189"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT"
  
  download_folder <-file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/raw/School/", sep="/"))
  confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/confidential/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/",province,"/anonymized/School", sep="/"))


} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")

}



#########################
# Launch Code
########################
#move working directory to github main folder
# dir <- here()
# setwd(paste(dir, "Countries",country_name,year,"School/01_data/", sep="/"))
# #launch file to access data from API
need_api=0
school_file<-"EPDash.dta"

#source('02_school_api.R', local=TRUE)
 
# #launch file to clean data=
rmarkdown::render("03_school_data_cleaner.Rmd")

source('04_school_anonymizer.R', local=TRUE)


