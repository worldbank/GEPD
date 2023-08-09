# Purpose:  Run file for high frequency and data quality checks for the survey of public officials
# Author: Brian Stacy
# This file will run four R scripts in order.
# Each file can be run independently, but you will be prompted for certain paths that may not be specified
# This file will sequency the R scripts the correct way to produce an
# R markdown html file with high frequency and data quality checks for the school survey.
# 1. public_officials_api.R                       #This file will access the Survey Solutions API and pull rawdata and paradata  
# 2. public_officials_data_cleaner.R              #This file opens the raw data and cleans it to produce our indicators for the Dashboard
# 3. public_officials_paradata.R                  #This file opens paradata produced by Survey Solutions to calculate length 
#                                        of time per module and other checks
# 4. public_officials_data_quality_checks.Rmd     #This file produces an R Markdown report containing several quality checks.

######################################
# Load Required Packages#
######################################
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
country_name <-'Chad'
country <- "TCD"
year <- '2023'

#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

if (Sys.getenv("USERNAME") == "WB469649" | Sys.getenv("USERNAME") == "wb469649"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT/"
  download_folder <-file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/raw/Public_Officials/", sep="/"))
  confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/Public_Officials", sep="/"))
  save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/anonymized/Public_Officials", sep="/"))
  
}else if (Sys.getenv("USERNAME") == "wb577189"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT"
  
  download_folder <-file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/raw/Public_Officials/", sep="/"))
  confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/Public_Officials", sep="/"))
  save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/anonymized/Public_Officials", sep="/"))
  
  
} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")
  
}
#########################
# Launch Code
########################
#move working directory to github main folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#main file name:
po_file<-"public_officials.dta"
need_api=0
backup_onedrive="no"

#launch file to access data from API
source('02_public_officials_api.R', local=TRUE)

#launch file to clean data
source('03_public_officials_cleaner.R', local=TRUE)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source('04_public_officials_anonymizer.R', local=TRUE)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

