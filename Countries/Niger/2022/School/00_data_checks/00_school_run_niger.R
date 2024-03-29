# Purpose:  Run file for high frequency and data quality checks for the school survey
# Author: Brian Stacy
# This file will run four R scripts in order.
# Each file can be run independently, but you will be prompted for certain paths that may not be specified
# This file will sequency the R scripts the correct way to produce an
# R markdown html file with high frequency and data quality checks for the school survey.
# 1. school_api.R                       #This file will access the Survey Solutions API and pull rawdata and paradata  
# 2. school_data_cleaner.R              #This file opens the raw data and cleans it to produce our indicators for the Dashboard
# 3. school_paradata.R                  #This file opens paradata produced by Survey Solutions to calculate length 
#                                        of time per module and other checks
# 4. school_data_quality_checks.Rmd     #This file produces an R Markdown report containing several quality checks.

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
country <-'Niger'
country_name <- "Niger"
year <- '2022'

#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

backup_onedrive="yes"

#Add your UPI here and set the directory paths of your choice.
if (Sys.getenv("USERNAME") == "wb469649"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work"
  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School", sep="/"))
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School", sep="/"))
  
} else if (Sys.getenv("USERNAME") == "wb577189"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work"

  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School_Survey", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School_Survey", sep="/"))
  
  # This is experimental and not currently in use.
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School_Survey", sep="/"))
  
} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")
  save_folder_onedrive <- choose.dir(default = "", caption = "Select folder to save backed up data to onedrive")
  
}

#########################
# Launch Code
########################
#move working directory to github main folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# change name of school file
school_file<-"EPDash.dta"

# #launch file to access data from API
#source('01_school_api.R', local=TRUE)
 
# #launch file to clear data=
source('02_school_data_cleaner - old.R', local=TRUE)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# #launch paradata file 
source('03_school_paradata.R', local=TRUE)

 #create R markdown file with quality checks
 rmarkdown::render('04_school_data_quality_checks.Rmd',
                   output_file =  paste("school_data_quality_checks_",country_name,".html", sep=''),
                   output_dir = save_folder_onedrive)
 