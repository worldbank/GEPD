# Purpose:  Run file to produce markdown reports for data analysis
# Author: Brian Stacy
# This file will run one R script.
# 1. school_data_analysis.Rmd     #This file produces an R Markdown report containing several analysis findings.

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




#Country name
country <-'PER'
country_name <- "Peru"
year <- '2019'

#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

backup_onedrive="no"


if (Sys.getenv("USERNAME") == "wb469649"){
  project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  open_folder <-file.path(paste(project_folder,country,year,"Data/clean/School", sep="/"))
  save_folder <- file.path("C:/Users/wb469649/Documents/Github/GEPD/Data", country_name,year,"Code/School/02_analysis/reports/", sep="/")
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work", country_name,year,"Data/clean/School", sep="/"))
  
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

# #launch file to access data from API
# source('school_api.R')
# 
# #launch file to clear data
# source('school_data_cleaner.R')
# 
#create R markdown file with quality checks
rmarkdown::render('school_data_analysis.Rmd',  
                  output_file =  paste("school_data_analysis_",country_name,".html", sep=''), 
                  output_dir = save_folder)