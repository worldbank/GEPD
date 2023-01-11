#Clean data files downloaded from API
#Written by Adrien Ciret 01/06/2022

#load relevant libraries

library(skimr)
library(naniar)
library(vtable)
library(digest)
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(sjlabelled)
#NOTE:  The R script to pull the data from the API should be run before this file



#Create function to save metadata for each question in each module
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}



#Country name and year of survey
country <-'Pakistan_all'
country_name <- "Pakistan_all"
year <- '2021'


#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

backup_onedrive="yes"

#Add your UPI here and set the directory paths of your choice.
if (Sys.getenv("USERNAME") == "wb469649"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/"
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/01. Pilot/raw/teach_ece", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/01. Pilot/clean/teach_ece", sep="/"))
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/02. Provinces/ICT", sep="/"))
  
} else if (Sys.getenv("USERNAME") == "wb577189"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work"
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/02. Provinces/ICT/raw/teach_ece", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/02. Provinces/ICT/clean/teach_ece", sep="/"))
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/02. Provinces/ICT/clean/teach_ece", sep="/"))
  
} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")
  save_folder_onedrive <- choose.dir(default = "", caption = "Select folder to save backed up data to onedrive")
  
}



############################
#read in AMPL-b file
############################

teachece<-read_dta(file.path(download_folder, "TEACH_ECE_PAK.dta"))


# read_dta(file.path(download_folder, "v1/TEACH_ECE_PAK.dta")) %>% 
#   bind_rows(read_dta(file.path(download_folder, "v2/TEACH_ECE_PAK.dta"))) 


## If blank interview, then we look for the replacing information


teachece <- teachece %>% 
  mutate(
    teach_ece_name_preload = if_else(school_name_preload=="Blank interview" & school_info_correct!=1, m1s0q2_name, school_name_preload),
    teach_ece_emis_preload = if_else(school_emis_preload=="Blank interview" & school_info_correct!=1, m1s0q2_emis, school_emis_preload)
    
    
  ) %>% 
  
  select(ends_with("_preload"), ends_with("_Latitude"), ends_with("_Longitude"))





