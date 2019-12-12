#anova to examine fraction of variance between and within schools
library(tidyverse)


#Country name and year of survey
country <-'PER'
country_name <- "Peru"
year <- '2019'

#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

backup_onedrive="no"

#Add your UPI here and set the directory paths of your choice.
if (Sys.getenv("USERNAME") == "wb469649"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/"
  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School", sep="/"))
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School", sep="/"))
  
} else if (Sys.getenv("USERNAME") == "wb550666"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb550666/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/"
  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School", sep="/"))
  
  # This is experimental and not currently in use.
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb550666/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School", sep="/"))
  
} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")
  save_folder_onedrive <- choose.dir(default = "", caption = "Select folder to save backed up data to onedrive")
  
}


load(paste(save_folder, "school_indicators_data.RData", sep="/"))

#Load original sample of schools
#Load original sample of schools
currentDate<-c("2019-07-22")
sample_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/school_sample_",currentDate,".RData", sep="")
load(sample_frame_name)




#################################
# ANOVA
#################################

anova <- aov(student_knowledge~factor(school_code), data=assess_4th_grade_anon)

summary(anova)
print(anova)
#> 402908/(403059+402908)=0.4999063


fit = lm(student_knowledge ~ factor(school_code), data=assess_4th_grade_anon)
anova(fit)


