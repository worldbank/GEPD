library(tidyverse)
library(haven)
#loop through all files in directory and convert .sav to .dta

dir <- 'C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/Peru DHS 2018/'

#get list of all files
files <- list.files(path=dir, pattern="*.sav", full.names=T, recursive = T, ignore.case = T)


for (x in files) {
  
  t <- read_spss(x) # load file
  new_name1<-str_replace(x, ".sav", '.csv')
  new_name<-str_replace(new_name1, ".SAV", '.csv')
  
  write_excel_csv(t, new_name)
  
  
}