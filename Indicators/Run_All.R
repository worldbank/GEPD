# Run all R files in this directory

#packages
library(tidyverse)
library(here)

#set working directory
setwd(here("Indicators/"))

#list of files to run
#get list of all files starting with "indicator_api_final_...R"
files <- list.files(pattern = "indicator_api_final")

#manual
files <- c(
  #"indicator_api_final.R" ,
  #"indicator_api_final_Ethiopia.R" ,
  #"indicator_api_final_GAB.R"  , 
  "indicator_api_final_Jordan.R"   ,   
  "indicator_api_final_Madagascar.R",
  "indicator_api_final_NER.R"      ,   
  "indicator_api_final_Rwanda.R"   ,  
  "indicator_api_final_SLE.R"    
)

#run all files
for (i in files) {
  source(i, local=TRUE, echo=TRUE)
  rm()
  
}
