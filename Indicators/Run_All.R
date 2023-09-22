# Run all R files in this directory

#packages
library(tidyverse)
library(here)

#set working directory
setwd(here("Indicators/"))

#list of files to run
#get list of all files starting with "indicator_api_final_...R"
files <- list.files(pattern = "indicator_api_final")

#run all files
for (i in files) {
  source(i, local=TRUE, echo=TRUE)
}
