#File written by Brian Stacy on July 30, 2020
# File will produce automated country briefs of 2 pages and 4 pages

# Load libraries
library(rmarkdown)
library(tidyverse)
library(here)
library(tidyr)
library(stringr)
library(scales)
library(readxl)
library(glue)
library(httr)
library(jsonlite)
library(wbstats)
library(ggrepel)
library(haven)


#anchor directory to correct place
here()
setwd(here())

#############
# Peru
#############

#Set the country name here
country_file_name <- "PER"
country <- "Peru"
country_uc <- "PERU"
year <- "2019"

# 2 Pager
rmarkdown::render('./Code/GEPD_Brief_2page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "2Pager.pdf", sep=''), 
                  output_dir = './Output/')

#4 Pager
rmarkdown::render('./Code/GEPD_Brief_4page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "4Pager.pdf", sep=''), 
                  output_dir = './Output/')


#############
# Jordan
#############

#Set the country name here
country_file_name <- "JOR"
country <- "Jordan"
country_uc <- "JORDAN"
year <- "2019"

# 2 Pager
rmarkdown::render('./Code/GEPD_Brief_2page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "2Pager.pdf", sep=''), 
                  output_dir = './Output/')

#4 Pager
rmarkdown::render('./Code/GEPD_Brief_4page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "4Pager.pdf", sep=''), 
                  output_dir = './Output/')