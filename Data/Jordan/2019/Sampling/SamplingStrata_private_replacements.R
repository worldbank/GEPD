#Written by Brian Stacy

#NOTES: This file is difficult to make generic, because sampling is always different in each country.  
#So much customization may be needed.  However, the basic outline may be useful for several countries.
# I have left many bits of code that may or may not be useful that are commented
# I assume in much of this file that we want a national number with a breakdown by urban/rural, but this can be modified
# Most of this file was based on the Peru sampling file


library(SamplingStrata)
library(haven)
library(wbstats)
library(ggmap)
library(leaflet)
library(maps)
library(maptools)
library(rgeos)
library(RANN)
library(sp)
library(rgdal)
library(naniar)
library(raster)
library(tidyverse)
library(readxl)
library(geosphere)
set.seed(5435132)

#################################
#Specify Directories, etc
#################################

#Specify directory where sampling frame located
dir_frame<-"C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Jordan/2019/Data/Sampling"

#specify name of sampling frame file.  This needs to be a csv file
file_frame<-"schools_Jordan.xlsx"

#specify date that sample was created.  This may be useful if you want to access stored results
#date_frame<-c("2019-07-22")

date_frame<-c("2019-10-11")
  


sample_file_name <- paste(dir_frame,"/school_sample_",date_frame,".RData", sep="")


load( file=sample_file_name)   



###################################
#Add set of private school for each directorate based on the number of private schools in directorate
###################################

#get list of private schools in sample
sampled_private<-sample_updated %>%
  mutate(longitude=as.character(longitude)) %>%
  mutate(latitude=as.character(latitude)) %>%
  mutate(longitude=as.numeric(longitude)) %>%
  mutate(latitude=as.numeric(latitude)) %>%
  filter(sample=='Sampled School') %>%
  filter(supervisory_authority == "Private") %>%
  arrange(governorate, district, longitude, latitude)


# private school number by governorate
governorate_private <- sampled_private %>%
  group_by(governorate, district) %>%
  summarise(num_private_schools_sampled=n())
  
#add private schools 
sample_private_replace <- data_set_updated %>%
  mutate(longitude=as.character(longitude)) %>%
  mutate(latitude=as.character(latitude)) %>%
  mutate(longitude=as.numeric(longitude)) %>%
  mutate(latitude=as.numeric(latitude)) %>%
  filter(is.na(sample)) %>%
  filter(supervisory_authority == "Private") %>%
  left_join(governorate_private) %>%
  mutate(num_private_schools_sampled=if_else(is.na(num_private_schools_sampled),0,as.numeric(num_private_schools_sampled))) %>%
  group_by(governorate, district) %>% 
  sample_n(num_private_schools_sampled, weight=total_students_grade_4 ) %>%
  select(1:34) %>%
  mutate(sample_private=1) %>%
  arrange(governorate, longitude, latitude) 

#add in school info for school that will be replaced
sample_private_replace$replaced_organization_code=sampled_private$organization_code
sample_private_replace$replaced_school_name=sampled_private$school_name
sample_private_replace$replaced_governorate=sampled_private$governorate
sample_private_replace$replaced_address=sampled_private$address
sample_private_replace$replaced_longitude=sampled_private$longitude
sample_private_replace$replaced_latitude=sampled_private$latitude


#calculate distance in meters between replacement and original school
xy_orig<-cbind(sample_private_replace$replaced_longitude, sample_private_replace$replaced_latitude)
xy_repl<-cbind(sample_private_replace$longitude, sample_private_replace$latitude)

sample_private_replace$distance_km_selected_replacement<-diag(distm(xy_orig,xy_repl,  fun = distHaversine ))/1000

geo <- sample_private_replace %>%
  select(governorate, district, longitude, latitude, replaced_longitude, replaced_latitude, distance_km_selected_replacement)

#Save sample file
currentDate<-Sys.Date()
sample_file_name <- paste(dir_frame,"/school_sample_private_replacements_",currentDate,".xlsx", sep="")
writexl::write_xlsx(sample_private_replace, path=sample_file_name)


######################################################
#### Map the selected schools #####
######################################################
sample_map <- sample_private_replace %>%
  filter(!is.na(sample)) %>%
  mutate(longitude=as.character(longitude)) %>%
  mutate(latitude=as.character(latitude)) %>%
  mutate(longitude=as.numeric(longitude)) %>%
  mutate(latitude=as.numeric(latitude)) 

sample_map_chosen <- sample_private_replace %>%
  mutate(longitude=as.character(longitude)) %>%
  mutate(latitude=as.character(latitude)) %>%
  mutate(longitude=as.numeric(longitude)) %>%
  mutate(latitude=as.numeric(latitude)) 




leaflet(data=sample_map_chosen) %>%
  addTiles()  %>%
  addMarkers( lng=~longitude,  lat=~latitude ) 


