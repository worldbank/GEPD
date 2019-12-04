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

set.seed(54351324)

#See SamplingStrata_v2.R file for file to choose 200 schools.
#In this file, we are adding an additional 50 schools (17 evening and 33 South schools) to add additional comparability

#################################
#Specify Directories, etc
#################################

#Specify directory where sampling frame located
dir_frame<-"C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Jordan/2019/Data/Sampling"

#specify name of sampling frame file.  This needs to be a csv file
file_frame<-"schools_Jordan.xlsx"

#specify date that sample was created.  This may be useful if you want to access stored results
#date_frame<-c("2019-07-22")

date_frame<-c("2019-09-18")
  

sample_file_name <- paste(dir_frame,"/school_sample_",date_frame,".RData", sep="")


load( file=sample_file_name)   

data_set_updated<-data_set_updated %>%
  mutate(sample_amman=ifelse(is.na(sample_amman),0,sample_amman)) %>%
  select(-sample_maputo)

#Add an extra 33 southern schools and 17 Evening schools to allow comparability
  
#add southern schools 
sample_southern<- data_set_updated %>%
  filter(is.na(sample)) %>%
  filter(territory == "South") %>%
  ungroup() %>% 
  sample_n(33, weight=total_students_grade_4 ) %>%
  mutate(sample_south_extra=1)

data_set_updated <- data_set_updated %>%
  left_join(sample_southern) %>%
  mutate(sample_south_extra=ifelse(is.na(sample_south_extra),0,sample_south_extra)) %>%
  mutate(sample=as.numeric(sample)) %>%
  mutate(sample=if_else(sample_south_extra==1, 1, sample)) 
#add evening schools
sample_evening<- data_set_updated %>%
  filter(is.na(sample)) %>%
  filter(foundation_period=="Evening") %>%
  ungroup() %>% 
  sample_n(17, weight=total_students_grade_4 ) %>%
  mutate(sample_evening_extra=1)

data_set_updated <- data_set_updated %>%
  left_join(sample_evening) %>%
  mutate(sample_evening_extra=ifelse(is.na(sample_evening_extra),0,sample_evening_extra)) %>%
  mutate(sample=as.numeric(sample)) %>%
  mutate(sample=if_else(sample_evening_extra==1, 1, sample)) %>%
mutate(sample=factor(sample, levels=c(1,2,3,4), labels=c('Sampled School', "Replacement 1", "Replacement 2", 'Amman Pilot Schools')))


sample_updated<-data_set_updated  %>%
  filter(!is.na(sample)) %>%
  select(-c("governorate_factor",
            "supervisory_authority_factor",
            "rural_factor",
            "id",
            "ID",
            "DOMAINVALUE",
            "STRATUM",
            "X1",
            "X2",
            "Y1",
            "Y2",
            "LABEL",
            "rev_total_4th",
            "domainvalue",
            "strato",
            "id.1",
            "stratum",
            "x1",
            "x2",
            "y1",
            "y2"))

sample_updated <- sample_updated %>% 
  arrange(district, sample) %>%
  group_by(district, sample) %>% 
  mutate(replace_group_id=row_number()) %>%
  group_by(district, replace_group_id) %>%
  mutate( replace_governorate=if_else((sample==1 | sample==4),as.character(NA), governorate)) %>%
  arrange(district, replace_group_id) 

#Save sample file
currentDate<-Sys.Date()
sample_file_name <- paste(dir_frame,"/school_sample_",currentDate,".xlsx", sep="")
writexl::write_xlsx(sample_updated, path=sample_file_name)

sample_frame_name <- paste(dir_frame,"/school_sample_",currentDate,".RData", sep="")
save(sample_updated, data_set_updated,
     file=sample_frame_name)   

######################################################
#### Map the selected schools #####
######################################################
sample_map <- data_set_updated %>%
     filter(!is.na(sample)) %>%
    mutate(longitude=as.character(longitude)) %>%
    mutate(latitude=as.character(latitude)) %>%
  mutate(longitude=as.numeric(longitude)) %>%
    mutate(latitude=as.numeric(latitude)) 

  sample_map_chosen <- data_set_updated %>%
     filter(sample=="Sampled School") %>%
     mutate(longitude=as.character(longitude)) %>%
     mutate(latitude=as.character(latitude)) %>%
     mutate(longitude=as.numeric(longitude)) %>%
     mutate(latitude=as.numeric(latitude)) 
   
   
   pal <- colorFactor(
     palette = c("red", "green", "blue"),
     domain = sample_map$sample
   )
   
leaflet(data=sample_map_chosen) %>%
    addTiles()  %>%
    addMarkers( lng=~longitude,  lat=~latitude ) 
    
  

  
  #################################################
  ###### Survey of Public Officials Sampling #####
  #################################################
  
  #This could also be split into a separate file.
  # sample_file_name <- paste(dir_frame,"school_sample_",date_frame,".csv", sep="")
  # load(file=sample_frame_name) 
  
  #Choose district offices to visit randomly among the list of districts we are already visiting for the schools
  #We will choose 20 of these district offices from the Regional level
  dist_list<- sample_updated %>%
    group_by(governorate) %>%
    summarise(n=n() )
  


  
  #Run an iteration where we choose 10 provinces (UGEL) linked to schools and link up to regions
  #columns to keep
  cols<-c( "governorate", "directorate")
  
  district_office_sample <- dist_list %>%
    sample_n(12)
  
  dist_list_alt2<- sample_updated %>%
    filter(governorate %in% as.vector(district_office_sample$governorate)) %>%
    group_by(governorate) %>%
    sample_n(1) %>%
    select(cols)
  
  #save as csv
  dist_frame_name <- paste(dir_frame,"/district_sample_",currentDate,".xlsx", sep="")
  writexl::write_xlsx(dist_list_alt2, path=dist_frame_name)
  

  
  ###save our sample  
  dist_frame_name <- paste(dir_frame,"/district_sample_",currentDate,".RData", sep="")
  
  save(district_office_sample,  dist_list_alt2,
       file=dist_frame_name) 

  
 