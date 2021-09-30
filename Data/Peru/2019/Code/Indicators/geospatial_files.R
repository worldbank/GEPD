#save geospatial files
library(tidyverse)

#Country name
country <-'PER'
country_name <- "Peru"
year <- '2019'

#directories
project_folder  <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT/"
confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/School", sep="/"))
save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/anonymized/School", sep="/"))

##################
# Schools
##################

#Load data
load(file.path(confidential_folder, "school_survey_data.RData"))

#load key file
keyfile <- read_csv(file.path(confidential_folder, "EPDash_linkfile_hashed.csv"))

#Load original sample of schools
currentDate<-c("2019-07-22")
sample_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/sampling/", sep="/"))
sample_frame_name <- paste(sample_folder,"/school_sample_",currentDate,".RData", sep="")

load(sample_frame_name)

#load key
geo_school_df <- final_school_data %>%
  mutate(codigo.modular=as.numeric(school_code_preload)) %>%
  left_join(data_set_updated) %>%
  mutate(longitude=as.character(longitude)) %>%
  mutate(latitude=as.character(latitude)) %>%
  mutate(lat=if_else(is.na(lat), as.numeric(latitude), lat),
         lon=if_else(is.na(lon), as.numeric(longitude), lon)) %>%
  left_join(keyfile) %>%
  select(hashed_school_code, hashed_school_province, hashed_school_district, lat, lon)

# write
write_excel_csv(geo_school_df,
                file.path(confidential_folder, paste0(country,"_school_geo.csv")))

##############
# Public Officials
##############

#Country name
country <-'PER'
country_name <- "Peru"
year <- '2019'

#directories
project_folder  <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT/"
confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/Public_officials", sep="/"))
save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/anonymized/Public_officials", sep="/"))

#Load data
load(file.path(confidential_folder, "public_officials_indicators_data.RData"))

#load key file
keyfile <- read_csv(file.path(confidential_folder, "public_official_linkfile_hashed.csv"))

geo_public_officials_df <- public_officials_dta_clean %>%
  left_join(keyfile) %>%
  select(hashed_office_id,hashed_province,hashed_district,hashed_position, lat, lon) %>%
  group_by(hashed_office_id) %>%
  fill(lat,.direction = 'updown') %>%
  fill(lon,.direction = 'updown')

# write
write_excel_csv(geo_public_officials_df,
                file.path(confidential_folder, paste0(country,"_public_officials_geo.csv")))

  