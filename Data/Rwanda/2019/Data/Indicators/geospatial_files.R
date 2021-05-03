#save geospatial files
library(tidyverse)

#Country name
country <-'RWA'
country_name <- "Rwanda"
year <- '2020'

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


#load key
geo_school_df <- final_school_data %>%
  left_join(keyfile) %>%
  select(hashed_school_code, hashed_school_province, hashed_school_district, lat, lon)

# write
write_excel_csv(geo_school_df,
                file.path(confidential_folder, paste0(country,"_school_geo.csv")))

##############
# Public Officials
##############

#Country name
country <-'RWA'
country_name <- "Rwanda"
year <- '2020'

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
  mutate(hashed_office_id = hashed_office) %>%
  select(id_code, hashed_office_id,hashed_position, lat, lon) %>%
  group_by(hashed_office_id) %>%
  fill(lat,.direction = 'updown') %>%
  fill(lon,.direction = 'updown') %>%
  filter(!is.na(id_code))

# write
write_excel_csv(geo_public_officials_df,
                file.path(confidential_folder, paste0(country,"_public_officials_geo.csv")))

  