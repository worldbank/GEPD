#save geospatial files
library(tidyverse)

############
# Prep some data
############


rm(list=ls())
#Country name
country <-'ETH'
country_name <- "Ethiopia"
year <- '2021'

#directories
project_folder  <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT/"
confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/School", sep="/"))
save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/anonymized/School", sep="/"))


#load school data for comparisons
load(paste(confidential_folder, "/school_survey_data.Rdata", sep=""))
#load key file
keyfile <- read_csv(file.path(confidential_folder, "EPDash_linkfile_hashed.csv"))
#get list of data frames and add _2021 as suffix
frames_2021 <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

for (x in frames_2021) {
  
  t <- get(x)
  assign(paste0(x,"_2021"), t )
  
}




year <- '2020'
#directories
project_folder  <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT/"
confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/School", sep="/"))
save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/anonymized/School", sep="/"))


#attach saved results for 2020
load(paste0(confidential_folder, "/school_survey_data.Rdata"))
#load key file
keyfile <- read_csv(file.path(confidential_folder, "EPDash_linkfile_hashed.csv"))
# Go through all dataframes and append 2020 and 2021 data together
for (x in frames_2021) {
  
  t <- get(x)
  t_2021 <- get(paste0(x,"_2021"))
  
  t <- t %>%
    mutate(date=2020) %>%
    bind_rows(t_2021) %>%
    mutate(date=if_else(is.na(date),2021,date)) 
  
  assign(x, t )
  
}



year <- '2020_2021'
confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/School", sep="/"))



  save(list = frames_2021,
       file = paste0(confidential_folder,"/school_survey_data.Rdata"))
  

############
# Now do linkages
#############  


  rm(list = ls())
  #Country name
  country <-'ETH'
  country_name <- "Ethiopia"
  year <- '2020_2021'
  
  
  #directories
  project_folder  <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT/"
  confidential_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/anonymized/School", sep="/"))
  
  

##################
# Schools
##################

#Load data
load(file.path(confidential_folder, "school_survey_data.RData"))



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
country <-'ETH'
country_name <- "Ethiopia"
year <- '2020_2021'

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
  select(id_code, hashed_office_id,hashed_position, interview__id, lat, lon) 

# write
write_excel_csv(geo_public_officials_df,
                file.path(confidential_folder, paste0(country,"_public_officials_geo.csv")))

  