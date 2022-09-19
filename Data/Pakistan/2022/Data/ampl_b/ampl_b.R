#Clean data files downloaded from API
#Written by Adrien Ciret 01/06/2022

#load relevant libraries

library(skimr)
library(naniar)
library(vtable)
library(digest)
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(sjlabelled)
#NOTE:  The R script to pull the data from the API should be run before this file



#Create function to save metadata for each question in each module
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}



#Country name and year of survey
country <-'Pakistan_all'
country_name <- "Pakistan_all"
year <- '2021'


#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

backup_onedrive="yes"

#Add your UPI here and set the directory paths of your choice.
if (Sys.getenv("USERNAME") == "wb469649"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/"
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/01. Pilot/raw/amplb", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/01. Pilot/clean/amplb", sep="/"))
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/01. Pilot/clean/amplb", sep="/"))
  
} else if (Sys.getenv("USERNAME") == "wb577189"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/"
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/01. Pilot/raw/amplb", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/01. Pilot/clean/amplb", sep="/"))
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/01. Pilot/clean/amplb", sep="/"))
  
} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")
  save_folder_onedrive <- choose.dir(default = "", caption = "Select folder to save backed up data to onedrive")
  
}



############################
#read in AMPL-b file
############################

amplb<-read_dta(file.path(download_folder, "v1/ampl_dash.dta")) %>% 
  bind_rows(read_dta(file.path(download_folder, "v2/ampl_dash.dta")))%>% 
  #clean the names and EMIS code
  mutate(
    school_emis_preload = if_else(school_info_correct==0, m1s0q2_emis, school_emis_preload),
    school_name_preload = if_else(school_info_correct==0, m1s0q2_name, school_name_preload)
    
  ) %>% filter(m1s0q1_name_other!="adrien")


roster <- read_dta(file.path(download_folder, "v1/questionnaire_roster.dta")) %>% 
  bind_rows(read_dta(file.path(download_folder, "v2/questionnaire_roster.dta")))






## Correct the issue with labelling
library(Hmisc)
label(roster[["Item2_1"]]) <- "MR004"
label(roster[["Item32_2"]]) <- "MR004"
label(roster[["Item52_1"]]) <- "MM089"
label(roster[["Item22_2"]]) <- "MM089"
# label(roster[["Item11_2"]]) <- "MM175"
# label(roster[["Item41_1"]]) <- "MM175"


## Metadata

meta_amplb <- makeVlist(roster) %>% select(-c(vallabel)) %>% rename(`Item number new`=name) %>% 
  filter(str_detect(`Item number new`, "^Item")) %>% 
  mutate(`Item number new` = gsub("(.*)_(.*)", "\\1", `Item number new`))


ampl_b <- roster %>% 
  select(-m3sb_troster) %>%
  left_join(amplb %>% select(interview__key, ends_with("preload"))) %>% 
  mutate(unique_id = as.character(lapply(student_name, function(x) {digest(x, algo="xxhash64", seed=531254, serialize = T)}))) %>% 
  relocate(ends_with("preload")) %>% 
  mutate(unique_id= paste0(unique_id, interview__id))


socio_eco <- ampl_b %>%  
  select_if(.predicate = (grepl("ST", Hmisc::label(.), ignore.case = TRUE))) %>% select(-c(1))

preamble <- ampl_b %>% select(1:15, unique_id) %>%
   bind_cols(socio_eco)%>% label_to_colnames(starts_with("Item7|Item6"))


key <- ampl_b %>% select(student_name, ends_with("id"), ends_with("key"))

ampl_b <- ampl_b %>% 
  relocate(unique_id) %>% 
  select(-student_name) %>% 
  distinct(unique_id, .keep_all=T)

#write.csv(ampl_b, "C:/Users/wb577189/OneDrive - WBG/Desktop/amplb_batch_1_sl.csv", row.names = F, na = "")
# write_dta(ampl_b, "C:/Users/wb577189/OneDrive - WBG/Desktop/amplb_batch_1_sl.dta")

## Item MM175 was a complex MC question, so we clean it and gather the response into one var
MM175 <- ampl_b %>%
  unite("Item11_2", Item11_2_1:Item11_2_4,  sep= "",
        remove = FALSE) %>%
  unite("Item41_1", Item41_1_1:Item41_1_4,  sep= "",
        remove = FALSE) %>%
  mutate_all(funs(str_replace(., "NANANANA", NA_character_)))  %>% select(unique_id, Item11_2, Item41_1) %>% 
  mutate(across(c(Item11_2, Item41_1), ~ as.numeric(.))) %>% 
  
  mutate(MM175 = coalesce(Item11_2, Item41_1)) %>% select(unique_id, MM175)


label(MM175[["MM175"]]) <- "MM175"



## first we pivot from wide to long

roster_long <- roster %>%
  mutate(across(c(Item1_1:Item76), ~ as.character(.))) %>% 
  pivot_longer(
    cols = starts_with("Item"),
    names_to = c("Item number", "Booklet type"),
    names_pattern = "Item(.*)_(.*)",
    values_to = "Answer"
  ) %>%

  mutate(`Item number` = as.numeric(`Item number`)) %>%

  mutate(`Item number new` = case_when(

    amplb_booklet == 2 & `Item number` > 30 ~ `Item number` - 30,
    amplb_booklet == 2 & `Item number` < 30 ~ `Item number` + 30,
    TRUE ~ `Item number`

  )) %>%

  mutate(`Item number new` = paste0("Item", `Item number new`, sep="")) %>%

 left_join(meta_amplb) %>%
  
  mutate(across(c(9:12), ~ as.numeric(.))) %>% filter(!str_detect(varlabel, "NULL")) 

  x <- roster_long %>%  pivot_wider(names_from = `Item number new`,
                values_from = Answer)


## Then we re order the question so that booklets 1 and 2 have literacy and numeracy items in the same order
  
 meta <-  labelled::generate_dictionary(ampl_b)
  
item_fun<- function(var){
  
  items <- makeVlist(roster) %>% select(-c(vallabel)) %>% rename(`Item number new`=name) %>% filter(varlabel==!!var) %>% pull(`Item number new`)
    
  x <- ampl_b %>% 
    select(unique_id, items) %>% 
    select(items) %>% 
    ## Have to remove the labels otherwise we cannot convert to numeric
    labelled::remove_labels() %>% 
    mutate(across(items, ~ as.numeric(.))) %>% 
    
    mutate(var = coalesce(!!!.)) %>% bind_cols(ampl_b %>% select(unique_id)) %>%  select(unique_id, var) 
  
    names(x)[2] <-  quo_name(enquo(var))
    
   assign(paste0("item_", quo_name(enquo(var))), x, envir = .GlobalEnv)
    
}


item_list <- roster_long %>% distinct(as.character(varlabel)) %>% pull()
  
for (i in item_list) {
  
  item_fun(var = i)
  
}

df_list <- mget(ls(pattern = "^item_[A-Z].*"))

#Remove duplicated columns


final <- df_list %>% reduce(full_join, by = "unique_id")%>%
  select(unique_id, everything()) %>% 
  left_join(preamble %>% select(-PF449)) %>% 
  ## Add complex MM175
  left_join(MM175) %>% 
  relocate(names(preamble)) %>% 
  distinct(unique_id, .keep_all=T)

n_distinct(final$unique_id)
n_distinct(final$school_emis_preload)


######################################
# sample_combined <- read.csv("C:/Users/wb577189/OneDrive - WBG/GEPD/CNT//SLE/SLE_2022_GEPD/SLE_2022_GEPD_v01_M/Data/Sampling/sample_schools_2022-05-08.csv") %>% 
#   bind_rows(read.csv("C:/Users/wb577189/OneDrive - WBG/GEPD/CNT//SLE/SLE_2022_GEPD/SLE_2022_GEPD_v01_M/Data/Sampling/sample_replacement_schools_2022-04-12.csv"))
# 
# #create weights for each school
# data_set_updated <- sample_combined %>%
#   mutate(N_sch=n()) %>% #get total number of schools
#   #now calculate probability of selecting woreda
#   group_by(idregion, iddistrict) %>%
#   mutate(N_school_district=n(),
#          N_students_district=sum(class4_combined,na.rm=T)) %>%
#   group_by(idregion) %>%
#   mutate(strat_total=sum(class4_combined, na.rm=T),
#          N_sel_strata=sum(as.numeric(sample=="Sampled School"),na.rm=T),
#          N_sch_strata=n()) #stratum weights (three schools selected)
# 
# df_weights_function <- function(dataset,scode, snumber, prov) {
#   scode<-enquo(scode)  
#   snumber<-enquo(snumber)
#   prov<-enquo(prov)
#   
#   dataset %>%
#     mutate(!! scode := as.numeric(.data$school_code)) %>%
#     left_join(weights_df %>% filter(Admin_Code %in% emis_list) %>% filter(Region %in% region_list)  %>% filter(Zone %in% zone_list)  %>% filter(Woreda %in% woreda_list)  %>% filter(Program==1)) %>%
#     left_join(data_set_updated %>% filter(Admin_Code %in% emis_list) %>% filter(Region %in% region_list) %>% filter(Zone %in% zone_list)   %>% filter(Woreda %in% woreda_list) %>% filter(Program==1))  %>%
#     mutate(
#       N_schools=N_sch,
#       N_schools_strata=N_sch_strata,
#       N_selected_strata=N_sel_strata,
#       school_weights=ipw,
#       ipw=ipw*!! snumber,
#       ipw=if_else(is.na(ipw),mean(ipw, na.rm=T), ipw)) %>%
#     mutate(province=Region,
#            urban_rural=Location
#     ) %>%
#     select(-one_of(colnames(data_set_updated[, -which(names(data_set_updated) == "Location" | names(data_set_updated) == "Region" | 
#                                                         names(data_set_updated) == "owner" )])))
# }






# ## Save the data
# write_dta(final %>% select(-`Please enter student's name`) %>% rename(assessment_y_n=`Was the student present during the assessment?`,
#                                                      type_booklet=`Type of booklet`), file.path(save_folder, "amplb_pak.dta"))
write.csv(final   %>% rename(assessment_y_n=`Was the student present during the assessment?`,
                                                     type_booklet=`Type of booklet`), file.path(save_folder, "amplb_pak.csv"), row.names = F)
# 
# write_dta(final %>% rename(assessment_y_n=`Was the student present during the assessment?`,
#                            type_booklet=`Type of booklet`), file.path(save_folder, "amplb_key_confidential_pak.dta"))
write.csv(final%>% rename(assessment_y_n=`Was the student present during the assessment?`,
                          type_booklet=`Type of booklet`), file.path(save_folder, "amplb_key_confidential_pak.csv"), row.names = F)



