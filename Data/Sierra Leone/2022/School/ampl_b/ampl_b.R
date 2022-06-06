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
country <-'Sierra Leone'
country_name <- "Sierra Leone"
year <- '2022'

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
  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School/AMPL_b", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School", sep="/"))
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School", sep="/"))
  
} else if (Sys.getenv("USERNAME") == "wb577189"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/"
  
  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School_Survey/AMPL_b", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School_Survey/AMPL_b", sep="/"))
  
  # This is experimental and not currently in use.
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School_Survey", sep="/"))
  
} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")
  save_folder_onedrive <- choose.dir(default = "", caption = "Select folder to save backed up data to onedrive")
  
}

############################
#read in AMPL-b file
############################

amplb<-read_dta(file.path(download_folder, "ampl_dash_v3.dta")) %>% 
  bind_rows(read_dta(file.path(download_folder, "ampl_dash.dta")))

roster <- read_dta(file.path(download_folder, "questionnaire_roster.dta")) %>% 
  bind_rows(read_dta(file.path(download_folder, "questionnaire_roster_v3.dta")))

meta_amplb <- makeVlist(roster) %>% select(-c(vallabel)) %>% rename(`Item number new`=name) %>% 
  filter(str_detect(`Item number new`, "^Item")) %>% 
  mutate(`Item number new` = gsub("(.*)_(.*)", "\\1", `Item number new`))


ampl_b <- roster %>% 
  select(-m3sb_troster) %>%
  left_join(amplb %>% select(interview__key, ends_with("preload"))) %>% 
  mutate(unique_id = as.character(lapply(student_name, function(x) {digest(x, algo="xxhash64", seed=531254, serialize = T)}))) %>% 
  relocate(ends_with("preload"))

preamble <- ampl_b %>% select(1:15, unique_id, starts_with("Item7")) %>% label_to_colnames(starts_with("Item7"))

key <- ampl_b %>% select(student_name, ends_with("id"), ends_with("key"))

ampl_b <- ampl_b %>% 
  relocate(unique_id) %>% 
  select(-student_name) %>% 
  distinct(unique_id, .keep_all=T)

#write.csv(ampl_b, "C:/Users/wb577189/OneDrive - WBG/Desktop/amplb_batch_1_sl.csv", row.names = F, na = "")
# write_dta(ampl_b, "C:/Users/wb577189/OneDrive - WBG/Desktop/amplb_batch_1_sl.dta")

## first we pivot from wide to long

roster_long <- roster %>%
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
  
  
  
item_fun<- function(var){
  
  items <- makeVlist(roster) %>% select(-c(vallabel)) %>% rename(`Item number new`=name) %>% filter(varlabel==!!var) %>% pull(`Item number new`)
    
  x <- ampl_b %>% 
    select(unique_id, items) %>% 
    mutate(across(items, ~ as.numeric(.))) %>% 
    select(items) %>% 
    mutate(var = coalesce(!!!.)) %>% bind_cols(ampl_b %>% select(unique_id)) %>%  select(unique_id, var) 
  
  names(x)[2] <-  quo_name(enquo(var))
    
   assign(paste0("item_", quo_name(enquo(var))), x, envir = .GlobalEnv)
    
}


item_list <- roster_long %>% distinct(as.character(varlabel)) %>% pull()
  
for (i in item_list) {
  
  item_fun(var = i)
  
}

df_list <- mget(ls(pattern = "^item_[A-Z].*"))

final <- df_list %>% reduce(full_join, by = "unique_id")%>%
  select(unique_id, everything()) %>% 
  left_join(preamble %>% select(-MM208, -MR003)) %>% 
  relocate(names(preamble))



## Save the data
write_dta(final %>% select(-student_name), file.path(save_folder, "amplb_sl.dta"))
write.csv(final %>% select(-student_name), file.path(save_folder, "amplb_sl.csv"))

write_dta(final, file.path(save_folder, "amplb_key_confidential_sl.dta"))
write.csv(final, file.path(save_folder, "amplb_key_confidential_sl.csv"))



