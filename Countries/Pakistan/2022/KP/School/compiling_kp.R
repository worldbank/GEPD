
## Set directory
setwd("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/KP/school_survey/raw")

## List the files searching for dta format

filelist <- list.files(pattern = ".dta",recursive=TRUE)
filelist


## Because different versions of the school survey were used, we end up with datasets that do not have the same dimension and this prevents us from binding the data. We identify where those differences are
files_map <- "C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/KP/school_survey/raw" 

output <- data.table::rbindlist(lapply(filelist, function(file) {
  dt <- read_dta(paste0(files_map, "/", file))
  list("number_of_cols" = ncol(dt), "number_of_rows" = nrow(dt), "name_of_file" = file)
})
)

## Difference - 809 instead of 812 columns for versions 4 and 5.
# 
# '%!in%' <- function(x,y)!('%in%'(x,y))
# 
# all_var <- names(read_dta("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/KP/school_survey/raw/version_6/EPDashboard.dta"))

read_dta("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/KP/school_survey/raw/version_4/EPDashboard.dta") %>% 
  mutate(m1sbq14_inpt_etri = NA,
         m1sbq16_infr__99 = NA,
         m1sbq17_infr__99 = NA) %>% 
  write_dta("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/KP/school_survey/raw/version_4/EPDashboard.dta")

read_dta("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/KP/school_survey/raw/version_5/EPDashboard.dta") %>% 
  mutate(m1sbq14_inpt_etri = NA,
         m1sbq16_infr__99 = NA,
         m1sbq17_infr__99 = NA) %>% 
  write_dta("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/KP/school_survey/raw/version_5/EPDashboard.dta")

library(haven)
library(magrittr)
library(dplyr)
## Pair them based on their names

lst1 <- lapply(split(filelist, basename(filelist)),function(x) do.call(rbind, 
                                                                         lapply(x,function(y) read_dta(y))))

## Create a new directory (if need be)
#dir.create("combined_versions")
mainDir <- paste(getwd(), "combined_versions", sep="/")

## Indicate where the combined datasets have to be saved
setwd(mainDir) #change the working directory to mainDir from the previous code

# subDir <- gsub("//..*", "", unique(basename(filelist)))
# subDir

## Compile and save the datasets
lapply(seq_along(lst1), function(i) write_dta(lst1[[i]], paste(getwd(), 
                                                                 names(lst1)[i], sep = "/")))


## Compile and save the datasets in the folder for the cleaning and wrangling
lapply(seq_along(lst1), function(i) write_dta(lst1[[i]], paste("C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT/PAK/PAK_2022_GEPD/PAK_2022_GEPD_v01_RAW/Data/KP/raw/School", 
                                                               names(lst1)[i], sep = "/")))





## Final check
list.files(recursive=TRUE)
