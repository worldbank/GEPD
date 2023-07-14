
## Set directory
setwd("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Pakistan_all/2021/Data/02. Provinces/KP/public_officials/raw")

## List the files searching for dta format

filelist <- list.files(pattern = ".dta",recursive=TRUE)
filelist

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
lapply(seq_along(lst1), function(i) write_dta(lst1[[i]], paste("C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT/PAK/PAK_2022_GEPD/PAK_2022_GEPD_v01_RAW/Data/KP/raw/Public_Officials", 
                                                               names(lst1)[i], sep = "/")))





## Final check
list.files(recursive=TRUE)
