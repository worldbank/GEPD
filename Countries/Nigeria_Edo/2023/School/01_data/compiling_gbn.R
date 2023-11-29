
## Set directory
setwd("C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_RAW/Data/raw/School")

## List the files searching for dta format

filelist <- list.files(pattern = ".dta",recursive=TRUE)
filelist

## Pair them based on their names
lst1 <- lapply(split(filelist, basename(filelist)),function(x) do.call(rbind, 
                                                                         lapply(x,function(y) read_dta(y))))

## Create a new directory (if need be)
dir.create("combined_versions")
mainDir <- paste(getwd(), "combined_versions", sep="/")

## Indicate where the combined datasets have to be saved
setwd(mainDir) #change the working directory to mainDir from the previous code

# subDir <- gsub("//..*", "", unique(basename(filelist)))
# subDir

## Compile and save the datasets
lapply(seq_along(lst1), function(i) write_dta(lst1[[i]], paste(getwd(), 
                                                                 names(lst1)[i], sep = "/")))


 ##Final clean up

key <- read_dta("C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_RAW/Data/raw/School/key/GEPD_GABON_DATA.dta")

var <- names(key)

school_final <-  read_dta("C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_RAW/Data/raw/School/EPDash.dta") %>% 
  select(-ends_with("_preload")) %>% 
  left_join(key) %>% 
  relocate(interview__key, ends_with("_preload"))

write_dta(school_final,path = "C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_RAW/Data/raw/School/EPDash.dta")
write_dta(not_included,path = "C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_RAW/Data/raw/School/to_verify.dta")

# not_included <- key %>% anti_join(read_dta("C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_RAW/Data/raw/School/EPDash.dta") %>% 
#                                     select(-ends_with("_preload")))


## Final check
list.files(recursive=TRUE)
