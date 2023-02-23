# Convert country indicators into DCS format
# written by Brian Stacy on July 18, 2022
library(tidyverse)
library(writexl)
library(here)

dir <- here()

#list of countries to update
updt_list <- c('MDG','ETH_pooled','PER','JOR')



if (exists('combined_df')) {
  rm(combined_df)
}

for (i in updt_list) {
  temp <- read_csv(paste0(dir, "/Indicators/GEPD_Indicators_API_",i,".csv")) %>%
    mutate(year=as.numeric(year))
  
  assign(i,temp)
  
  if (!exists('combined_df')) {
    combined_df <- temp
  } else {
    combined_df <- combined_df %>% bind_rows(temp)
  }
  
}

Data <- combined_df %>%
  transmute(
    Time=paste0("YR",year),
    Country=countrycode,
    Series=Series,
    Scale=0,
    Data=value
  )

Metadata <- combined_df %>%
  transmute(
    Country=countrycode,
    Series=Series,
    Time=paste0("YR",year),
    Footnote=value_metadata,
    `Series Survey Source`="GEPD"
  )

write_xlsx(x=list("Data - Long format"=Data, "Country-Series-Time_Table"=Metadata),
           path=paste0(dir, "/Indicators/GEPD_Indicators_API_",Sys.Date(),".xlsx"))