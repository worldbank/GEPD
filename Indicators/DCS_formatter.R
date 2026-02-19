# Convert country indicators into DCS format
# written by Brian Stacy on July 18, 2022
library(tidyverse)
library(writexl)
library(here)

dir <- here()

# #list of countries to update
# updt_list <- c('MDG','ETH_pooled','PER','JOR')
# 
# 
# 
# if (exists('combined_df')) {
#   rm(combined_df)
# }
# 
# for (i in updt_list) {
#   temp <- read_csv(paste0(dir, "/Indicators/GEPD_Indicators_API_",i,".csv")) %>%
#     mutate(year=as.numeric(year))
#   
#   assign(i,temp)
#   
#   if (!exists('combined_df')) {
#     combined_df <- temp
#   } else {
#     combined_df <- combined_df %>% bind_rows(temp)
#   }
#   
# }

combined_df <- read_csv('C:\\Users\\WB469649\\WBG\\HEDGE Files - GEPD-Confidential\\General\\Country_Data\\Indicators\\GEPD_Indicators_latest_120825_with_colors.csv')

# ensure year exists as character for downstream mapping
if (!"year" %in% names(combined_df)) {
  combined_df$year <- NA_character_
} else {
  combined_df$year <- as.character(combined_df$year)
}

# add countrycode variable
combined_df <- combined_df %>%
  mutate(
    countrycode = case_when(
      country == "Peru" ~ "PER",
      country == "Jordan" | country == "Jordan (2019)" | country == "Jordan (2023)" ~ "JOR",
      country == "Rwanda" ~ "RWA",
      country == "Ethiopia" | country == "Ethiopia (2020/2021)" | country == "Ethiopia (2025)" ~ "ETH",
      country == "Madagascar" ~ "MDG",
      country == "Sierra Leone" ~ "SLE",
      country == "Niger" ~ "NER",
      country == "Gabon" ~ "GAB",
      country == "Chad" ~ "TCD",
      country == "Nigeria - Edo State" ~ "NGE",
      country == "Pakistan - ICT" | country == "Pakistan ICT" ~ "PCT",
      country == "Pakistan - KP" | country == "Pakistan KP" ~ "PKP",
      country == "Pakistan - Balochistan" | country == "Pakistan Balochistan" ~ "PBA",
      country == "Pakistan - Sindh" | country == "Pakistan Sindh" ~ "PSI",
      country == "Pakistan - Punjab" | country == "Pakistan Punjab" ~ "PPU",
      country == "Central African Republic" ~ "CAF",
      country == "Bangladesh" ~ "BGD",
      country == "Montenegro" ~ "MNE",
      TRUE ~ NA_character_
    ),
    year = case_when(
      !is.na(year) & year != "" ~ year,
      country == "Peru" ~ "2019",
      country == "Jordan (2019)" ~ "2019",
      country == "Jordan (2023)" ~ "2023",
      country == "Jordan" ~ "2023",
      country == "Rwanda" ~ "2020",
      country == "Ethiopia (2020/2021)" ~ "2021",
      country == "Ethiopia (2025)" ~ "2025",
      country == "Madagascar" ~ "2021",
      country == "Sierra Leone" ~ "2022",
      country == "Niger" ~ "2022",
      country == "Gabon" ~ "2023",
      country == "Chad" ~ "2023",
      country == "Nigeria - Edo State" ~ "2023",
      country == "Pakistan - ICT" | country == "Pakistan ICT" ~ "2022",
      country == "Pakistan - KP" | country == "Pakistan KP" ~ "2022",
      country == "Pakistan - Balochistan" | country == "Pakistan Balochistan" ~ "2023",
      country == "Pakistan - Sindh" | country == "Pakistan Sindh" ~ "2023",
      country == "Pakistan - Punjab" | country == "Pakistan Punjab" ~ "2023",
      country == "Central African Republic" ~ "2024",
      country == "Bangladesh" ~ "2024",
      country == "Montenegro" ~ "2025",
      country == "Ethiopia" ~ "2025",
      TRUE ~ NA_character_
    )
  )

  #drop SE.PRM.LCAP.R and SE.PRM.LCAP.U
combined_df <- combined_df %>%
  filter(!(Series %in% c("SE.PRM.LCAP.R", "SE.PRM.LCAP.U", "SE.GEPD.PROE","SE.GEPD.PROE.1"))) 

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



