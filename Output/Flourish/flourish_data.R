# Script to create data for flourish visuals
# Brian Stacy 
# April 28, 2023

#libraries
library(tidyverse)
library(here)
library(haven)
library(flextable)
#remotes::install_github("Sebastien-Le/YesSiR")
library(YesSiR) # to export a flextable into MS Excel: exportxlsx() function
#directories


if (str_to_lower(Sys.info()["user"]) == "wb469649") {
  
  dir <- here()
  shared_loc <- 'C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Main_Documents/GEPD_FCDO_report/'
} else if (str_to_lower(Sys.info()["user"]) == "wb577189") {
  
  dir <- "C:/Users/wb577189/OneDrive - WBG/Documents/GitHub/GEPD"
  shared_loc <- 'C:/Users/wb577189/WBG/Ezequiel Molina - Dashboard (Team Folder)/Main_Documents/GEPD_FCDO_report/'
}


indicators_dir <- paste(dir, 'Indicators', sep="/")
out_dir <- paste(dir, 'Output', sep="/")

#path to confidential directory

if (str_to_lower(Sys.info()["user"]) == "wb469649") {
  #directories
  confidential_dir<- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/"
  anonymized_dir <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/"
  
} else if (str_to_lower(Sys.info()["user"]) == "wb577189") {
  #directories
  confidential_dir<- "C:/Users/wb577189/OneDrive - WBG/GEPD-Confidential/"
  anonymized_dir <- "C:/Users/wb577189/OneDrive - WBG/GEPD/"
  
}


#list countries
countries <- c("PER", "JOR", "RWA", "MDG", "ETH", "SLE", "NER")

# read in the data

#schools
schools <- read_dta(paste0(confidential_dir, "General/combined_school_office_df.dta")) %>%
  transmute(
    country=country,
    iso3c=case_when(
      country=="Peru" ~ "PER",
      country=="Jordan" ~ "JOR",
      country=="Rwanda" ~ "RWA",
      country=="Madagascar" ~ "MDG",
      country=="Ethiopia" ~ "ETH",
      country=="Sierra Leone" ~ "SLE",
      country=="Niger" ~ "NER"
    ),
    `4th Grade Student Knowledge (0-100)`= student_knowledge,
    `1st Grade Student Knowledge (0-100)`=ecd_student_knowledge,
    `Student Attendance Rate in the School`=student_attendance,
    `Teacher Presence Rate at School`=100-sch_absence_rate,
    `Teacher Content Knowledge (0-100)` = content_knowledge,
    `Teacher Pedagogical Skill` = teach_score,
    `Availability of Basic Inputs (0-5)`=inputs,
    `School Infrastructure Score (0-5)`=infrastructure,
    `Principal Knowledge of School (1-5)`=principal_knowledge_score,
    `Principal Management Practices at School (1-5)`=principal_management,
    `Operational Management of School (1-5)`=operational_management,
    `Instructional Leadership at School (1-5)`=instructional_leadership,
    `Urban/Rural Status`=if_else(rural==1, "Rural", "Urban")
  )

write_excel_csv(schools, paste0(confidential_dir, "General//Flourish/schools.csv"))


binned_schools <- schools %>%
  group_by(country, iso3c) %>%
  mutate(
    school_count=n(),
    bin_number=floor(school_count/10),
    bin=ntile(`4th Grade Student Knowledge (0-100)`, n=mean(bin_number))
    ) %>%
  group_by(country, iso3c, bin) %>%
  summarise(across(c(
    `4th Grade Student Knowledge (0-100)`,
    `1st Grade Student Knowledge (0-100)`,
    `Student Attendance Rate in the School`,
    `Teacher Presence Rate at School`,
    `Teacher Content Knowledge (0-100)`,
    `Teacher Pedagogical Skill` ,
    `Availability of Basic Inputs (0-5)`,
    `School Infrastructure Score (0-5)`,
    `Principal Knowledge of School (1-5)`,
    `Principal Management Practices at School (1-5)`,
    `Operational Management of School (1-5)`,
    `Instructional Leadership at School (1-5)`  ),
    ~mean(.,na.rm=TRUE)))


write_excel_csv(binned_schools, paste0(confidential_dir, "General//Flourish/binned_schools.csv"))

#grade 4
combined_g4 <- read_dta(paste0(confidential_dir, "General/combined_g4.dta")) %>%
  select(iso3c, student_age, student_male, student_knowledge, math_student_knowledge, literacy_student_knowledge, student_proficient, math_student_proficient, literacy_student_proficient) %>%
  mutate(country=case_when(
    iso3c=="PER" ~ "Peru",
    iso3c=="JOR" ~ "Jordan",
    iso3c=="RWA" ~ "Rwanda",
    iso3c=="MDG" ~ "Madagascar",
    iso3c=="ETH" ~ "Ethiopia",
    iso3c=="SLE" ~ "Sierra Leone",
    iso3c=="NER" ~ "Niger"
  )) %>%
  mutate(across(c('student_proficient', 'math_student_proficient', 'literacy_student_proficient'), ~if_else(.x==100,"Proficient", "Not Proficient"))) %>%
  rename(
    Age=student_age, 
    Male=student_male, 
    `4th Grade Student Knowledge (0-100)`=student_knowledge, 
    `4th Grade Math Test Score`=math_student_knowledge, 
    `4th Grade Literacy Test Score`=literacy_student_knowledge, 
    `4th Grade Student Proficient`=student_proficient, 
    `4th Grade Student Proficient - Math`=math_student_proficient, 
    `4th Grade Student Proficient - Literacy`=literacy_student_proficient
  ) %>%
  filter(!is.na(`4th Grade Student Knowledge (0-100)`))

write_excel_csv(combined_g4, paste0(confidential_dir, "General//Flourish/student_achievement_g4.csv"))


binned_student <- combined_g4 %>%
  group_by(country, iso3c, Male) %>%
  mutate(
    school_count=n(),
    bin_number=floor(school_count/10),
    bin=ntile(`4th Grade Student Knowledge (0-100)`, n=mean(bin_number))
  ) %>%
  group_by(country, iso3c, bin, Male) %>%
  summarise(across(c(
    `4th Grade Student Knowledge (0-100)`),
    ~mean(.,na.rm=TRUE))) %>%
  mutate(Gender=if_else(Male==1, "Male", "Female"),
         Proficient = if_else(`4th Grade Student Knowledge (0-100)`>82.9, "Student above minimal proficiency", "Student not above minimal proficiency"))


write_excel_csv(binned_student, paste0(confidential_dir, "General//Flourish/binned_student.csv"))

# Indicators


#read in indicator metadata
indicators <- read_csv(paste(dir,'Indicators/indicators.csv', sep = "/"))

# read in indicator data
PER_data <- read_csv(paste0(indicators_dir, "/GEPD_Indicators_API_", 'PER', ".csv")) %>%
  mutate(country="Peru",
         date='2019') %>%
  select(-year)

RWA_data <- read_csv(paste0(indicators_dir, "/GEPD_Indicators_API_", 'RWA', ".csv")) %>%
  mutate(country="Rwanda",
         date='2020') %>%
  select(-year)


JOR_data <- read_csv(paste0(indicators_dir, "/GEPD_Indicators_API_", 'JOR', ".csv")) %>%
  mutate(country="Jordan",
         date='2019') %>%
  select(-year)

SLE_data <- read_csv(paste0(indicators_dir, "/GEPD_Indicators_API_", 'SLE', ".csv")) %>%
  mutate(country="Sierra Leone",
         date='2022') %>%
  select(-year)

NER_data <- read_csv(paste0(indicators_dir, "/GEPD_Indicators_API_", 'NER', ".csv")) %>%
  mutate(country="Niger",
         date='2022') %>%
  select(-year)


ETH_data <- read_csv(paste0(indicators_dir, "/GEPD_Indicators_API_", 'ETH_pooled', ".csv")) %>%
  mutate(country="Ethiopia",
         date='2020-2021')  %>%
  select(-year)

# %>%
#   rename(value=value_pooled)

MDG_data <- read_csv(paste0(indicators_dir, "/GEPD_Indicators_API_", 'MDG', ".csv")) %>%
  mutate(country="Madagascar",
         date='2021') %>%
  select(-year)


# Indicator names
indicator_names_df <- PER_data %>%
  select(Series, `Indicator Name`)

combined_df_raw <- bind_rows(PER_data,JOR_data, RWA_data, ETH_data,  MDG_data, SLE_data, NER_data)




combined_df <- combined_df_raw %>%
  arrange(Series) %>%
  mutate(
    value=round(value,1),
    value_color=case_when(
      value_metadata=="Needs Improvement" ~ "#FF0000",
      value_metadata=="Caution" ~ "#FCD606",
      value_metadata=="On Target" ~ "#85C546",
      TRUE ~ "#808080"
    ),
    Series=str_replace_all(Series, "SE.LPV","SE.GEPD"))


combined_wide_df <- combined_df %>%
  select(Series, country, value) %>%
  pivot_wider(
    names_from=country, 
    values_from=value
  ) %>%
  left_join(indicator_names_df)

combined_wide_color <- combined_df %>%
  select(Series, country, value_color) %>%
  pivot_wider(
    names_from=country, 
    values_from=value_color
  ) %>%
  left_join(indicator_names_df)
#create function for creating country comparison tables
country_tab_fn <- function(variables, title) {
  
  
  tab_df <- combined_wide_df %>%
    filter(Series %in% variables) %>%
    arrange(factor(Series, levels=variables))
  
  #set colors
  tab_color_df <- combined_wide_color %>%
    filter(Series %in% variables) %>%
    arrange(factor(Series, levels=variables))
  
  Peru_col <- tab_color_df$Peru
  Jordan_col <- tab_color_df$Jordan
  Rwanda_col <- tab_color_df$Rwanda
  Ethiopia_col <- tab_color_df$Ethiopia
  Madagascar_col <- tab_color_df$Madagascar
  SLE_col <- tab_color_df$`Sierra Leone`
  Niger_col <- tab_color_df$Niger
  
  
  #build table
  tab_df <- tab_df  %>%
    select(`Indicator Name`, Peru, Jordan, Rwanda, Ethiopia, Madagascar, `Sierra Leone`, Niger)
  
  
  ovr_table <- flextable(tab_df) %>%
    add_header_lines(title) %>%
    add_footer_lines('Source: UIS, GLAD, GEPD, World Bank. 
 Green indicates indicator "on-target", yellow indicates "requires caution", red indicates "needs improvement"' ) %>%
    theme_vanilla()
  
  #colors
  ovr_table <- ovr_table %>%
    bg(j = c('Peru'),
       bg = Peru_col) %>%
    bg(j = c('Jordan'),
       bg = Jordan_col) %>%
    bg(j = c('Ethiopia'),
       bg = Ethiopia_col) %>%
    bg(j = c('Rwanda'),
       bg = Rwanda_col) %>%
    bg(j = c('Madagascar'),
       bg = Madagascar_col)   %>%
    bg(j = c('Sierra Leone'),
       bg = SLE_col)  %>%
    bg(j = c('Niger'),
       bg = Madagascar_col)  
  
  
  ovr_table %>%
    set_table_properties(layout = "autofit")
  
  
}

# Produce a table showing means by country for select practice indicators
practice_indicators <- c(
  'SE.PRM.LERN',
  'SE.PRM.EFFT',
  'SE.PRM.CONT',
  'SE.PRM.PEDG',
  'SE.PRM.INPT',
  'SE.PRM.INFR',
  'SE.PRM.LCAP',
  'SE.PRM.ATTD',
  'SE.PRM.OPMN',
  'SE.PRM.ILDR',
  'SE.PRM.PKNW',
  'SE.PRM.PMAN'
)
practices <- country_tab_fn(practice_indicators, 'GEPD Practice Indicators')
exportxlsx(practices, path = paste0(confidential_dir, "General//Flourish/practice_indicators.xlsx"))

practice_tags <- "SE.PRM.PROE|SE.LPV.PRIM|SE.PRM.LERN|SE.PRM.TENR|SE.PRM.EFFT|SE.PRM.CONT|SE.PRM.ATTD|SE.PRM.LCAP|SE.PRM.PEDG|SE.LPV"


combined_df_raw %>%
  filter(Series %in% practice_indicators) %>%
  mutate(sort=factor(Series, levels=practice_indicators)) %>%
  arrange(sort) %>%
  mutate(gradient=case_when(
    grepl(practice_tags, Series) & value <85 ~ 1 + 2*(value)/85,
    grepl(practice_tags, Series) & value >=85 & value<90 ~ 3 + (value-85)/5,
    grepl(practice_tags, Series) & value >=90 ~ 4 + (value-90)/10,
    TRUE ~ value
  )) %>%
  write_excel_csv(paste0(confidential_dir, "General//Flourish/practice_indicators_long.csv"))


#urban/rural
t <- combined_wide_df %>%
  filter(Series %in% c('SE.PRM.LERN.1', 'SE.PRM.LERN.1.R', 'SE.PRM.LERN.1.U')) %>%
  mutate(col=case_when(
    Series=='SE.PRM.LERN.1' ~ "Overall", 
    Series=='SE.PRM.LERN.1.R' ~ "Rural", 
    Series=='SE.PRM.LERN.1.U' ~ "Urban"
  ))

t %>%
  mutate(Group="Proficient") %>%
  bind_rows(t %>% mutate(across(c("Peru",
                                  "Jordan",
                                  "Rwanda",
                                  "Ethiopia",
                                  "Madagascar",
                                  "Sierra Leone",
                                  "Niger"),~100-.),
                         Group="Not Proficient")) %>%
  pivot_longer(
    cols = c("Peru",
             "Jordan",
             "Rwanda",
             "Ethiopia",
             "Madagascar",
             "Sierra Leone",
             "Niger"),
    names_to="Country",
    values_to="Value"
  ) %>%
  write_excel_csv(paste0(confidential_dir, "General//Flourish/g4_urban_rural.csv"))

# public officials

public_officials_df <- read_dta(paste0(confidential_dir, "General/combined_public_officials.dta"))


#create likert data

likert_columns <-   c(
    #'NLG1q1',
    #'NLG1q2',
    'NLG1q3',
    #'NLG2q1',
    #'NLG2q2',
    #'NLG2q3',
    #' 'NLG3q1',
    'NLG3q2',
    #' 'NLG3q3',
    #' 'NLG4q1',
    #' 'NLG4q2',
    'NLG4q3',
    #' 'ACM2q1',
    #' 'ACM2q2',
    #'ACM2q3',
    #' 'ACM3q1',
    'ACM3q2',
    #'ACM3q3',
    #' 'ACM4q1',
    #' 'ACM4q2',
    'ACM4q3',
    #' 'QB1q1',
    #' 'QB1q3',
    #' 'QB2q1',
    'QB2q2',
    #' 'QB2q3',
    #' 'QB3q1',
    #' 'QB3q2',
    #' 'QB3q3',
    #' 'QB4q1',
    #' 'QB4q2',
    #' 'QB4q4a',
    #' 'QB4q4b',
    #' 'QB4q4c',
    #' 'QB4q4d',
    #' 'QB4q4e',
    #' 'QB4q4f',
    'QB4q4g',
    #' 'QB4q4h',
    'IDM1q1',
    #' 'IDM1q2',
    #' 'IDM1q3',
    #' 'IDM2q1',
    #' 'IDM2q2',
    #' 'IDM2q3',
    #' #'IDM3q1',
    #' #'IDM3q2',
    #' #'IDM3q3',
    'IDM4q1'
    #'IDM4q2'
    #' 'IDM4q3'
)

#read in metadata
public_officials_metadata  <- read_csv(paste0(confidential_dir, "General//Flourish/public_officials_metadata.csv")) %>%
  mutate(name=str_to_lower(name))

likert_columns <- as.character(lapply(likert_columns, str_to_lower))

likert_df <- public_officials_df %>%
  select(iso3c,likert_columns) %>%
  pivot_longer(cols=likert_columns,
               names_to = 'name',
               values_to='value') %>%
  group_by(iso3c, name) %>%
  summarise(Mean=mean(value, na.rm = TRUE),
            `1-Negative Answer`=mean(value==1, na.rm = TRUE),
            `2`=mean(value==2, na.rm = TRUE),
            `3`=mean(value==3, na.rm = TRUE),
            `4`=mean(value==4, na.rm = TRUE),
            `5-Positive Answer`=mean(value==5, na.rm = TRUE),
            ) %>%
  mutate(country=case_when(
    iso3c==1 ~ "NER",
    iso3c==2 ~ "SLE",
    iso3c==3 ~ "MDG",
    iso3c==4 ~ "ETH",
    iso3c==5 ~ "RWA",
    iso3c==6 ~ "JOR",
    iso3c==7 ~ "PER"
  )) %>%
  left_join(public_officials_metadata)

write_excel_csv(likert_df, paste0(confidential_dir, "General//Flourish/likert_df.csv"))





