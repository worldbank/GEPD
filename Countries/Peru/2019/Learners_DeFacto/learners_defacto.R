library(tidyverse)
library(haven)
#score expert data (this requires a lot of hard coding and transcribing)
defacto_dir <- "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Other_Indicators"
#read in data

#define function to help clean this data read in (variable read in as factor, so this fixes this)
read_var <- function(var) {
  as.numeric(as.character(var))
}

#read in data
defacto_dta_learners <- readxl::read_xlsx(path=paste(defacto_dir, 'Learners_defacto_indicators.xlsx', sep="/"),  .name_repair = 'universal') 
defacto_dta_learners_shaped<-data.frame(t(defacto_dta_learners[-1]))
colnames(defacto_dta_learners_shaped) <- defacto_dta_learners$question

#create indicators
defacto_dta_learners_final <- defacto_dta_learners_shaped %>%
  rownames_to_column() %>%
  filter(rowname=='Scoring') %>%
  select(-rowname)


#nutrition
defacto_dta_learners <- defacto_dta_learners %>%
  group