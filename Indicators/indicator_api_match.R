#Read in indicators.md file
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(skimr)
library(naniar)
library(vtable)
library(readxl)
library(readr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#Read in list of indicators
indicators <- read_delim(here::here('Indicators','indicators.md'), delim="|", trim_ws=TRUE)
indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)


indicators <- indicators %>%
  select(-c('X1', 'X8'))

indicator_names <-  indicators$indicator_tag
indicator_names <- sapply(indicator_names, tolower)

names(indicators)<-make.names(names(indicators), unique=TRUE)


#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- indicators$indicator_tag



#Read in Sergio's excel
subquestions<-read_excel('GEPD_Indicators_Info_v4.xlsx', sheet='SubQuestions') 

df<-indicators %>%
  left_join(subquestions) %>%
  select(Series, indicator_tag, Indicator.Name, starts_with('Column_'), starts_with('Sub'))

#Pivot longer
df_longer<-df %>%
  pivot_longer(cols=c('Column_2', 'Column_3', 'Column_4', 'Column_5', 'Column_6',
                      'Subquestion_1', 'Subquestion_2', 'Subquestion_3',
                      'Subquestion_4', 'Subquestion_5', 'Subquestion_6',
                      'Subquestion_7', 'Subquestion_8','Subquestion_9',
                      'Subquestion_10', 'Subquestion_11', 'Subquestion_12',
                      'Subquestion_13', 'Subquestion_14', 'Subquestion_15',
                      'Subquestion_16', 'Subquestion_17', 'Subquestion_18',
                      'Subquestion_19', 'Subquestion_20'),
               values_to='short_desc') %>%
  filter(short_desc!="") %>%
  filter(short_desc!="Overall") 

#break up name into two components

#now modify API IDs
api_final<-df_longer %>%
  separate(name, c("type", "num"), "_") %>%
  mutate(Series=case_when(
    (type=="Subquestion" & num=="1") ~ Series,
    (type=="Column" & num!="1") ~ paste(Series, substr(short_desc,1,1), sep="."),
    (type=="Subquestion" & num!="1") ~ paste(Series, num, sep="."),
    TRUE ~ Series  )) %>%
  mutate(Indicator.Name=case_when(
    (type=="Subquestion" & num=="1") ~ Indicator.Name,
    (type=="Column" & num!="1") ~ paste(Indicator.Name, short_desc, sep=" - "),
    (type=="Subquestion" & num!="1") ~ short_desc,
    TRUE ~ Indicator.Name  )) %>%
  select(-Column_1, -type, -num, -indicator_tag)

write_csv(api_final, 'GEPD_Indicators_API_Info.csv')


