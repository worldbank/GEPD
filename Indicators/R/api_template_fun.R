# Load libraries
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(skimr)
library(naniar)
library(vtable)
library(readxl)
library(readr)
library(WDI)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

api_template_fun <- function(variables) {
  
  
  
  ############################
  #Read in indicators.md file
  ###########################
  #Read in list of indicators
  if (Sys.getenv("USERNAME") == "wb577189"){
    
    indicators <- read.csv('C:/Users/wb577189/OneDrive - WBG/Documents/GitHub/GEPD/Indicators/GEPD_Indicators_Info.csv')
    indicators <- indicators %>%
      filter(Series!="---") %>%
      separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE) %>% 
      rename(`Indicator.Name`= Indicator)
    }else {
    
    indicators <- read_csv(here::here('Indicators','indicators.csv'))
    indicators <- indicators %>%
      filter(Series!="---") %>%
      separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE) 
    
  }

  
  
  
  indicator_names <-  indicators$indicator_tag
  indicator_names <- sapply(indicator_names, tolower)
  
  names(indicators)<-make.names(names(indicators), unique=TRUE)
  
  
  #get metadata on indicators
  #Read in list of indicators
  if (Sys.getenv("USERNAME") == "wb577189"){
    
    indicator_choices <- read_delim('C:/Users/wb577189/OneDrive - WBG/Documents/GitHub/GEPD/Indicators/indicators_choices.md', delim="|", trim_ws=TRUE)
  }else {
  indicator_choices <- read_delim(here::here('Indicators','indicators_choices.md'), delim="|", trim_ws=TRUE)
  }
  indicator_choices <- indicator_choices %>%
    filter(Series!="---") %>%
    separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)
  
  
  indicator_choices <- indicator_choices %>%
    select(-c('...1', '...6')) %>%
    rename("Source Note"="How is the indicator scored?" ) 
  
  #get metadata on indicators
  #Read in list of indicators
  if (Sys.getenv("USERNAME") == "wb577189"){
    
    indicator_choices <- read_delim('C:/Users/wb577189/OneDrive - WBG/Documents/GitHub/GEPD/Indicators/indicators_choices.md', delim="|", trim_ws=TRUE)
  }else {
    indicator_choices <- read_delim(here::here('Indicators','indicators_choices.md'), delim="|", trim_ws=TRUE)
  }
  
  indicator_choices <- indicator_choices %>%
    filter(Series!="---") %>%
    separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)
  
  
  indicator_choices <- indicator_choices %>%
    select(-c('...1', '...6')) %>%
    rename("Source Note"="How is the indicator scored?" ) 
  
  
  names(indicator_choices)<-make.names(names(indicator_choices), unique=TRUE)
  
  
  #Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
  indicator_names <- indicators$indicator_tag
  
  
  
  #Read in Sergio's excel with subquestions to include
  if (Sys.getenv("USERNAME") == "wb577189"){
    
    subquestions <- read_excel('C:/Users/wb577189/OneDrive - WBG/Documents/GitHub/GEPD/Indicators/GEPD_Indicators_Info_v5.xlsx', sheet='SubQuestions')
  }else {
    subquestions<-read_excel(here::here('Indicators','GEPD_Indicators_Info_v5.xlsx'), sheet='SubQuestions') 
  }
  
  df<-indicators %>%
    left_join(subquestions) %>%
    select(Series, indicator_tag, Indicator.Name,  starts_with('Column_'), starts_with('Sub')) 
  
  
  
  df_overall <- df %>%
    select(Series, Indicator.Name, indicator_tag ) 
  
  
  df_defacto_dejure <- df %>%
    filter(grepl("Policy Lever", Indicator.Name )) %>%
    select(Series, Indicator.Name ) %>%
    mutate('De Facto' = "DF",
           'De Jure' = "DJ") %>%
    pivot_longer(cols=c('De Facto', 'De Jure'),
                 names_to="type",
                 values_to="type_val") %>%
    mutate(Series=paste(Series, type_val, sep="."),
           Indicator.Name=paste("(",type,") ",Indicator.Name, sep="")) %>%
    select(Series, Indicator.Name)
  
  # Prepare a data frame, on which we can add values
  
  ##########################
  # Now create database for upload with correct format for EdStats
  ###########################
  #Pivot longer
  df_longer<-df %>%
    pivot_longer(cols=c(
      'Subquestion_1', 'Subquestion_2', 'Subquestion_3',
      'Subquestion_4', 'Subquestion_5', 'Subquestion_6',
      'Subquestion_7', 'Subquestion_8','Subquestion_9',
      'Subquestion_10', 'Subquestion_11', 'Subquestion_12',
      'Subquestion_13', 'Subquestion_14', 'Subquestion_15',
      'Subquestion_16', 'Subquestion_17', 'Subquestion_18',
      'Subquestion_19', 'Subquestion_20'),
      values_to='short_desc') %>%
    filter(short_desc!="") %>%
    filter(short_desc!="Overall") %>%
    pivot_longer(cols=c(    "Column_2", "Column_3", "Column_4","Column_5", "Column_6"),
                 values_to='urban_rural_gender',
                 names_to = 'urban_rural_gender_name')  %>%
    select(-urban_rural_gender_name) %>%
    filter(urban_rural_gender!="") 
  
  #break up name into two components
  # (type=="Column" & num!="1") ~ paste(Series, substr(short_desc,1,1), sep="."),
  # (type=="Column" & num!="1") ~ paste(Indicator.Name, short_desc, sep=" - "),
  
  #now modify API IDs
  df_sub<-df_longer %>%
    separate(name, c("type", "num"), "_") %>%
    mutate(Series=paste(Series, num, sep="."))  %>% #add tag for subindicators
    mutate(Series=case_when( #add tag for urban/rural gender
      ( urban_rural_gender=="Overall") ~ Series,
      ( urban_rural_gender!="Overall") ~ paste(Series, substr(urban_rural_gender,1,1), sep="."),
      TRUE ~ Series  )) %>%
    mutate(Indicator.Name= short_desc) %>%
    mutate(Indicator.Name=case_when( #add tag for urban/rural gender for indicator name
      (urban_rural_gender=="Overall") ~ Indicator.Name,
      (urban_rural_gender!="Overall") ~ paste(Indicator.Name, urban_rural_gender, sep=" - "),
      TRUE ~ Indicator.Name  )) %>%
    select(-Column_1, -type, -num, -urban_rural_gender) 
  
  api_template  <- df_overall %>%
    bind_rows(df_defacto_dejure) %>%
    bind_rows(df_sub) %>%
    arrange(Series) %>%
    select(Series, Indicator.Name)
  
  indicator_match  <- df_overall %>%
    bind_rows(df_defacto_dejure) %>%
    bind_rows(df_sub) %>%
    arrange(Series) %>%
    select(Series, Indicator.Name, indicator_tag)
  
  #add extra metadata
  api_template <- api_template %>%
    mutate(Source="Global Education Policy Dashboard",
           'Source Organization'="World Bank") %>%
    left_join(indicator_choices) %>%
    mutate(Source.Note = gsub("(\n|<br/>)"," ",Source.Note)) %>%
    mutate(Source.Note = str_replace(Source.Note, "-", ",")) %>%
    rename('Source Note'=Source.Note,
           'Indicator Name'=Indicator.Name) %>%
    select(-c(indicator_tag, Value))
  
  api_template
}