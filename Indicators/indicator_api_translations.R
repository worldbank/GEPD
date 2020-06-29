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


#get metadata on indicators
#Read in list of indicators
indicator_choices <- read_delim(here::here('Indicators','indicators_choices.md'), delim="|", trim_ws=TRUE)
indicator_choices <- indicator_choices %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)


indicator_choices <- indicator_choices %>%
  select(-c('X1', 'X6')) %>%
  rename("Source Note"="How is the indicator scored?" ) 


names(indicator_choices)<-make.names(names(indicator_choices), unique=TRUE)


#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- indicators$indicator_tag



#Read in excel translations
api_converter <- function(language) {
  

  subquestions<-read_excel(paste('./Translations/Translations_Website/Indicator_Information/GEPD_Indicators_Info_v7_',language,'.xlsx',sep=""), sheet='SubQuestions') 
  
  df<-indicators %>%
    left_join(subquestions) %>%
    select(Series, indicator_tag, Indicator, Indicator.Name,  starts_with('Column_'), starts_with('Sub')) 
  
  
  
  df_overall <- df %>%
    mutate(Indicator.Name=Indicator) %>%
    select(Series, Indicator.Name ) 
    
  
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
    #select(-urban_rural_gender_name) %>%
    filter(urban_rural_gender!="") 
  
  #break up name into two components
  # (type=="Column" & num!="1") ~ paste(Series, substr(short_desc,1,1), sep="."),
  # (type=="Column" & num!="1") ~ paste(Indicator.Name, short_desc, sep=" - "),
  
  #now modify API IDs
  df_sub<-df_longer %>%
    separate(name, c("type", "num"), "_") %>%
    mutate(Series=paste(Series, num, sep="."))  %>% #add tag for subindicators
    mutate(Series=case_when( #add tag for urban/rural gender
      ( urban_rural_gender_name=="Column_2") ~ Series,
      ( urban_rural_gender_name=="Column_3") ~ paste(Series, 'R', sep="."),
      ( urban_rural_gender_name=="Column_4") ~ paste(Series, 'U', sep="."),
      ( urban_rural_gender_name=="Column_5") ~ paste(Series, 'F', sep="."),
      ( urban_rural_gender_name=="Column_6") ~ paste(Series, 'M', sep="."),
      TRUE ~ Series  )) %>%
    mutate(Indicator.Name= short_desc) %>%
    mutate(Indicator.Name=case_when( #add tag for urban/rural gender for indicator name
      (urban_rural_gender_name=="Column_2") ~ Indicator.Name,
      (urban_rural_gender_name!="Column_2") ~ paste(Indicator.Name, urban_rural_gender, sep=" - "),
      TRUE ~ Indicator.Name  )) %>%
    select(-Column_1, -type, -num, -indicator_tag, -starts_with('urban_rural_gender')) 
    
  api_final  <- df_overall %>%
    bind_rows(df_defacto_dejure) %>%
    bind_rows(df_sub) %>%
    arrange(Series) %>%
    select(Series, Indicator.Name)
  
  #add extra metadata
  api_final <- api_final %>%
    mutate(Source="Global Education Policy Dashboard",
           'Source Organization'="World Bank") %>%
    left_join(indicator_choices) %>%
    mutate(Source.Note = gsub("(\n|<br/>)"," ",Source.Note)) %>%
    mutate(Source.Note = str_replace(Source.Note, "-", ",")) %>%
    select(-c(indicator_tag, Value))
  
  
  #make some adjustments to column names if not english
  if (language=="Spanish") {
    api_final <- api_final %>%
      rename('Source_Note_ES'=Source.Note,
             'Indicator_Name_ES'=Indicator.Name,
             'Source_ES'=Source) %>%
      select(-`Source Organization`, -Source_Note_ES, - Source_ES)
    
  } else if (language=="French") {
    api_final <- api_final %>%
      rename('Source_Note_FR'=Source.Note,
             'Indicator_Name_FR'=Indicator.Name,
             'Source_FR'=Source)  %>%
      select(-`Source Organization`, -Source_Note_FR, - Source_FR)
    
  } else if (language=="Arabic") {
    api_final <- api_final %>%
      rename('Source_Note_AR'=Source.Note,
             'Indicator_Name_AR'=Indicator.Name,
             'Source_AR'=Source) %>%
      select(-`Source Organization`, -Source_Note_AR, - Source_AR)
    
  } else if (language=="Portuguese") {
    api_final <- api_final %>%
      rename('Source_Note_PT'=Source.Note,
             'Indicator_Name_PT'=Indicator.Name,
             'Source_PT'=Source) %>%
      select(-`Source Organization`, -Source_Note_PT, - Source_PT)
    
  } else {
    api_final <- api_final %>%
      rename('Source Note'=Source.Note,
             'Indicator Name'=Indicator.Name) 
  }
  #produce dataframe
    assign(paste("api_final_", language, sep=""),api_final, envir = .GlobalEnv)

    write_excel_csv(api_final, paste('./Translations/GEPD_Indicator_metadata_',language,'.csv', sep=""))
  
  
}

api_converter('English')
api_converter('French')
api_converter('Spanish')
api_converter('Arabic')
api_converter('Portuguese')
