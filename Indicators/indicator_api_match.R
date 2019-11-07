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



#Read in Sergio's excel
subquestions<-read_excel('GEPD_Indicators_Info_v5.xlsx', sheet='SubQuestions') 

df<-indicators %>%
  left_join(subquestions) %>%
  select(Series, indicator_tag, Indicator.Name,  starts_with('Column_'), starts_with('Sub')) 



df_overall <- df %>%
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
  select(-Column_1, -type, -num, -indicator_tag, -urban_rural_gender) 
  
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
  rename('Source Note'=Source.Note,
         'Indicator Name'=Indicator.Name) %>%
  select(-c(indicator_tag, Value))


#export Indicators_metatdata section
write_excel_csv(api_final, 'GEPD_Indicators_API_Info.csv')

#Tags
practice_tags <- "SE.PRM.PROE|SE.LPV.PRIM|SE.PRM.LERN|SE.PRM.TENR|SE.PRM.EFFT|SE.PRM.CONT|SE.PRM.ATTD"


#Peru
#populate dummy data
api_dummy_p_Peru <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name)) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 100, 0.7)) %>%
  mutate(
    value_metadata=case_when(
      value <=50 ~ "Needs Improvement",
      value >50 & value<=80 ~ "Caution",
      value >80 ~ "On Target"
    ))

api_dummy_c_Peru <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(!(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name))) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 5, 0.7)) %>%
  mutate(
         value_metadata=case_when(
           value <=2 ~ "Needs Improvement",
           value >2 & value<=3 ~ "Caution",
           value >=4 ~ "On Target"
         ))

Peru_dummy_data <- api_dummy_p_Peru %>%
  bind_rows(api_dummy_c_Peru) %>%
  arrange(Series) %>%
  mutate(year=2019,
         cty_or_agg="cty",
         countrycode="PER")

#Jordan
#populate dummy data
api_dummy_p_Jordan <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name)) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 100, 0.7)) %>%
  mutate(
    value_metadata=case_when(
      value <=50 ~ "Needs Improvement",
      value >50 & value<=80 ~ "Caution",
      value >80 ~ "On Target"
    ))

api_dummy_c_Jordan <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(!(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name))) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 5, 0.7)) %>%
  mutate(
    value_metadata=case_when(
      value <=2 ~ "Needs Improvement",
      value >2 & value<=3 ~ "Caution",
      value >=4 ~ "On Target"
    ))

Jordan_dummy_data <- api_dummy_p_Jordan %>%
  bind_rows(api_dummy_c_Jordan) %>%
  arrange(Series) %>%
  mutate(year=2019,
         cty_or_agg="cty",
         countrycode="JOR")

#Rwanda
#populate dummy data
api_dummy_p_Rwanda <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name)) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 100, 0.7)) %>%
  mutate(
    value_metadata=case_when(
      value <=50 ~ "Needs Improvement",
      value >50 & value<=80 ~ "Caution",
      value >80 ~ "On Target"
    ))

api_dummy_c_Rwanda <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(!(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name))) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 5, 0.7)) %>%
  mutate(
    value_metadata=case_when(
      value <=2 ~ "Needs Improvement",
      value >2 & value<=3 ~ "Caution",
      value >=4 ~ "On Target"
    ))

Rwanda_dummy_data <- api_dummy_p_Rwanda %>%
  bind_rows(api_dummy_c_Rwanda) %>%
  arrange(Series) %>%
  mutate(year=2019,
         cty_or_agg="cty",
         countrycode="RWA")

#Pakistan - Punjab
#populate dummy data
api_dummy_p_Punjab <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name)) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 100, 0.7)) %>%
  mutate(
    value_metadata=case_when(
      value <=50 ~ "Needs Improvement",
      value >50 & value<=80 ~ "Caution",
      value >80 ~ "On Target"
    ))

api_dummy_c_Punjab <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(!(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name))) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 5, 0.7)) %>%
  mutate(
    value_metadata=case_when(
      value <=2 ~ "Needs Improvement",
      value >2 & value<=3 ~ "Caution",
      value >=4 ~ "On Target"
    ))

Punjab_dummy_data <- api_dummy_p_Punjab %>%
  bind_rows(api_dummy_c_Punjab) %>%
  arrange(Series) %>%
  mutate(year=2019,
         cty_or_agg="cty",
         countrycode="PAK")

#Ethiopia
#populate dummy data
api_dummy_p_Ethiopia <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name)) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 100, 0.7)) %>%
  mutate(
    value_metadata=case_when(
      value <=50 ~ "Needs Improvement",
      value >50 & value<=80 ~ "Caution",
      value >80 ~ "On Target"
    ))

api_dummy_c_Ethiopia <- api_final %>%
  rename(Indicator.Name='Indicator Name') %>%
  filter(!(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name))) %>%
  rename(  'Indicator Name'=Indicator.Name) %>%
  select(Series, 'Indicator Name') %>%
  mutate(value=rbinom(n(), 5, 0.7)) %>%
  mutate(
    value_metadata=case_when(
      value <=2 ~ "Needs Improvement",
      value >2 & value<=3 ~ "Caution",
      value >=4 ~ "On Target"
    ))

Ethiopia_dummy_data <- api_dummy_p_Ethiopia %>%
  bind_rows(api_dummy_c_Ethiopia) %>%
  arrange(Series) %>%
  mutate(year=2019,
         cty_or_agg="cty",
         countrycode="ETH")



country_dummy_data <- Peru_dummy_data %>%
  bind_rows(Jordan_dummy_data) %>%
  bind_rows(Rwanda_dummy_data) %>%
  bind_rows(Ethiopia_dummy_data) 

write_excel_csv(country_dummy_data, 'GEPD_Indicators_dummy.csv')
