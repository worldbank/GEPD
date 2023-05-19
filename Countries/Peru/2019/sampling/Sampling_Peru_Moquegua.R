library(SamplingStrata)
library(haven)
library(tidyr)
library(tidyverse)
library(dplyr)

set.seed(54351324)

#################################
#read in sampling frame from Peru
#################################
df=read.csv("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/Base SIAGIE_Primaria_2019 para Policy Dashboard.csv")

#make column names lower case 
colnames(df)<-tolower(colnames(df))

#create variable ID for strata and keep schools with more than 1 4th grade student
df<-df %>%
  mutate(rural=(area=="Rural")) %>%
  mutate(total_4th=x4to.boys+x4to.girls) %>%
  mutate(total_1st=x1ro.boys+x1ro.girls) %>%
  filter(total_4th>=3 & total_1st>=3 ) %>%
  filter(anexo==0)

domains<-df %>% group_by(departamento) %>% summarise(n=n())

#create variable so that sorting is done reversed
df$rev_total_4th=-df$total_4th

#produce table with randomly selected schools
sample_random<- df %>%
  filter(departamento=="MOQUEGUA") %>%
  group_by(departamento, rural) %>%
  sample_n(1, weight=total_4th )


##################################
#Select Replacement Schools
###############################

#create new datatable with new name based on sample.  This is done to just be 100% our raw sampled schools are never touched again by code.
sample_final<-sample_random %>%
  mutate(sample_dashboard=1)

#update orginal dataframe with sampled schools.
data_set_updated <- df %>%
  left_join(sample_final) %>%
  mutate(sample_dashboard=ifelse(is.na(sample_dashboard),0,sample_dashboard))

#This will be done so that 2 replacements in every province are chosen  
#get a list of provincial districts sampled

provinces <- sample_final %>%
  group_by(provincia) %>%
  summarise(n_prov=n())

province_list<-as.vector(provinces$provincia)

#produce table with randomly replacement selected schools
sample_replacement1<- data_set_updated %>%
  left_join(provinces) %>%
  filter(provincia %in% province_list) %>%
  filter(sample_dashboard==0) %>%
  group_by(provincia) %>%
  sample_n(n_prov, weight=total_4th ) %>%
  mutate(sample_replacement1=1) %>%
  select(-n_prov)

data_set_updated <- data_set_updated %>%
  left_join(sample_replacement1) %>%
  mutate(sample_replacement1=ifelse(is.na(sample_replacement1),0,sample_replacement1))


sample_replacement2<- data_set_updated %>%
  left_join(provinces) %>%
  filter(provincia %in% province_list) %>%
  filter(sample_dashboard==0 & sample_replacement1==0) %>%
  group_by(provincia) %>%
  sample_n(n_prov, weight=total_4th ) %>%
  mutate(sample_replacement2=1)

data_set_updated <- data_set_updated %>%
  left_join(sample_replacement2) %>%
  mutate(sample_replacement2=ifelse(is.na(sample_replacement2),0,sample_replacement2)) %>%
  mutate(sample=case_when(
    sample_dashboard==1  ~ 1,
    sample_replacement1==1 ~ 2,
    sample_replacement2==1 ~ 3))

#get nearest neighbor replacement for each sampled school
sample_optimal_match<-sample_optimal[,c("latitude", "longitude")]
rownames(sample_optimal_match) <- sample_optimal$codigo.modular
sample_replacement1_match<-sample_replacement1[,c("latitude", "longitude")]
rownames(sample_replacement1_match) <- sample_replacement1$codigo.modular

neighbor1<-nn2(sample_optimal_match, sample_replacement1_match, k=1)

sample_updated<-data_set_updated  %>%
  filter(!is.na(sample))

#Save sample file
currentDate<-Sys.Date()
sample_file_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/school_sample_Moquegua_",currentDate,".csv", sep="")
write.csv(sample_updated,sample_file_name)

sample_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/school_sample_Moquegua_",currentDate,".RData", sep="")
save(sample_updated, data_set_updated,
     file=sample_frame_name)   

