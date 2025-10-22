# Load libraries
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(skimr)
library(readxl)
library(readr)
library(WDI)
library(srvyr)
library(here)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#specify path to data
data_dir <- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/CNT/PER/PER_2019_GEPD/PER_2019_GEPD_v01_M/Data/"



#read in databases for indicators
load(paste(data_dir, "School/school_indicators_data_anon.RData", sep="/"))
load(paste(data_dir, "Public_Officials/public_officials_indicators_data_anon.RData", sep="/"))

strata <- c('STRATUM')

options(survey.lonely.psu="adjust")


#Create a function which will generate new binary variable using case_when, but 
#if value is misisng it will generate binary variable to be missing
#This is done a lot so will create function for it.
#e.g. school_absent=case_when(
#         m2sbq6_efft==6  ~ 1,
#         m2sbq6_efft!=6   ~ 0,
#         is.na(m2sbq6_efft) ~ as.numeric(NA))


bin_var <- function(var, val) {
  case_when(
    var==val  ~ 1,
    var!=val   ~ 0,
    is.na(var) ~ as.numeric(NA))
}


bin_var_NA0 <- function(var, val) {
  case_when(
    var==val  ~ 1,
    var!=val   ~ 0,
    is.na(var) ~ 0)
}

#create function to extract mean and sd from survey data
indicator_stats <- function(name, indicator, dataset, tag,  unit) {

  name <- str_trim(name)
  indicator <- str_trim(indicator)
  
  if (dataset=='school') {
    
    
    if (unit=="All") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep=""))
      
      
    } else if (unit=="Female") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_F", "_anon", sep=""))
      
    } else if (unit=="Male") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_M", "_anon", sep=""))
      
    } else if (unit=="Rural") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(rural==TRUE)
      
    } else if (unit=="Urban") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(rural==FALSE)
      
    }

  stat_df %>%
    #create column named indicator that evaluates expression in indicator argument
    mutate(
      VALUE=eval(parse(text=indicator))
    ) %>%
    filter(!is.na(ipw)) %>%
    filter(!is.infinite(ipw)) %>%
    select(VALUE, one_of(strata), ipw ) %>%
    pivot_longer(cols='VALUE',
                 names_to = 'indicators',
                 values_to='value') %>%
    as_survey_design(strata=strata,
                     weight=ipw) %>%
    ungroup() %>%
    summarise(mean=survey_mean(value, na.rm=T, vartype=c('se', 'ci','var')),
              N=sum(!is.na(value))) %>%
    as_tibble() %>%
    mutate(Series=name) %>%
    select(Series, everything())
  
    
  } else if (dataset== 'public_officials') {
    
    if (unit=="All") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep=""))
      
      
    } else if (unit=="central") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(govt_tier=="Ministry of Education (or equivalent)")
      
    } else if (unit=="regional") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(rural=='Regional office (or equivalent)')
      
    }
    else if (unit=="district") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(rural=='District office (or equivalent)')
      
    }

  stat_df %>%
    #create column named indicator that evaluates expression in indicator argument
    mutate(
      VALUE=eval(parse(text=indicator)),
      ipw=1
    ) %>%
    select(VALUE, ipw) %>%
    pivot_longer(cols='VALUE',
                 names_to = 'indicators',
                 values_to='value') %>%
    as_survey_design(
                     weight=ipw) %>%
    ungroup() %>%
    summarise(mean=survey_mean(value, na.rm=T, vartype=c('se', 'ci','var'))) %>%
    as_tibble() %>%
    mutate(Series=name) %>%
    select(Series, everything())
  }
  

}


#######################################
# Proficiency on GEPD Assessment	(LERN)
#######################################

#api_final[grep('LERN', api_final$Series),1]

indicators <-   list(
    c("SE.PRM.LERN",'student_proficient', "school", "LERN",  "All"),
    c("SE.PRM.LERN.1",'student_proficient', "school", "LERN",  "All"),
    c("SE.PRM.LERN.1.F",'student_proficient', "school", "LERN",  "Female"),
    c("SE.PRM.LERN.1.M",'student_proficient', "school", "LERN",  "Male"),
    c("SE.PRM.LERN.1.R",'student_proficient', "school", "LERN",  "Rural"),
    c("SE.PRM.LERN.1.U",'student_proficient', "school", "LERN",  "Urban"),
    c("SE.PRM.LERN.2",'literacy_student_proficient', "school", "LERN",  "All"),  
    c("SE.PRM.LERN.2.F",'literacy_student_proficient', "school", "LERN",  "Female"),
    c("SE.PRM.LERN.2.M",'literacy_student_proficient', "school", "LERN",  "Male"),
    c("SE.PRM.LERN.2.R",'literacy_student_proficient', "school", "LERN",  "Rural"),
    c("SE.PRM.LERN.2.U",'literacy_student_proficient', "school", "LERN",  "Urban"),
    c("SE.PRM.LERN.3",'math_student_proficient', "school", "LERN",  "All"),
    c("SE.PRM.LERN.3.F",'math_student_proficient', "school", "LERN",  "Female"),
    c("SE.PRM.LERN.3.M",'math_student_proficient', "school", "LERN",  "Male"),
    c("SE.PRM.LERN.3.R",'math_student_proficient', "school", "LERN",  "Rural"),
    c("SE.PRM.LERN.3.U",'math_student_proficient', "school", "LERN",  "Urban"),

#######################################
  # Teacher Effort		(EFFT)
  #######################################
  

      c("SE.PRM.EFFT"     ,"100-sch_absence_rate", "school", "EFFT",  "All"),
      c("SE.PRM.EFFT.1"   ,"100-absence_rate", "school", "EFFT",  "All"),
      c("SE.PRM.EFFT.1.F" ,"100-absence_rate", "school", "EFFT",  "Female"),
      c("SE.PRM.EFFT.1.M" ,"100-absence_rate", "school", "EFFT",  "Male"),
      c("SE.PRM.EFFT.1.R" ,"100-absence_rate", "school", "EFFT",  "Rural"),
      c("SE.PRM.EFFT.1.U" ,"100-absence_rate", "school", "EFFT",  "Urban"),
      c("SE.PRM.EFFT.2"   ,"100-sch_absence_rate", "school", "EFFT",  "All"),  
      c("SE.PRM.EFFT.2.F" ,"100-sch_absence_rate", "school", "EFFT",  "Female"),
      c("SE.PRM.EFFT.2.M" ,"100-sch_absence_rate", "school", "EFFT",  "Male"),
      c("SE.PRM.EFFT.2.R" ,"100-sch_absence_rate", "school", "EFFT",  "Rural"),
      c("SE.PRM.EFFT.2.U" ,"100-sch_absence_rate", "school", "EFFT",  "Urban"),


  
  
  #######################################
  # 	Teacher Content Knowledge	(CONT)
  #######################################
  

      c("SE.PRM.CONT    ", "content_proficiency", "school", "CONT",  "All"),
      c("SE.PRM.CONT.1  ", "content_proficiency", "school", "CONT",  "All"),
      c("SE.PRM.CONT.1.F", "content_proficiency", "school", "CONT",  "Female"),
      c("SE.PRM.CONT.1.M", "content_proficiency", "school", "CONT",  "Male"),
      c("SE.PRM.CONT.1.R", "content_proficiency", "school", "CONT",  "Rural"),
      c("SE.PRM.CONT.1.U", "content_proficiency", "school", "CONT",  "Urban"),
      c("SE.PRM.CONT.2  ", "literacy_content_proficiency", "school", "CONT",  "All"),  
      c("SE.PRM.CONT.2.F", "literacy_content_proficiency", "school", "CONT",  "Female"),
      c("SE.PRM.CONT.2.M", "literacy_content_proficiency", "school", "CONT",  "Male"),
      c("SE.PRM.CONT.2.R", "literacy_content_proficiency", "school", "CONT",  "Rural"),
      c("SE.PRM.CONT.2.U", "literacy_content_proficiency", "school", "CONT",  "Urban"),
      c("SE.PRM.CONT.3  ", "math_content_proficiency", "school", "CONT",  "All"),
      c("SE.PRM.CONT.3.F", "math_content_proficiency", "school", "CONT",  "Female"),
      c("SE.PRM.CONT.3.M", "math_content_proficiency", "school", "CONT",  "Male"),
      c("SE.PRM.CONT.3.R", "math_content_proficiency", "school", "CONT",  "Rural"),
      c("SE.PRM.CONT.3.U", "math_content_proficiency", "school", "CONT",  "Urban"),

  
  #######################################
  # Teacher Pedagogical Skills	(PEDG)
  #######################################
  

      c("SE.PRM.PEDG     ","100*as.numeric(teach_score>=3)", "school", "PEDG",  "All"),
      c("SE.PRM.PEDG.1   ","100*as.numeric(teach_score>=3)", "school", "PEDG",  "All"),
      #SE.PRM.PEDG.1.F", "100*teach_score>=3", "school", "PEDG",  "Female"),
      #SE.PRM.PEDG.1.M", "100*teach_score>=3", "school", "PEDG",  "Male"),
      c("SE.PRM.PEDG.1.R ","100*as.numeric(teach_score>=3)", "school", "PEDG",  "Rural"),
      c("SE.PRM.PEDG.1.U ","100*as.numeric(teach_score>=3)", "school", "PEDG",  "Urban"),
      c("SE.PRM.PEDG.2   ","100*as.numeric(classroom_culture>=3)", "school", "PEDG",  "All"),  
      #SE.PRM.PEDG.2.F", "100*classroom_culture>=3", "school", "PEDG",  "Female"),
      #SE.PRM.PEDG.2.M", "100*classroom_culture>=3", "school", "PEDG",  "Male"),
      c("SE.PRM.PEDG.2.R ","100*as.numeric(classroom_culture>=3)", "school", "PEDG",  "Rural"),
      c("SE.PRM.PEDG.2.U ","100*as.numeric(classroom_culture>=3)", "school", "PEDG",  "Urban"),
      c("SE.PRM.PEDG.3   ","100*as.numeric(instruction>=3)", "school", "PEDG",  "All"),
      #SE.PRM.PEDG.3.F", "100*instruction>=3", "school", "PEDG",  "Female"),
      #SE.PRM.PEDG.3.M", "100*instruction>=3", "school", "PEDG",  "Male"),
      c("SE.PRM.PEDG.3.R ","100*as.numeric(instruction>=3)", "school", "PEDG",  "Rural"),
      c("SE.PRM.PEDG.3.U ","100*as.numeric(instruction>=3)", "school", "PEDG",  "Urban"),
      c("SE.PRM.PEDG.4   ","100*as.numeric(socio_emotional_skills>=3)", "school", "PEDG",  "All"),
      #SE.PRM.PEDG.4.F", "100*socio_emotional_skills>=3", "school", "PEDG",  "Female"),
      #SE.PRM.PEDG.4.M", "100*socio_emotional_skills>=3", "school", "PEDG",  "Male"),
      c("SE.PRM.PEDG.4.R ","100*as.numeric(socio_emotional_skills>=3)", "school", "PEDG",  "Rural"),
      c("SE.PRM.PEDG.4.U ","100*as.numeric(socio_emotional_skills>=3)", "school", "PEDG",  "Urban"),

  
  #######################################
  # 	Basic Inputs	(INPT)
  #######################################
  

      #(De Facto) Average number of classroom inputs in classrooms	
      c("SE.PRM.INPT     ","inputs", "school", "INPT",  "All"),
      c("SE.PRM.INPT.1   ","inputs", "school", "INPT",  "All"),
      c("SE.PRM.INPT.1.R ","inputs", "school", "INPT",  "Rural"),
      c("SE.PRM.INPT.1.U ","inputs", "school", "INPT",  "Urban"),
      c("SE.PRM.INPT.3   ","33*textbooks+ 67*pens_etc", "school", "INPT",  "All") ,
      c("SE.PRM.INPT.3.R ","33*textbooks+ 67*pens_etc", "school", "INPT",  "Rural") ,
      c("SE.PRM.INPT.3.U ","33*textbooks + 67*pens_etc", "school", "INPT",  "Urban"),
      c("SE.PRM.INPT.2.R ","100*blackboard_functional", "school", "INPT",  "Rural"),
      c("SE.PRM.INPT.2.U ","100*blackboard_functional", "school", "INPT",  "Urban"),
      c("SE.PRM.INPT.4   ","100*share_desk", "school", "INPT",  "All"),
      c("SE.PRM.INPT.4.R ","100*share_desk", "school", "INPT",  "Rural"),
      c("SE.PRM.INPT.4.U ","100*share_desk", "school", "INPT",  "Urban"),
      c("SE.PRM.INPT.5   ","100*access_ict", "school", "INPT",  "All"),
      c("SE.PRM.INPT.5.R ","100*access_ict", "school", "INPT",  "Rural"),
      c("SE.PRM.INPT.5.U ","100*access_ict", "school", "INPT",  "Urban"),

  
  
  #######################################
  # 	Basic Infrastructure	(INFR)
  #######################################
  

      #(De Facto) Average number of infrastructure aspects present in schools	
      c("SE.PRM.INFR     ","infrastructure	", "school", "INFR",  "All"),
      c("SE.PRM.INFR.1   ","infrastructure	", "school", "INFR",  "All"),
      c("SE.PRM.INFR.1.R ","infrastructure	", "school", "INFR",  "Rural"),
      c("SE.PRM.INFR.1.U ","infrastructure	", "school", "INFR",  "Urban"),
      #(De Facto) Perc","nt of schools with drinking water	
      c("SE.PRM.INFR.2   ","100*drinking_water	", "school", "INFR",  "All"),
      c("SE.PRM.INFR.2.R ","100*drinking_water	", "school", "INFR",  "Rural"),
      c("SE.PRM.INFR.2.U ","100*drinking_water	", "school", "INFR",  "Urban"),
      #(De Facto) Perc","nt of schools with functioning toilets
      c("SE.PRM.INFR.3   ","100*functioning_toilet	", "school", "INFR",  "All"),
      c("SE.PRM.INFR.3.R ","100*functioning_toilet	", "school", "INFR",  "Rural"),
      c("SE.PRM.INFR.3.U ","100*functioning_toilet	", "school", "INFR",  "Urban"),
      #(De Facto) Perc","nt of schools with access to electricity	
      c("SE.PRM.INFR.4   ","100*class_electricity", "school", "INFR",  "All"),
      c("SE.PRM.INFR.4.R ","100*class_electricity", "school", "INFR",  "Rural"),
      c("SE.PRM.INFR.4.U ","100*class_electricity", "school", "INFR",  "Urban"),
      #(De Facto) Perc","nt of schools with access to internet	
      c("SE.PRM.INFR.5   ","100*internet", "school", "INFR",  "All"),
      c("SE.PRM.INFR.5.R ","100*internet", "school", "INFR",  "Rural"),
      c("SE.PRM.INFR.5.U ","100*internet", "school", "INFR",  "Urban"),
      #	(De Facto) Per","ent of schools accessible to children with special needs	
      c("SE.PRM.INFR.6   ","100*disability_accessibility", "school", "INFR",  "All"),
      c("SE.PRM.INFR.6.R ","100*disability_accessibility", "school", "INFR",  "Rural"),
      c("SE.PRM.INFR.6.U ","100*disability_accessibility", "school", "INFR",  "Urban"),

  
  #######################################
  # Learning Capacity	(LCAP)
  #######################################
  
  #api_final[grep('LERN', api_final$Series),1]
  

      c("SE.PRM.LCAP    ", "ecd_student_proficiency	", "school", "LCAP",  "All"),
      c("SE.PRM.LCAP.1  ", "ecd_student_knowledge	", "school", "LCAP",  "All"),
      c("SE.PRM.LCAP.1.F", "ecd_student_knowledge	", "school", "LCAP",  "Female"),
      c("SE.PRM.LCAP.1.M", "ecd_student_knowledge	", "school", "LCAP",  "Male"),
      c("SE.PRM.LCAP.1.R", "ecd_student_knowledge	", "school", "LCAP",  "Rural"),
      c("SE.PRM.LCAP.1.U", "ecd_student_knowledge	", "school", "LCAP",  "Urban"),
      c("SE.PRM.LCAP.2  ", "ecd_math_student_knowledge", "school", "LCAP",  "All"),  
      c("SE.PRM.LCAP.2.F", "ecd_math_student_knowledge", "school", "LCAP",  "Female"),
      c("SE.PRM.LCAP.2.M", "ecd_math_student_knowledge", "school", "LCAP",  "Male"),
      c("SE.PRM.LCAP.2.R", "ecd_math_student_knowledge", "school", "LCAP",  "Rural"),
      c("SE.PRM.LCAP.2.U", "ecd_math_student_knowledge", "school", "LCAP",  "Urban"),
      c("SE.PRM.LCAP.3  ", "ecd_literacy_student_knowledge", "school", "LCAP",  "All"),
      c("SE.PRM.LCAP.3.F", "ecd_literacy_student_knowledge", "school", "LCAP",  "Female"),
      c("SE.PRM.LCAP.3.M", "ecd_literacy_student_knowledge", "school", "LCAP",  "Male"),
      c("SE.PRM.LCAP.3.R", "ecd_literacy_student_knowledge", "school", "LCAP",  "Rural"),
      c("SE.PRM.LCAP.3.U", "ecd_literacy_student_knowledge", "school", "LCAP",  "Urban"),
      c("SE.PRM.LCAP.4  ", "ecd_exec_student_knowledge", "school", "LCAP",  "All"),
      c("SE.PRM.LCAP.4.F", "ecd_exec_student_knowledge", "school", "LCAP",  "Female"),
      c("SE.PRM.LCAP.4.M", "ecd_exec_student_knowledge", "school", "LCAP",  "Male"),
      c("SE.PRM.LCAP.4.R", "ecd_exec_student_knowledge", "school", "LCAP",  "Rural"),
      c("SE.PRM.LCAP.4.U", "ecd_exec_student_knowledge", "school", "LCAP",  "Urban"),
      c("SE.PRM.LCAP.5  ", "ecd_soc_student_knowledge", "school", "LCAP",  "All"),
      c("SE.PRM.LCAP.5.F", "ecd_soc_student_knowledge", "school", "LCAP",  "Female"),
      c("SE.PRM.LCAP.5.M", "ecd_soc_student_knowledge", "school", "LCAP",  "Male"),
      c("SE.PRM.LCAP.5.R", "ecd_soc_student_knowledge", "school", "LCAP",  "Rural"),
      c("SE.PRM.LCAP.5.U", "ecd_soc_student_knowledge", "school", "LCAP",  "Urban"),

  
  #######################################
  # Student Attendance	(ATTD)
  #######################################
  

      c("SE.PRM.ATTD    ", "student_attendance	", "school", "ATTD",  "All"),
      c("SE.PRM.ATTD.1  ", "student_attendance	", "school", "ATTD",  "All"),
      c("SE.PRM.ATTD.1.F", "student_attendance	", "school", "ATTD",  "Female"),
      c("SE.PRM.ATTD.1.M", "student_attendance	", "school", "ATTD",  "Male"),
      c("SE.PRM.ATTD.1.R", "student_attendance	", "school", "ATTD",  "Rural"),
      c("SE.PRM.ATTD.1.U", "student_attendance	", "school", "ATTD",  "Urban"),


  #######################################
  # Operactional Management (OPMN)
  #######################################
  

      c("SE.PRM.OPMN", "operational_management	", "school", "OPMN",  "All"),
      #(De Facto) Average score for the presence and quality of core operational management functions	
      c("SE.PRM.OPMN.1  ", "operational_management	", "school", "OPMN",  "All"),
      c("SE.PRM.OPMN.1.F", "operational_management	", "school", "OPMN",  "Female"),
      c("SE.PRM.OPMN.1.M", "operational_management	", "school", "OPMN",  "Male"),
      c("SE.PRM.OPMN.1.R", "operational_management	", "school", "OPMN",  "Rural"),
      c("SE.PRM.OPMN.1.U", "operational_management	", "school", "OPMN",  "Urban"),
      #(De Facto) Average score for infrastructure repair/maintenance	
      c("SE.PRM.OPMN.2  ", "1+2*vignette_1", "school", "OPMN",  "All"),  
      c("SE.PRM.OPMN.2.F", "1+2*vignette_1", "school", "OPMN",  "Female"),
      c("SE.PRM.OPMN.2.M", "1+2*vignette_1", "school", "OPMN",  "Male"),
      c("SE.PRM.OPMN.2.R", "1+2*vignette_1", "school", "OPMN",  "Rural"),
      c("SE.PRM.OPMN.2.U", "1+2*vignette_1", "school", "OPMN",  "Urban"),
      #(De Facto) Ave,rage score for ensuring  availability of school inputs	
      c("SE.PRM.OPMN.3  ", "1+2*vignette_2", "school", "OPMN",  "All"),
      c("SE.PRM.OPMN.3.F", "1+2*vignette_2", "school", "OPMN",  "Female"),
      c("SE.PRM.OPMN.3.M", "1+2*vignette_2", "school", "OPMN",  "Male"),
      c("SE.PRM.OPMN.3.R", "1+2*vignette_2", "school", "OPMN",  "Rural"),
      c("SE.PRM.OPMN.3.U", "1+2*vignette_2", "school", "OPMN",  "Urban"),


  
  
  #######################################
  # Instructional Leadership	(ILDR)
  #######################################
  
  

      c("SE.PRM.ILDR     ","instructional_leadership		", "school", "ILDR",  "All"),
      #(De Facto) Aver,age score for the presence and quality of instructional leadership	
      c("SE.PRM.ILDR.1   ","instructional_leadership		", "school", "ILDR",  "All"),
      c("SE.PRM.ILDR.1.F ","instructional_leadership		", "school", "ILDR",  "Female"),
      c("SE.PRM.ILDR.1.M ","instructional_leadership		", "school", "ILDR",  "Male"),
      c("SE.PRM.ILDR.1.R ","instructional_leadership		", "school", "ILDR",  "Rural"),
      c("SE.PRM.ILDR.1.U ","instructional_leadership		", "school", "ILDR",  "Urban"),
      #(De Facto) Perc,ent of teachers reporting having had their class observed	
      c("SE.PRM.ILDR.2   ","100*classroom_observed", "school", "ILDR",  "All"),  
      c("SE.PRM.ILDR.2.F ","100*classroom_observed", "school", "ILDR",  "Female"),
      c("SE.PRM.ILDR.2.M ","100*classroom_observed", "school", "ILDR",  "Male"),
      c("SE.PRM.ILDR.2.R ","100*classroom_observed", "school", "ILDR",  "Rural"),
      c("SE.PRM.ILDR.2.U ","100*classroom_observed", "school", "ILDR",  "Urban"),
      #(De Facto) Perc,ent of teachers reporting that the classroom observation happened recently
      c("SE.PRM.ILDR.3   ","100*classroom_observed_recent", "school", "ILDR",  "All"),
      c("SE.PRM.ILDR.3.F ","100*classroom_observed_recent", "school", "ILDR",  "Female"),
      c("SE.PRM.ILDR.3.M ","100*classroom_observed_recent", "school", "ILDR",  "Male"),
      c("SE.PRM.ILDR.3.R ","100*classroom_observed_recent", "school", "ILDR",  "Rural"),
      c("SE.PRM.ILDR.3.U ","100*classroom_observed_recent", "school", "ILDR",  "Urban"),
      #(De Facto) Perc,ent of teachers reporting having discussed the results of the classroom observation	
      c("SE.PRM.ILDR.4   ","100*discussed_observation", "school", "ILDR",  "All"),  
      c("SE.PRM.ILDR.4.F ","100*discussed_observation", "school", "ILDR",  "Female"),
      c("SE.PRM.ILDR.4.M ","100*discussed_observation", "school", "ILDR",  "Male"),
      c("SE.PRM.ILDR.4.R ","100*discussed_observation", "school", "ILDR",  "Rural"),
      c("SE.PRM.ILDR.4.U ","100*discussed_observation", "school", "ILDR",  "Urban"),
      #(De Facto) Perc,ent of teachers reporting that the discussion was over 30 minutes	
      c("SE.PRM.ILDR.5   ","100*discussion_30_min", "school", "ILDR",  "All"),  
      c("SE.PRM.ILDR.5.F ","100*discussion_30_min", "school", "ILDR",  "Female"),
      c("SE.PRM.ILDR.5.M ","100*discussion_30_min", "school", "ILDR",  "Male"),
      c("SE.PRM.ILDR.5.R ","100*discussion_30_min", "school", "ILDR",  "Rural"),
      c("SE.PRM.ILDR.5.U ","100*discussion_30_min", "school", "ILDR",  "Urban"),
      #(De Facto) Perc,ent of teachers reporting that they were provided with feedback in that discussion	
      c("SE.PRM.ILDR.6   ","100*feedback_observation", "school", "ILDR",  "All"),  
      c("SE.PRM.ILDR.6.F ","100*feedback_observation", "school", "ILDR",  "Female"),
      c("SE.PRM.ILDR.6.M ","100*feedback_observation", "school", "ILDR",  "Male"),
      c("SE.PRM.ILDR.6.R ","100*feedback_observation", "school", "ILDR",  "Rural"),
      c("SE.PRM.ILDR.6.U ","100*feedback_observation", "school", "ILDR",  "Urban"),
      #(De Facto) Perc,ent of teachers reporting having lesson plans	
      c("SE.PRM.ILDR.7   ","100-100*lesson_plan", "school", "ILDR",  "All"),  
      c("SE.PRM.ILDR.7.F ","100-100*lesson_plan", "school", "ILDR",  "Female"),
      c("SE.PRM.ILDR.7.M ","100-100*lesson_plan", "school", "ILDR",  "Male"),
      c("SE.PRM.ILDR.7.R ","100-100*lesson_plan", "school", "ILDR",  "Rural"),
      c("SE.PRM.ILDR.7.U ","100-100*lesson_plan", "school", "ILDR",  "Urban"),
      #(De Facto) Perc,ent of teachers reporting that they had discussed their lesson plans with someone else (pricinpal, pedagogical coordinator, another teacher)	
      c("SE.PRM.ILDR.8   ","100*m3sdq24_ildr", "school", "ILDR",  "All"),  
      c("SE.PRM.ILDR.8.F ","100*m3sdq24_ildr", "school", "ILDR",  "Female"),
      c("SE.PRM.ILDR.8.M ","100*m3sdq24_ildr", "school", "ILDR",  "Male"),
      c("SE.PRM.ILDR.8.R ","100*m3sdq24_ildr", "school", "ILDR",  "Rural"),
      c("SE.PRM.ILDR.8.U ","100*m3sdq24_ildr", "school", "ILDR",  "Urban"),
      

  #######################################
  # Principal School Knowledge	(PKNW)
  #######################################

      c("SE.PRM.PKNW    ", "principal_knowledge_score		", "school", "PKNW",  "All"),
      #(De Facto) Ave,rage score for the extent to which principals are familiar with certain key aspects of the day-to-day workings of the school		
      c("SE.PRM.PKNW.1  ", "principal_knowledge_score		", "school", "PKNW",  "All"),
      c("SE.PRM.PKNW.1.F", "principal_knowledge_score		", "school", "PKNW",  "Female"),
      c("SE.PRM.PKNW.1.M", "principal_knowledge_score		", "school", "PKNW",  "Male"),
      c("SE.PRM.PKNW.1.R", "principal_knowledge_score		", "school", "PKNW",  "Rural"),
      c("SE.PRM.PKNW.1.U", "principal_knowledge_score		", "school", "PKNW",  "Urban"),
      #(De Facto) Per,cent of principals familiar with teachers' content knowledge	
      c("SE.PRM.PKNW.2  ", "100*(add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3", "school", "PKNW",  "All"),  
      c("SE.PRM.PKNW.2.F", "100*(add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3", "school", "PKNW",  "Female"),
      c("SE.PRM.PKNW.2.M", "100*(add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3", "school", "PKNW",  "Male"),
      c("SE.PRM.PKNW.2.R", "100*(add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3", "school", "PKNW",  "Rural"),
      c("SE.PRM.PKNW.2.U", "100*(add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3", "school", "PKNW",  "Urban"),
      #(De Facto) Per,cent of principals familiar with teachers' experience	
      c("SE.PRM.PKNW.3  ", "100*experience_pknw", "school", "PKNW",  "All"),
      c("SE.PRM.PKNW.3.F", "100*experience_pknw", "school", "PKNW",  "Female"),
      c("SE.PRM.PKNW.3.M", "100*experience_pknw", "school", "PKNW",  "Male"),
      c("SE.PRM.PKNW.3.R", "100*experience_pknw", "school", "PKNW",  "Rural"),
      c("SE.PRM.PKNW.3.U", "100*experience_pknw", "school", "PKNW",  "Urban"),
      #(De Facto) Per,cent of principals familiar with availability of classroom inputs	
      c("SE.PRM.PKNW.4  ", "100*(textbooks_pknw+blackboard_pknw)/2", "school", "PKNW",  "All"),  
      c("SE.PRM.PKNW.4.F", "100*(textbooks_pknw+blackboard_pknw)/2", "school", "PKNW",  "Female"),
      c("SE.PRM.PKNW.4.M", "100*(textbooks_pknw+blackboard_pknw)/2", "school", "PKNW",  "Male"),
      c("SE.PRM.PKNW.4.R", "100*(textbooks_pknw+blackboard_pknw)/2", "school", "PKNW",  "Rural"),
      c("SE.PRM.PKNW.4.U", "100*(textbooks_pknw+blackboard_pknw)/2", "school", "PKNW",  "Urban"),

  #######################################
  # Principal Management Skills	(PMAN)
  #######################################

      c("SE.PRM.PMAN    ", "principal_management		", "school", "PMAN",  "All"),
      #(De Facto) Ave,rage score for the extent to which principals master two key managerial skills - problem-solving in the short-term, and goal-setting in the long term	
      c("SE.PRM.PMAN.1  ", "principal_management		", "school", "PMAN",  "All"),
      c("SE.PRM.PMAN.1.F", "principal_management		", "school", "PMAN",  "Female"),
      c("SE.PRM.PMAN.1.M", "principal_management		", "school", "PMAN",  "Male"),
      c("SE.PRM.PMAN.1.R", "principal_management		", "school", "PMAN",  "Rural"),
      c("SE.PRM.PMAN.1.U", "principal_management		", "school", "PMAN",  "Urban"),
      #(De Facto) Ave,rage score for the extent to which principals master problem-solving in the short-term	
      c("SE.PRM.PMAN.2  ", "goal_setting", "school", "PMAN",  "All"),  
      c("SE.PRM.PMAN.2.F", "goal_setting", "school", "PMAN",  "Female"),
      c("SE.PRM.PMAN.2.M", "goal_setting", "school", "PMAN",  "Male"),
      c("SE.PRM.PMAN.2.R", "goal_setting", "school", "PMAN",  "Rural"),
      c("SE.PRM.PMAN.2.U", "goal_setting", "school", "PMAN",  "Urban"),
      #(De Facto) Ave,rage score for the extent to which principals master goal-setting in the long term	
      c("SE.PRM.PMAN.3  ", "problem_solving", "school", "PMAN",  "All"),
      c("SE.PRM.PMAN.3.F", "problem_solving", "school", "PMAN",  "Female"),
      c("SE.PRM.PMAN.3.M", "problem_solving", "school", "PMAN",  "Male"),
      c("SE.PRM.PMAN.3.R", "problem_solving", "school", "PMAN",  "Rural"),
      c("SE.PRM.PMAN.3.U", "problem_solving", "school", "PMAN",  "Urban"),

  #######################################
  # Policy Lever (Teaching) - Attraction	(TATT)
  #######################################
  #api_final[grep('TATT', api_final$Series),1:2]

      c("SE.PRM.TATT ","teacher_attraction		", "school", "TATT",  "All"),        
      #(De Jure) Ave,rage starting public-school teacher salary as percent of GDP per capita	
      #SE.PRM.TATT.1, 100*expert_df$teacher_salary,
      #(De Facto) Pe,rcent of teachers reporting being satisfied or very satisfied with their social status in the community	
      c("SE.PRM.TATT.2 ","100*teacher_satisfied_status		", "school", "TATT",  "All"),   
      #(De Facto) Pe,rcent of teachers reporting being satisfied or very satisfied with their job as teacher	
      c("SE.PRM.TATT.3 ","100*teacher_satisfied_job		", "school", "TATT",  "All"),  
      #(De Facto) Pe,rcent of teachers reporting having received financial bonuses in addition to their salaries	
      c("SE.PRM.TATT.4 ","100*teacher_bonus		", "school", "TATT",  "All"),    
      #(De Facto) Pe,rcent of teachers reporting that there are incentives (financial or otherwise) for teachers to teach certain subjects/grades and/or in certain areas	
      c("SE.PRM.TATT.5 ","100*if_else((teacher_bonus_hard_staff==1 | teacher_bonus_subj_shortages==1),1,0	)	", "school", "TATT",  "All"),
      #(De Facto) Pe,rcent of teachers that performance matters for promotions	
      c("SE.PRM.TATT.6 ", "100*better_teachers_promoted		", "school", "TATT",  "All"),  
      #(De Jure) Is ,there a well-established career path for teachers?	
      #SE.PRM.TATT.7,  -999     ,
      #(De Facto) Pe,rcent of teachers that report salary delays in the past 12 months	
      c("SE.PRM.TATT.8 ", "100*salary_delays		", "school", "TATT",  "All"),  
      #(De Facto) Po,licy Lever (Teaching) - Attraction	
      #SE.PRM.TATT.D,F expert_df$teacher_attraction,
      #(De Jure) Pol,icy Lever (Teaching) - Attraction	
      c("SE.PRM.TATT.DJ", "teacher_attraction		", "school", "TATT",  "All"),


  #######################################
  # Policy Lever (Teaching) - Selection & Deployment	(TSDP)
  #######################################

    c("SE.PRM.TSDP  " ,"teacher_selection_deployment		", "school", "TSDP",  "All"),
    #Policy Lever (,Teaching) - Selection & Deployment                                 
    #SE.PRM.TSDP.1 , expert_df$criteria_admittance,
    #(De Jure) Requ,irements to enter into initial education programs                  
    #SE.PRM.TSDP.2 ", " -999,
    #(De Facto) Ave,rage quality of applicants accepted into initial education programs
    #SE.PRM.TSDP.3 ", "expert_df$criteria_become,
    #(De Jure) Requ,irements to become a primary school teacher                        
    c("SE.PRM.TSDP.4  ","1+2*teacher_selection		", "school", "TSDP",  "All"),
    #(De Facto) Req,uirements to become a primary school teacher                       
    #SE.PRM.TSDP.5 ,=expert_df$criteria_transfer,
    #(De Jure) Requ,irements to fulfill a transfer request                             
    c("SE.PRM.TSDP.6  ","1+2*teacher_deployment		", "school", "TSDP",  "All"),
    #(De Facto) Req,uirements to fulfill a transfer request                            
    #SE.PRM.TSDP.7 ", "-999,
    #(De Jure) Sele,ctivity of teacher hiring process                                  
    c("SE.PRM.TSDP.DF ","teacher_selection_deployment		", "school", "TSDP",  "All"),
    #(De Facto) Pol,icy Lever (Teaching) - Selection & Deployment                      
    #SE.PRM.TSDP.DJ, =expert_df$teacher_selection_deployment
    #(De Jure) Poli,cy Lever (Teaching) - Selection & Deployment   

  #######################################
  # Policy Lever (Teaching) - Support	(TSUP)
  #######################################

  
  c("SE.PRM.TSUP ","teacher_support		", "school", "TSUP",  "All"),   
  #Policy Lever", (Teaching) - Support                                                                                        
  #SE.PRM.TSUP.",1  =expert_df$practicum ,
  #(De Jure) Pr,acticum required as part of pre-service training                                                             
  c("SE.PRM.TSUP.2", "100-100*m3sdq6_tsup-1			", "school", "TSUP",  "All"), 
  #(De Facto) P,ercent reporting they completed a practicum as part of pre-service training                                  
  c("SE.PRM.TSUP.3",  "100*m3sdq3_tsup		", "school", "TSUP",  "All"),
  #(De Facto) P",ercent of teachers reporting that they participated in an induction and/or mentorship program                
  #c("SE.PRM.TSUP.4",  =expert_df$prof_development,
  #(De Jure) Pa",rticipation in professional development has professional implications for teachers                           
  c("SE.PRM.TSUP.5",  "100*m3sdq9_tsup			", "school", "TSUP",  "All"),
  #(De Facto) P",ercent of teachers reporting having attended in-service trainings in the past 12 months                      
  c("SE.PRM.TSUP.6",  "m3sdq10_tsup			", "school", "TSUP",  "All"),
  #(De Facto) A",verage length of the trainings attended                                                                      
  c("SE.PRM.TSUP.7",  "m3sdq11_tsup			", "school", "TSUP",  "All"),
  #(De Facto) A",verage span of time (in weeks) of those trainings                                                            
  c("SE.PRM.TSUP.8",  "100*(m3sdq13_tsup-1)/4			", "school", "TSUP",  "All"),
  #(De Facto) A",verage percent of time spent inside the classrooms during the trainings                                      
  c("SE.PRM.TSUP.9", "100*opportunities_teachers_share			", "school", "TSUP",  "All"), 
  #(De Facto) P",ercent of teachers that report having opportunities to come together with other teachers to discuss ways of ~
  c("SE.PRM.TSUP.DF","teacher_support		", "school", "TSUP",  "All"),  
  #(De Facto) P",olicy Lever (Teaching) - Support                                                                             
  #SE.PRM.TSUP.",DJ =expert_df$teacher_support
  #(De Jure) Po",licy Lever (Teaching) - Support  

  #######################################
  # Policy Lever (Teaching) - Evaluation	(TEVL)
  #######################################

  c("SE.PRM.TEVL  ","teaching_evaluation		", "school", "TEVL",  "All"),     #Policy Lever (Teaching) - Evaluation                                                                                     
  #SE.PRM.TEVL.1", "expert_df$evaluation_law, #(De Jure) Legislation assigns responsibility of evaluating the performance of teachers to a public authority (national)
  #SE.PRM.TEVL.2", =expert_df$evaluation_law_school, #(De Jure) Legislation assigns responsibility of evaluating the performance of teachers to the schools                    
  c("SE.PRM.TEVL.3 ","100*formally_evaluated		", "school", "TEVL",  "All"),   #(De Facto) Percent of teachers that report being evaluated in the past 12 months                                         
  #SE.PRM.TEVL.4", "expert_df$evaluation_criteria, #(De Jure) The criteria to evaluate teachers is clear                                                                     
  c("SE.PRM.TEVL.5 ","m3sbq8_tmna__1	+m3sbq8_tmna__2 + m3sbq8_tmna__3 + m3sbq8_tmna__4 + m3sbq8_tmna__5 + m3sbq8_tmna__6 + m3sbq8_tmna__7 + m3sbq8_tmna__8 + m3sbq8_tmna__97		", "school", "TEVL",  "All"),  #(De Facto) Number of criteria used to evaluate teachers                                                                  
  c("SE.PRM.TEVL.6 ","100*negative_consequences		", "school", "TEVL",  "All"),  #(De Facto) Percent of teachers that report there would be consequences after two negative evaluations                    
  c("SE.PRM.TEVL.7 ","100*positive_consequences		", "school", "TEVL",  "All"),  #(De Facto) Percent of teachers that report there would be consequences after two positive evaluations                    
  #SE.PRM.TEVL.8", =expert_df$negative_evaluations, #(De Jure) There are clear consequences for teachers who receive two or more negative evaluations                         
  #SE.PRM.TEVL.9", =expert_df$positive_evaluations, #(De Jure) There are clear consequences for teachers who receive two or more positive evaluations                         
  c("SE.PRM.TEVL.DF", "teaching_evaluation		", "school", "TEVL",  "All"),  #(De Facto) Policy Lever (Teaching) - Evaluation                                                                          
  #SE.PRM.TEVL.DJ", =expert_df$teaching_evaluation#(De Jure) Policy Lever (Teaching) - Evaluation 

  #######################################
  # Policy Lever (Teaching) - Monitoring & Accountability 	(TMNA)
  #######################################

  c("SE.PRM.TMNA   ","teacher_monitoring		", "school", "TMNA",  "All"),    #Policy Lever (Teaching) - Monitoring & Accountability                                                       
  c("SE.PRM.TMNA.3 ","100*attendance_rewarded		", "school", "TMNA",  "All"),   #(De Facto) Teacher report receiving monetary compensation (aside from salary) for being present             
  c("SE.PRM.TMNA.4 ","100*miss_class_admin		", "school", "TMNA",  "All"),   #(De Facto) Percent of teachers that report having been absent because of administrative processes           
  c("SE.PRM.TMNA.5 ","100*attendence_sanctions		", "school", "TMNA",  "All"),  #(De Facto) Percent of teachers that report that there would be consequences for being absent 40% of the time
  c("SE.PRM.TMNA.DF", "teacher_monitoring		", "school", "TMNA",  "All"),  #(De Facto) Policy Lever (Teaching) - Monitoring & Accountability                                            

  #######################################
  # Policy Lever (Teaching) - Intrinsic Motivation 	(TINM)
  #######################################

  c("SE.PRM.TINM   ","intrinsic_motivation		", "school", "TINM",  "All"),    #Policy Lever (Teaching) - Intrinsic Motivation                                                                           
  c("SE.PRM.TINM.1 ","SE_PRM_TINM_1		", "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if the ~
  c("SE.PRM.TINM.10", "SE_PRM_TINM_10		", "school", "TINM",  "All"), #(De Facto) Percent of teachers that agree or strongly agrees with \"Students can change even their basic intelligence l~
  c("SE.PRM.TINM.11", "motivation_teaching		", "school", "TINM",  "All"), #(De Facto) Percent of teachers who state that intrinsic motivation was the main reason to become teachers                
  c("SE.PRM.TINM.12", "m3sdq2_tmna		", "school", "TMNA",  "All"), #(De Facto) New teachers are required to undergo a probationary period                                                    
  c("SE.PRM.TINM.2 ","SE_PRM_TINM_2		", "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if stud~
  c("SE.PRM.TINM.3 ","SE_PRM_TINM_3		", "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if the ~
  c("SE.PRM.TINM.4 ","SE_PRM_TINM_4		", "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they attend scho~
  c("SE.PRM.TINM.5 ","SE_PRM_TINM_5		", "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they come to sch~
  c("SE.PRM.TINM.6 ","SE_PRM_TINM_6		", "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they are motivat~
  c("SE.PRM.TINM.7 ","SE_PRM_TINM_7		", "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students have a certain amount of intelligence and ~
  c("SE.PRM.TINM.8 ","SE_PRM_TINM_8		", "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with To be honest, students can't really change how inte~
  c("SE.PRM.TINM.9 ","SE_PRM_TINM_9		", "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students can always substantially change how intell~
  c("SE.PRM.TINM.DF", "intrinsic_motivation		", "school", "TINM",  "All"), #(De Facto) Policy Lever (Teaching) - Intrinsic Motivation                                                                

  #######################################
  # Policy Lever (Inputs & Infrastructure) - Standards 	(ISTD)
  #######################################

  c("SE.PRM.ISTD    "," standards_monitoring		", "school", "ISTD",  "All"), #Policy Lever (Inputs & Infrastructure) - Standards                                                                       
  c("SE.PRM.ISTD.10 "," 100*m1scq14_imon__4		", "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that schools have access to drinking water?              
  c("SE.PRM.ISTD.12 "," 100*m1scq14_imon__1		", "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that schools have functioning toilets?                   
  c("SE.PRM.ISTD.14 "," 100*m1scq14_imon__3		", "school", "ISTD",  "All"),#(De Facto) Do you know if there is there a policy in place to require that schools are accessible to children with speci~
  c("SE.PRM.ISTD.2  "," 100*m1scq13_imon__2		", "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that students have access to the prescribed textbooks?   
  c("SE.PRM.ISTD.6  "," 100*m1scq13_imon__5		", "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that students have access to PCs, laptops, tablets, and/~
  c("SE.PRM.ISTD.8  "," 100*m1scq14_imon__2		", "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that schools have access to electricity?                 
  c("SE.PRM.ISTD.DF "," standards_monitoring		", "school", "ISTD",  "All"),#(De Facto) Policy Lever (Inputs & Infrastructure) - Standards                                                            

  #######################################
  # Policy Lever (Inputs & Infrastructure) - Monitoring 	(IMON)
  #######################################
  

  c("SE.PRM.IMON    "," sch_monitoring		", "school", "IMON",  "All"),    #Policy Lever (Inputs & Infrastructure) - Monitoring                                                                      
  c("SE.PRM.IMON.1  "," m1scq1_imon		", "school", "IMON",  "All"),  #(De Facto) Percent of schools that report there is someone monitoring that basic inputs are available to students        
  c("SE.PRM.IMON.2  "," parents_involved		", "school", "IMON",  "All"),  #(De Facto) Percent of schools that report that parents or community members are involved in the monitoring of availabili~
  c("SE.PRM.IMON.3  "," m1scq5_imon		", "school", "IMON",  "All"),  #(De Facto) Percent of schools that report that there is an inventory to monitor availability of basic inputs             
  c("SE.PRM.IMON.4  "," m1scq7_imon		", "school", "IMON",  "All"),  #(De Facto) Percent of schools that report there is someone monitoring that basic infrastructure is available             
  c("SE.PRM.IMON.5  "," bin_var(m1scq10_imon,1)		", "school", "IMON",  "All"),  #(De Facto) Percent of schools that report that parents or community members are involved in the monitoring of availabili~
  c("SE.PRM.IMON.6  "," m1scq11_imon		", "school", "IMON",  "All"),  #(De Facto) Percent of schools that report that there is an inventory to monitor availability of basic infrastructure     
  c("SE.PRM.IMON.DF ", " sch_monitoring		", "school", "IMON",  "All"), #(De Facto) Policy Lever (Inputs & Infrastructure) - Monitoring                                                           





  #######################################
  # Policy Lever (School Management) - Clarity of Functions 	(SCFN)
  #######################################

  c("SE.PRM.SCFN    ","sch_management_clarity		", "school", "SCFN",  "All"),  #Policy Lever (School Management) - Clarity of Functions                                                                  
  c("SE.PRM.SCFN.1  ","100*infrastructure_scfn		", "school", "SCFN",  "All"),  #(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of the maintenance~
  c("SE.PRM.SCFN.11 ","100*principal_hiring_scfn		", "school", "SCFN",  "All"), #(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of principal hirin~
  c("SE.PRM.SCFN.13 ","100*principal_supervision_scfn		", "school", "SCFN",  "All"),#(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of principal super~
  c("SE.PRM.SCFN.3  ","100*materials_scfn		", "school", "SCFN",  "All"), #(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of the procurement~
  c("SE.PRM.SCFN.5  ","100*hiring_scfn		", "school", "SCFN",  "All"),#(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of teacher hiring ~
  c("SE.PRM.SCFN.7  ","100*supervision_scfn		", "school", "SCFN",  "All"),#(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of teacher supervi~
  c("SE.PRM.SCFN.9  ","100*student_scfn		", "school", "SCFN",  "All"),#(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of student learnin~
  c("SE.PRM.SCFN.DF ","sch_management_clarity		", "school", "SCFN",  "All"),#(De Facto) Policy Lever (School Management) - Clarity of Functions                                                       

  #######################################
  # Policy Lever (School Management) - Attraction 	(SATT)
  #######################################

  c("SE.PRM.SATT   ","sch_management_attraction		", "school", "SATT",  "All"),  #Policy Lever (School Management) - Attraction                                                                             
  c("SE.PRM.SATT.2 ","principal_salary		", "school", "SATT",  "All"),  #(De Facto) Average principal salary as percent of GDP per capita                                                          
  c("SE.PRM.SATT.3 ", "principal_satisfaction		", "school", "SATT",  "All"),#(De Facto) Percent of principals reporting being satisfied or very satisfied with their social status in the community    
  c("SE.PRM.SATT.DF", "sch_management_attraction		", "school", "SATT",  "All"), #(De Facto) Policy Lever (School Management) - Attraction                                                                  

  #######################################
  # Policy Lever (School Management) - Selection & Deployment 	(SSLD)
  #######################################

  c("SE.PRM.SSLD    "," sch_selection_deployment		", "school", "SSLD",  "All"),#Policy Lever (School Management) - Selection & Deployment                                                                
  c("SE.PRM.SSLD.10 ", " 100*m7sgq2_ssld==1		", "school", "SSLD",  "All"), #(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is yea~
  c("SE.PRM.SSLD.11 "," 100*m7sgq2_ssld==2		", "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is qua~
  c("SE.PRM.SSLD.12 "," 100*m7sgq2_ssld==3		", "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is dem~
  c("SE.PRM.SSLD.13 "," 100*m7sgq2_ssld==4		", "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is hav~
  c("SE.PRM.SSLD.14 "," 100*m7sgq2_ssld==6		", "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is pol~
  c("SE.PRM.SSLD.15 "," 100*m7sgq2_ssld==7		", "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is eth~
  c("SE.PRM.SSLD.16 "," 100*m7sgq2_ssld==8		", "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is kno~
  c("SE.PRM.SSLD.3  "," 100*m7sgq1_ssld__1		", "school", "SSLD",  "All"), #(De Facto) Percent of principals that report that the factors considered when selecting a principal include years of exp~
  c("SE.PRM.SSLD.4  "," 100*m7sgq1_ssld__2		", "school", "SSLD",  "All"),#(De Facto)  Percent of principals that report that the factors considered when selecting a principal include quality of ~
  c("SE.PRM.SSLD.5  "," 100*m7sgq1_ssld__3		", "school", "SSLD",  "All"), #(De Facto) Percent of principals that report that the factors considered when selecting a principal include demonstrated~
  c("SE.PRM.SSLD.6  "," 100*m7sgq1_ssld__4		", "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the factors considered when selecting a principal include good relatio~
  c("SE.PRM.SSLD.7  "," 100*m7sgq1_ssld__6		", "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the factors considered when selecting a principal include political af~
  c("SE.PRM.SSLD.8  "," 100*m7sgq1_ssld__7		", "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the factors considered when selecting a principal include ethnic group 
  c("SE.PRM.SSLD.9  "," 100*m7sgq1_ssld__8		", "school", "SSLD",  "All"), #(De Facto) Percent of principals that report that the factors considered when selecting a principal include knowledge of~
  c("SE.PRM.SSLD.DF "," sch_selection_deployment		", "school", "SSLD",  "All"), #(De Facto) Policy Lever (School Management) - Selection & Deployment                                                     

  #######################################
  # Policy Lever (School Management) - Support 	(SSUP)
  #######################################

  c("SE.PRM.SSUP    "," sch_support		", "school", "SSUP",  "All"), #Policy Lever (School Management) - Support                                                                        
  c("SE.PRM.SSUP.10 "," 100*m7sgq5_ssup		", "school", "SSUP",  "All"),#(De Facto) Percent of principals that report having used the skills they gained at the last training they attended
  c("SE.PRM.SSUP.11 "," m7sgq7_ssup		", "school", "SSUP",  "All"),#(De Facto) Average number of trainings that principals report having been offered to them in the past year        
  c("SE.PRM.SSUP.6  "," 100*m7sgq3_ssup		", "school", "SSUP",  "All"), #(De Facto) Percent of principals that report ever having received formal training                                 
  c("SE.PRM.SSUP.7  "," 100*m7sgq4_ssup__1		", "school", "SSUP",  "All"),#(De Facto) Percent of principals that report having received management training for new principals               
  c("SE.PRM.SSUP.8  "," 100*m7sgq4_ssup__2		", "school", "SSUP",  "All"), #(De Facto) Percent of principals that report having received in-service training                                  
  c("SE.PRM.SSUP.9  "," 100*m7sgq4_ssup__3		", "school", "SSUP",  "All"),#(De Facto) Percent of principals that report having received mentoring/coaching by experienced principals         
  c("SE.PRM.SSUP.DF ",  " sch_support		", "school", "SSUP",  "All"),  #(De Facto) Policy Lever (School Management) - Support                                                             

  #######################################
  # Policy Lever (School Management) - Evaluation 	(SEVL)
  #######################################

  c("SE.PRM.SEVL   "," principal_evaluation		", "school", "SEVL",  "All"), #Policy Lever (School Management) - Evaluation                                                          
  c("SE.PRM.SEVL.3 "," 100*m7sgq8_sevl		", "school", "SEVL",  "All"), #(De Facto) Percent of principals that report having been evaluated  during the last school year        
  c("SE.PRM.SEVL.4 "," 100*principal_eval_tot>1		", "school", "SEVL",  "All"), #(De Facto) Percent of principals that report having been evaluated on multiple factors                 
  c("SE.PRM.SEVL.5 "," 100*principal_negative_consequences	", "school", "SEVL",  "All"), #(De Facto) Percent of principals that report there would be consequences after two negative evaluations
  c("SE.PRM.SEVL.6 "," 100*principal_positive_consequences	", "school", "SEVL",  "All"),#(De Facto) Percent of principals that report there would be consequences after two positive evaluations
  c("SE.PRM.SEVL.DF","  principal_evaluation		", "school", "SEVL",  "All"), #(De Facto) Policy Lever (School Management) - Evaluation                                               

  #######################################
  # Politics & Bureaucratic Capacity - Quality of Bureaucracy 	(BQBR)
  #######################################

  c("SE.PRM.BQBR  ", " quality_bureaucracy"		, "public_officials", "BQBR",  "All"),#Politics & Bureaucratic Capacity - Quality of Bureaucracy                                                                  
  c("SE.PRM.BQBR.1", " quality_bureaucracy"		, "public_officials", "BQBR",  "All"),#Average score for Quality of Bureaucracy; where a score of 1 indicates low effectiveness and 5 indicates high effectiveness
  c("SE.PRM.BQBR.2", " knowledge_skills"		, "public_officials", "BQBR",  "All"),#(Quality of Bureaucracy) average score for knowledge and skills                                                            
  c("SE.PRM.BQBR.3", " work_environment"		, "public_officials", "BQBR",  "All"),#(Quality of Bureaucracy) average score for work environment                                                                
  c("SE.PRM.BQBR.4", " merit"		, "public_officials", "BQBR",  "All"),#(Quality of Bureaucracy) average score for merit                                                                           
  c("SE.PRM.BQBR.5"," motivation_attitudes"		, "public_officials", "BQBR",  "All"),#Quality of Bureaucracy) average score for motivation and attitudes      

  #######################################
  # Politics & Bureaucratic Capacity - Impartial Decision-Making 	(BIMP)
  #######################################

  c("SE.PRM.BIMP  "," impartial_decision_making"		, "public_officials", "BIMP",  "All"), #Politics & Bureaucratic Capacity - Impartial Decision-Making                                                               
  c("SE.PRM.BIMP.1"," impartial_decision_making"		, "public_officials", "BIMP",  "All"), #Average score for Impartial Decision-Making; where a score of 1 indicates low effectiveness and 5 indicates high effective~
  c("SE.PRM.BIMP.2"," politicized_personnel_management"		, "public_officials", "BIMP",  "All"), #(Impartial Decision-Making) average score for politicized personnel management                                             
  c("SE.PRM.BIMP.3"," politicized_policy_making"		, "public_officials", "BIMP",  "All"), #(Impartial Decision-Making) average score for politicized policy-making                                                    
  c("SE.PRM.BIMP.4"," politicized_policy_implementation"		, "public_officials", "BIMP",  "All"), #(Impartial Decision-Making) average score for politicized policy implementation                                            
  c("SE.PRM.BIMP.5"," employee_unions_as_facilitators"		, "public_officials", "BIMP",  "All"), #(Impartial Decision-Making) average score for employee unions as facilitators 

  #######################################
  # Politics & Bureaucratic Capacity - Mandates & Accountability 	(BMAC)
  #######################################

  c("SE.PRM.BMAC   "," mandates_accountability"		, "public_officials", "BMAC",  "All"),#Politics & Bureaucratic Capacity - Mandates & Accountability                                                               
  c("SE.PRM.BMAC.1 "," mandates_accountability"		, "public_officials", "BMAC",  "All"),#Average score for Mandates & Accountability; where a score of 1 indicates low effectiveness and 5 indicates high effective~
  c("SE.PRM.BMAC.2 "," coherence"		, "public_officials", "BMAC",  "All"),#(Mandates & Accountability) Average score for coherence                                                                    
  c("SE.PRM.BMAC.3 "," transparency"		, "public_officials", "BMAC",  "All"),#(Mandates & Accountability) Average score for transparency                                                                 
  c("SE.PRM.BMAC.4 "," accountability"		, "public_officials", "BMAC",  "All"),#(Mandates & Accountability) Average score for accountability of public officials    

  #######################################
  # Politics & Bureaucratic Capacity - National Learning Goals 	(BNLG)
  #######################################

  c("SE.PRM.BNLG  ", " national_learning_goals"		, "public_officials", "BNLG",  "All"),#Politics & Bureaucratic Capacity - National Learning Goals                                                                 
  c("SE.PRM.BNLG.1", " national_learning_goals"		, "public_officials", "BNLG",  "All"),#Average score for National Learning Goals; where a score of 1 indicates low effectiveness and 5 indicates high effectivene~
  c("SE.PRM.BNLG.2", " targeting"		, "public_officials", "BNLG",  "All"),#(National Learning Goals) Average score for targeting                                                                      
  c("SE.PRM.BNLG.3", " monitoring"		, "public_officials", "BNLG",  "All"),#(National Learning Goals) Average score for monitoring                                                                     
  c("SE.PRM.BNLG.4", " incentives"		, "public_officials", "BNLG",  "All"),#(National Learning Goals) Average score for incentives                                                                     
  c("SE.PRM.BNLG.5", " community_engagement"		, "public_officials", "BNLG",  "All")#(National Learning Goals) Average score for community engagement   


  )

#apply the function to indicators list
#loop through the elements in indicators
#combine results in a dataframe

#initialize empty dataframe
indicator_data <- data.frame()

for (i in 1:length(indicators)) {
  
  #get indicator name
  name <- indicators[[i]][1]
  
  #get indicator code
  indicator <- indicators[[i]][2]
  
  #get dataset
  dataset <- indicators[[i]][3]
  
  #get tag
  tag <- indicators[[i]][4]
  
  #get unit
  unit <- indicators[[i]][5]
  
  #get indicator data
  indicator_data <- rbind(indicator_data, indicator_stats(name, indicator, dataset, tag, unit))
  
}

source(here('Indicators/R/api_template_fun.R'), echo=TRUE)
api_template <- api_template_fun()

#join metadata

indicator_data <- left_join(indicator_data, api_template) %>%
  rename(
    `Mean`=mean,
    `Standard Error`=mean_se,
    `Lower Bound`=mean_low,
    `Upper Bound`=mean_upp,
    `Variance`=mean_var

  )

  #save as csv
  write_excel_csv(indicator_data, here('Indicators/PER_means_stderr.csv'))