#anova to examine fraction of variance between and within schools
library(tidyverse)


#Country name and year of survey
country <-'PER'
country_name <- "Peru"
year <- '2019'

#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

backup_onedrive="no"

#Add your UPI here and set the directory paths of your choice.
if (Sys.getenv("USERNAME") == "wb469649"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/"
  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School", sep="/"))
  
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School", sep="/"))
  
} else if (Sys.getenv("USERNAME") == "wb550666"){
  #project_folder  <- "//wbgfscifs01/GEDEDU/datalib-edu/projects/gepd"
  project_folder  <- "C:/Users/wb550666/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/"
  
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School", sep="/"))
  
  # This is experimental and not currently in use.
  backup_onedrive="yes"
  save_folder_onedrive <- file.path(paste("C:/Users/wb550666/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/", country_name,year,"Data/clean/School", sep="/"))
  
}else if (str_to_lower(Sys.getenv("USERNAME")) == "wb577189"){
  
  project_folder<-"C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work"
  download_folder <-file.path(paste(project_folder,country_name,year,"Data/raw/School", sep="/"))
  save_folder <- file.path(paste(project_folder,country_name,year,"Data/clean/School", sep="/"))
  
} else {
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
  save_folder <- choose.dir(default = "", caption = "Select folder to save final data")
  save_folder_onedrive <- choose.dir(default = "", caption = "Select folder to save backed up data to onedrive")
  
}


load(paste(save_folder, "school_indicators_data.RData", sep="/"))


#Load original sample of schools
#Load original sample of schools
currentDate<-c("2019-07-22")
sample_frame_name <- paste("C:/Users/wb577189/OneDrive - WBG/My files/Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/school_sample_",currentDate,".RData", sep="")

sample_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/school_sample_",currentDate,".RData", sep="")
load(sample_frame_name)


assess_4th_grade_anon_aov<-assess_4th_grade_anon %>%
  group_by(school_code) %>%
  mutate(total_4th_count=n()) %>%
  ungroup() %>%
  mutate(codigo.modular=as.numeric(school_code)) %>%
  left_join(data_set_updated) %>%
  mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th/total_4th_count)

school_dta_short_aov<- school_dta_short %>%
  mutate(codigo.modular=as.numeric(school_code)) %>%
  left_join(data_set_updated) %>%
  mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th)



#################################
# ANOVA
#################################

anova <- aov(math_student_knowledge~factor(school_code), data=assess_4th_grade_anon_aov, weights = school_ipw)

summary(anova)
print(anova)
#> 402908/(403059+402908)=0.4999063


fit = lm(student_knowledge ~ factor(school_code), data=assess_4th_grade_anon_aov, weights = school_ipw)
anova(fit)


wtd.mean(school_dta_short_aov$student_knowledge, weights =school_dta_short_aov$school_ipw )

wtd.mean(assess_4th_grade_anon_aov$student_knowledge, weights =assess_4th_grade_anon_aov$school_ipw )


write_excel_csv( assess_4th_grade_anon_aov, path =  file.path(paste(save_folder, 'assess_fourth_grade_anon.csv', sep="/" )) )



library(stargazer)
library(sandwich)
school_dta_short_merge <- school_dta_short %>%
  select(-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge', 
            'student_proficient', 'student_proficient_70', 'student_proficient_75',
            'literacy_student_proficient', 'literacy_student_proficient_70', 'literacy_student_proficient_75',
            'math_student_proficient', 'math_student_proficient_70', 'math_student_proficient_75'))






covariates<-c( 'presence_rate',
               'content_knowledge',
               'student_attendance',
               'ecd_student_knowledge',
               'inputs',
               'infrastructure',
               'operational_management',
               'instructional_leadership',
               'principal_knowledge_score',
               'principal_management',
               'teacher_attraction', 
               'teacher_selection_deployment', 
               'teacher_support', 
               'teaching_evaluation', 
               'teacher_monitoring',
               'intrinsic_motivation', 
               'standards_monitoring',
               'school_monitoring', 
               'school_management_clarity',
               'school_management_attraction', 
               'school_selection_deployment', 
               'school_support', 
               'principal_evaluation')

my_formula <- as.formula(paste('student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg_school<-lm(my_formula, school_dta_short_aov, weights = school_dta_short_aov$school_ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg_school, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg_school)

stargazer( multi_reg, type = "html",
           se        = list(robust_multi_se),
           title = "Multivariate OLS Regression using School Level GEPD Data",
           style='aer',
           notes= c('Observations weighted using sampling weights.',
                    'Heteroskedasticity robust standard errors in parenthesis.', 
                    'Log GDP per Sq km is the log of GDP in 2010 within a one square kilometer radius of the school.', 
                    'GDP measures were produced by researchers at the World Bank DECRG.',  
                    'Data available here:  https://datacatalog.worldbank.org/dataset/gross-domestic-product-2010')
)






reg_df<- assess_4th_grade_anon %>%
  group_by(school_code) %>%
  mutate(total_4th_count=n()) %>%
  ungroup() %>%
  left_join(school_dta_short_merge) %>%
  mutate(codigo.modular=as.numeric(school_code)) %>%
  left_join(data_set_updated) %>%
  mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th/total_4th_count)


covariates<-c( 'presence_rate',
               'content_knowledge',
               'student_attendance',
               'ecd_student_knowledge',
               'inputs',
               'infrastructure',
               'operational_management',
               'instructional_leadership',
               'principal_knowledge_score',
               'principal_management',
               'teacher_attraction', 
               'teacher_selection_deployment', 
               'teacher_support', 
               'teaching_evaluation', 
               'teacher_monitoring',
               'intrinsic_motivation', 
               'standards_monitoring',
               'school_monitoring', 
               'school_management_clarity',
               'school_management_attraction', 
               'school_selection_deployment', 
               'school_support', 
               'principal_evaluation')

my_formula <- as.formula(paste('student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg<-lm(my_formula, reg_df, weights = reg_df$school_ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg)

stargazer( multi_reg, type = "html",
           se        = list(robust_multi_se),
           title = "Multivariate OLS Regression using School Level GEPD Data",
           style='aer',
           notes= c('Observations weighted using sampling weights.',
                    'Heteroskedasticity robust standard errors in parenthesis.', 
                    'Log GDP per Sq km is the log of GDP in 2010 within a one square kilometer radius of the school.', 
                    'GDP measures were produced by researchers at the World Bank DECRG.',  
                    'Data available here:  https://datacatalog.worldbank.org/dataset/gross-domestic-product-2010')
)