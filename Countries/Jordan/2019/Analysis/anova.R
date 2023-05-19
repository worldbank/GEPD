#anova to examine fraction of variance between and within schools
library(tidyverse)


#Country name and year of survey
country <-'JOR'
country_name <- "Jordan"
year <- '2019'

#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

backup_onedrive="no"

project_folder<-"//wbgfscifs01/GEDEDU/datalib-edu/Projects/GEPD/CNT/"
data_folder<-file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_M", sep="_"),"Data", sep="/"))

load(paste(data_folder, "School/school_indicators_data_anon.RData", sep="/"))


#################################
# ANOVA
#################################

anova <- aov(math_student_knowledge~factor(hashed_school_code), data=assess_4th_grade_anon_anon, weights = ipw)

summary(anova)
print(anova)
#> 693839391/(693839391+1025043491)


# fit = lm(student_knowledge ~ factor(hashed_school_code), data=assess_4th_grade_anon_anon, weights = ipw)
# anova(fit)


wtd.mean(school_dta_short_anon$student_knowledge, weights =school_dta_short_anon$ipw )

wtd.mean(school_dta_short_anon$student_knowledge, weights =school_dta_short_anon$ipw )


write_excel_csv( assess_4th_grade_anon_aov, path =  file.path(paste(save_folder, 'assess_fourth_grade_anon.csv', sep="/" )) )



library(stargazer)
library(sandwich)
school_dta_short_merge <- school_dta_short_anon %>%
  select(-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge', 
            'student_proficient', 'student_proficient_70', 'student_proficient_75',
            'literacy_student_proficient', 'literacy_student_proficient_70', 'literacy_student_proficient_75',
            'math_student_proficient', 'math_student_proficient_70', 'math_student_proficient_75'))






covariates<-c( 'presence_rate',
               'content_knowledge',
               'teach_score',
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
               'sch_monitoring', 
               'sch_management_clarity',
               'sch_management_attraction', 
               'sch_selection_deployment', 
               'sch_support', 
               'principal_evaluation')

my_formula <- as.formula(paste('student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg_school<-lm(my_formula, school_dta_short_anon, weights = school_dta_short_anon$ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg_school, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg_school)


my_formula <- as.formula(paste('math_student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg_school<-lm(my_formula, school_dta_short_anon, weights = school_dta_short_anon$ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg_school, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg_school)

my_formula <- as.formula(paste('literacy_student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg_school<-lm(my_formula, school_dta_short_anon, weights = school_dta_short_anon$ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg_school, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg_school)


nogiraffe_df<-final_indicator_data_LERN_anon %>%
  left_join(school_dta_short_anon            )


my_formula <- as.formula(paste('literacy_student_knowledge_nogiraffe ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg_school<-lm(my_formula, nogiraffe_df, weights = nogiraffe_df$ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg_school, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg_school)
# 
# stargazer( multi_reg, type = "html",
#            se        = list(robust_multi_se),
#            title = "Multivariate OLS Regression using School Level GEPD Data",
#            style='aer',
#            notes= c('Observations weighted using sampling weights.',
#                     'Heteroskedasticity robust standard errors in parenthesis.', 
#                     'Log GDP per Sq km is the log of GDP in 2010 within a one square kilometer radius of the school.', 
#                     'GDP measures were produced by researchers at the World Bank DECRG.',  
#                     'Data available here:  https://datacatalog.worldbank.org/dataset/gross-domestic-product-2010')
# )






reg_df<- assess_4th_grade_anon_anon %>%
  group_by(hashed_school_code) %>%
  mutate(total_4th_count=n()) %>%
  ungroup() %>%
  left_join(school_dta_short_merge, by="hashed_school_code") %>%
  mutate(ipw=ipw.x/total_4th_count)


covariates<-c( 'presence_rate',
               'content_knowledge',
               'teach_score',
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
               'sch_monitoring', 
               'sch_management_clarity',
               'sch_management_attraction', 
               'sch_selection_deployment', 
               'sch_support', 
               'principal_evaluation')

my_formula <- as.formula(paste('student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg<-lm(my_formula, reg_df, weights = reg_df$school_ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg)

my_formula <- as.formula(paste('literacy_student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg<-lm(my_formula, reg_df, weights = reg_df$school_ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg)

# stargazer( multi_reg, type = "html",
#            se        = list(robust_multi_se),
#            title = "Multivariate OLS Regression using School Level GEPD Data",
#            style='aer',
#            notes= c('Observations weighted using sampling weights.',
#                     'Heteroskedasticity robust standard errors in parenthesis.', 
#                     'Log GDP per Sq km is the log of GDP in 2010 within a one square kilometer radius of the school.', 
#                     'GDP measures were produced by researchers at the World Bank DECRG.',  
#                     'Data available here:  https://datacatalog.worldbank.org/dataset/gross-domestic-product-2010')
# )