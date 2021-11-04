#anova to examine fraction of variance between and within schools
library(tidyverse)
library(estimatr)
library(modelr)
library(questionr)

#Country name and year of survey
country <-'ETH'
country_name <- "Ethiopia"
year <- '2020_2021'

#########################
# File paths #
#########################
#The download_folder will be the location of where raw data is downloaded from the API
#The save_folder will be the location of where cleaned data is stored

backup_onedrive="no"

if (str_to_lower(Sys.getenv("USERNAME")) == "wb469649"){
  
project_folder<-"C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/CNT"
data_folder<-file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_M", sep="_"),"Data", sep="/"))

} else if (str_to_lower(Sys.getenv("USERNAME")) == "wb577189"){
  
  project_folder<-"C:/Users/wb577189/OneDrive - WBG/CNT/"
  data_folder<-file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data", sep="/"))
  save_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/confidential", sep="/"))
  
  
}

load(paste(data_folder, "School/school_indicators_data_anon.RData", sep="/"))


#################################
# ANOVA
#################################

anova <- aov(student_knowledge~factor(hashed_school_code), data=assess_4th_grade_anon_anon, weights = ipw)

summary(anova)
print(anova)
#10336371815/(10336371815+14063203040)
#[1] 0.4236292

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
               #'teach_score',
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
multi_reg_school<-lm(my_formula, school_dta_short_anon)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg_school, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg_school)

df_plot_reg <- school_dta_short_anon %>%
  select(student_knowledge, covariates, ipw) %>%
  add_predictions(multi_reg_school)

p1<-ggplot(df_plot_reg, aes(x=pred, y=student_knowledge)) + 
  geom_point() +
  geom_smooth(method = 'lm')
p1

my_formula <- as.formula(paste('student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg_school_weights<-lm(my_formula, school_dta_short_anon, weights = school_dta_short_anon$ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg_school, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg_school_weights)

df_plot_reg_w <- school_dta_short_anon %>%
  select(student_knowledge, covariates, ipw) %>%
  add_predictions(multi_reg_school_weights)

p_w<-ggplot(df_plot_reg_w, aes(x=pred, y=student_knowledge)) + 
  geom_point() +
  geom_smooth(method = 'lm')
p_w


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
multi_reg<-lm(my_formula, reg_df, weights = reg_df$ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

summary(multi_reg)

my_formula <- as.formula(paste('literacy_student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg<-lm(my_formula, reg_df, weights = reg_df$ipw)   
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



################
# Regression by Indicator
################

mod_fun <- function(df) {
  lm_robust(student_knowledge ~ .x  , data = school_dta_short_anon, se_type='HC3')
}

partial_regression <- school_dta_short_anon %>%
  dplyr::select(student_knowledge, covariates) %>% # just keep indicators for regression on GDP
  purrr::map(~lm_robust(student_knowledge ~ .x  , data = school_dta_short_anon, se_type='HC3')) %>%
  purrr::map(coef) %>%
  map_dbl(".x") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(.x = x) 

kable(partial_regression)


reg_plot_df <- school_dta_short_anon %>%
  dplyr::select(student_knowledge, covariates) %>% # just keep indicators for regression on GDP
  pivot_longer(
    cols=c(             'presence_rate',
                        'content_knowledge',
                        #'teach_score',
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
                        'principal_evaluation'),
    names_to = "type",
    values_to="indicators"
  )


mod_fun <- function(df) {      
  lm_robust(student_knowledge ~ indicators  , data = df, se_type='HC3') 
}

mod_fun_gdp <- function(df) {      
  lm_robust(student_knowledge ~ indicators  , data = df, se_type='HC3') 
}

b_fun <- function(mod)   {   
  coef(summary(mod))[2,1] 
}

se_fun <- function(mod)   {   
  coef(summary(mod))[2,2] 
}


r2_fun <- function(mod) {
  summary(mod)$r.squared
}

knowledge_regs <- reg_plot_df %>%
  group_by(type) %>%
  nest() %>%
  mutate(model=purrr::map(data, mod_fun)) %>% 
  mutate(model_gdp=purrr::map(data, mod_fun_gdp)) %>% 
  mutate(   beta = map_dbl(model, b_fun),
            se = map_dbl(model, se_fun),
            r2 = map_dbl(model, r2_fun),
            beta_gdp = map_dbl(model_gdp, b_fun),
            se_gdp = map_dbl(model_gdp, se_fun),
            r2_gdp = map_dbl(model_gdp, r2_fun)) %>%
  dplyr::select(type, beta, beta_gdp, se, r2,  se_gdp, r2_gdp, everything())

#plot of coefficient plots without GDP
ggplot(data=knowledge_regs, aes(x=type, y=beta)) +
  geom_point() + 
  geom_errorbar(aes(ymin=(beta-1.96*se),
                    ymax=(beta+1.96*se))) +
  coord_flip() +
  theme_bw() +
  ggtitle(str_wrap("Coefficients and Confidence Intervals of Indicators in Regression Without GDP Satellite Controls", 60))

#plot of coefficient plots without GDP
ggplot(data=knowledge_regs, aes(x=type, y=r2)) +
  geom_point() + 
  coord_flip() +
  theme_bw() +
  ggtitle(str_wrap("R^2 of Indicators in Regression Without GDP Satellite Controls", 60))

