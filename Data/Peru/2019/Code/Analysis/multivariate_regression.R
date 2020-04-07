library(stargazer)
library(sandwich)
school_dta_short_merge <- school_dta_short %>%
  select(-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge', 
            'student_proficient', 'student_proficient_70', 'student_proficient_75',
            'literacy_student_proficient', 'literacy_student_proficient_70', 'literacy_student_proficient_75',
            'math_student_proficient', 'math_student_proficient_70', 'math_student_proficient_75'))

reg_df<- assess_4th_grade_anon %>%
  left_join(school_dta_short_merge) %>%
  mutate(codigo.modular=as.numeric(school_code)) %>%
  left_join(data_set_updated) %>%
  mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th/total_4th_count)


covariates<-c( 'presence_rate',
               'content_knowledge',
               'ecd_student_knowledge',

               'inputs',
               'infrastructure',
               'operational_management',
               'instructional_leadership',
               'principal_knowledge_score',
               'principal_management')

my_formula <- as.formula(paste('student_knowledge ~ ', paste(covariates, collapse=" + "), sep=""))
multi_reg<-lm(my_formula, reg_df, weights = reg_df$ipw)   
# Adjust standard errors
cov1_multi         <- vcovHC(multi_reg, type = "HC1")
robust_multi_se    <- sqrt(diag(cov1_multi))

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