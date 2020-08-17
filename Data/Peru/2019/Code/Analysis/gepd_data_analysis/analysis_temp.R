library(tidyverse)
library(skimr)
library(srvyr)
library(DT)
library(Hmisc)
load("//wbgfscifs01/GEDEDU/datalib-edu/projects/GEPD-Confidential/CNT/PER/PER_2019_GEPD/PER_2019_GEPD_v01_RAW/Data/anonymized/School/school_indicators_data_anon.RData")



#code






#create subset with just main indicators
indicators_list<-c('student_proficient',
                   'student_attendance', 
                   'presence_rate',
                   'content_proficiency', 
                   'teach_prof',
                   'ecd_student_proficiency', 
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
                   'principal_evaluation', 
                   'national_learning_goals',
                   'mandates_accountability',
                   'quality_bureaucracy',
                   'impartial_decision_making'
)


main_indicator_labels2<-c('Proficiency on GEPD Assessment', 
                          'Student Attendance',
                          'Teacher Effort', 
                          "Teacher Content Knowledge", 
                          "Teacher Pedagogical Skills",
                          'Capacity for Learning', 
                          'Basic Inputs', 
                          'Basic Infrastructure', 
                          'Operational Management', 
                          'Instructional Leadership', 
                          'Principal Knowledge of School',
                          'Principal Management Skills', 
                          'Policy Lever (Teaching) - Attraction',
                          'Policy Lever (Teaching) - Selection & Deployment',
                          'Policy Lever (Teaching) - Support', 
                          'Policy Lever (Teaching) - Evaluation', 
                          'Policy Lever (Teaching) - Monitoring & Accountability', 
                          'Policy Lever (Teaching) - Intrinsic Motivation', 
                          'Policy Lever (Inputs & Infrastructure) - Standards',
                          'Policy Lever (Inputs & Infrastructure) - Monitoring',
                          "Policy Lever (School Management) - Clarity of Functions", 
                          'Policy Lever (School Management) - Attraction' ,                   
                          'Policy Lever (School Management) - Selection & Deployment'  ,      
                          'Policy Lever (School Management) - Support' ,                      
                          'Policy Lever (School Management) - Evaluation'    , 
                          'Politics & Bureaucratic Capacity - National Learning Goals' ,
                          'Politics & Bureaucratic Capacity - Mandates & Accountability'   ,  
                          'Politics & Bureaucratic Capacity - Quality of Bureaucracy'    ,    
                          'Politics & Bureaucratic Capacity - Impartial Decision-Making'    
                          
                          
) 


labels_df_2<-data.frame(indicators=as.character(indicators_list),
                        indicator_labels=as.character(main_indicator_labels2))
##########################
#summary statistics table
#########################


#Build list of dataframes to append together
df_student_proficient <- assess_4th_grade_anon_anon %>% 
  mutate(student_proficient=100*as.numeric(student_knowledge>=86.6)) %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(g4_stud_weight_component),ipw, ipw*g4_stud_weight_component)) %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(student_proficient, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='student_proficient')

#student attendance
df_student_attendance <- school_dta_short_anon %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(student_attendance, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='student_attendance')

#teacher absence
df_presence_rate <- school_dta_short_anon %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(abs_weight_component),ipw, ipw*abs_weight_component)) %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(presence_rate, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='presence_rate')

#content knowledge
df_content_proficiency <- teacher_assessment_dta_anon %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(teacher_weight_component),ipw, ipw*teacher_weight_component)) %>% 
  mutate(STRATUM=if_else((STRATUM==24|is.na(STRATUM)),17,as.numeric(STRATUM)),
         hashed_school_code=if_else(is.na(hashed_school_code),'abcde',hashed_school_code),
         ipw=if_else(is.na(ipw), median(ipw, na.rm=T), ipw),
         content_knowledge=if_else(is.na(literacy_content_knowledge), math_content_knowledge, literacy_content_knowledge),
         content_proficiency=100*as.numeric(content_knowledge>=80)) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(content_proficiency, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='content_proficiency')

#TEACH
df_teach_prof <- school_dta_short_anon  %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(teacher_weight_component),ipw, ipw*teacher_weight_component)) %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(teach_prof, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='teach_prof')

#1st grade assessment
df_ecd_student_proficiency <- ecd_dta_anon_anon   %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(g1_stud_weight_component),ipw, ipw*g1_stud_weight_component)) %>% 
  mutate(STRATUM=if_else((STRATUM==24|STRATUM==5|STRATUM==22),17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(ecd_student_proficiency, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='ecd_student_proficiency')

#inputs
df_inputs <- school_dta_short_anon %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(inputs, vartype = "ci", na.rm=T))  %>%
  mutate(indicators='inputs')

#infrastructure
df_infrastructure  <- school_dta_short_anon  %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(infrastructure, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='infrastructure')

#operational management
df_operational_management   <- school_dta_short_anon  %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(operational_management, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='operational_management')

#instructional leadership
df_instructional_leadership  <- school_dta_short_anon  %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(instructional_leadership, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='instructional_leadership')

#principal knowledge
df_principal_knowledge_score  <- school_dta_short_anon  %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(principal_knowledge_score, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='principal_knowledge_score')

#principal management
df_principal_management  <- school_dta_short_anon  %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(principal_management, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='principal_management')

#teacher attraction
df_teacher_attraction  <- school_dta_short_anon %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(teacher_weight_component),ipw, ipw*teacher_weight_component)) %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(teacher_attraction, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='teacher_attraction')

#teacher_selection_deployment
df_teacher_selection_deployment  <- school_dta_short_anon %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(teacher_weight_component),ipw, ipw*teacher_weight_component)) %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(teacher_selection_deployment, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='teacher_selection_deployment')

#teacher_support
df_teacher_support  <- school_dta_short_anon %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(teacher_weight_component),ipw, ipw*teacher_weight_component)) %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(teacher_support, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='teacher_support')

#teaching_evaluation
df_teaching_evaluation  <- school_dta_short_anon %>% 
  left_join(school_weights_anon ) %>%
  mutate(ipw=if_else(is.na(teacher_weight_component),ipw, ipw*teacher_weight_component)) %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(teaching_evaluation, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='teaching_evaluation')

#teacher_monitoring
df_teacher_monitoring  <- school_dta_short_anon %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(teacher_weight_component),ipw, ipw*teacher_weight_component)) %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(teacher_monitoring, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='teacher_monitoring')

#intrinsic_motivation
df_intrinsic_motivation  <- school_dta_short_anon %>% 
  left_join(school_weights_anon ) %>% 
  mutate(ipw=if_else(is.na(teacher_weight_component),ipw, ipw*teacher_weight_component))  %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(intrinsic_motivation, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='intrinsic_motivation')

#standards_monitoring
df_standards_monitoring  <- school_dta_short_anon %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(standards_monitoring, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='standards_monitoring')

#sch_monitoring
df_sch_monitoring  <- school_dta_short_anon %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(sch_monitoring, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='sch_monitoring')

#sch_management_clarity
df_sch_management_clarity  <- school_dta_short_anon %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(sch_management_clarity, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='sch_management_clarity')

#sch_management_attraction
df_sch_management_attraction  <- school_dta_short_anon  %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(sch_management_attraction, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='sch_management_attraction')

#sch_selection_deployment
df_sch_selection_deployment  <- school_dta_short_anon %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(sch_selection_deployment, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='sch_selection_deployment')

#sch_support
df_sch_support  <- school_dta_short_anon %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(sch_support, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='sch_support')

# principal_evaluation
df_principal_evaluation <- school_dta_short_anon %>% 
  mutate(STRATUM=if_else(STRATUM==24,17,as.numeric(STRATUM))) %>%
  as_survey_design(ids = hashed_school_code, strata=STRATUM, weights=ipw ) %>%
  summarise(mean = survey_mean(principal_evaluation, vartype = "ci", na.rm=T)) %>%
  mutate(indicators='principal_evaluation')


#create combined dataset
if (exists('sumstats_school_df')) {
  rm('sumstats_school_df')
}

for (i in indicators_list) {
  if (exists(paste('df',i, sep="_"))) {
    #form temp data frame with each  data
   
    if (!exists('sumstats_school_df')) {
      temp <- get(paste('df',i, sep="_")) 
    
      sumstats_school_df<-temp
    } else {
    temp <- get(paste('df',i, sep="_")) 
    sumstats_school_df <- sumstats_school_df %>%
      bind_rows(temp)
    }
  } 
}

  # School Survey
  metadata<-metadta
  
  

  
  #add variable label
  sumstats_school_df <- sumstats_school_df %>%
    left_join(labels_df_2) %>%
    mutate(varlabel=indicator_labels) %>%
    mutate(ci=paste("[",round(mean_low,2),", ", round(mean_upp,2),"]", sep="")) %>%
    select(varlabel, mean, ci)
  
  #Now do breakdown by Urban/Rural
  #urban
  sumstats_school_urban <- school_dta_short_anon %>%
    filter(rural==FALSE) 
  
  
  sch_ipw<-sumstats_school_urban$ipw 
  
  sumstats_school_urban <- sumstats_school_urban %>%
    select(one_of(indicators_list)) 
  
  
  
  sumstats_school_urban_df<-my_skim(sumstats_school_urban) %>%
    yank("numeric") %>%
    mutate(variable=skim_variable) %>%
    select(variable, mean, sd, p0, p25, p50, p75, p100, complete,  hist) 
  
  
  #add variable label
  sumstats_school_urban_df <- sumstats_school_urban_df %>%
    mutate(name=variable,
           indicators=variable) %>%
    left_join(labels_df_2) %>%
    mutate(varlabel=indicator_labels) %>%
    mutate(ci_low=as.numeric(mean)-1.96*(as.numeric(sd)/sqrt(as.numeric(complete))),
           ci_high=as.numeric(mean)+1.96*(as.numeric(sd)/sqrt(as.numeric(complete)))) %>%
    mutate(ci=paste("[",round(ci_low,2),", ", round(ci_high,2),"]", sep="")) %>%
    mutate(mean_urban=mean,
           ci_urban=ci) %>%
    select(varlabel, mean_urban, ci_urban)
  
  #rural
  sumstats_school_rural <- school_dta_short_anon  %>%
    filter(rural==TRUE) 
  
  
  sch_ipw<-sumstats_school_rural$ipw 
  
  sumstats_school_rural <- sumstats_school_rural %>%
    select(one_of(indicators_list)) 
  
  
  sumstats_school_rural_df<-my_skim(sumstats_school_rural) %>%
    yank("numeric") %>%
    mutate(variable=skim_variable) %>%
    select(variable, mean, sd, p0, p25, p50, p75, p100, complete,  hist) 
  
  
  #add variable label
  sumstats_school_rural_df <- sumstats_school_rural_df %>%
    mutate(name=variable,
           indicators=variable) %>%
    left_join(labels_df_2) %>%
    mutate(varlabel=indicator_labels) %>%
    mutate(ci_low=as.numeric(mean)-1.96*(as.numeric(sd)/sqrt(as.numeric(complete))),
           ci_high=as.numeric(mean)+1.96*(as.numeric(sd)/sqrt(as.numeric(complete)))) %>%
    mutate(ci=paste("[",round(ci_low,2),", ", round(ci_high,2),"]", sep="")) %>%
    mutate(mean_rural=mean,
           ci_rural=ci) %>%
    select(varlabel, mean_rural, ci_rural)
  
  #now bind urban/rural with the main results
  sumstats_school_df <- sumstats_school_df %>%
    left_join(sumstats_school_urban_df) %>%
    left_join(sumstats_school_rural_df)
  
  
  
  #Survey of Public Officials
  metadata<-public_officials_metadata
  
  #add function to produce weighted summary stats
  my_skim<-    skim_with( numeric = sfl( mean = ~ mean(.,   na.rm=TRUE),
                                         sd = ~ sqrt(var(.,   na.rm=TRUE)),
                                         p25 = ~ (quantile(., probs=c(0.25),   na.rm=TRUE)),
                                         p50 = ~ (quantile(., probs=c(0.5),  na.rm=TRUE)),
                                         p75 = ~ (quantile(., probs=c(0.75),  na.rm=TRUE)),
                                         complete = ~ sum(!is.na(.))))    
  
  
  sumstats_public_officials <- public_officials_dta_clean_anon %>%
    select(one_of(indicators_list) ) 
  
  
  
  sumstats_public_officials_df<-my_skim(sumstats_public_officials) %>%
    yank("numeric") %>%
    mutate(variable=skim_variable) %>%
    select(variable, mean, sd, p0, p25, p50, p75, p100, complete,  hist) 
  
  
  #add variable label
  sumstats_public_officials_df <- sumstats_public_officials_df %>%
    mutate(name=variable,
           indicators=variable) %>%
    left_join(labels_df_2) %>%
    mutate(varlabel=indicator_labels) %>%
    mutate(ci_low=as.numeric(mean)-1.96*(as.numeric(sd)/sqrt(as.numeric(complete))),
           ci_high=as.numeric(mean)+1.96*(as.numeric(sd)/sqrt(as.numeric(complete)))) %>%
    mutate(ci=paste("[",round(ci_low,2),", ", round(ci_high,2),"]", sep="")) %>%
    mutate(mean_urban=as.numeric(NA),
           ci_urban=as.numeric(NA),
           mean_rural=as.numeric(NA),
           ci_rural=as.numeric(NA)) %>%
    select(varlabel, mean, ci, mean_urban, ci_urban, mean_rural, ci_rural)
  
  
  sumstats_df <- sumstats_school_df %>%
    bind_rows(sumstats_public_officials_df) %>%
    arrange(factor(varlabel, levels=main_indicator_labels2))
  
  sumstats_df <- sumstats_df %>%
    inner_join(indicator_choices, by=c('varlabel'='Indicator.Name')) 
  
  sumstats_df <- sumstats_df %>%
    select(varlabel, Value, mean, ci, mean_urban, ci_urban, mean_rural, ci_rural)
  
  #add in custom column sub-headers
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th( rowspan = 2, 'Indicator'),
        th( rowspan = 2, 'Value Range'),
        th(colspan = 2, 'Overall'),
        th(colspan = 2, 'Urban'),
        th(colspan = 2, 'Rural'),
        th(rowspan = 2, str_wrap('Ratio of Rural to Urban',10))
      ),
      tr(
        lapply(rep(c('Mean', '95% Confident Interval'), 3), th)
      )
    )
  ))
  
  # create 19 breaks and 20 rgb color values ranging from white to red
  
  sumstats_df <- sumstats_df %>%
    mutate(ratio=(as.numeric(mean_rural))/as.numeric(mean_urban))
  
  brks <- seq(0, max(sumstats_df$ratio, na.rm=T), length.out = 19)
  clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  
  DT::datatable(sumstats_df, caption="Summary Statistics of Dashboard Indicators - Peru 2019",
                container = sketch, rownames=FALSE,
                class='cell-border stripe',
                escape = FALSE,
                extensions = c ('Buttons', 'FixedHeader'), 
                options=list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 60,
                  scrollX = TRUE, 
                  paging=FALSE,
                  ordering=F)) %>%
    formatRound(columns = c('mean', 'ci', 'mean_urban', 'ci_urban', 'mean_rural', 'ci_rural', 'ratio' ),
                digits=2)  %>% 
    formatStyle('ratio', backgroundColor = styleInterval(brks, clrs))
  
  