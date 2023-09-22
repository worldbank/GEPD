library(tidyverse)
library(haven)
library(readxl)
#score expert data (this requires a lot of hard coding and transcribing)
#set directory to bring in data
if(Sys.info()["user"] == "wb577189"){
  
  expert_dir<- "C:/Users/wb577189/OneDrive - WBG/GEPD/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_M/Data/Policy_Survey"
  
  
  
} else if (str_to_lower(Sys.info()["user"]) == "wb469649") {
  
  expert_dir<- "C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD/CNT/GAB/GAB_2023_GEPD/GAB_2023_GEPD_v01_M/Data/Policy_Survey/"
  
  
}
#read in data

#define function to help clean this data read in (variable read in as factor, so this fixes this)
read_var <- function(var) {
  as.numeric(as.character(var))
}

# # accessing all the sheets
# sheet = excel_sheets(paste(expert_dir, 'PolicySurvey_Madagascar.xlsx', sep="/"))
# 
# # applying sheet names to dataframe names
# data_frame = lapply(setNames(sheet, sheet),
#                     function(x) read_excel("PolicySurvey_Madagascar.xlsx", sheet=x))
# 
# # attaching all dataframes together
# data_frame = bind_rows(data_frame, .id="Sheet")


#start with teachers
##########################
  expert_dta_teachers <- readxl::read_xlsx(path=paste(expert_dir, 'PolicySurvey - final.xlsx', sep="/"), sheet = 'Enseignants', .name_repair = 'universal') %>% 
  fill(Question..) %>% 
  filter(!is.na(Question))


expert_dta_teachers_shaped<-data.frame(t(expert_dta_teachers[-1])) 
colnames(expert_dta_teachers_shaped) <- expert_dta_teachers$Question..
expert_dta_teachers_shaped <- expert_dta_teachers_shaped[c(1:3),]

#create indicators
expert_dta_teachers_final <- expert_dta_teachers_shaped %>%
  janitor::clean_names(.) %>% 
  rownames_to_column() %>%
  filter(rowname=='Scores') %>%
  select(-rowname)

attr(expert_dta_teachers_final, "variable.labels") <- expert_dta_teachers$Question


#teacher attraction
#starting salary
expert_dta_teachers_final <- expert_dta_teachers_final %>%
  mutate(teacher_attraction=read_var(a4),
         teacher_salary=(300000*12/2469049)) # GDP per capita https://data.worldbank.org/indicator/NY.GDP.PCAP.KN?locations=GA

#teacher selection and deployment
#
expert_dta_teachers_final <- expert_dta_teachers_final %>%
  mutate(criteria_admittance=read_var(a5),
         criteria_become=read_var(a6),
         criteria_transfer=read_var(a7)) %>%
mutate(teacher_selection_deployment=(criteria_admittance+criteria_become+criteria_transfer)/3)

#Teacher Support
expert_dta_teachers_final <- expert_dta_teachers_final %>%
  mutate(practicum=read_var(a8),
         prof_development=read_var(a9)) %>%
  mutate(teacher_support=case_when(
    practicum+prof_development==0 ~ 1,
    practicum+prof_development==1 ~ 3,
    practicum+prof_development==2 ~ 5,
  ))
  
#Teacher Evaluation
expert_dta_teachers_final <- expert_dta_teachers_final %>%
  mutate(evaluation_law=read_var(a11),
         evaluation_law_school=read_var(a12),
         evaluation_criteria=read_var(a13)/5,
         negative_evaluations=read_var(a15),
         positive_evaluations=read_var(a17)) %>%
  mutate(teaching_evaluation=1+evaluation_law/4 + evaluation_law_school/4+evaluation_criteria/2+
           negative_evaluations+positive_evaluations) 

#Teacher Monitoring
expert_dta_teachers_final <- expert_dta_teachers_final %>%
  mutate(absence_collected=read_var(a1),
         attendance_rewarded=read_var(a3)) %>%
  mutate(teacher_monitoring=case_when(
    absence_collected+attendance_rewarded==0 ~ 1,
    absence_collected+attendance_rewarded==1 ~ 3,
    absence_collected+attendance_rewarded==2 ~ 5,
  ))
  
#Teacher Intrinsic Motivation
#based on whether or not probationary period
expert_dta_teachers_final <- expert_dta_teachers_final %>%
  mutate(probationary_period=read_var(a18)) %>%
  mutate(intrinsic_motivation=1+4*probationary_period)
  
##############################
# Inputs
##############################
expert_dta_inputs <- readxl::read_xlsx(path=paste(expert_dir, 'PolicySurvey - final.xlsx', sep="/"), sheet = 'Intrants', .name_repair = 'universal') %>% 
  fill(`Question..`) %>%
  filter(!is.na(Question)) 
  


expert_dta_inputs_shaped<-data.frame(t(expert_dta_inputs[-1]))
colnames(expert_dta_inputs_shaped) <- expert_dta_inputs$Question..
expert_dta_input_shaped <- expert_dta_inputs[c(1:3),]


#create indicators
expert_dta_inputs_final <- expert_dta_inputs_shaped %>%
  janitor::clean_names(.) %>% 
  rownames_to_column() %>%
  filter(rowname=='Scores') %>%
  select(-rowname)

attr(expert_dta_inputs_final, "variable.labels") <- expert_dta_inputs$Question

#Inputs Standards
expert_dta_inputs_final<-expert_dta_inputs_final %>%
  mutate(textbook_policy=read_var(b1),
         materials_policy=read_var(b2),
         connectivity_program=read_var(b3),
         electricity_policy=read_var(b4),
         water_policy=read_var(b5),
         toilet_policy=read_var(b6),
         disability_policy=read_var(b7)) %>%
  mutate(inputs_standards=1+
           (textbook_policy+materials_policy)/2+
           (connectivity_program+electricity_policy)/2+
           (water_policy+toilet_policy)/2 +
           disability_policy)

##############################
# School Management
###############################
expert_dta_school_management <- readxl::read_xlsx(path=paste(expert_dir, 'PolicySurvey - final.xlsx', sep="/"), sheet = 'Management ecole', .name_repair = 'universal') %>% 
  fill(Question..) %>% 
  filter(!is.na(Question))


expert_dta_school_management_shaped<-data.frame(t(expert_dta_school_management[-1]))
colnames(expert_dta_school_management_shaped) <- expert_dta_school_management$Question..

#create indicators
expert_dta_school_management_final <- expert_dta_school_management_shaped %>%
  janitor::clean_names(.) %>% 
  rownames_to_column() %>%
  filter(rowname=='Scores') %>%
  select(-rowname)

attr(expert_dta_school_management_final, "variable.labels") <- expert_dta_school_management$Question

#school management clarity

expert_dta_school_management_final <- expert_dta_school_management_final %>%
  mutate(infrastructure_scfn=read_var(c1),
         materials_scfn=read_var(c1_2),
         hiring_scfn=read_var(c1_3),
         supervision_scfn=read_var(c1_4),
         student_scfn=read_var(c1_5),
         principal_hiring_scfn=read_var(c1_6),
         principal_supervision_scfn=read_var(c1_7)
  ) %>%
  mutate(sch_management_clarity=1+
           (infrastructure_scfn+materials_scfn)/2+
           (hiring_scfn + supervision_scfn)/2 +
           student_scfn +
           (principal_hiring_scfn+ principal_supervision_scfn)/2
  )


#school management attraction
expert_dta_school_management_final <- expert_dta_school_management_final %>%
  mutate(professionalized=read_var(c3)) %>%
  mutate(sch_management_attraction=1+4*professionalized)

##### School School Management Selection and Deployment
expert_dta_school_management_final <- expert_dta_school_management_final %>%
  mutate(principal_rubric=read_var(c4),
         principal_factors=read_var(c5)) %>%
  mutate(sch_selection_deployment=1+principal_rubric+(3/5)*principal_factors)
  
# school management support
expert_dta_school_management_final <- expert_dta_school_management_final %>%
  mutate(principal_training_required=read_var(c8),
         principal_training_type=read_var(c9),
         principal_training_type1=1, #do this manual based on responses
         principal_training_type2=1, #do this manual based on responses
         principal_training_type3=0, #do this manual based on responses
         principal_training_frequency=read_var(c10),
         principal_training_frequency_2=4 #do this manual based on responses
         ) %>%
  mutate(sch_support=1+principal_training_required+2*principal_training_type/3+
           principal_training_frequency)

# school management evaluation
expert_dta_school_management_final <- expert_dta_school_management_final %>%
  mutate(principal_monitor_law=read_var(c6),
         principal_monitor_criteria=read_var(c7)) %>%
  mutate(principal_evaluation=1+principal_monitor_law+principal_monitor_criteria)

################################
# Learners 
################################
expert_dta_learners <- readxl::read_xlsx(path=paste(expert_dir, 'PolicySurvey - final.xlsx', sep="/"), sheet = 'Eleves', .name_repair = 'universal') %>% 
fill(Question..) %>% 
filter(!is.na(Question))


expert_dta_learners_shaped<-data.frame(t(expert_dta_learners[-1]))

colnames(expert_dta_learners_shaped) <- expert_dta_learners$Question..

#create indicators
expert_dta_learners_final <- expert_dta_learners_shaped %>%
  janitor::clean_names(.) %>% 
  rownames_to_column() %>%
  filter(rowname=='Scores') %>%
  select(-rowname)

attr(expert_dta_learners_final, "variable.labels") <- expert_dta_learners$Question

#nutrition
expert_dta_learners_final <- expert_dta_learners_final %>%
  mutate(iodization=read_var(d1)/4,
         iron_fortification=read_var(d2)/4,
         breastfeeding=read_var(d3),
         school_feeding=read_var(d5)) %>%
  mutate(nutrition_programs=1+iodization + iron_fortification + breastfeeding + school_feeding)

#health programs
expert_dta_learners_final <- expert_dta_learners_final %>%
  mutate(immunization=read_var(d6),
         healthcare_young_children=read_var(d7),
         deworming=read_var(d8),
         antenatal_skilled_delivery=read_var(d9)-1) %>%
  mutate(health_programs=1+4/3*(immunization + healthcare_young_children + 0.5*antenatal_skilled_delivery))


#ECE programs
expert_dta_learners_final <- expert_dta_learners_final %>%
  mutate(pre_primary_free_some=read_var(d10),
         developmental_standards=read_var(d11),
         ece_qualifications=read_var(d12)-1,
         ece_in_service=read_var(d13)) %>%
  mutate(ece_programs=1+pre_primary_free_some + developmental_standards + ece_qualifications/3 + ece_in_service)

# financial capacity
expert_dta_learners_final <- expert_dta_learners_final %>%
  mutate(anti_poverty=read_var(d16)) %>%
  mutate(financial_capacity=1+2*anti_poverty)

# caregiver skills
expert_dta_learners_final <- expert_dta_learners_final %>%
  mutate(good_parent_sharing=read_var(d14),
         promote_ece_stimulation=read_var(d15)) %>%
  mutate(caregiver_skills=1+2*good_parent_sharing+promote_ece_stimulation)


################################
#trim to just important variables
##############################
#school management
#school_management_drop<-expert_dta_school_management$Question..
expert_dta_school_management_final <- expert_dta_school_management_final %>%
  select(-starts_with("C", ignore.case=FALSE))

  #select(-all_of(school_management_drop))

#inputs
#inputs_drop<-expert_dta_inputs$Question..
expert_dta_inputs_final <- expert_dta_inputs_final %>%
  select(-starts_with("B", ignore.case=FALSE))
#select(-all_of(inputs_drop))

#teachers

#teachers_drop<-expert_dta_teachers$Question..
expert_dta_teachers_final <- expert_dta_teachers_final %>%
  select(-starts_with("A", ignore.case=FALSE))

  #select(-all_of(teachers_drop))

#learners

#learners_drop<-expert_dta_learners$Question..
expert_dta_learners_final <- expert_dta_learners_final %>%
  select(-starts_with("D", ignore.case=FALSE))

  #select(-all_of(learners_drop))


expert_dta_final<-expert_dta_teachers_final %>%
  bind_cols(expert_dta_inputs_final) %>%
  bind_cols(expert_dta_school_management_final) %>%
  bind_cols(expert_dta_learners_final) %>%
  #select(-A11.1) %>%
  mutate(group="De Jure") 

write_dta(expert_dta_final,path=paste(expert_dir, 'expert_dta_final.dta', sep="/"))


