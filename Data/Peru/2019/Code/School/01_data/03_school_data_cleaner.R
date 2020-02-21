#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries

library(skimr)
library(naniar)
library(vtable)
library(digest)
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
#NOTE:  The R script to pull the data from the API should be run before this file



#Create function to save metadata for each question in each module
#The attr function retrieves metadata imported by haven. E.g. attr(school_dta$m1s0q2_code, "label")
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}







############################
#read in teacher roster file
############################

teacher_roster<-read_dta(file.path(download_folder, "questionnaire_selected.dta")) %>%
  mutate(teacher_name=m2saq2,
         teacher_number=questionnaire_selected__id)

###########################
#read in school level file
###########################
school_dta<-read_dta(file.path(download_folder, "EPDash_v2.dta"))
vtable(school_dta)
#rename a few key variables up front
school_dta<- school_dta %>%
  mutate(enumerator_name_other= m1s0q1_name_other  ,
         enumerator_number=if_else(!is.na(m1s0q1_name),m1s0q1_name, as.double(m1s0q1_number_other)) ,
         survey_time=m1s0q8,
         lat=m1s0q9__Latitude,
         lon=m1s0q9__Longitude,
         school_code=if_else(!is.na(school_code_preload),as.double(school_code_preload), as.double(m1s0q2_code)),
         m7_teach_count_pknw=m7_teach_count, #this variable was mistakenly not tagged as pknw
         total_enrolled=m1saq7) %>%
  mutate(school_code=if_else(school_code==0, 328328, school_code)) %>%
  mutate(school_code=if_else(school_code==62181, 558163, school_code))  #fix an error where the school code was loaded incorrectly

#create school metadata frame
school_metadta<-makeVlist(school_dta)

#Read in list of indicators
indicators <- read_delim(here::here('Indicators','indicators.md'), delim="|", trim_ws=TRUE)

indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)

#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- indicators$indicator_tag

#list additional info that will be useful to keep in each indicator dataframe
preamble_info <- c( 'interview__id', 'school_code',
                    'school_name_preload', 'school_address_preload', 
                    'school_province_preload', 'school_district_preload', 'school_code_preload', 'school_emis_preload',
                    'school_info_correct', 'm1s0q2_name', 'm1s0q2_code', 'm1s0q2_emis',
                    'survey_time', 'lat', 'lon', 'total_enrolled' , 'm7saq8', 'm7saq10'
)

drop_school_info <- c(
  
)

#Create a list of info to drop from final teacher files aggregated to school level. This will be necessary for merging these databases later

drop_teacher_info <- c( "questionnaire_roster__id", "teacher_name", "teacher_number", "available", 
                        "teacher_position", "teacher_grd1", "teacher_grd2", "teacher_grd3", "teacher_grd4", 
                        "teacher_grd5", "teacher_language", "teacher_math", "teacher_both_subj", "teacher_other_subj", 
                        "teacher_education", "teacher_year_began", "teacher_age" )


#create school database with just preamble info.  This will be useful for merging on school level info to some databases
school_data_preamble_temp <- school_dta %>%
  group_by(school_code) %>%
  select( preamble_info) %>%
  select(-interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 

school_data_preamble <- school_dta %>%
  group_by(interview__id) %>%
  select(interview__id, school_code) %>%
  left_join(school_data_preamble_temp)

#use dplyr select(contains()) to search for variables with select tags and create separate databases by indicator
#This will make the information for each indicator contained in an independent database
#Will need to join the school level information with teacher level questionnaire information for some indicators.  This will be done later.
for (i in indicator_names ) {
  temp_df<-school_dta %>%
    select( contains(i))
  if (ncol(temp_df) > 0) {
    temp_df<-school_dta %>%
      select(preamble_info, contains(i))
    assign(paste("school_data_",i, sep=""), temp_df )
  }
}




#########################################
#read in teacher questionnaire level file
#########################################
teacher_questionnaire<-read_dta(file.path(download_folder, "questionnaire_roster.dta"))
teacher_questionnaire_metadta<-makeVlist(teacher_questionnaire)



#Add school preamble info
teacher_questionnaire <- teacher_questionnaire %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything())


#filter out teachers who did not consent to interview

teacher_questionnaire <- teacher_questionnaire %>%
  filter(m3s0q1==1)

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



#rename a few key variables up front
teacher_questionnaire<- teacher_questionnaire %>%
  mutate(teacher_name=m3sb_troster  ,
         teacher_number=m3sb_tnumber ,
         available=m3s0q1,
         teacher_position=m3saq1,
         teacher_grd1=bin_var(m3saq2__1,1),
         teacher_grd2=bin_var(m3saq2__2,1),
         teacher_grd3=bin_var(m3saq2__3,1),
         teacher_grd4=bin_var(m3saq2__4,1),
         teacher_grd5=bin_var(m3saq2__5,1),
         teacher_language=bin_var(m3saq3__1,1),
         teacher_math=bin_var(m3saq3__2,1),
         teacher_both_subj=bin_var(m3saq3__3,1),
         teacher_other_subj=bin_var(m3saq3__97,1),
         teacher_education=m3saq4,
         teacher_year_began=m3saq5,
         teacher_age=m3saq6,
  )

teacher_questionnaire<- teacher_questionnaire %>%
  mutate(temp=rowSums(select(.,teacher_grd1, teacher_grd2, teacher_grd3, teacher_grd4, teacher_grd5))) %>% # here we count the number of grades present in the classroom
  mutate(multigrade=case_when(temp > 1 ~ 1,
                              TRUE ~ 0)) %>% #this variable is equal to 1 if there are several grades and zero otherwise
  select(-temp)


#  We create a unique grade variable
teacher_questionnaire <- teacher_questionnaire %>%
  mutate(grade=case_when(teacher_grd1==1 & multigrade!=1 ~ 1,
                         teacher_grd2==1 & multigrade!=1 ~ 2,
                         teacher_grd3==1 & multigrade!=1 ~ 3,
                         teacher_grd4==1 & multigrade!=1 ~ 4,
                         teacher_grd5==1 & multigrade!=1 ~ 5,
                         multigrade==1 ~ 6,
                         TRUE ~ NA_real_)) %>%
  mutate(grade=factor(grade, levels=c(1,2,3,4,5,6), labels=c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Multigrade")))

label(teacher_questionnaire$grade) <- "Grade"



#list additional info that will be useful to keep in each indicator dataframe
preamble_info_teacher <- c('interview__id', 'questionnaire_roster__id', 'teacher_name', 'teacher_number', 'm7saq10',
                           'available', 'teacher_position', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 'teacher_grd5',
                           'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'teacher_education', 'teacher_year_began',
                           'teacher_age')



#use dplyr select(contains()) to search for variables with select tags and create separate databases
for (i in indicator_names ) {
  temp_df<-teacher_questionnaire %>%
    select( contains(i))
  if (ncol(temp_df) > 0) {
    temp_df<-teacher_questionnaire %>%
      select(school_code, preamble_info_teacher, contains(i))
    assign(paste("teacher_questionnaire_",i, sep=""), temp_df )
  }
}



#############################################
##### Teacher Absence ###########
#############################################

# School survey. Percent of teachers absent. Teacher is coded absent if they are: 
#   - not in school 
# - in school but absent from the class 


bin_var <- function(var, val) {
  case_when(
    var==val  ~ 1,
    var!=val   ~ 0,
    is.na(var) ~ as.numeric(NA))
}



bin_var_2 <- function(var, val) {
  if_else(var==val, 
          as.numeric(1),
          as.numeric(var))
}

#read in teacher absence file
teacher_absence_dta<-read_dta(file.path(download_folder, "questionnaire_selected.dta"))
teacher_absence_metadta<-makeVlist(teacher_absence_dta)

#Add school preamble info
teacher_absence_dta <- teacher_absence_dta %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything()) 



#number missing
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(n_mssing_EFFT=n_miss_row(.))

#rename a few key variables up front
teacher_absence_dta<- teacher_absence_dta %>%
  mutate(teacher_name=m2saq2  ,
         teacher_number=questionnaireteachcode ,
         teacher_position=m2saq4,
         teacher_permanent=bin_var(m2saq5,1),
         teacher_contract=bin_var(m2saq5,2),
         teacher_temporary=bin_var(m2saq5,3),
         teacher_volunteer=bin_var(m2saq5,4),
         teacher_ngo=bin_var(m2saq5,5)==5,
         teacher_other=case_when(
           m2saq5==97 ~ m2saq5_other,
           m2saq5!=97 ~ "NA"
         ),
         teacher_fulltime=bin_var(m2saq6,1),
         teacher_male=bin_var(m2saq3,1),
         teacher_grd1=bin_var(m2saq7__1,1),
         teacher_grd2=bin_var(m2saq7__2,1),
         teacher_grd3=bin_var(m2saq7__3,1),
         teacher_grd4=bin_var(m2saq7__4,1),
         teacher_grd5=bin_var(m2saq7__5,1),
         teacher_language=bin_var(m2saq8__1,1),
         teacher_math=bin_var(m2saq8__2,1),
         teacher_both_subj=bin_var(m2saq8__3,1),
         teacher_other_subj=bin_var(m2saq8__97,1),
  )

teacher_absence_dta<- teacher_absence_dta %>%
  mutate(temp=rowSums(select(.,teacher_grd1, teacher_grd2, teacher_grd3, teacher_grd4, teacher_grd5))) %>% # here we count the number of grades present in the classroom
  mutate(multigrade=case_when(temp > 1 ~ 1,
                              TRUE ~ 0)) %>% #this variable is equal to 1 if there are several grades and zero otherwise
  select(-temp)

#  We create a unique grade variable

teacher_absence_dta <- teacher_absence_dta %>%
  mutate(grade=case_when(teacher_grd1==1 & multigrade!=1 ~ 1,
                         teacher_grd2==1 & multigrade!=1 ~ 2,
                         teacher_grd3==1 & multigrade!=1 ~ 3,
                         teacher_grd4==1 & multigrade!=1 ~ 4,
                         teacher_grd5==1 & multigrade!=1 ~ 5,
                         multigrade==1 ~ 6,
                         TRUE ~ NA_real_)) %>%
  mutate(grade=factor(grade, levels=c(1,2,3,4,5,6), labels=c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Multigrade")))

label(teacher_absence_dta$grade) <- "Grade"

#list additional info that will be useful to keep in each indicator dataframe
preamble_info_absence <- c('interview__id', 'questionnaire_selected__id', 'teacher_name', 'teacher_number',
                           'teacher_position', 'teacher_permanent', 'teacher_contract', 'teacher_temporary', 'teacher_volunteer', 'teacher_ngo', 'teacher_other',
                           'teacher_fulltime', 'teacher_male', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 'teacher_grd5', 'grade',
                           'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'subject_joined', 'm2sbq6_efft')

#create indicator for whether each teacher was absent from school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(sch_absence_rate=100*case_when(
    m2sbq6_efft==6 | teacher_available==2 ~ 1,
    m2sbq6_efft!=6   ~ 0,
    is.na(m2sbq6_efft) ~ as.numeric(NA)))

#create indicator for whether each teacher was absent from classroom or school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(absence_rate=100*case_when(
    m2sbq6_efft==6 | m2sbq6_efft==5 |  teacher_available==2 ~ 1,
    m2sbq6_efft==1 | m2sbq6_efft==3 | m2sbq6_efft==2 | m2sbq6_efft==4  ~ 0,
    is.na(m2sbq6_efft) ~ as.numeric(NA)) )

#create indicator for whether each principal was absent from school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(principal_absence=100*case_when(
    m2sbq3_efft==8  ~ 1,
    m2sbq3_efft!=8   ~ 0,
    is.na(m2sbq3_efft) ~ as.numeric(NA))) %>%
  mutate(absence_rate=if_else(is.na(absence_rate), principal_absence, absence_rate ),
         sch_absence_rate=if_else(is.na(sch_absence_rate), principal_absence, sch_absence_rate ),
         presence_rate=100-absence_rate)


teacher_absence_final<- teacher_absence_dta %>%
  select(preamble_info_absence, contains('absent'))




#Build teacher absence practice indicator
final_indicator_data_EFFT <- teacher_absence_dta %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select( -starts_with('interview'), -starts_with('enumerator'), -c('teacher_name', 'm2saq2'))

#Breakdowns by Male/Female
final_indicator_data_EFFT_M <- teacher_absence_dta %>%
  filter(teacher_male==1) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select( -starts_with('interview'), -starts_with('enumerator'), -c('teacher_name', 'm2saq2'))

final_indicator_data_EFFT_F <- teacher_absence_dta %>%
  filter(teacher_male==0) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select( -starts_with('interview'), -starts_with('enumerator'), -c('teacher_name', 'm2saq2'))

#############################################
##### Student Attendance ###########
#############################################

#Percent of 4th grade students who are present during an unannounced visit.

final_indicator_data_ATTD<- school_data_INPT %>%
  select(preamble_info, m4scq4_inpt, m4scq4_inpt, m4scq12_inpt )  %>%
  mutate(student_attendance=m4scq4_inpt/m4scq12_inpt) %>%
  mutate(student_attendance=if_else(m4scq4_inpt>m4scq12_inpt, m4scq12_inpt/m4scq4_inpt,as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=if_else(student_attendance>1, 1, as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=100*student_attendance) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.)) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))



#Breakdowns by Male/Female

num_boys <- school_dta %>%
  select(interview__id, m4scq4n_girls, m4scq13_girls )

final_indicator_data_ATTD_M<- school_data_INPT %>%
  left_join(num_boys) %>%
  select(preamble_info, m4scq4_inpt, m4scq4_inpt, m4scq4n_girls, m4scq12_inpt, m4scq13_girls )  %>%
  mutate(student_attendance=m4scq4n_girls/m4scq13_girls) %>%
  mutate(student_attendance=if_else(student_attendance>1, 1,as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=100*student_attendance) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.)) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))

final_indicator_data_ATTD_F<- school_data_INPT %>%
  left_join(num_boys) %>%
  select(preamble_info, m4scq4_inpt, m4scq4_inpt, m4scq4n_girls, m4scq12_inpt, m4scq13_girls )  %>%
  mutate(student_attendance=(m4scq4_inpt-m4scq4n_girls)/(m4scq12_inpt-m4scq13_girls)) %>%
  mutate(student_attendance=if_else(student_attendance>1, 1,as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=if_else(student_attendance<0, 0,as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=100*student_attendance) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.)) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))

#############################################
##### Teacher Knowledge ###########
#############################################


# School survey. Fraction correct on teacher assessment. In the future, we will align with SDG criteria for minimum proficiency.

#read in data from difference questionnaire.  This was done because the exams were graded back in the central office.
school_dta_21<-read_dta(file.path(paste(download_folder,'version_21', sep="/"), "EPDash.dta"))

school_dta_21<- school_dta_21 %>%
  mutate(school_code=if_else(!is.na(school_code_preload),as.double(school_code_preload), as.double(m1s0q2_code))
  )

preamble_info_21 <- c('school_code' )

school_data_preamble_21<- school_dta_21 %>%
  select(interview__key, preamble_info_21)

teacher_assessment_dta_21<-read_dta(file.path(paste(download_folder,'version_21', sep="/"), "teacher_assessment_answers.dta"))
teacher_metadata <- makeVlist(teacher_assessment_dta_21)

#Add school preamble info
teacher_assessment_dta <- teacher_assessment_dta_21 %>%
  left_join(school_data_preamble_21) %>%
  select(preamble_info_21, everything()) 


#number missing
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(n_mssing_CONT=n_miss_row(.))





#Drop columns that end in "mistake".  THis is not necessary for computing indicator
teacher_assessment_dta <- teacher_assessment_dta %>% 
  select(-ends_with("mistake"))

#drop the correct the letter
teacher_assessment_dta <- teacher_assessment_dta %>% 
  select(-starts_with("m5s1q3"))

#recode assessment variables to be 1 if student got it correct and zero otherwise
teacher_assessment_dta<- teacher_assessment_dta %>%
  mutate_at(vars(starts_with("m5s1q"), starts_with("m5s2q")), ~bin_var_2(.,2)  ) %>%
  mutate_at(vars(starts_with("m5s1q"), starts_with("m5s2q")), ~bin_var(.,1)  )


#create indicator for % correct on teacher assessment
#Note:  in the future we could incorporate irt program like mirt


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m5s1q, which is the prefix of literacy items
teacher_assessment_dta$literacy_length<-length(grep(x=colnames(teacher_assessment_dta), pattern="m5s1q"))


lit_items<-colnames(teacher_assessment_dta[,grep(x=colnames(teacher_assessment_dta), pattern="m5s1q")])

#calculate teachers lit items correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(literacy_content_knowledge=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q")], na.rm=TRUE),
         correct_letter=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q3")], na.rm=TRUE),
         cloze=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q2")], na.rm=TRUE),
         grammar=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q1")], na.rm=TRUE),
         read_passage=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q4")], na.rm=TRUE))

####Math####
#calculate # of math items
teacher_assessment_dta$math_length<-length(grep(x=colnames(teacher_assessment_dta), pattern="m5s2q"))

math_items<-colnames(teacher_assessment_dta[,grep(x=colnames(teacher_assessment_dta), pattern="m5s2q")])

#calculate teachers math items correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(math_content_knowledge=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s2q")], na.rm=TRUE),
         arithmetic_number_relations=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="number")], na.rm=TRUE),
         geometry=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="geometric")], na.rm=TRUE),
         interpret_data=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="data")], na.rm=TRUE))

#rename a few key variables up front
teacher_assessment_dta<- teacher_assessment_dta %>%
  mutate(g4_teacher_name=m5sb_troster  ,
         g4_teacher_number=m5sb_tnum   )


#pull apart dataset with just domains
teacher_assessment_domains <- teacher_assessment_dta %>%
  dplyr::select(typetest, school_code, literacy_content_knowledge, correct_letter, cloze, grammar, read_passage,
                math_content_knowledge, arithmetic_number_relations, geometry, interpret_data
  )


teacher_assessment_language <-teacher_assessment_dta %>%
  select(typetest, school_code, m5sb_tnum, lit_items) %>%
  filter(typetest==2) %>%
  select(-typetest)

teacher_assessment_math <- teacher_assessment_dta %>%
  select(typetest, school_code, m5sb_tnum, math_items) %>%
  filter(typetest==1) %>%
  select(-typetest)



save(teacher_assessment_language, teacher_assessment_math, teacher_assessment_domains, teacher_metadata, 
     file = file.path(confidential_folder, "dashboard_teacher_assessment_data.RData"))


#calculate % correct for literacy, math, and total
final_indicator_data_CONT <- teacher_assessment_dta %>%
  group_by(school_code) %>%
  add_count(school_code,name='m5_teach_count') %>%
  add_count(typetest,name='m5_teach_count_math') %>%
  mutate(m5_teach_count_math= if_else(typetest==1, as.numeric(m5_teach_count_math), as.numeric(NA))) %>%
  mutate(content_knowledge=case_when(
    (!is.na(math_content_knowledge) & !is.na(literacy_content_knowledge)) ~ (math_content_knowledge+literacy_content_knowledge)/2,
    is.na(math_content_knowledge)  ~ literacy_content_knowledge,
    is.na(literacy_content_knowledge)  ~ math_content_knowledge)) %>%
  mutate(content_proficiency=100*as.numeric(content_knowledge>=80),
         content_proficiency_70=100*as.numeric(content_knowledge>=70),
         content_proficiency_75=100*as.numeric(content_knowledge>=75),
         literacy_content_proficiency=100*as.numeric(literacy_content_knowledge>=80),
         literacy_content_proficiency_70=100*as.numeric(literacy_content_knowledge>=70),
         literacy_content_proficiency_75=100*as.numeric(literacy_content_knowledge>=75),
         math_content_proficiency=100*as.numeric(math_content_knowledge>=80),
         math_content_proficiency_70=100*as.numeric(math_content_knowledge>=70),
         math_content_proficiency_75=100*as.numeric(math_content_knowledge>=75)) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  
  select(-ends_with('length'), -ends_with('items'), -typetest, -starts_with('interview'), -starts_with('enumerator'),
         -starts_with('g4_teacher'), -c('teacher_assessment_answers__id', 'm5sb_troster', 'm5sb_tnum'))

#Breakdown by Male/Female
final_indicator_data_CONT_M <- teacher_assessment_dta %>%
  mutate(questionnaire_selected__id=g4_teacher_number) %>%
  left_join(teacher_absence_dta, by=c('school_code', 'questionnaire_selected__id')) %>%
  filter(m2saq3==1) %>%
  group_by(school_code) %>%
  add_count(school_code,name='m5_teach_count') %>%
  mutate(content_knowledge=case_when(
    (!is.na(math_content_knowledge) & !is.na(literacy_content_knowledge)) ~ (math_content_knowledge+literacy_content_knowledge)/2,
    is.na(math_content_knowledge)  ~ literacy_content_knowledge,
    is.na(literacy_content_knowledge)  ~ math_content_knowledge)) %>%
  mutate(content_proficiency=100*as.numeric(content_knowledge>=80),
         content_proficiency_70=100*as.numeric(content_knowledge>=70),
         content_proficiency_75=100*as.numeric(content_knowledge>=75),
         literacy_content_proficiency=100*as.numeric(literacy_content_knowledge>=80),
         literacy_content_proficiency_70=100*as.numeric(literacy_content_knowledge>=70),
         literacy_content_proficiency_75=100*as.numeric(literacy_content_knowledge>=75),
         math_content_proficiency=100*as.numeric(math_content_knowledge>=80),
         math_content_proficiency_70=100*as.numeric(math_content_knowledge>=70),
         math_content_proficiency_75=100*as.numeric(math_content_knowledge>=75)) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'),
         -starts_with('g4_teacher'), -c('teacher_assessment_answers__id', 'm5sb_troster', 'm5sb_tnum'))

final_indicator_data_CONT_F <- teacher_assessment_dta %>%
  mutate(questionnaire_selected__id=g4_teacher_number) %>%
  left_join(teacher_absence_dta, by=c('school_code', 'questionnaire_selected__id')) %>%
  filter(m2saq3==2) %>%
  group_by(school_code) %>%
  add_count(school_code,name='m5_teach_count') %>%
  mutate(content_knowledge=case_when(
    (!is.na(math_content_knowledge) & !is.na(literacy_content_knowledge)) ~ (math_content_knowledge+literacy_content_knowledge)/2,
    is.na(math_content_knowledge)  ~ literacy_content_knowledge,
    is.na(literacy_content_knowledge)  ~ math_content_knowledge)) %>%
  mutate(content_proficiency=100*as.numeric(content_knowledge>=80),
         content_proficiency_70=100*as.numeric(content_knowledge>=70),
         content_proficiency_75=100*as.numeric(content_knowledge>=75),
         literacy_content_proficiency=100*as.numeric(literacy_content_knowledge>=80),
         literacy_content_proficiency_70=100*as.numeric(literacy_content_knowledge>=70),
         literacy_content_proficiency_75=100*as.numeric(literacy_content_knowledge>=75),
         math_content_proficiency=100*as.numeric(math_content_knowledge>=80),
         math_content_proficiency_70=100*as.numeric(math_content_knowledge>=70),
         math_content_proficiency_75=100*as.numeric(math_content_knowledge>=75)) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'),
         -starts_with('g4_teacher'), -c('teacher_assessment_answers__id', 'm5sb_troster', 'm5sb_tnum'))


#get database for after 2015, when jaime went in office
final_indicator_data_CONT_after_2015 <- teacher_assessment_dta %>%
  mutate(questionnaire_roster__id=g4_teacher_number) %>%
  left_join(teacher_questionnaire, by=c('school_code', 'questionnaire_roster__id')) %>%
  filter(m3saq5>=2015) %>%
  group_by(school_code) %>%
  add_count(school_code,name='m5_teach_count') %>%
  mutate(content_knowledge=case_when(
    (!is.na(math_content_knowledge) & !is.na(literacy_content_knowledge)) ~ (math_content_knowledge+literacy_content_knowledge)/2,
    is.na(math_content_knowledge)  ~ literacy_content_knowledge,
    is.na(literacy_content_knowledge)  ~ math_content_knowledge)) %>%
  mutate(content_proficiency=100*as.numeric(content_knowledge>=80),
         content_proficiency_70=100*as.numeric(content_knowledge>=70),
         content_proficiency_75=100*as.numeric(content_knowledge>=75),
         literacy_content_proficiency=100*as.numeric(literacy_content_knowledge>=80),
         literacy_content_proficiency_70=100*as.numeric(literacy_content_knowledge>=70),
         literacy_content_proficiency_75=100*as.numeric(literacy_content_knowledge>=75),
         math_content_proficiency=100*as.numeric(math_content_knowledge>=80),
         math_content_proficiency_70=100*as.numeric(math_content_knowledge>=70),
         math_content_proficiency_75=100*as.numeric(math_content_knowledge>=75)) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'),
         -starts_with('g4_teacher'), -c('teacher_assessment_answers__id', 'm5sb_troster', 'm5sb_tnum'))




#############################################
##### 4th Grade Assessment ###########
#############################################

#read in 4th grade assessment level file
assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))

#Add school preamble info
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything()) 


#number missing
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(n_mssing_LERN=n_miss_row(.))

#create indicator for % correct on student assessment
#Note:  in the future we could incorporate irt program like mirt


#rename a few key variables up front
assess_4th_grade_dta<- assess_4th_grade_dta %>%
  mutate(student_name=m8s1q1  ,
         student_number=fourth_grade_assessment__id,
         student_age=m8s1q2,
         student_male=bin_var(m8s1q3,1),
  )

# create a function to score questions m8saq2 and m8saq3, in which students identify letters/words that enumerator calls out.
# This question is tricky, because enumerators would not always follow instructions to say out loud the same letters/words
# In order to account for this, will assume if 80% of the class has a the exact same response, then this is the letter/word called out
# Score this so that if there is a deviation from what 80% of the class says, then it is wrong.
call_out_scorer <- function(var, pctl) {
  1-abs(var - quantile(var, pctl, na.rm=T))
}
# #old scoring code:
# mutate(m8saq2_id=rowMeans(select(.,m8saq2_id__3,m8saq2_id__4, m8saq2_id__6), na.rm=TRUE),
#        m8saq3_id=rowMeans(select(.,m8saq3_id__2,m8saq2_id__6, m8saq2_id__7), na.rm=TRUE),
#        m8saq4_id=if_else(m8saq4_id!=99, m8saq4_id/5,0),
#        m8saq7_word_choice=bin_var(m8saq7_word_choice,2),
#        m8sbq1_number_sense=rowMeans(select(.,m8sbq1_number_sense__3,m8sbq1_number_sense__4, m8sbq1_number_sense__1), na.rm=TRUE)) 

#recode assessment variables to be 1 if student got it correct and zero otherwise
assess_4th_grade_dta<- assess_4th_grade_dta %>%
  mutate_at(vars(starts_with("m8saq5"), 
                 starts_with("m8saq6"),
                 starts_with("m8sbq2"),
                 starts_with("m8sbq3"),
                 starts_with("m8sbq4"),
                 starts_with("m8sbq5"),
                 starts_with("m8sbq6"),
  ), ~bin_var(.,1)  ) %>% #now handle the special cases
  mutate(m8saq4_id=if_else(m8saq4_id==5,4,m8saq4_id)) %>% #fix case where some enumerators recorded the pre-filled answer.
  group_by(school_code) %>%
  mutate_at(vars(starts_with("m8saq2_id"),starts_with("m8saq3_id"), starts_with("m8sbq1_number_sense")),
            ~call_out_scorer(.,0.8)) %>%
  ungroup() %>%
  mutate(m8saq2_id=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq2_id")])-7)/3, #subtract some letters not assessed and make out of 3 points
         m8saq3_id=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq3_id")])-7)/3) %>%
  mutate(m8saq4_id=if_else(m8saq4_id!=99, m8saq4_id/4,0),
         m8saq7_word_choice=bin_var(m8saq7_word_choice,2),
         m8sbq1_number_sense=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq1_number_sense")])-7)/3)         %>%
  mutate_at(vars(starts_with("m8saq2_id"),starts_with("m8saq3_id"), starts_with("m8sbq1_number_sense")),
            ~case_when(
              0<=. & .<=1 ~ .,
              .<0 ~0,
              .>1 ~1)) %>%
  select(-starts_with("m8saq2_id__"),-starts_with("m8saq3_id__"),-starts_with("m8sbq1_number_sense__"))


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items
assess_4th_grade_dta$literacy_length<-length(grep(x=colnames(assess_4th_grade_dta), pattern="m8saq"))

lit_items<-colnames(assess_4th_grade_dta[,grep(x=colnames(assess_4th_grade_dta), pattern="m8saq")])


#calculate students lit items correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(literacy_student_knowledge=100*rowMeans(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq")], na.rm=TRUE))

####Math####
#calculate # of math items
assess_4th_grade_dta$math_length<-length(grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq"))

math_items<-colnames(assess_4th_grade_dta[,grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq")])


#calculate students math items correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(math_student_knowledge=100*rowMeans(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq")], na.rm=TRUE))

####Total score####
#calculate students percent correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(student_knowledge=(math_student_knowledge+literacy_student_knowledge)/2) %>%
  mutate(student_proficient=100*as.numeric(student_knowledge>=86.6), #26/30
         student_proficient_70=100*as.numeric(student_knowledge>=70),
         student_proficient_75=100*as.numeric(student_knowledge>=75),
         literacy_student_proficient=100*as.numeric(literacy_student_knowledge>=92), #12/13 points
         literacy_student_proficient_70=100*as.numeric(literacy_student_knowledge>=70),
         literacy_student_proficient_75=100*as.numeric(literacy_student_knowledge>=75),
         math_student_proficient=100*as.numeric(math_student_knowledge>=82), #14/17 points
         math_student_proficient_70=100*as.numeric(math_student_knowledge>=70),
         math_student_proficient_75=100*as.numeric(math_student_knowledge>=75),
         math_student_proficient_60=100*as.numeric(math_student_knowledge>=60))


#save  4th grade data at student level anonymized
assess_4th_grade_anon <- assess_4th_grade_dta %>%
  select(school_code, student_number, student_age, student_male, 
         student_knowledge, math_student_knowledge, literacy_student_knowledge,
         math_items, lit_items)

#Add in assessment metadata
assess_4th_grade_dta_meta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))

assess_4th_grade_metadta<-makeVlist(assess_4th_grade_dta_meta) %>%
  mutate(indicator_tag='LERN' )
# assess_4th_grade_metadata <- makeVlist(assess_4th_grade_dta)


save(assess_4th_grade_anon, assess_4th_grade_metadta, 
     file = file.path(confidential_folder, "dashboard_4th_grade_assessment_data.RData"))





#calculate % correct for literacy, math, and total
final_indicator_data_LERN <- assess_4th_grade_dta %>%
  left_join(school_dta[,c('interview__id', 'm8_teacher_name', 'm8_teacher_code')]) %>%
  group_by(school_code) %>%
  mutate(n_students=n()) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items') , -starts_with('interview'), -starts_with('enumerator'))


#Breakdowns by Male/Female
final_indicator_data_LERN_M <- assess_4th_grade_dta %>%
  left_join(school_dta[,c('interview__id', 'm8_teacher_name', 'm8_teacher_code')]) %>%
  filter(m8s1q3==1) %>%
  group_by(school_code) %>%
  mutate(n_students=n()) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items') , -starts_with('interview'), -starts_with('enumerator'))

final_indicator_data_LERN_F <- assess_4th_grade_dta %>%
  left_join(school_dta[,c('interview__id', 'm8_teacher_name', 'm8_teacher_code')]) %>%
  filter(m8s1q3==2) %>%
  group_by(school_code) %>%
  mutate(n_students=n()) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items') , -starts_with('interview'), -starts_with('enumerator'))


#############################################
##### ECD Assessment ###########
#############################################

# School survey. Fraction correct on the Early Childhoold Assessment given to students in school.


#read in ecd level file
ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))

ecd_dta_17<-read_dta(file.path(paste(download_folder,'version_17', sep="/"), "ecd_assessment.dta"))

ecd_dta_metadata <- makeVlist(ecd_dta_17)


#Add school preamble info
ecd_dta <- ecd_dta %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything())  


#create indicator for % correct on student assessment
#Note:  in the future we could incorporate irt program like mirt
#number of missing values
ecd_dta <- ecd_dta %>%
  mutate(n_mssing_ECD=n_miss_row(.))

#rename a few key variables up front
ecd_dta<- ecd_dta %>%
  mutate(ecd_student_name=m6s1q1  ,
         ecd_student_number=ecd_assessment__id,
         ecd_student_age=m6s1q2,
         ecd_student_male=bin_var(m6s1q3,1),
         ecd_consent=bin_var(m6s1q4,1)
  )

list_topics<-c("vocabn", "comprehension","letters","words","sentence","name_writing","print",
               "countingproduce_set","number_ident","number_compare","simple_add",
               "backward_digit","head_shoulders",
               "perspective","conflict_resol")



#recode ECD variables to be 1 if student got it correct and zero otherwise
ecd_dta<- ecd_dta %>%
  mutate_at(vars(ends_with("comprehension"),
                 ends_with("letters"),
                 ends_with("words"),
                 ends_with("sentence"),
                 ends_with("name_writing"),
                 ends_with("print"),
                 ends_with("produce_set"),
                 ends_with( "number_ident"),
                 ends_with("number_compare"),
                 ends_with("simple_add"),
                 ends_with("backward_digit"),
                 ends_with("perspective"),
                 ends_with("conflict_resol")), ~bin_var(.,1)  ) %>%
  mutate_at(vars(ends_with("head_shoulders")), ~if_else(.x==2,1,0,missing=NULL)) %>%
  mutate_at(vars(ends_with("vocabn")), ~case_when(.x==98 ~ as.numeric(NA),
                                                  .x==99 ~ 0,
                                                  .x==77 ~ 0,
                                                  (.x!=98 & .x!=99 & .x>=10) ~ 1,
                                                  (.x!=98 & .x!=99 & .x<10) ~ as.numeric(.x)/10,
                                                  is.na(.x) ~ as.numeric(NA))) %>%
  mutate_at(vars(ends_with("counting")), ~case_when(.x==98 ~ as.numeric(NA),
                                                    .x==99 ~ 0,
                                                    .x==77 ~ 0,
                                                    (.x!=98 & .x!=99 & .x>=30) ~ 1,
                                                    (.x!=98 & .x!=99 & .x<30) ~ as.numeric(.x)/30,
                                                    is.na(.x) ~ as.numeric(NA)))



####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items


lit_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "vocabn|comprehension|letters|words|sentence|name_writing|print")])

ecd_dta$literacy_length<-length(lit_items)

#calculate students lit items correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_literacy_student_knowledge=100*rowMeans(.[grep(x=colnames(ecd_dta), 
                                                            pattern="vocabn|comprehension|letters|words|sentence|name_writing|print")], na.rm=TRUE))

####Math####
#calculate # of math items

math_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "counting|produce_set|number_ident|number_compare|simple_add")])

ecd_dta$math_length<-length(math_items)


#calculate students math items correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_math_student_knowledge=100*rowMeans(.[grep(x=colnames(ecd_dta), 
                                                        pattern="counting|produce_set|number_ident|number_compare|simple_add")], na.rm=TRUE))

####Executive Functioning####
#calculate # of Exec Function items

exec_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "backward_digit|head_shoulders")])

ecd_dta$exec_length<-length(exec_items)


#calculate students excec items correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_exec_student_knowledge=100*rowMeans(.[grep(x=colnames(ecd_dta), 
                                                        pattern="backward_digit|head_shoulders")], na.rm=TRUE))

####Socio-Emotional####
#calculate # of Exec Function items

#NOTE:  Ending persepectives and conflict resolution in $ is a grep trick to make sure columns with training characters aren't included.  
#This means specifically the perspetive_responses conflict_resol_responses columns, which are text and just for quality control.
soc_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "perspective$|conflict_resol$")])

ecd_dta$soc_length<-length(soc_items)


#calculate students excec items correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_soc_student_knowledge=100*rowMeans(.[grep(x=colnames(ecd_dta), 
                                                       pattern="perspective$|conflict_resol$")], na.rm=TRUE))


####Total score####
#calculate students percent correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_student_knowledge=(ecd_math_student_knowledge+ecd_literacy_student_knowledge+
                                  ecd_exec_student_knowledge + ecd_soc_student_knowledge)/4) %>%
  mutate(ecd_student_proficiency=100*as.numeric(ecd_student_knowledge>=80),
         ecd_math_student_proficiency=100*as.numeric(ecd_math_student_knowledge>=80),
         ecd_literacy_student_proficiency=100*as.numeric(ecd_literacy_student_knowledge>=80),
         ecd_exec_student_proficiency=100*as.numeric(ecd_exec_student_knowledge>=80),
         ecd_soc_student_proficiency=100*as.numeric(ecd_soc_student_knowledge>=80)
  )
#save ecd data at student level anonymized
ecd_dta_anon <- ecd_dta %>%
  select(school_code, ecd_student_number, ecd_student_age, ecd_student_male, 
         ecd_student_knowledge, ecd_math_student_knowledge, ecd_literacy_student_knowledge, ecd_soc_student_knowledge, ecd_exec_student_knowledge,
         ecd_student_proficiency, ecd_math_student_proficiency, ecd_literacy_student_proficiency, ecd_soc_student_proficiency, ecd_exec_student_proficiency,
         math_items, lit_items, soc_items, exec_items)


save(ecd_dta_anon, ecd_dta_metadata, 
     file = file.path(confidential_folder, "dashboard_ecd_data.RData"))

#calculate % correct for literacy, math, and total
final_indicator_data_LCAP <- ecd_dta %>%
  left_join(school_dta[,c('interview__id', 'm6_teacher_name', 'm6_teacher_code', 'm6_class_count', 'm6_instruction_time')]) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'))

#Breakdowns of Male/Female
final_indicator_data_LCAP_M <- ecd_dta %>%
  left_join(school_dta[,c('interview__id', 'm6_teacher_name', 'm6_teacher_code', 'm6_class_count', 'm6_instruction_time')]) %>%
  filter(m6s1q3==1) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'))

final_indicator_data_LCAP_F <- ecd_dta %>%
  left_join(school_dta[,c('interview__id', 'm6_teacher_name', 'm6_teacher_code', 'm6_class_count', 'm6_instruction_time')]) %>%
  filter(m6s1q3==2) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'))


#############################################
##### School Inputs ###########
#############################################

# School survey. Total score starts at 1 and points added are the sum of whether a school has: 
#   - Functional blackboard 
# - Pens, pencils, textbooks, exercise books 
# - Fraction of students in class with a desk 
# - Used ICT in class and have access to ICT in the school.


#functioning blackboard and chalk
school_data_INPT <- school_data_INPT %>%
  mutate(blackboard_functional=case_when(
    m4scq10_inpt==1 & m4scq9_inpt==1 & m4scq8_inpt==1  ~ 1,
    m4scq10_inpt==0 | m4scq9_inpt==0 | m4scq8_inpt==0 ~ 0)) 

#pens, pencils, textbooks, exercise books
school_data_INPT <- school_data_INPT %>%
  mutate(share_textbook=(m4scq5_inpt)/(m4scq4_inpt)) %>%
  mutate(share_pencil=(m4scq6_inpt)/(m4scq4_inpt)) %>%
  mutate(share_exbook=(m4scq7_inpt)/(m4scq4_inpt)) %>%
  mutate(pens_etc=case_when(
    share_pencil>=0.9 & share_exbook>=0.9  ~ 1,
    share_pencil<0.9 | share_exbook<0.9 ~ 0),
    textbooks=case_when(
      share_textbook>=0.9   ~ 1,
      share_textbook<0.9  ~ 0)) 

#basic classroom furniture
school_data_INPT <- school_data_INPT %>%
  mutate(share_desk=1-(m4scq11_inpt)/(m4scq4_inpt))


#Used ICT 
school_teacher_questionnaire_INPT <- teacher_questionnaire_INPT %>%
  group_by(school_code) %>%
  summarise(used_ict_pct=mean(m3sbq4_inpt, na.rm=TRUE))

school_data_INPT <- school_data_INPT %>%
  mutate(used_ict_num=case_when(
    m1sbq12_inpt==0  ~ 0,
    (m1sbq12_inpt>=1 ) ~ m1sbq14_inpt,
    (is.na(m1sbq12_inpt==0) | is.na(m1sbq14_inpt)) ~ as.numeric(NA)
  ))

#access to ICT
school_data_INPT <- school_data_INPT %>%
  mutate(access_ict=case_when(
    m1sbq12_inpt==0 | m1sbq13_inpt==0 ~ 0,
    (m1sbq12_inpt>=1 & m1sbq13_inpt==1 ) ~ 1,
    (m1sbq12_inpt>=1 & m1sbq13_inpt==1 ) ~ 0.5, #Internet didn't work when tested
    (is.na(m1sbq12_inpt==0) | is.na(m1sbq13_inpt) ) ~ as.numeric(NA)
  ))



inpt_list<-c('blackboard_functional', 'pens_etc', 'textbooks', 'share_desk',  'used_ict', 'access_ict')

final_indicator_data_INPT <- school_data_INPT %>%
  left_join(school_teacher_questionnaire_INPT) %>%
  mutate(used_ict=if_else((used_ict_pct>=0.5 & used_ict_num>=3), 1,0))     %>%  #Set percentage of teachers to use ICT over 50% and number over 3
  group_by(school_code) %>%
  select(preamble_info, inpt_list, contains('INPT')) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_INPT=n_miss_row(.)) %>%
  mutate(inputs=textbooks+blackboard_functional + pens_etc + share_desk +  0.5*used_ict + 0.5*access_ict) %>%
  select(preamble_info, inputs, everything()) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))




#############################################
##### School Infrastructure ###########
#############################################

# School survey. Total score starts at 1 and points added are the sum of whether a school has: 
#   - Access to adequate drinking water 
# -Functional toilets.  Extra points available if are separate for boys/girls, private, useable, and have hand washing facilities 
# - Electricity  in the classroom 
# - Internet
# - School is accessible for those with disabilities (road access, a school ramp for wheelchairs, an entrance wide enough for wheelchairs, ramps to classrooms where needed, accessible toilets, and disability screening for seeing, hearing, and learning disabilities with partial credit for having 1 or 2 or the 3).)

#drinking water
school_data_INFR <- school_data_INFR %>%
  #
  mutate(drinking_water=if_else((m1sbq9_infr==1 | m1sbq9_infr==2 | m1sbq9_infr==5 | m1sbq9_infr==6), 1,0, as.numeric(NA) ))

#functioning toilets
school_data_INFR <- school_data_INFR %>%
  mutate(toilet_exists=if_else(m1sbq1_infr==7 ,0,1),
         toilet_separate=if_else((m1sbq2_infr==1 | m1sbq2_infr==3),1,0),
         toilet_private=as.numeric(m1sbq4_infr),
         toilet_usable=as.numeric(m1sbq5_infr),
         toilet_handwashing=as.numeric(m1sbq7_infr),
         toilet_soap=as.numeric(m1sbq8_infr)) %>%
  mutate(functioning_toilet=case_when(
    # exist, separate for boys/girls, clean, private, useable,  handwashing available
    toilet_exists==1 & toilet_usable==1 & toilet_separate==1  & toilet_private==1  & toilet_handwashing==1 ~ 1,
    toilet_exists==0 | toilet_usable==0 | toilet_separate==0  | toilet_private==0  | toilet_handwashing==0 ~ 0
  )) 

#visibility
school_data_INFR <- school_data_INFR %>%
  left_join(select(school_data_INPT, interview__id, m4scq8_inpt, m4scq9_inpt, m4scq10_inpt, m1sbq15_inpt )) %>%
  mutate(visibility=case_when(
    m4scq10_inpt==1 &  m4scq8_inpt==1  ~ 1,
    m4scq10_inpt==0 & m4scq8_inpt==1 ~ 0)) 

#electricity
school_data_INFR <- school_data_INFR %>%
  mutate(class_electricity=if_else(m1sbq11_infr==1,1,0)) 


#accessibility for people with disabilities
final_indicator_data_INFR <- school_data_INFR %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(
    disab_road_access=bin_var(m1s0q2_infr,1),
    disab_school_ramp=case_when(
      m1s0q3_infr==0 ~ 1,
      (m1s0q4_infr	==1 & m1s0q3_infr	==1) ~ 1,
      (m1s0q4_infr	==0 & m1s0q3_infr	==1) ~ 0,
      is.na(m4scq1_infr) ~ as.numeric(NA)),
    disab_school_entr=bin_var(m1s0q5_infr,1),
    disab_class_ramp=case_when(
      m4scq1_infr==0 ~ 1,
      (m4scq1_infr==1 & m4scq1_infr==1) ~ 1,
      (m4scq1_infr==0 & m4scq1_infr==1) ~ 0,
      is.na(m4scq1_infr) ~ as.numeric(NA)),
    disab_class_entr=bin_var(m4scq3_infr,1),
    disab_screening=rowMeans(select(.,m1sbq17_infr__1,m1sbq17_infr__2,m1sbq17_infr__3), na.rm = TRUE),
    #sum up all components for overall disability accessibility score
    disability_accessibility=(disab_road_access+disab_school_ramp+disab_school_entr+
                                disab_class_ramp+disab_class_entr+
                                if_else(m1sbq1_infr==7,0,as.numeric(m1sbq6_infr))+
                                disab_screening)/7
  ) %>%
  mutate(internet=case_when(
    m1sbq15_inpt==2  ~ 1,
    m1sbq15_inpt==1  ~ 0.5,
    m1sbq15_inpt==0   ~ 0,
    is.na(as.numeric(m1sbq15_inpt)) ~ 0,
    TRUE ~ 0) ) # 1 point if internet working, 0.5 if doesn't work well, 0 if not at all


infr_list<-c('drinking_water', 'functioning_toilet', 'internet',  'class_electricity', 'disability_accessibility')

final_indicator_data_INFR <- final_indicator_data_INFR %>%
  mutate(n_mssing_INFR=n_miss_row(.)) %>%
  mutate(infrastructure=(drinking_water+ functioning_toilet+ internet + class_electricity+ disability_accessibility)) %>%
  select(preamble_info, infrastructure, everything()) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  






#############################################
##### Teacher Pedagogical Skill ###########
#############################################

#define variables for TEACH
# label variables

var.labels = c(route="Route ID", 
               school_clip="Clip Graded for each School",
               person = "Person Scoring",
               s_0_1_1 = "0.1 Teacher provides learning activity to most students",
               s_0_1_2 = "0.1 Students are on task",
               s_0_2_1 = "0.1 Teacher provides learning activity to most students",
               s_0_2_2 = "0.1 Students are on task",
               s_0_3_1 = "0.1 Teacher provides learning activity to most students",
               s_0_3_2 = "0.1 Students are on task",
               s_a1 = "SUPPORTIVE LEARNING ENVIRONMENT: Segment 1",
               s_a1_1 = "1.1 The teacher treats all students respectfully",
               s_a1_2 = "1.2 The teacher uses positive language with students",
               s_a1_3 = "1.3 The teacher responds to students' need",
               s_a1_4 = "1.4 The teacher does not exhibit gender bias and challenges gender stereotypes in the classroom",
               s_a2 = "POSITIVE BEHAVIORAL EXPECTATIONS: Segment 1",
               s_a2_1 = "2.1 The teacher sets clear behavioral expectations for classroom activities",
               s_a2_2 = "2.2 The teacher acknowledges positive student behavior",
               s_a2_3 = "2.3 The teacher redirects misbehavior and focuses on the expected behavior",
               s_b3 = "LESSON FACILITATION: Segment 1",
               s_b3_1 = "3.1 The teacher explicitly articulates the objectives of the lesson",
               s_b3_2 = "3.2 The teacher's explanation of content is clear",
               s_b3_3 = "3.3 The teacher makes connections in the lesson that relate to other content",
               s_b3_4 = "3.4 The teacher models by enacting, or thinking aloud",
               s_b4 = "CHECKS FOR UNDERSTANDING: Segment 1",
               s_b4_1 = "4.1 The teacher uses questions, prompts or other strategies to determine students's level of understanding",
               s_b4_2 = "4.2 The teacher monitors most students during independent/group work",
               s_b4_3 = "4.3 The teacher adjusts teaching to the level of the students",
               s_b5 = "FEEDBACK: Segment 1",
               s_b5_1 = "5.1 The teacher provides specific comments or prompts that help clarify students' misunderstandings",
               s_b5_2 = "5.2 The teacher provides specific comments or prompts that help identify students' successes",
               s_b6 = "CRITICAL THINKING: Segment 1",
               s_b6_1 = "6.1 The teacher asks open-ended questions",
               s_b6_2 = "6.2 The teacher provides thinking tasks",
               s_b6_3 = "6.3 The students ask open-ended questions or perform thinking tasks",
               s_c7 = "AUTONOMY: Segment 1",
               s_c7_1 = "7.1 The teacher provides students with choices",
               s_c7_2 = "7.2 The teacher provides students with opportunities to take on roles in the classroom",
               s_c7_3 = "7.3 The students volunteer to participate in the classroom",
               s_c8 = "PERSEVERANCE: Segment 1",
               s_c8_1 = "8.1 The teacher acknowledges students' effort",
               s_c8_2 = "8.2 The teacher has a positive attitude towards studens' challenges",
               s_c8_3 = "8.3 The teacher encourages goal-setting",
               s_c9 = "SOCIAL AND COLLABORATIVE SKILLS: Segment 1",
               s_c9_1 = "9.1 The teacher promotes students,Äô collaboration through peer interaction",
               s_c9_2 = "9.2 The teacher promotes students' interpersonal skills",
               s_c9_3 = "9.3 Students collaborate with one another through peer interaction",
               enum_comments = "Additional comments by enumerator:"
)


teach_dta <- readxl::read_excel(path=file.path(download_folder, "TEACH_Final_Scores.xlsx"), sheet = "ALL_Scores", skip=2) %>%
  select(-c('...48'))
label(teach_dta) = as.list(var.labels[match(names(var.labels), names(var.labels))])
names(teach_dta) = names(var.labels)


#pull out school code from video clip name
teacher_pedagogy_segments <- teach_dta %>%
  separate(school_clip, into=c('school_code', 'clip'),
           sep= " Clip ") %>%
  mutate(school_code=as.numeric(str_trim(school_code)),
         clip=str_trim(clip))

#recode scores to be numeric


low_medium_high <- c(
                     "s_0_1_2",
                     "s_0_2_2",
                     "s_0_3_2",
                     "s_a2_1",
                     "s_a2_2",
                     "s_a2_3",
                     "s_b3_1",
                     "s_b3_2",
                     "s_b3_3",
                     "s_b3_4",
                     "s_b5_1",
                     "s_b5_2",
                     "s_b6_1",
                     "s_b6_2",
                     "s_b6_3",
                     "s_c7_1",
                     "s_c7_2",
                     "s_c7_3",
                     "s_c8_1",
                     "s_c8_2",
                     "s_c8_3",
                     "s_c9_1",
                     "s_c9_2",
                     "s_c9_3")

low_medium_high_na <- c("s_a1_1",
                        "s_a1_2",
                        "s_a1_3",
                        "s_a1_4",
                        "s_b4_1",
                        "s_b4_2",
                        "s_b4_3"

)


yes_no <- c("s_0_1_1",
            "s_0_2_1",
            "s_0_3_1"
)

overall <- c('s_a1',
             's_a2',
             's_b3',
             's_b4',
             's_b5',
             's_b6',
             's_c7',
             's_c8',
             's_c9'
)


teacher_pedagogy_segments <- teacher_pedagogy_segments %>%  
  mutate_at(vars(overall),~(if_else(. %in% c('1','2','3','4','5'),as.numeric(.),as.numeric(NA) ))) %>%
  mutate_at(vars(low_medium_high,low_medium_high_na,yes_no),~(if_else(. %in% c('L','M','H','Y','N'),.,as.character(NA) ))) %>%
  mutate_at(vars(low_medium_high), ~(if_else(. %in% c('L','M','H'),.,as.character(NA) ))) %>%
  mutate_at(vars(low_medium_high_na), ~(if_else(. %in% c('L','M','H'),.,as.character(NA) ))) %>%
  mutate_at(vars(low_medium_high,low_medium_high_na,yes_no),~(str_replace_all(.,"[[:punct:]]",""))) %>%
  mutate_at(vars(low_medium_high), ~case_when(
    .=="L" ~ 2,
    .=="M" ~ 3,
    .=="H" ~ 4,
    TRUE ~ as.numeric(NA)
  )) %>%
      mutate_at(vars(low_medium_high_na), ~case_when(
        .=="NA" ~ 1,
        .=="L" ~ 2,
        .=="M" ~ 3,
        .=="H" ~ 4,
        TRUE ~ as.numeric(NA)
      )) %>%
        mutate_at(vars(yes_no), ~case_when(
          .=="N" ~ 0,
          .=="Y" ~ 1,
          TRUE ~ as.numeric(NA)
        )) %>%
          mutate_at(vars(low_medium_high), ~(factor(., levels=c(2,3,4), labels=c("Low", "Medium", "High")))) %>%
          mutate_at(vars(low_medium_high_na), ~(factor(., levels=c(1,2,3,4), labels=c("NA", "Low", "Medium", "High")))) %>%
          mutate_at(vars(yes_no), ~(factor(.,levels=c(0,1), labels=c("No", "Yes"))))


#create sub-indicators from TEACH
teacher_pedagogy_segments <- teacher_pedagogy_segments %>%
  mutate(classroom_culture=rowMeans(select(.,s_a1, s_a2)),
         instruction=rowMeans(select(.,s_b3, s_b4, s_b5, s_b6)),
         socio_emotional_skills=rowMeans(select(.,s_c7, s_c8, s_c9))
  ) %>%
  mutate(teach_score=rowMeans(select(.,classroom_culture, instruction, socio_emotional_skills)))

# Time on task - First measure (Yes/No on "Teacher provides learning activites to most students")
# Generate a variable computing the proportion of times each teacher for each segment is providing a learning activity to students
# We are only taking into account teachers for which we have at least 2 snapshots observed

teacher_pedagogy_segments <- teacher_pedagogy_segments %>%
  mutate(nb_tt1=3-(is.na(s_0_1_1) + is.na(s_0_2_1) + is.na(s_0_3_1))) %>%
  mutate_at(vars(s_0_1_1, s_0_2_1, s_0_3_1), ~case_when(.=="Yes" ~ 1,
                                               .=="No" ~ 0,
                                               TRUE ~ NA_real_)) %>%
  mutate(timeontask1=if_else(nb_tt1>=2, rowMeans(select(.,s_0_1_1, s_0_2_1, s_0_3_1), na.rm=TRUE), NA_real_))


#een tt_yes=rowmean(s_0_1_1_yes s_0_2_1_yes s_0_3_1_yes) if nb_tt1>=2
#replace tt_yes=tt_yes*100
#egen tt_no=rowmean(s_0_1_1_no s_0_2_1_no s_0_3_1_no) if nb_tt1>=2
#replace tt_no=tt_no*100

# Time on task - Second measure
# Proportion of classes where a low number of students are on task, a medium number of students are on task
teacher_pedagogy_segments <- teacher_pedagogy_segments %>%
  mutate(tot_low=rowSums(select(.,s_0_1_2,s_0_2_2,s_0_3_2) == "Low"),
         tot_medium=rowSums(select(.,s_0_1_2,s_0_2_2,s_0_3_2) == "Medium"),
         tot_high=rowSums(select(.,s_0_1_2,s_0_2_2,s_0_3_2) == "High"))

# We count the number of snapshots observed (in case the observation lasted less than 15 minutes) and for which the teacher was providing a learning activity

# For each of the variables "Low", "Medium" and "High", we create our own mean (in case the observation lasted less than 15 minutes or teacher was not providing a learning activity)
# We are only taking into account teachers for which we have at least 2 snapshots observed

teacher_pedagogy_segments <- teacher_pedagogy_segments %>%
  mutate(nb_tt2=3-(is.na(s_0_1_2) + is.na(s_0_2_2) + is.na(s_0_3_2)),
         tt_low=if_else(nb_tt2 >= 2, 100*tot_low/nb_tt2, NA_real_),
         tt_medium=if_else(nb_tt2 >= 2, 100*tot_medium/nb_tt2, NA_real_),
         tt_high=if_else(nb_tt2 >= 2, 100*tot_high/nb_tt2, NA_real_))



# Now merge on school information
final_indicator_data_PEDG<- school_dta %>%
  select(preamble_info, m4saq1, m4saq1_number )  %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select( -starts_with('interview'), -starts_with('enumerator')) %>%
  left_join(teacher_pedagogy_segments) %>%
  mutate(teach_prof=100*as.numeric(teach_score>=3),                      #rate teacher as proficient in teach and the subcomponents if they score at least 3
         classroom_culture_prof=100*as.numeric(classroom_culture>=3),
         instruction_prof=100*as.numeric(instruction>=3),
         socio_emotional_skills_prof=100*as.numeric(socio_emotional_skills>=3)) %>%
  group_by(school_code) %>%
  mutate(number_segments=  sum(!is.na(teach_score))) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 


#Breakdowns by Male/Female





#final_indicator_data_PEDG <- ''


#############################################
##### School Operational Management ###########
#############################################

#Princials/head teachers are given two vignettes:
#- One on solving the problem of a hypothetical leaky roof 
#- One on solving a problem of inadequate numbers of textbooks.  
#Each vignette is worth 2 points.  
#
#The indicator will measure two things: presence of functions and quality of functions. In each vignette: 
#- 0.5 points are awarded for someone specific having the responsibility to fix 
#- 0.5 point is awarded if the school can fully fund the repair, 0.25 points is awarded if the school must get partial help from the community, and 0 points are awarded if the full cost must be born by the community 
#- 1 point is awarded if the problem is fully resolved in a timely manner, with partial credit given if problem can only be partly resolved.

final_indicator_data_OPMN <- school_data_OPMN %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(
    vignette_1_resp=if_else((m7sbq1_opmn==0 & (m7sbq4_opmn==4 | m7sbq4_opmn==98)), 0, 0.5),
    vignette_1_finance=case_when(
      m7sbq2_opmn==1 ~ 0.5,
      (m7sbq2_opmn==2 | m7sbq2_opmn==97) ~ 0.25,
      m7sbq2_opmn==3 ~ 0,
      m7sbq1_opmn==0 & !(m7sbq4_opmn==4 | m7sbq4_opmn==98) ~ 0.5),
    vignette_1_address=if_else(m7sbq1_opmn==1, case_when(
      m7sbq3_opmn==1 ~ 0,
      (m7sbq3_opmn==2 | m7sbq3_opmn==97) ~ .5,
      m7sbq3_opmn==3 ~ 1),
      case_when(
        m7sbq5_opmn==1 ~ 0,
        m7sbq5_opmn==2 ~ .5,
        m7sbq5_opmn==3 ~ 1))) %>% 
  #give total score for this vignette
  mutate(vignette_1=vignette_1_resp+vignette_1_finance+vignette_1_address) %>% 
  mutate(vignette_2_resp=if_else(m7scq1_opmn==98, 0, 0.5), # no one responsible that is known
         vignette_2_finance=if_else(m7scq1_opmn==1,0,0.5),      #parents are forced to buy textbooks          
         #give partial credit based on how quickly it will be solved <1 month, 1-3, 3-6, 6-12, >1 yr
         vignette_2_address=case_when(
           m7scq2_opmn==1 ~ 1,
           m7scq2_opmn==2 ~ .75,
           m7scq2_opmn==3 ~ .5,
           m7scq2_opmn==4 ~ .25,
           (m7scq2_opmn==5 |m7scq2_opmn==98) ~ 0)) %>%
  mutate(vignette_2=vignette_2_resp+vignette_2_finance+vignette_2_address) %>%          #sum all components for overall score
  mutate(operational_management=1+vignette_1+vignette_2 )


final_indicator_data_OPMN <- final_indicator_data_OPMN %>%
  mutate(n_mssing_OPMN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  

#Breakdowns by Male/Female
final_indicator_data_OPMN_M <- final_indicator_data_OPMN %>%
  filter(m7saq10==1) %>%
  mutate(n_mssing_OPMN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  

final_indicator_data_OPMN_F <- final_indicator_data_OPMN %>%
  filter(m7saq10==2) %>%
  mutate(n_mssing_OPMN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  

#############################################
##### School Instructional Leadership ###########
#############################################

# School survey. Total score starts at 1 and points added are the sum of whether a teacher has: 
#   - Had a classroom observation in past year 
# - Had a discussion based on that observation that lasted longer than 10 min 
# - Received actionable feedback from that observation 
# - Teacher had a lesson plan and discussed it with another person


#list additional info that will be useful to keep in each indicator dataframe
preamble_info_teacher_drop_ildr <- c('interview__id', 'questionnaire_roster__id', 'teacher_name', 'teacher_number', 
                           'available', 'teacher_position', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 'teacher_grd5',
                           'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'teacher_education', 'teacher_year_began',
                           'teacher_age')

final_indicator_data_ILDR <- teacher_questionnaire_ILDR %>%
  mutate(n_mssing_ILDR=n_miss_row(.)) %>%
  mutate(classroom_observed=bin_var(m3sdq15_ildr,1),
         classroom_observed_recent=if_else((classroom_observed==1 & m3sdq16_ildr<=12),1,0), #set recent to mean under 12 months
         purpose_observation=case_when(
           m3sdq18_ildr__1==1 ~ "Evaluation",
           m3sdq18_ildr__2==1 ~ "Professional Development",
           m3sdq18_ildr__3==1 ~ "Monitoring",
           m3sdq18_ildr__97==1 ~ m3sdq18_other_ildr ),
         discussion_30_min=bin_var(m3sdq20_ildr,1),
         discussed_observation=if_else((classroom_observed==1 & m3sdq19_ildr==1 & m3sdq20_ildr==3),1,0), #make sure there was discussion and lasted more than 30 min
         feedback_observation=if_else((m3sdq21_ildr==1 & (m3sdq22_ildr__1==1 | m3sdq22_ildr__2==1 | m3sdq22_ildr__3==1
                                                          | m3sdq22_ildr__4==1 | m3sdq22_ildr__5==1)),1,0), #got feedback and was specific
         lesson_plan=if_else(m3sdq23_ildr==1,1,0),
         lesson_plan_w_feedback=if_else((m3sdq23_ildr==1 & m3sdq24_ildr==1),1,0)) %>%
  mutate(feedback_observation=if_else(m3sdq15_ildr==1 & m3sdq19_ildr==1, feedback_observation, 0)) %>% #fix an issue where teachers that never had classroom observed arent asked this question.
  mutate(instructional_leadership=1+0.5*classroom_observed + 0.5*classroom_observed_recent + discussed_observation + feedback_observation + lesson_plan_w_feedback) %>%
  mutate(instructional_leadership=if_else(classroom_observed==1,instructional_leadership, 1.5 + lesson_plan_w_feedback )) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-preamble_info_teacher_drop_ildr  )  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  


#Breakdowns by Male/Female
final_indicator_data_ILDR_M <- final_indicator_data_ILDR %>%
  filter(m7saq10==1) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  

final_indicator_data_ILDR_F <- final_indicator_data_ILDR %>%
  filter(m7saq10==2) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  


#############################################
##### School Principal School Knowledge ###########
#############################################
# The aim of this indicator is to measure the extent to which principals have the knowledge about their own schools that is necessary for them to be effective managers. A score from 1 to 5 capturing the extent to which the principal is familiar with certain key aspects of the day-to-day workings of the school (in schools that have principals). Principal receives points in the following way: 
#   - 5 points. Principal gets all 90-100% of questions within accuracy bounds (defined below). 
# - 4 points. Principal gets 80-90% of question within accuracy bounds. 
# - 3 points. Principal gets 70-80% of question within accuracy bounds. 
# - 2 points. Principal gets 60-70% of question within accuracy bounds. 
# - 1 points. Principal gets under 60% of question within accuracy bounds. 
# 
# Accuracy bounds for each question. 
# Within 1 teacher/student for each of the following: 
#   - Out of these XX teachers, how many do you think would be able to correctly add triple digit numbers (i.e. 343+215+127)? 
#   - Out of these XX teachers, how many do you think would be able to correctly to multiply double digit numbers (i.e. 37 x 13)? 
#   - Out of these XX teachers, how many do you think would be able to complete sentences with the correct world (i.e. The accident _____ (see, saw, had seen, was seen) by three people)? 
#   - Any of these XX teachers have less than 3 years of experience? 
#   - Out of these XX teachers, which ones have less than 3 years of experience as a teacher? 
# Within 3 teacher/student for each of the following: 
#   - In the selected 4th grade classroom, how many of the pupils have the relevant textbooks? 
#   Must identify whether or not blackboard was working in a selected 4th grade classroom.




#first create a database containing actual values for each question for the principal
pknw_actual_cont <- final_indicator_data_CONT %>%
  select(school_code, m5_teach_count, m5_teach_count_math, m5s2q1c_number, m5s2q1e_number, m5s1q1f_grammer ) 

pknw_actual_exper <- teacher_questionnaire %>%
  select(school_code, m3sb_tnumber, m3sb_troster,m3saq5, m3saq6 ) %>%
  mutate(experience=(2019-m3saq5)) %>%
  filter(experience <3) %>% 
  group_by(school_code) %>%
  summarise(teacher_count_experience_less3=n())

pknw_actual_school_inpts <- final_indicator_data_INPT %>%
  select(school_code, blackboard_functional, m4scq5_inpt, m4scq4_inpt)

pknw_actual_combined <- pknw_actual_school_inpts %>%
  left_join(pknw_actual_cont) %>%
  left_join(pknw_actual_exper) %>%
  mutate(teacher_count_experience_less3=if_else(is.na(teacher_count_experience_less3), as.numeric(0), as.numeric(teacher_count_experience_less3)),
         m5s2q1c_number=m5s2q1c_number*m5_teach_count,
         m5s2q1e_number=m5s2q1e_number*m5_teach_count,
         m5s1q1f_grammer=m5s1q1f_grammer*m5_teach_count)


#create function to compare principal responses to actual
# if principal is within 1 student/teacher, then score as 1, 0 otherwise
principal_scorer <- function(var_guess, var_actual, var_total, margin1, margin2) {
  if_else(
    ((1-abs(var_guess-var_actual)/var_total>= as.numeric(margin1)) | (var_guess-var_actual <= as.numeric(margin2))),
    1,
    0)
}

final_indicator_data_PKNW <- school_data_PKNW %>%
  group_by(school_code) %>%
  select(school_code, m7saq10, m7sfq5_pknw, m7sfq6_pknw, m7sfq7_pknw, m7sfq9_pknw_filter, m7sfq10_pknw, m7sfq11_pknw, m7_teach_count_pknw) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_PKNW=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  %>%
  left_join(pknw_actual_combined) %>%
  mutate(add_triple_digit_pknw=principal_scorer(m7sfq5_pknw, m5s2q1c_number, m7_teach_count_pknw,0.8,1),
         multiply_double_digit_pknw=principal_scorer(m7sfq6_pknw, m5s2q1e_number, m7_teach_count_pknw,0.8,1),
         complete_sentence_pknw=principal_scorer(m7sfq7_pknw, m5s1q1f_grammer, m7_teach_count_pknw,0.8,1),
         experience_pknw=principal_scorer(m7sfq9_pknw_filter, teacher_count_experience_less3, m7_teach_count_pknw,0.8,1),
         textbooks_pknw=principal_scorer(m7sfq10_pknw, m4scq5_inpt, m4scq4_inpt,0.8,3),
         blackboard_pknw=if_else(m7sfq11_pknw==blackboard_functional,1,0)) %>%
  mutate(principal_knowledge_avg=rowMeans(select(.,add_triple_digit_pknw, multiply_double_digit_pknw, complete_sentence_pknw, experience_pknw, textbooks_pknw, blackboard_pknw), na.rm=TRUE)) %>%
  mutate(principal_knowledge_score=case_when(
    principal_knowledge_avg >0.9 ~ 5,
    (principal_knowledge_avg >0.8 & principal_knowledge_avg<=0.9) ~ 4,
    (principal_knowledge_avg >0.7 & principal_knowledge_avg<=0.8) ~ 3,
    (principal_knowledge_avg >0.6 & principal_knowledge_avg<=0.7) ~ 2,
    (principal_knowledge_avg <=0.6 ) ~ 1  )
  ) %>%
  select(school_code, m7saq10,  m7sfq5_pknw,m5s2q1c_number, m7sfq6_pknw, m5s2q1e_number, m7sfq7_pknw, m5s1q1f_grammer, m7sfq9_pknw_filter, teacher_count_experience_less3,  m7sfq10_pknw,m4scq5_inpt,  m7sfq11_pknw, blackboard_functional, principal_knowledge_score, add_triple_digit_pknw, 
         multiply_double_digit_pknw, complete_sentence_pknw, experience_pknw, textbooks_pknw, blackboard_pknw, m7_teach_count_pknw) %>%
  select(school_code, m7saq10, m7sfq5_pknw, m7sfq6_pknw, m7sfq7_pknw, m7sfq9_pknw_filter, m7sfq10_pknw, m7sfq11_pknw, principal_knowledge_score, add_triple_digit_pknw, 
         multiply_double_digit_pknw, complete_sentence_pknw, experience_pknw, textbooks_pknw, blackboard_pknw, m7_teach_count_pknw)


#Breakdowns by Male/Female
final_indicator_data_PKNW_M <- final_indicator_data_PKNW %>%
  filter(m7saq10==1) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  

final_indicator_data_PKNW_F <- final_indicator_data_PKNW %>%
  filter(m7saq10==2) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  

#############################################
##### School Principal Management Skills ###########
#############################################


# Score of 1-5 based on sum of following: 
# goal setting:
#   - 1 Point. School Goals Exists 
# - 1 Point. School goals are clear to school director, teachers, students, parents, and other members of community (partial credit available) 
# - 1 Point. Specific goals related to improving student achievement ( improving test scores, improving pass rates, reducing drop out, reducing absenteeism, improving pedagogy, more resources for infrastructure, more resources for inputs) 
# - 1 Point. School has defined system to measure goals (partial credit available)
# problem solving:
# - 1.33 point on proactive (partial credit for just notifying a superior) on absence issue
# - 0.33 for each group principal would contact to gather info on absence
# - 1.33 point for working with local authorities, 0.5 points for organizing remedial classes, 0.25 for just informing parents
#Create variables for whether school goals exists, are clear, are relevant to learning, and are measured in an appropriate way.

final_indicator_data_PMAN <- school_data_PMAN %>%
  mutate(school_goals_exist=bin_var(m7sdq1_pman,1),
         school_goals_clear=if_else(m7sdq1_pman==1, 
                                    rowMeans(select(.,m7sdq3_pman__1, m7sdq3_pman__2, m7sdq3_pman__3, m7sdq3_pman__4, m7sdq3_pman__5), na.rm=TRUE),
                                    0) ,
         school_goals_relevant_total=rowSums(select(.,m7sdq4_pman__1, m7sdq4_pman__2, m7sdq4_pman__3, m7sdq4_pman__4, m7sdq4_pman__5, m7sdq4_pman__6, m7sdq4_pman__7, m7sdq4_pman__8, m7sdq4_pman__97), na.rm=TRUE),
         school_goals_relevant=if_else(m7sdq1_pman==1, 
                                       case_when(
                                         (school_goals_relevant_total > 0) ~ 1,
                                         (school_goals_relevant_total == 0) ~ 0),
                                       0),
         school_goals_measured=if_else((m7sdq1_pman==1), 
                                       case_when(
                                         (m7sdq5_pman==1) ~ 0,
                                         (m7sdq5_pman==2 | m7sdq5_pman==97 ) ~ 0.5,
                                         (m7sdq5_pman==3) ~ 1),
                                       0)) %>%
  mutate(goal_setting=1+school_goals_exist+school_goals_clear+school_goals_relevant+school_goals_measured) %>%
  # Now for problem solving
  mutate(
    problem_solving_proactive=case_when(
                                  (m7seq1_pman==4 ) ~ 1,
                                  (m7seq1_pman==2 | m7seq1_pman==3 ) ~ 0.5,
                                  (m7seq1_pman==1 | m7seq1_pman==98 ) ~ 0,
                                  TRUE ~ 0),
    problem_solving_info_collect=(m7seq2_pman__1+m7seq2_pman__2 + m7seq2_pman__3 + m7seq2_pman__4)/4,
    problem_solving_stomach=case_when(
                              (m7seq3_pman==4 ) ~ 1,
                              (m7seq1_pman==3 ) ~ 0.5,
                              (m7seq1_pman==1 | m7seq1_pman==2 | m7seq1_pman==98 ) ~ 0.25,
                              TRUE ~ 0)
                                 
    ) %>%
  mutate(problem_solving=1+(4/3)*problem_solving_proactive+(4/3)*problem_solving_info_collect+(4/3)*problem_solving_stomach) %>%
  

  mutate(principal_management=(goal_setting+problem_solving)/2)
final_indicator_data_PMAN <- final_indicator_data_PMAN %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_PMAN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  


#Breakdowns by Male/Female
final_indicator_data_PMAN_M <- final_indicator_data_PMAN %>%
  filter(m7saq10==1) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  

final_indicator_data_PMAN_F <- final_indicator_data_PMAN %>%
  filter(m7saq10==2) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
#############################################
##### Teacher Teaching Attraction ###########
#############################################

# In the school survey, a number of De Facto questions on teacher attraction are asked. 0.8 points is awarded for each of the following: 
#   - 0.8 Points. Teacher satisfied with job 
# - 0.8 Points. Teacher satisfied with status in community 
# - 0.8 Points. Would better teachers be promoted faster? 
#   - 0.8 Points. Do teachers receive bonuses? 
#   - 0.8 Points. One minus the fraction of months in past year with a salary delay.


#create function to clean teacher attitudes questions.  Need to reverse the order for scoring for some questions.  
#Should have thought about this, when programming in Survey Solutions and scale 1-5.

attitude_fun  <- function(x) {
  case_when(
    x==99 ~ as.numeric(NA),
    x==4 ~ 5,
    x==3 ~ 3.67,
    x==2 ~ 2.33,
    x==1 ~ 1
  )
}

attitude_fun_rev  <- function(x) {
  case_when(
    x==99 ~ as.numeric(NA),
    x==1 ~ 5,
    x==2 ~ 3.67,
    x==3 ~ 2.33,
    x==4 ~ 1
  )
}


teacher_questionnaire_TATT <- teacher_questionnaire_TATT %>%
  mutate(teacher_satisfied_job=attitude_fun_rev(m3seq1_tatt)/5,
         teacher_satisfied_status=attitude_fun_rev(m3seq2_tatt)/5,
         better_teachers_promoted=bin_var(m3seq3_tatt,1),
         teacher_bonus=bin_var(m3seq4_tatt,1),
         teacher_bonus_attend=if_else(m3seq4_tatt==1,
                                      bin_var(m3seq5_tatt__1,1),
                                      0),
         teacher_bonus_student_perform=if_else(m3seq4_tatt==1,
                                               bin_var(m3seq5_tatt__2,1),
                                               0),
         teacher_bonus_extra_duty=if_else(m3seq4_tatt==1,
                                          bin_var(m3seq5_tatt__3,1),
                                          0),
         teacher_bonus_hard_staff=if_else(m3seq4_tatt==1,
                                          bin_var(m3seq5_tatt__4,1),
                                          0),
         teacher_bonus_subj_shortages=if_else(m3seq4_tatt==1,
                                              bin_var(m3seq5_tatt__5,1),
                                              0),
         teacher_bonus_add_qualif=if_else(m3seq4_tatt==1,
                                          bin_var(m3seq5_tatt__6,1),
                                          0),
         teacher_bonus_school_perform=if_else(m3seq4_tatt==1,
                                              bin_var(m3seq5_tatt__7,1),
                                              0),
         teacher_bonus_other=if_else(m3seq4_tatt==1,
                                     if_else(m3seq5_tatt__97==1,m3seq5_other_tatt,"NA"),
                                     "NA"),
         salary_delays=if_else(m3seq6_tatt==1, m3seq7_tatt,0)) %>%
  mutate(salary_delays=if_else(salary_delays>12,12,salary_delays)) %>%
  mutate(teacher_attraction=1+0.8*teacher_satisfied_job+.8*teacher_satisfied_status+.8*better_teachers_promoted+.8*teacher_bonus+.8*(1-salary_delays/12))


final_indicator_data_TATT <- teacher_questionnaire_TATT %>%
  mutate(n_mssing_TATT=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  





#############################################
##### Teacher Teaching Selection and Deployment ###########
#############################################

# School Survey. The De Facto portion of the Teacher Selection and Deployment Indicator considers two issues: how teachers are selected into the profession and how teachers are assigned to positions (transferred) once in the profession. Research shows that degrees and years of experience explanin little variation in teacher quality, so more points are assigned for systems that also base hiring on content knowledge or pedagogical skill. 2 points are available for the way teachers are selected and 2 points are available for deployment. 
# 
# Selection 
# - 0 Points. None of the below 
# - 1 point. Teachers selected based on completion of coursework, educational qualifications, graduating from tertiary program (including specialized programs), selected based on experience 
# - 2 points. Teacher recruited based on passing written content knowledge test, passed interview stage assessment, passed an assessment conducted by supervisor based on practical experience, conduct during mockup class. 
# 
# Deployment 
# - 0 Points. None of the below 
# - 1 point. Teachers deployed based on years of experience or job title hierarchy 
# - 2 points. Teacher deployed based on performance assessed by school authority, colleagues, or external evaluator, results of interview.


teacher_questionnaire_TSDP <- teacher_questionnaire_TSDP %>%
  mutate(
    teacher_selection=case_when(
      (m3sdq1_tsdp__5==1 | m3sdq1_tsdp__6==1 | m3sdq1_tsdp__8==1 | m3sdq1_tsdp__9==1 )  ~ 2,
      (m3sdq1_tsdp__1==1 | m3sdq1_tsdp__2==1 | m3sdq1_tsdp__3==1 | m3sdq1_tsdp__4==1 | m3sdq1_tsdp__7==1) ~ 1,
      (m3sdq1_tsdp__1==0 & m3sdq1_tsdp__2==0 & m3sdq1_tsdp__3==0 & m3sdq1_tsdp__4==0 & m3sdq1_tsdp__5==0 & m3sdq1_tsdp__6==0 & m3sdq1_tsdp__7==0 & m3sdq1_tsdp__8==0 & m3sdq1_tsdp__9==0) ~ 0
    ),
    teacher_deployment=case_when(
      (m3seq8_tsdp__3==1 | m3seq8_tsdp__4==1 | m3seq8_tsdp__5==1  )  ~ 2,
      (m3seq8_tsdp__1==1 | m3seq8_tsdp__2==1 | m3seq8_tsdp__97==1) ~ 1,
      ((m3seq8_tsdp__1==0 & m3seq8_tsdp__2==0 & m3seq8_tsdp__3==0 & m3seq8_tsdp__4==0 & m3seq8_tsdp__5==0) | ( m3seq8_tsdp__99==1)) ~ 0
      
    )
  ) %>%
  mutate(teacher_selection_deployment=1+teacher_selection+teacher_deployment)


final_indicator_data_TSDP <- teacher_questionnaire_TSDP %>%  
  mutate(n_mssing_TSDP=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  



#############################################
##### Teacher Teaching Support ###########
#############################################


# School survey. Our teaching support indicator asks teachers about participation and the experience with several types of formal/informal training: 
#   
#   Pre-Service (Induction) Training: 
#   - 0.5 Points. Had a pre-service training 
# - 0.5 Points. Teacher reported receiving usable skills from training 
# 
# Teacher practicum (teach a class with supervision) 
# - 0.5 Points. Teacher participated in a practicum 
# - 0.5 Points. Practicum lasted more than 3 months and teacher spent more than one hour per day teaching to students. 
# 
# In-Service Training: 
#   - 0.5 Points. Had an in-service training 
# - 0.25 Points. In-service training lasted more than 2 total days 
# - 0.125 Points. More than 25% of the in-service training was done in the classroom. 
# - 0.125 Points. More than 50% of the in-service training was done in the classroom. 
# 
# Opportunities for teachers to come together to share ways of improving teaching: 
#   - 1 Point if such opportunities exist.

#Add in question on teach opportunities so share ways of teaching
opp_share<- teacher_questionnaire_ILDR %>%
  select(interview__id, m3sdq14_ildr) %>%
  mutate(opportunities_teachers_share=bin_var(m3sdq14_ildr,1))

teacher_questionnaire_TSUP <- teacher_questionnaire_TSUP %>%
  mutate(pre_training_exists=bin_var(m3sdq3_tsup,1)/2,
         pre_training_useful=if_else(m3sdq3_tsup==1,
                                     bin_var(m3sdq4_tsup,1),
                                     0)/2,
         pre_training_practicum=if_else(m3sdq3_tsup==1,
                                        bin_var(m3sdq6_tsup,1),
                                        0)/2,
         pre_training_practicum_lngth=case_when(
           (m3sdq6_tsup==1 & m3sdq7_tsup>=3 & m3sdq8_tsup>=1) ~  0.5,
           (m3sdq6_tsup==1 & (m3sdq7_tsup<3 | m3sdq8_tsup<1))  ~ 0,
           m3sdq6_tsup==2 ~ 0,
           m3sdq3_tsup==0 ~ 0,
           TRUE ~ 0),
         in_service_exists=bin_var(m3sdq9_tsup,1),
         in_servce_lngth=case_when(
           (m3sdq9_tsup==1 & m3sdq10_tsup>2 ) ~ 1,
           (m3sdq9_tsup==1 & m3sdq10_tsup<=2) ~ 0,
           m3sdq9_tsup==0 ~ 0,
           TRUE ~ 0
         ),
         in_service_classroom=case_when(
           (m3sdq9_tsup==1 & m3sdq13_tsup>=3)  ~ 1,
           (m3sdq9_tsup==1 & m3sdq13_tsup==2)  ~ 0.5,
           (m3sdq9_tsup==1 & m3sdq13_tsup==1)  ~ 0,
           m3sdq9_tsup==0 ~ 0,
           TRUE ~ 0
         )
  ) %>%
  left_join(opp_share) %>%
  mutate(pre_service=pre_training_exists+pre_training_useful,
         practicum=pre_training_practicum+pre_training_practicum_lngth,
         in_service=0.5*in_service_exists+0.25*in_servce_lngth+0.25*in_service_classroom) %>%
  mutate(teacher_support=1+pre_service+practicum+in_service+opportunities_teachers_share) 
# mutate(teacher_support=if_else(teacher_support>5,5,teacher_support)) #need to fix






final_indicator_data_TSUP <- teacher_questionnaire_TSUP %>%
  mutate(n_mssing_TSUP=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  



#############################################
##### Teacher Teaching Evaluation ###########
#############################################

# School survey. This policy lever measures whether there is a teacher evaluation system in place, and if so, the types of decisions that are made based on the evaluation results. Score is the sum of the following: 
#   - 1 Point. Was teacher formally evaluated in past school year? 
#   - 1 Point total. 0.2 points for each of the following: Evaluation included evaluation of attendance, knowledge of subject matter, pedagogical skills in the classroom, students' academic achievement, students' socio-emotional development 
# - 1 Point. Consequences exist if teacher receives 2 or more negative evaluations 
# - 1 Point. Rewards exist if teacher receives 2 or more positive evaluations


#list of teacher evluation questions
tevl<-c(
  'm3sbq7_tmna__1', 'm3sbq7_tmna__2', 'm3sbq7_tmna__3', 'm3sbq7_tmna__4','m3sbq7_tmna__5', 'm3sbq7_tmna__6', 'm3sbq7_tmna__97',
  'm3sbq8_tmna__2', 'm3sbq8_tmna__3', 'm3sbq8_tmna__4','m3sbq8_tmna__5', 'm3sbq8_tmna__6', 'm3sbq8_tmna__7', 'm3sbq8_tmna__8', 'm3sbq8_tmna__97', 'm3sbq8_tmna__98',
  'm3sbq9_tmna__1', 'm3sbq9_tmna__2', 'm3sbq9_tmna__3', 'm3sbq9_tmna__4', 'm3sbq9_tmna__7', 'm3sbq9_tmna__97', 'm3sbq9_tmna__98',
  'm3bq10_tmna__1', 'm3bq10_tmna__2', 'm3bq10_tmna__3', 'm3bq10_tmna__4', 'm3bq10_tmna__7', 'm3bq10_tmna__97', 'm3bq10_tmna__98')

teacher_questionnaire_TEVL <- teacher_questionnaire_TMNA %>%
  dplyr::select(school_code, preamble_info_teacher, tevl, m3sbq6_tmna, m3sbq8_tmna__1)

teacher_questionnaire_TEVL<- teacher_questionnaire_TEVL %>%
  mutate(formally_evaluated=bin_var(m3sbq6_tmna,1),
         evaluation_content=if_else(m3sbq6_tmna==1,
                                    (m3sbq8_tmna__1+m3sbq8_tmna__2+ m3sbq8_tmna__3 + m3sbq8_tmna__5 + m3sbq8_tmna__6)/5,
                                    0),
         negative_consequences=case_when(
           (m3sbq9_tmna__1==1 | m3sbq9_tmna__2==1 | m3sbq9_tmna__3==1 | m3sbq9_tmna__4==1 | m3sbq9_tmna__97==1) ~ 1,
           (is.na(m3sbq9_tmna__1) & is.na(m3sbq9_tmna__2) & is.na(m3sbq9_tmna__3) & is.na(m3sbq9_tmna__4) & is.na(m3sbq9_tmna__97)) ~ as.numeric(NA),
           TRUE ~ 0),
         positive_consequences=case_when(
           (m3bq10_tmna__1==1 | m3bq10_tmna__2==1 | m3bq10_tmna__3==1 | m3bq10_tmna__4==1 | m3bq10_tmna__97==1) ~ 1,
           (is.na(m3bq10_tmna__1) & is.na(m3bq10_tmna__2) & is.na(m3bq10_tmna__3) & is.na(m3bq10_tmna__4) & is.na(m3bq10_tmna__97)) ~ as.numeric(NA),
           TRUE ~ 0)
  ) %>%
  mutate(teaching_evaluation=1+formally_evaluated+evaluation_content+negative_consequences+positive_consequences)


final_indicator_data_TEVL <- teacher_questionnaire_TEVL %>%
  mutate(n_mssing_TEVL=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  





#############################################
##### Teacher  Monitoring and Accountability ###########
#############################################

# School Survey. This policy lever measures the extent to which teacher presence is being monitored, whether attendance is rewarded, and whether there are consequences for chronic absence. Score is the sum of the following: 
#   - 1 Point. Teachers evaluated by some authority on basis of absence. 
# - 1 Point. Good attendance is rewarded. 
# - 1 Point. There are consequences for chronic absence (more than 30% absence). 
# - 1 Point. One minus the fraction of teachers that had to miss class because of any of the following: collect paycheck, school administrative procedure, errands or request of the school district office, other administrative tasks.

teacher_questionnaire_TMNA2 <- teacher_questionnaire_TATT %>%
  select(interview__id, school_code, teacher_name, teacher_number, m3seq4_tatt, m3seq5_tatt__1, m3sbq1_tatt__1, m3sbq1_tatt__2, m3sbq1_tatt__3, m3sbq1_tatt__97, m3sbq1_other_tatt )

teacher_questionnaire_TMNA <- teacher_questionnaire_TMNA %>%
  dplyr::select(-tevl)

teacher_questionnaire_TMNA <- teacher_questionnaire_TMNA %>%
  left_join(teacher_questionnaire_TMNA2)

teacher_questionnaire_TMNA <- teacher_questionnaire_TMNA %>%
  mutate(attendance_evaluated=if_else(m3sbq6_tmna==1,
                                      case_when(
                                        (m3sbq8_tmna__1==1) ~ 1,
                                        TRUE ~ 0
                                      ),
                                      0),
         attendance_rewarded=if_else(m3seq4_tatt==1,
                                     case_when(
                                       (m3seq5_tatt__1==1) ~ 1,
                                       TRUE ~ 0
                                     ),
                                     0),
         attendence_sanctions=case_when(
           (m3sbq2_tmna__1==1 | m3sbq2_tmna__2==1 | m3sbq2_tmna__3==1 | m3sbq2_tmna__4==1 | m3sbq2_tmna__97==1) ~ 1,
           (is.na(m3sbq2_tmna__1) & is.na(m3sbq2_tmna__2) & is.na(m3sbq2_tmna__3) & is.na(m3sbq2_tmna__4) & is.na(m3sbq2_tmna__97)) ~ as.numeric(NA),
           TRUE ~ 0
         ),
         miss_class_admin=case_when(
           (m3sbq1_tatt__1==1 | m3sbq1_tatt__2==1 | m3sbq1_tatt__3==1 | m3sbq1_tatt__97==1) ~ 1,
           (m3sbq1_tatt__1==0 & m3sbq1_tatt__2==0 & m3sbq1_tatt__3==0 & m3sbq1_tatt__97==0) ~ 0,
           (grepl('salud', teacher_questionnaire_TMNA$m3sbq1_other_tatt)) ~ 0, #some teachers reported missing for health reasons, we don't want these included.
           TRUE ~ as.numeric(NA)
         )
  )  %>%
  mutate(teacher_monitoring=1+attendance_evaluated + 1*attendance_rewarded + 1*attendence_sanctions + (1-miss_class_admin))

final_indicator_data_TMNA <- teacher_questionnaire_TMNA %>%
  mutate(n_mssing_TMNA=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  




#############################################
##### Teacher  Intrinsic Motivation ###########
#############################################

# 
# School Survey. This lever measures whether teachers are intrinsically motivated to teach. The question(s) aim to address this 
# phenomenon by measuring the level of intrinsic motivation among teachers as well as teacher values that may be relevant for 
# ensuring that the teacher is motivated to focus on all children and not just some. Average score (1 (worst) - 5 (best)) on items 
# given to teachers on intrinsic motivation.

intrinsic_motiv_q_rev <- c('m3scq1_tinm','m3scq2_tinm', 'm3scq3_tinm', 'm3scq4_tinm', 'm3scq5_tinm', 'm3scq6_tinm',
                           'm3scq7_tinm', 'm3scq10_tinm')

intrinsic_motiv_q <- c( 'm3scq11_tinm', 'm3scq14_tinm')

intrinsic_motiv_q_all <- c('m3scq1_tinm','m3scq2_tinm', 'm3scq3_tinm', 'm3scq4_tinm', 'm3scq5_tinm', 'm3scq6_tinm',
                           'm3scq7_tinm', 'm3scq10_tinm', 'm3scq11_tinm', 'm3scq14_tinm')

teacher_questionnaire_TINM2 <- teacher_questionnaire_TMNA %>%
  dplyr::select(school_code, preamble_info_teacher, m3sdq2_tmna)

final_indicator_data_TINM <- teacher_questionnaire_TINM %>%
  left_join(teacher_questionnaire_TINM2) %>%
  mutate(n_mssing_TINM=n_miss_row(.)) %>%
  mutate(    
    SE_PRM_TINM_1 = 100*if_else(m3scq1_tinm>=4,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if the ~
    SE_PRM_TINM_2 = 100*if_else(m3scq2_tinm>=4,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if stud~
    SE_PRM_TINM_3 = 100*if_else(m3scq3_tinm>=4,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if the ~
    SE_PRM_TINM_4 = 100*if_else(m3scq4_tinm>=4,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they attend scho~
    SE_PRM_TINM_5 = 100*if_else(m3scq5_tinm>=4,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they come to sch~
    SE_PRM_TINM_6 = 100*if_else(m3scq6_tinm>=4,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they are motivat~
    SE_PRM_TINM_7 = 100*if_else(m3scq7_tinm>=4,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students have a certain amount of intelligence and ~
    SE_PRM_TINM_8 = 100*if_else(m3scq10_tinm>=4,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with To be honest, students can't really change how inte~
    SE_PRM_TINM_9 = 100*if_else(m3scq11_tinm>=4,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students can always substantially change how intell~
    SE_PRM_TINM_10 = 100*if_else(m3scq14_tinm>=4,1,0) #(De Facto) Percent of teachers that agree or strongly agrees with \"Students can change even their basic intelligence l~
  ) %>%
  mutate_at(intrinsic_motiv_q_rev, attitude_fun_rev ) %>%
  mutate_at(intrinsic_motiv_q, attitude_fun ) %>%
  mutate(acceptable_absent=(m3scq1_tinm+ m3scq2_tinm + m3scq3_tinm)/3,
         students_deserve_attention=(m3scq4_tinm+ m3scq5_tinm + m3scq6_tinm )/3,
         growth_mindset=(m3scq7_tinm + m3scq10_tinm + m3scq11_tinm + m3scq14_tinm)/4,
         motivation_teaching=case_when(
           m3scq15_tinm__3>=1 ~ 0,
           (m3scq15_tinm__3!=1 & (m3scq15_tinm__1>=1 | m3scq15_tinm__2>=1 | m3scq15_tinm__4>=1 & m3scq15_tinm__5>=1)) ~ 1,
           TRUE ~ as.numeric(NA)
         )) %>%
  mutate(intrinsic_motivation=1+0.8*(0.2*acceptable_absent + 0.2*students_deserve_attention + 0.2*growth_mindset + motivation_teaching+bin_var(m3sdq2_tmna,1))) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  



#############################################
##### School  Inputs and Infrastructure Standards ###########
#############################################
#   - 1 Point. Are there standards in place to monitor blackboard and chalk, pens and pencils, basic classroom furniture, computers, textbooks, exercise books, toilets, electricity, drinking water, accessibility for those with disabilities? (partial credit available) 

school_data_ISTD <- school_data_IMON %>%
  mutate(standards_monitoring_input=rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                    pattern="m1scq13_imon__")], na.rm=TRUE),
         standards_monitoring_infrastructure=rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                             pattern="m1scq14_imon__")], na.rm=TRUE) ) %>%
  mutate(standards_monitoring=(standards_monitoring_input*6+standards_monitoring_infrastructure*4)/2)


final_indicator_data_ISTD <- school_data_ISTD %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_ISTD=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  

#############################################
##### School  Inputs and Infrastructure Monitoring ###########
#############################################

# School Survey. This lever measures the extent to which there is a monitoring system in place to ensure that the inputs that must be available at the schools are in fact available at the schools. This set of questions will include three aspects: 
# - 1 Point. Are all input items (functioning blackboard, chalk, pens, pencils, textbooks, exercise books in 4th grade classrooms, basic classroom furniture, and at least one computer in the schools) being monitored? (partial credit available) 
# - 1 Point. Are all infrastructure items (functioning toilets, electricity, drinking water, and accessibility for people with disabilities) being monitored? (partial credit available) 
# - 1 Point. Is the community involved in the monitoring?

school_data_IMON <- school_data_IMON %>%
  mutate(m1scq3_imon=bin_var(m1scq3_imon,1),
         m1scq5_imon=case_when(
           m1scq5_imon==0 ~ 0,
           m1scq5_imon==1 ~ 1,
           m1scq5_imon==2 ~ 0.5,
           TRUE ~ 0
         )) %>%
  mutate(monitoring_inputs=if_else(m1scq1_imon==1,
                                   rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                   pattern="m1scq4_imon__")], na.rm=TRUE),
                                   0),
         monitoring_infrastructure=if_else(m1scq7_imon==1,
                                           rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                           pattern="m1scq9_imon__")], na.rm=TRUE),
                                           0),
  ) %>%
  mutate(parents_involved=if_else(m1scq3_imon==1,1,0,0)) %>%
  mutate(sch_monitoring=1+1.5*monitoring_inputs+1.5*monitoring_infrastructure+parents_involved)



final_indicator_data_IMON <- school_data_IMON %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_IMON=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  


#############################################
##### School School Management Clarity of Functions  ###########
#############################################

school_data_SCFN <- school_data_PKNW %>%
  mutate(infrastructure_scfn=if_else((m7sfq15a_pknw__0==1 | m7sfq15a_pknw__98==1),0,1),
         materials_scfn=if_else((m7sfq15b_pknw__0==1 | m7sfq15b_pknw__98==1),0,1),
         hiring_scfn=if_else((m7sfq15c_pknw__0==1 | m7sfq15c_pknw__98==1),0,1),
         supervision_scfn=if_else((m7sfq15d_pknw__0==1 | m7sfq15d_pknw__98==1),0,1),
         student_scfn=if_else((m7sfq15e_pknw__0==1 | m7sfq15e_pknw__98==1),0,1),
         principal_hiring_scfn=if_else((m7sfq15f_pknw__0==1 | m7sfq15f_pknw__98==1),0,1),
         principal_supervision_scfn=if_else((m7sfq15g_pknw__0==1 | m7sfq15g_pknw__98==1),0,1)
  ) %>%
  mutate(sch_management_clarity=1+
           (infrastructure_scfn+materials_scfn)/2+
           (hiring_scfn + supervision_scfn)/2 +
           student_scfn +
           (principal_hiring_scfn+ principal_supervision_scfn)/2
  )
final_indicator_data_SCFN <- school_data_SCFN %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SCFN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  

#############################################
##### School School Management Attraction  ###########
#############################################

# This policy lever measures whether the right candidates are being attracted to the profession of school principals. The questions will aim to capture the provision of benefits to attract and maintain the best people to serve as principals. 
# 
# Scoring: 
#   -score is between 1-5 based on how satisfied the principal is with status in community. We will also add in component based on Principal salaries.
# For salary, based GDP per capita from 2018 World Bank  https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=JO.  

school_data_SATT <- school_data_SATT %>%
  mutate(principal_satisfaction=attitude_fun_rev(m7shq1_satt),
         principal_salary=12*m7shq2_satt/22813.06	) %>%
  mutate(
    principal_salary_score=case_when(
      between(principal_salary,0,0.5) ~ 1,
      between(principal_salary,0.5,0.75) ~ 2,
      between(principal_salary,0.75,1) ~ 3,
      between(principal_salary,1,1.5) ~ 4,
      between(principal_salary,1.5,5) ~ 5)) %>%
  mutate(sch_management_attraction=(principal_satisfaction+principal_salary_score)/2)

final_indicator_data_SATT <- school_data_SATT %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SATT=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  



#############################################
##### School School Management Selection and Deployment  ###########
#############################################


# This policy lever measures whether the right candidates being selected. These questions will probe what the recruitment process is like to 
# ensure that these individuals are getting the positions. The question would ultimately be based on: 1) there is a standard approach for selecting principals,
# 2) that approach relies on professional/academic requirements, and 3) those requirements are common in practice. 
# 
# Scoring: 
#   - 1 (lowest score) Most important factor is political affiliations or ethnic group. 
# - 2 Political affiliations or ethnic group is a consideration, but other factors considered as well. 
# - 3 Most important factor is years of experience, good relationship with owner/education department, and does not factor in quality teaching, demonstrated management qualities, or knowledge of local community. 
# - 4 Quality teaching, demonstrated management qualities, or knowledge of local community is a consideration in hiring, but not the most important factor 
# - 5 Quality teaching, demonstrated management qualities, or knowledge of local community is the most important factor in hiring.

school_data_SSLD <- school_data_SSLD %>%
  mutate(sch_selection_deployment=case_when(
    (m7sgq2_ssld==2 | m7sgq2_ssld==3 | m7sgq2_ssld==8) ~ 5,
    (m7sgq2_ssld==6 | m7sgq2_ssld==7) ~ 1,
    (!(m7sgq2_ssld==6 | m7sgq2_ssld==7) & (m7sgq1_ssld__2==1 | m7sgq1_ssld__3==1 | m7sgq1_ssld__8==1) ) ~ 4,
    (!(m7sgq2_ssld==6 | m7sgq2_ssld==7) & (m7sgq1_ssld__1==1 | m7sgq1_ssld__4==1 | m7sgq1_ssld__5==1 | m7sgq1_ssld__97==1) ) ~ 3,
    (m7sgq1_ssld__6==1 | m7sgq1_ssld__7==1 ) ~ 2, 
    TRUE ~ as.numeric(NA))
  )

final_indicator_data_SSLD <- school_data_SSLD %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  



#############################################
##### School School Management Support  ###########
#############################################


# This policy lever measures the extent to which principals receive training and/or exposure to other professional opportunities that could help them be better school leaders. 
# The questions aim to figure out if such programs are provided, and if they are, their level of quality. 
# 
# Scoring (sum of components below): 
#   - 1 Point. Principal has received formal training on managing school. 
# - 1/3 Point. Had management training for new principals. 
# - 1/3 Point. Had management in-service training. 
# - 1/3 Point. Had mentoring/coaching by experienced principals. 
# - 1 Point. Have used skills gained at training. 
# - 1 Point. Principals offered training at least once per year

school_data_SSUP <- school_data_SSUP %>%
  mutate(prinicipal_trained=bin_var(m7sgq3_ssup,1),
         principal_training=if_else(m7sgq3_ssup==1,
                                    rowMeans(.[grep(x=colnames(school_data_SSUP), 
                                                    pattern="m7sgq4_ssup__")], na.rm=TRUE),
                                    0),
         principal_used_skills=if_else(m7sgq3_ssup==1,
                                       bin_var(m7sgq5_ssup,1),0),
         principal_offered=if_else((m7sgq7_ssup==2 | m7sgq7_ssup==3 | m7sgq7_ssup==4 | m7sgq7_ssup==5),1,0)
  ) %>%
  mutate(sch_support=1+prinicipal_trained+principal_training+principal_used_skills+principal_offered)

final_indicator_data_SSUP <- school_data_SSUP %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSUP=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  


#############################################
##### School School Management Evaluation  ###########
#############################################

# School Survey. This policy lever measures the extent to which principal performance is being monitored and enforced via accountability measures. 
# The idea is that the indicator will be based on: 1) there is a legislation outlining the need to monitor, 2) principals are being evaluated, 3) 
# principals are being evaluated on multiple things, and 4) there the accountability mechanisms in place.



school_data_SEVL<- school_data_SEVL %>%
  mutate(principal_formally_evaluated=bin_var(m7sgq8_sevl,1),
         principal_eval_tot=rowSums(.[grep(x=colnames(school_data_SEVL), 
                                           pattern="m7sgq10_sevl__")], na.rm=TRUE)-m7sgq10_sevl__98) %>%
  mutate(principal_evaluation_multiple=if_else(m7sgq8_sevl==1,
                                               case_when(
                                                 principal_eval_tot>=5 ~ 1,
                                                 (principal_eval_tot>1 & principal_eval_tot<5) ~ 0.666667,
                                                 principal_eval_tot==1 ~ 0.3333333,
                                                 TRUE ~ 0
                                               ),
                                               0),
         principal_negative_consequences=case_when(
           (m7sgq11_sevl__1==1 | m7sgq11_sevl__2==1 | m7sgq11_sevl__3==1 | m7sgq11_sevl__4==1 | m7sgq11_sevl__97==1) ~ 1,
           TRUE ~ 0),
         principal_positive_consequences=case_when(
           (m7sgq12_sevl__1==1 | m7sgq12_sevl__2==1 | m7sgq12_sevl__3==1 | m7sgq12_sevl__4==1 | m7sgq12_sevl__97==1) ~ 1,
           TRUE ~ 0)
  ) %>%
  mutate(principal_evaluation=1+principal_formally_evaluated+principal_evaluation_multiple+principal_negative_consequences+principal_positive_consequences)

final_indicator_data_SEVL <- school_data_SEVL %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SEVL=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  




#############################################
##### School Level Info ###########
#############################################

#Build school level database

#first create temp dataset with only required info (school_code + indicator info).  Main thing here is to drop enumerator code, interview ID, which mess up merges
#list additional info that will be useful to keep in each indicator dataframe
drop_info <- c('interview__id', 'interview__key',                    
               'school_name_preload', 'school_address_preload', 
               'school_province_preload', 'school_district_preload', 'school_code_preload', 'school_emis_preload',
               'school_info_correct', 'm1s0q2_name', 'm1s0q2_code', 'm1s0q2_emis',
               'survey_time', 'lat', 'lon', 'm7saq10' )

keep_info <-       c('school_code',
                     'school_name_preload', 'school_address_preload', 
                     'school_province_preload', 'school_district_preload', 'school_code_preload', 'school_emis_preload',
                     'school_info_correct', 'm1s0q2_name', 'm1s0q2_code', 'm1s0q2_emis',
                     'survey_time', 'lat', 'lon', 'total_enrolled')

if (exists('final_school_data')) {
  rm('final_school_data')
}

ind_dta_list<-c()

school_data_preamble_short<-school_data_preamble %>%
  group_by(school_code) %>%
  select(all_of(keep_info)) %>%
  summarise_all(~first(na.omit(.)))

final_school_data <- school_data_preamble_short


for (i in indicator_names ) {
  if (exists(paste("final_indicator_data_",i, sep=""))) {
    #form temp data frame with each schools data
    temp<-get(paste("final_indicator_data_",i, sep="")) 
    
    #add element to list
    ind_dta_list<-c(ind_dta_list, paste("final_indicator_data_",i, sep=""))
    
    
    print(i)
    #Merge this to overall final_school_data frame
    if (!exists('final_school_data')) {
      final_school_data<-temp
      print(i)
      write_excel_csv(temp, path = file.path(paste(confidential_folder,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
      if (backup_onedrive=="yes") {
        write_excel_csv(temp, path = file.path(paste(confidential_folder_onedrive,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
      }
      
    } else {
      final_school_data<-final_school_data %>%
        left_join(temp, by='school_code') %>%
        select(-ends_with(".x"), -ends_with(".y"))
      
      write_excel_csv(temp, path = file.path(paste(confidential_folder,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
      if (backup_onedrive=="yes") {
        write_excel_csv(temp, path = file.path(paste(confidential_folder_onedrive,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
      }
    }
  }
}




#Create list of key indicators
ind_list<-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge', 
            'student_proficient', 'student_proficient_70', 'student_proficient_75',
            'literacy_student_proficient', 'literacy_student_proficient_70', 'literacy_student_proficient_75',
            'math_student_proficient', 'math_student_proficient_70', 'math_student_proficient_75',
            'presence_rate','absence_rate', 'sch_absence_rate', 'student_attendance',
            'content_knowledge', 'math_content_knowledge', 'literacy_content_knowledge', 
            'content_proficiency',  'content_proficiency_70', 'content_proficiency_75',
            'literacy_content_proficiency',  'literacy_content_proficiency_70', 'literacy_content_proficiency_75',
            'math_content_proficiency',  'math_content_proficiency_70', 'math_content_proficiency_75',
            'teach_score','classroom_culture','instruction','socio_emotional_skills',
            'teach_prof','classroom_culture_prof','instruction_prof','socio_emotional_skills_prof',
            'ecd_student_proficiency', 'ecd_math_student_proficiency', 'ecd_literacy_student_proficiency', 'ecd_exec_student_proficiency', 'ecd_soc_student_proficiency',
            'ecd_student_knowledge', 'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
            'inputs', 'blackboard_functional', 'pens_etc', 'textbooks', 'share_desk', 'used_ict', 'access_ict',
            'infrastructure','drinking_water', 'functioning_toilet', 'internet', 'class_electricity','disability_accessibility','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
            'operational_management', 'vignette_1', 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', 'vignette_2', 'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
            'intrinsic_motivation', 'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
            'instructional_leadership', 'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
            'principal_knowledge_score', 'add_triple_digit_pknw', 'multiply_double_digit_pknw', 'complete_sentence_pknw', 'experience_pknw', 'textbooks_pknw', 'blackboard_pknw',
            'principal_management', 'goal_setting', 'school_goals_exist','school_goals_clear','school_goals_relevant','school_goals_measured', 
            'problem_solving', 'problem_solving_proactive','problem_solving_info_collect','problem_solving_stomach',
            'teacher_attraction', 'teacher_satisfied_job', 'teacher_satisfied_status', 'better_teachers_promoted' ,'teacher_bonus', 'salary_delays',
            'teacher_selection_deployment', 'teacher_selection','teacher_deployment',
            'teacher_support', 'pre_service','practicum','in_service','opportunities_teachers_share',
            'teaching_evaluation', 'formally_evaluated', 'evaluation_content', 'negative_consequences','positive_consequences',
            'teacher_monitoring','attendance_evaluated' , 'attendance_rewarded' , 'attendence_sanctions', 'miss_class_admin',
            'sch_management_clarity', 'infrastructure_scfn','materials_scfn','hiring_scfn', 'supervision_scfn', 'student_scfn' , 'principal_hiring_scfn', 'principal_supervision_scfn',
            'standards_monitoring',
            'sch_monitoring', 'monitoring_inputs','monitoring_infrastructure','parents_involved',
            'sch_management_attraction', 'principal_satisfaction',
            'sch_selection_deployment', 
            'sch_support', 'prinicipal_trained','principal_training','principal_used_skills','principal_offered',
            'principal_evaluation', 'principal_formally_evaluated','principal_evaluation_multiple','principal_negative_consequences','principal_positive_consequences'
)





final_school_data <- final_school_data %>%
  left_join(school_data_preamble_short) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(all_of(keep_info), ind_list, everything())



write.csv(final_school_data, file = file.path(confidential_folder, "final_complete_school_data.csv"))
write_dta(final_school_data, path = file.path(confidential_folder, "final_complete_school_data.dta"), version = 14)
if (backup_onedrive=="yes") {
  write.csv(final_school_data, file = file.path(confidential_folder_onedrive, "final_complete_school_data.csv"))
  write_dta(final_school_data, path = file.path(confidential_folder_onedrive, "final_complete_school_data.dta"), version = 14)
}
#If indicator in this list doesn't exists, create empty column with Missing values


for (i in ind_list ) {
  if(!(i %in% colnames(final_school_data))) {
    print(i)
    final_school_data[, i] <- NA
  }
}





school_dta_short <- final_school_data %>%
  select(all_of(keep_info),  ind_list)


write.csv(school_dta_short, file = file.path(confidential_folder, "final_indicator_school_data.csv"))
write_dta(school_dta_short, path = file.path(confidential_folder, "final_indicator_school_data.dta"), version = 14)

if (backup_onedrive=="yes") {
  write.csv(school_dta_short, file = file.path(confidential_folder_onedrive, "final_indicator_school_data.csv"))
  write_dta(school_dta_short, path = file.path(confidential_folder_onedrive, "final_indicator_school_data.dta"), version = 14)
}


#save teacher level files
teacher_data_list <- c('school_dta', 'school_dta_short',  'school_data_preamble', 'final_school_data', 'teacher_questionnaire','teacher_absence_final', 'ecd_dta', 'teacher_assessment_dta', 'teacher_roster',
                       'teacher_questionnaire_ILDR',
                       'teacher_questionnaire_INPT',
                       'teacher_questionnaire_TATT',
                       'teacher_questionnaire_TEVL',
                       'teacher_questionnaire_TINM',
                       'teacher_questionnaire_TMNA',
                       'teacher_questionnaire_TSDP',
                       'teacher_questionnaire_TSUP',
                       'teacher_questionnaire_ILDR',
                       'school_data_SATT',
                       'school_data_SCFN',
                       'school_data_SEVL',
                       'school_data_SSLD',
                       'school_data_SSUP')

save(list=teacher_data_list, file = file.path(confidential_folder, "teacher_survey_data.RData"))


#################################
# Read in Satellite Data on GDP
#################################
#Data downloaded from Here:
#https://datacatalog.worldbank.org/dataset/gross-domestic-product-2010 also see
#https://preview.grid.unep.ch/index.php?preview=data&events=socec&evcat=1&lang=eng
# In the distributed global GDP dataset sub-national GRP and national GDP data are allocated to 
# 30 arc second (approximately 1km) grid cells in proportion to the population residing in that cell. 
# The method also distinguishes between rural and urban population, assuming the latter to have a higher 
# GDP per capita. Input data are from 1) a global time-series dataset of GDP, with subnational gross regional 
# product (GRP) for 74 countries, compiled by the World Bank Development Economics Research Group (DECRG). 2) 
# Gridded population projections for the year 2009, based on a population grid for the year 2005 provided by 
# LandScanTM Global Population Database (Oak Ridge, TN: Oak Ridge National Laboratory). This dataset has been 
# extrapolated to year 2010 by UNEP/GRID-Geneva. Unit is estimated value of production per cell, in thousand of 
# constant 2000 USD. Cell level anomalies may occur due to poor alignment of multiple input data sources, and it 
# is strongly recommended that users attempt to verify information, or consult original sources, in order to determine 
# suitability for a particular application. This product was compiled by DECRG for the Global Assessment Report on Risk 
# Reduction (GAR). It was modeled using global data. Credit: GIS processing World Bank DECRG, Washington, DC, 
# extrapolation UNEP/GRID-Geneva.

#Load original sample of schools
currentDate<-c("2019-07-22")
sample_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/sampling/", sep="/"))
sample_frame_name <- paste(sample_folder,"/school_sample_",currentDate,".RData", sep="")

load(sample_frame_name)


#open the raster
raster_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/Maps/GDP_PERU/", sep="/")) 

gdp_raster <- raster::raster(paste(raster_folder, "/GDP.tif", sep="/"))

#add GDP to database
school_gdp <- school_dta_short %>%
  mutate(codigo.modular=as.numeric(school_code_preload)) %>%
  left_join(data_set_updated) %>%
  mutate(longitude=as.character(longitude)) %>%
  mutate(latitude=as.character(latitude)) %>%
  mutate(lat=if_else(is.na(lat), as.numeric(latitude), lat),
         lon=if_else(is.na(lon), as.numeric(longitude), lon)) %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  dplyr::select(school_code, lon, lat)


sp::coordinates(school_gdp) <- c("lon","lat")
school_gdp$GDP <- raster::extract(gdp_raster, school_gdp, 
                                  buffer=1000, # 1000m radius
                                  fun=mean,na.rm=T,
                                  method='simple')


school_gdp <- as.data.frame(school_gdp) %>%
  mutate(GDP=as.numeric(GDP)) %>%
  select(school_code, GDP)

####################################
# Multiple Imputation of missing values
###################################

#use random forest approach to multiple imputation.  Some published research suggest this is a better approach than other methods.
#https://academic.oup.com/aje/article/179/6/764/107562
impdata<-mice::mice(school_dta_short, m=1,
                    method='rf',
                    maxit = 50, seed = 500)

school_dta_short_imp <- mice::complete(impdata, 1)


##########################################
#define function to create weights for summary statistics
##########################################
#Load original sample of schools
currentDate<-c("2019-07-22")
sample_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/sampling/", sep="/"))
sample_frame_name <- paste(sample_folder,"/school_sample_",currentDate,".RData", sep="")

load(sample_frame_name)


df_weights_function <- function(dataset,scode, snumber, prov) {
  scode<-enquo(scode)  
  snumber<-enquo(snumber)
  prov<-enquo(prov)
  
  dataset %>%
    mutate(!! scode := as.numeric(.data$school_code)) %>%
    left_join(data_set_updated) %>%
    mutate(ipw=if_else(is.na(.data$weights), median(.data$weights, na.rm=T), .data$weights)*!! snumber ) %>%
    select(-one_of(colnames(data_set_updated[, -which(names(data_set_updated) == "rural")])))
}


weights <- df_weights_function(school_dta_short, codigo.modular, total_4th, departamento) %>%
  select(school_code,  ipw, rural)

################################
#Store Key Created Datasets
################################

school_metadta$varlabel<-as.character(school_metadta$varlabel)
ecd_dta_metadata$varlabel<-as.character(ecd_dta_metadata$varlabel)
assess_4th_grade_metadta$varlabel<-as.character(assess_4th_grade_metadta$varlabel)
teacher_questionnaire_metadta$varlabel<-as.character(teacher_questionnaire_metadta$varlabel)
teacher_metadata$varlabel<-as.character(teacher_metadata$varlabel)

metadta<-bind_rows(school_metadta,  ecd_dta_metadata, assess_4th_grade_metadta, teacher_questionnaire_metadta, teacher_metadata)


#saves the following in R and stata format
#add male/female breakdowns to ind_data_list

ind_dta_list<-c(ind_dta_list, c("final_indicator_data_ATTD_M", "final_indicator_data_ATTD_F", 
                                "final_indicator_data_CONT_M", "final_indicator_data_CONT_F", "final_indicator_data_CONT_after_2015",
                                "final_indicator_data_EFFT_M", "final_indicator_data_EFFT_F", 
                                "final_indicator_data_LCAP_M", "final_indicator_data_LCAP_F", 
                                "final_indicator_data_LERN_M", "final_indicator_data_LERN_F",
                                "final_indicator_data_LERN_M", "final_indicator_data_LERN_F",
                                "final_indicator_data_OPMN_M", "final_indicator_data_OPMN_F",
                                "final_indicator_data_ILDR_M", "final_indicator_data_ILDR_F",
                                "final_indicator_data_PKNW_M", "final_indicator_data_PKNW_F",
                                "final_indicator_data_PMAN_M", "final_indicator_data_PMAN_F"))


data_list <- c(ind_dta_list, 'school_dta', 'school_dta_short', 'school_dta_short_imp', 'school_data_preamble', 'final_school_data', 'teacher_questionnaire','teacher_absence_final', 'ecd_dta', 'teacher_assessment_dta', 'teacher_roster', 
               "indicators", 'metadta', 'school_gdp', 'assess_4th_grade_anon', 'ecd_dta_anon')
dta_list <- c(ind_dta_list, 'school_dta', 'school_dta_short', 'school_dta_short_imp', 'teacher_questionnaire','teacher_absence_final', 'ecd_dta', 'teacher_assessment_dta', 'teacher_roster', 
                 'school_gdp', 'assess_4th_grade_anon', 'ecd_dta_anon')
save(list=data_list, file = file.path(confidential_folder, "school_survey_data.RData"))


save(list=c(ind_dta_list,"school_dta_short", 'school_dta_short_imp', "indicators", 'metadta', 'school_gdp', 'assess_4th_grade_anon' ), file = file.path(confidential_folder, "school_indicators_data.RData"))



names(dta_list) <- dta_list
#mapply(dta_list,  paste(paste0(names(dta_list), '.dta')))

#mapply(write_dta, dta_list,  path = file.path(paste(confidential_folder,"data", sep="/"), paste0(names(dta_list), '.dta')))
names(dta_list) <- dta_list
for(i in names(dta_list)){
  print( i)
  write_dta(get(dta_list[[i]]), file.path(paste(confidential_folder,"data", sep="/"), paste0(names(dta_list[[i]]), '.dta')))
}


if (backup_onedrive=="yes") {
  save(list=data_list, file = file.path(confidential_folder_onedrive, "school_survey_data.RData"))
  names(dta_list) <- dta_list
  for(i in names(dta_list)){
    print( i)
    write_dta(get(dta_list[[i]]), file.path(paste(confidential_folder_onedrive,"data", sep="/"), paste0(dta_list[[i]], '.dta')))
  }
  
  
  
    }
