#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(skimr)
library(naniar)
library(vtable)
#NOTE:  The R script to pull the data from the API should be run before this file

#move working directory to github main folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')

#Create function to save metadata for each question in each module
#The attr function retrieves metadata imported by haven. E.g. attr(school_dta$m1s0q2_code, "label")
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}





# File paths
#You can enter your UPI and specify a path, or you can select manually in a new window
if (Sys.getenv("USERNAME") == "wb469649"){
  project_folder  <- "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work"
  download_folder <-file.path(paste(project_folder,country,year,"Data/raw/School", sep="/"))
} else {
  
  download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
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
school_dta<-read_dta(file.path(download_folder, "EPDash.dta"))
vtable(school_dta)
#rename a few key variables up front
school_dta<- school_dta %>%
  mutate(enumerator_name_other= m1s0q1_name_other  ,
         enumerator_number=if_else(!is.na(m1s0q1_name),m1s0q1_name, as.double(m1s0q1_number_other)) ,
         survey_time=m1s0q8,
         lat=m1s0q9__Latitude,
         lon=m1s0q9__Longitude,
         school_code=if_else(!is.na(school_code_preload),as.double(school_code_preload), as.double(m1s0q2_code))
  )

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
preamble_info <- c('interview__id', 'school_code',
                   'school_name_preload', 'school_address_preload', 
                   'school_province_preload', 'school_district_preload', 'school_code_preload', 'school_emis_preload',
                   'school_info_correct', 'm1s0q2_name', 'm1s0q2_code', 'm1s0q2_emis',
                   'survey_time', 'lat', 'lon', 
                   'enumerator_name_other', 'enumerator_number')

#Create a list of info to drop from final teacher files aggregated to school level. This will be necessary for merging these databases later

drop_teacher_info <- c( "school_code", "school_code_preload", "school_emis_preload", "survey_time", "lat", "lon",
                        "questionnaire_roster__id", "teacher_name", "teacher_number", "available", 
                       "teacher_position", "teacher_grd1", "teacher_grd2", "teacher_grd3", "teacher_grd4", 
                       "teacher_grd5", "teacher_language", "teacher_math", "teacher_both_subj", "teacher_other_subj", 
                       "teacher_education", "teacher_year_began", "teacher_age" )


#create school database with just preamble info.  This will be useful for merging on school level info to some databases
school_data_preamble <- school_dta %>%
  group_by(interview__id) %>%
  select( preamble_info)

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
  select(preamble_info, enumerator_name_other, everything())




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
preamble_info_teacher <- c('interview__id', 'questionnaire_roster__id', 'teacher_name', 'teacher_number',
                          'available', 'teacher_position', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 'teacher_grd5',
                          'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'teacher_education', 'teacher_year_began',
                          'teacher_age')



#use dplyr select(contains()) to search for variables with select tags and create separate databases
for (i in indicator_names ) {
  temp_df<-teacher_questionnaire %>%
    select( contains(i))
  if (ncol(temp_df) > 0) {
    temp_df<-teacher_questionnaire %>%
      select(preamble_info, preamble_info_teacher, contains(i))
    assign(paste("teacher_questionnaire_",i, sep=""), temp_df )
  }
}



#############################################
##### Teacher Absence ###########
#############################################

# School survey. Percent of teachers absent. Teacher is coded absent if they are: 
#   - not in school 
# - in school but absent from the class 
# - in the class but not teaching.

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
  mutate(school_absent=case_when(
    m2sbq6_efft==6 | teacher_available==2 ~ 1,
    
    m2sbq6_efft!=6   ~ 0,
    is.na(m2sbq6_efft) ~ as.numeric(NA)))

#create indicator for whether each teacher was absent from classroom or school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(absent=case_when(
    m2sbq6_efft==6 | m2sbq6_efft==5 | m2sbq6_efft==2 | teacher_available==2 ~ 1,
    m2sbq6_efft==1 | m2sbq6_efft==3 |  m2sbq6_efft==4  ~ 0,
    is.na(m2sbq6_efft) ~ as.numeric(NA)) )

#create indicator for whether each principal was absent from school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(principal_absent=case_when(
    m2sbq3_efft==8  ~ 1,
    m2sbq3_efft!=8   ~ 0,
    is.na(m2sbq3_efft) ~ as.numeric(NA)))


teacher_absence_final<- teacher_absence_dta %>%
  select(preamble_info_absence, contains('absent'))




#Build teacher absence practice indicator
final_school_data_EFFT <- teacher_absence_dta %>%
  group_by(school_code) %>%
  summarise(school_absence_rate=mean(school_absent, na.rm=TRUE), 
            absence_rate=mean(absent, na.rm=TRUE),
            principal_absence_rate=mean(principal_absent, na.rm=TRUE),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number)
  )


#############################################
##### Student Attendance ###########
#############################################

#Percent of 4th grade students who are present during an unannounced visit.

final_school_data_ATTD<- school_data_INPT %>%
  select(preamble_info, m4scq4_inpt, m4scq4_inpt, m4scq12_inpt ) %>%
  mutate(student_attendance=m4scq4_inpt/m4scq12_inpt) %>%
  mutate(student_attendance=if_else(m4scq4_inpt>m4scq12_inpt, m4scq12_inpt/m4scq4_inpt,student_attendance))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.)) 

#############################################
##### Teacher Knowledge ###########
#############################################

# School survey. Fraction correct on teacher assessment. In the future, we will align with SDG criteria for minimum proficiency.

teacher_assessment_dta<-read_dta(file.path(download_folder, "teacher_assessment_answers.dta"))

#Add school preamble info
teacher_assessment_dta <- teacher_assessment_dta %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything())

#number missing
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(n_mssing_CONT=n_miss_row(.))


#rename a few key variables up front
teacher_assessment_dta<- teacher_assessment_dta %>%
  mutate(g4_teacher_name=m5sb_troster  ,
         g4_teacher_number=m5sb_tnum   )


#Drop columns that end in "mistake".  THis is not necessary for computing indicator
teacher_assessment_dta <- teacher_assessment_dta %>% 
  select(-ends_with("mistake"))

#recode assessment variables to be 1 if student got it correct and zero otherwise
teacher_assessment_dta<- teacher_assessment_dta %>%
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
  mutate(literacy_content_knowledge=rowSums(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q")], na.rm=TRUE))

####Math####
#calculate # of math items
teacher_assessment_dta$math_length<-length(grep(x=colnames(teacher_assessment_dta), pattern="m5s2q"))

math_items<-colnames(teacher_assessment_dta[,grep(x=colnames(teacher_assessment_dta), pattern="m5s2q")])

#calculate teachers math items correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(math_content_knowledge=rowSums(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s2q")], na.rm=TRUE))

####Total score####
#calculate teachers percent correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(content_knowledge=(math_content_knowledge+literacy_content_knowledge)/(literacy_length+math_length),
         math_content_knowledge=math_content_knowledge/math_length,
         literacy_content_knowledge=literacy_content_knowledge/literacy_length)


#calculate % correct for literacy, math, and total
final_school_data_CONT <- teacher_assessment_dta %>%
  group_by(school_code) %>%
  summarise(content_knowledge=mean(content_knowledge),
            math_content_knowledge=mean(math_content_knowledge),
            literacy_content_knowledge=mean(literacy_content_knowledge),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number))



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
  mutate(m8saq2_id=rowSums(select(.,m8saq2_id__3,m8saq2_id__4, m8saq2_id__6), na.rm=TRUE)/3,
         m8saq3_id=rowSums(select(.,m8saq3_id__2,m8saq2_id__6, m8saq2_id__7), na.rm=TRUE)/3,
         m8saq4_id=if_else(m8saq4_id!=99, m8saq4_id/5,0),
         m8saq7_word_choice=bin_var(m8saq7_word_choice,2),
         m8sbq1_number_sense=rowSums(select(.,m8sbq1_number_sense__3,m8sbq1_number_sense__4, m8sbq1_number_sense__1), na.rm=TRUE)/3)         %>%
  select(-starts_with("m8saq2_id__"),-starts_with("m8saq3_id__"),-starts_with("m8sbq1_number_sense__"))


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items
assess_4th_grade_dta$literacy_length<-length(grep(x=colnames(assess_4th_grade_dta), pattern="m8saq"))

lit_items<-colnames(assess_4th_grade_dta[,grep(x=colnames(assess_4th_grade_dta), pattern="m8saq")])


#calculate students lit items correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(literacy_student_knowledge=rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq")], na.rm=TRUE))

####Math####
#calculate # of math items
assess_4th_grade_dta$math_length<-length(grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq"))

math_items<-colnames(assess_4th_grade_dta[,grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq")])


#calculate students math items correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(math_student_knowledge=rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq")], na.rm=TRUE))

####Total score####
#calculate students percent correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(student_knowledge=(math_student_knowledge+literacy_student_knowledge)/(literacy_length+math_length),
         math_student_knowledge=math_student_knowledge/math_length,
         literacy_student_knowledge=literacy_student_knowledge/literacy_length)


#calculate % correct for literacy, math, and total
final_school_data_LERN <- assess_4th_grade_dta %>%
  left_join(school_dta[,c('interview__id', 'm8_teacher_name', 'm8_teacher_code')]) %>%
  group_by(school_code) %>%
  summarise(student_knowledge=mean(student_knowledge),
            math_student_knowledge=mean(math_student_knowledge),
            literacy_student_knowledge=mean(literacy_student_knowledge),
            g4_teacher_name=first(m8_teacher_name),
            g4_teacher_code=first(m8_teacher_code),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number),
            n_students=n())




#############################################
##### ECD Assessment ###########
#############################################

# School survey. Fraction correct on the Early Childhoold Assessment given to students in school.


#read in ecd level file
ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))


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
  mutate(student_name=m6s1q1  ,
         student_number=ecd_assessment__id,
         student_age=m6s1q2,
         student_male=bin_var(m6s1q3,1),
         consent=bin_var(m6s1q4,1)
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
  mutate(literacy_student_knowledge=rowSums(.[grep(x=colnames(ecd_dta), 
                                                   pattern="vocabn|comprehension|letters|words|sentence|name_writing|print")], na.rm=TRUE))

####Math####
#calculate # of math items

math_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "counting|produce_set|number_ident|number_compare|simple_add")])

ecd_dta$math_length<-length(math_items)


#calculate students math items correct
ecd_dta <- ecd_dta %>%
  mutate(math_student_knowledge=rowSums(.[grep(x=colnames(ecd_dta), 
                                               pattern="counting|produce_set|number_ident|number_compare|simple_add")], na.rm=TRUE))

####Executive Functioning####
#calculate # of Exec Function items

exec_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "backward_digit|head_shoulders")])

ecd_dta$exec_length<-length(exec_items)


#calculate students excec items correct
ecd_dta <- ecd_dta %>%
  mutate(exec_student_knowledge=rowSums(.[grep(x=colnames(ecd_dta), 
                                               pattern="backward_digit|head_shoulders")], na.rm=TRUE))

####Socio-Emotional####
#calculate # of Exec Function items

soc_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "perspective$|conflict_resol$")])

ecd_dta$soc_length<-length(soc_items)


#calculate students excec items correct
ecd_dta <- ecd_dta %>%
  mutate(soc_student_knowledge=rowSums(.[grep(x=colnames(ecd_dta), 
                                              pattern="perspective$|conflict_resol$")], na.rm=TRUE))


####Total score####
#calculate students percent correct
ecd_dta <- ecd_dta %>%
  mutate(student_knowledge=(math_student_knowledge+literacy_student_knowledge+exec_student_knowledge + soc_student_knowledge)/(literacy_length+math_length+exec_length+soc_length),
         math_student_knowledge=math_student_knowledge/math_length,
         literacy_student_knowledge=literacy_student_knowledge/literacy_length,
         exec_student_knowledge=exec_student_knowledge/exec_length,
         soc_student_knowledge=soc_student_knowledge/soc_length)


#calculate % correct for literacy, math, and total
final_school_data_LCAP <- ecd_dta %>%
  left_join(school_dta[,c('interview__id', 'm6_teacher_name', 'm6_teacher_code', 'm6_class_count', 'm6_instruction_time')]) %>%
  group_by(school_code) %>%
  summarise(ecd_student_knowledge=mean(student_knowledge),
            ecd_math_student_knowledge=mean(math_student_knowledge),
            ecd_literacy_student_knowledge=mean(literacy_student_knowledge),
            ecd_exec_student_knowledge=mean(exec_student_knowledge),
            ecd_soc_student_knowledge=mean(soc_student_knowledge),
            g1_teacher_name=first(m6_teacher_name),
            g1_teacher_code=first(m6_teacher_code),
            class_size=first(m6_class_count),
            instruction_time=first(m6_instruction_time),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number))


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
    share_textbook>=0.9 & share_pencil>=0.9 & share_exbook>=0.9  ~ 1,
    share_textbook<0.9 | share_pencil<0.9 | share_exbook<0.9 ~ 0)) 

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
                  m1sbq12_inpt==0 | m1sbq13_inpt==0 | m1sbq15_inpt<2 ~ 0,
                  (m1sbq12_inpt>=1 & m1sbq13_inpt==1 & m1sbq15_inpt==2) ~ 0.5,
                  (is.na(m1sbq12_inpt==0) | is.na(m1sbq13_inpt) | is.na(m1sbq15_inpt<2)) ~ as.numeric(NA)
                  ))



inpt_list<-c('blackboard_functional', 'pens_etc', 'share_desk',  'used_ict', 'access_ict')

final_school_data_INPT <- school_data_INPT %>%
  left_join(school_teacher_questionnaire_INPT) %>%
  mutate(used_ict=if_else((used_ict_pct>=0.5 & used_ict_num>=3), 0.5,0))     %>%  #Set percentage of teachers to use ICT over 50% and number over 3
  group_by(school_code) %>%
  select(preamble_info, inpt_list, contains('INPT')) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_INPT=n_miss_row(.)) %>%
  mutate(inputs=1+rowSums(select(.,blackboard_functional, pens_etc, share_desk,  used_ict, access_ict), na.rm=TRUE)) %>%
  select(preamble_info, inputs, everything())



#############################################
##### School Infrastructure ###########
#############################################

# School survey. Total score starts at 1 and points added are the sum of whether a school has: 
#   - Access to adequate drinking water 
# -Functional toilets that are separate for boys/girls, private, useable, accessible, and have hand washing facilities 
# - Electricity and Visibility in the classroom 
# - School is accessible for those with disabilities (road access, a school ramp for wheelchairs, an entrance wide enough for wheelchairs, ramps to classrooms where needed, and disability screening for seeing, hearing, and learning disabilities with partial credit for having 1 or 2 or the 3).)

#drinking water
school_data_INFR <- school_data_INFR %>%
  #
  mutate(drinking_water=if_else((m1sbq9_infr==1 | m1sbq9_infr==2 | m1sbq9_infr==5 | m1sbq9_infr==6), 1,0, as.numeric(NA) ))

#functioning toilets
school_data_INFR <- school_data_INFR %>%
  mutate(functioning_toilet=case_when(
    # exist, separate for boys/girls, clean, private, useable, accessible, handwashing available
    m1sbq1_infr!=7 & m1sbq2_infr==1 & m1sbq3_infr!=3 & m1sbq4_infr==1 & m1sbq5_infr==1 & m1sbq6_infr==1 & m1sbq7_infr==1 & m1sbq8_infr==1 ~ 1,
    m1sbq1_infr==7  ~ 0,
    m1sbq1_infr!=7 & ( m1sbq2_infr==0 | m1sbq3_infr==3 | m1sbq4_infr!=1 | m1sbq4_infr!=1 | m1sbq6_infr==0 | m1sbq7_infr==0 | m1sbq8_infr==0) ~ 0)) 

#visibility
school_data_INFR <- school_data_INFR %>%
  left_join(select(school_data_INPT, interview__id, m4scq8_inpt, m4scq9_inpt, m4scq10_inpt )) %>%
  mutate(visibility=case_when(
    m4scq10_inpt==1 &  m4scq8_inpt==1  ~ 0.5,
    m4scq10_inpt==0 & m4scq8_inpt==1 ~ 0)) 

#electricity
school_data_INFR <- school_data_INFR %>%
  mutate(class_electricity=if_else(m1sbq11_infr==1,0.5,0)) 


#accessibility for people with disabilities
final_school_data_INFR <- school_data_INFR %>%
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
    disab_screening=rowSums(select(.,m1sbq17_infr__1,m1sbq17_infr__2,m1sbq17_infr__3), na.rm = TRUE)/3,
    #sum up all components for overall disability accessibility score
    disability_accessibility=(disab_road_access+disab_school_ramp+disab_school_entr+
                                disab_class_ramp+disab_class_entr+disab_screening)/6
  )


infr_list<-c('drinking_water', 'functioning_toilet', 'visibility',  'class_electricity', 'disability_accessibility')

final_school_data_INFR <- final_school_data_INFR %>%
  select(preamble_info, infr_list, contains('INFR'), contains('disab')) %>%
  mutate(n_mssing_INFR=n_miss_row(.)) %>%
  mutate(infrastructure=1+rowSums(select(.,drinking_water, functioning_toilet, visibility,  class_electricity, disability_accessibility), na.rm=TRUE)) %>%
  select(preamble_info, infrastructure, everything())





#############################################
##### Teacher Pedagogical Skill ###########
#############################################



#final_school_data_PEDG <- ''


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

final_school_data_OPMN <- school_data_OPMN %>%
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
  

final_school_data_OPMN <- final_school_data_OPMN %>%
  mutate(n_mssing_OPMN=n_miss_row(.)) 


#############################################
##### School Instructional Leadership ###########
#############################################

# School survey. Total score starts at 1 and points added are the sum of whether a teacher has: 
#   - Had a classroom observation in past year 
# - Had a discussion based on that observation that lasted longer than 10 min 
# - Received actionable feedback from that observation 
# - Teacher had a lesson plan and discussed it with another person

final_school_data_ILDR <- teacher_questionnaire_ILDR %>%
  mutate(n_mssing_ILDR=n_miss_row(.)) %>%
  mutate(classroom_observed=bin_var(m3sdq15_ildr,0.5),
         classroom_observed_recent=if_else((classroom_observed==1 & m3sdq16_ildr<=12),0.5,0), #set recent to mean under 12 months
         purpose_observation=case_when(
           m3sdq18_ildr__1==1 ~ "Evaluation",
           m3sdq18_ildr__2==1 ~ "Professional Development",
           m3sdq18_ildr__3==1 ~ "Monitoring",
           m3sdq18_ildr__97==1 ~ m3sdq18_other_ildr ),
         discussed_observation=if_else((classroom_observed==1 & m3sdq19_ildr==1 & m3sdq20_ildr>1),1,0), #make sure there was discussion and lasted more than 10 min
         feedback_observation=if_else((m3sdq21_ildr==1 & (m3sdq22_ildr__1==1 | m3sdq22_ildr__2==1 | m3sdq22_ildr__3==1
                                                          | m3sdq22_ildr__4==1 | m3sdq22_ildr__5==1)),1,0), #got feedback and was specific
         lesson_plan_w_feedback=if_else((m3sdq23_ildr==1 & m3sdq24_ildr==1),1,0)) %>%
mutate(instructional_leadership=1+rowSums(select(., classroom_observed, classroom_observed_recent, discussed_observation, feedback_observation, lesson_plan_w_feedback), na.rm=TRUE)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)




#############################################
##### School Principal School Knowledge ###########
#############################################



final_school_data_PKNW <- school_data_PKNW %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_PKNW=n_miss_row(.)) 

#############################################
##### School Principal Management Skills ###########
#############################################


# Score of 1-5 based on sum of following: 
#   - 1 Point. School Goals Exists 
# - 1 Point. School goals are clear to school director, teachers, students, parents, and other members of community (partial credit available) 
# - 1 Point. Specific goals related to improving student achievement ( improving test scores, improving pass rates, reducing drop out, reducing absenteeism, improving pedagogy, more resources for infrastructure, more resources for inputs) 
# - 1 Point. School has defined system to measure goals (partial credit available)

#Create variables for whether school goals exists, are clear, are relevant to learning, and are measured in an appropriate way.

final_school_data_PMAN <- school_data_PMAN %>%
  mutate(school_goals_exist=bin_var(m7sdq1_pman,1),
         school_goals_clear=if_else(m7sdq1_pman==1, 
                                    rowSums(select(.,m7sdq3_pman__1, m7sdq3_pman__2, m7sdq3_pman__3, m7sdq3_pman__4, m7sdq3_pman__5), na.rm=TRUE)/5,
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
  mutate(principal_management=1+school_goals_exist+school_goals_clear+school_goals_relevant+school_goals_measured)

final_school_data_PMAN <- final_school_data_PMAN %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_PMAN=n_miss_row(.)) 

#############################################
##### Teacher Teaching Attraction ###########
#############################################

# In the school survey, a number of De Facto questions on teacher attraction are asked. One point is awarded for each of the following: 
#   - 1 Point. Teacher satisfied with job 
# - 1 Point. Teacher satisfied with status in community 
# - 1 Point. Would better teachers be promoted faster? 
#   - 1 Point. Do teachers receive bonuses?


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
  mutate(teacher_attraction=1+teacher_satisfied_job+teacher_satisfied_status+better_teachers_promoted+teacher_bonus)
  

final_school_data_TATT <- teacher_questionnaire_TATT %>%
  mutate(n_mssing_TATT=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)




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


final_school_data_TSDP <- teacher_questionnaire_TSDP %>%  
  mutate(n_mssing_TSDP=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)


#############################################
##### Teacher Teaching Support ###########
#############################################


# School survey. Our teaching support indicator asks teachers about participation and the experience with two types of training: pre-service training (induction training) and in-service training. A maximum of 2 points are assigned for quality pre-service training and 2 points for quality in-service training. 
# 
# Pre-Service (Induction) Training: 
#   - 0.5 Points. Had a pre-service training 
# - 0.5 Points. Teacher reported receiving usable skills from training 
# - 0.5 Points. Pre-service training contained a teacher practicum (teach a class with supervision) 
# - 0.5 Points. Practicum lasted more than 3 months and teacher spent more than one hour per day teaching to students. 
# 
# In-Service Training: 
#   - 0.5 Points. Had an in-service training 
# - 0.5 Points. In-service training lasted more than 2 total days. 
# - 0.5 Points. More than 25% of the in-service training was done in the classroom. 
# - 0.5 Points. More than 50% of in-service training done in the classroom.

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
            m3sdq3_tsup==0 ~ 0 ),
         in_service_exists=bin_var(m3sdq9_tsup,1),
         in_servce_lngth=case_when(
           (m3sdq9_tsup==1 & m3sdq10_tsup>2 ) ~ 1,
           (m3sdq9_tsup==1 & m3sdq10_tsup<=2) ~ 0,
           m3sdq9_tsup==0 ~ 0
         ),
         in_service_classroom=case_when(
           (m3sdq9_tsup==1 & m3sdq13_tsup>=3)  ~ 2,
           (m3sdq9_tsup==1 & m3sdq13_tsup==2)  ~ 1,
           (m3sdq9_tsup==1 & m3sdq13_tsup==1)  ~ 0,
           m3sdq9_tsup==0 ~ 0
         )
                                     ) %>%
  mutate(pre_service=pre_training_exists+pre_training_practicum+pre_training_practicum_lngth,
         in_service=in_service_exists+in_servce_lngth+in_service_classroom) %>%
  mutate(teacher_support=pre_service+in_service)


final_school_data_TSUP <- teacher_questionnaire_TSUP %>%
  mutate(n_mssing_TSUP=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)


#############################################
##### Teacher Teaching Evaluation ###########
#############################################

# School survey. This policy lever measures whether there is a teacher evaluation system in place, and if so, the types of decisions that are made based on the evaluation results. Score is the sum of the following: 
# - 1 Point. Was teacher formally evaluated in past school year? 
# - 1 Point. Evaluation included evaluation of either knowledge of subject matter or pedagogical skills 
# - 1 Point. Consequences exist if teacher receives 2 or more negative evaluations 
# - 1 Point. Rewards exist if teacher receives 2 or more positive evaluations


#list of teacher evluation questions
tevl<-c(
        'm3sbq7_tmna__1', 'm3sbq7_tmna__2', 'm3sbq7_tmna__3', 'm3sbq7_tmna__4','m3sbq7_tmna__5', 'm3sbq7_tmna__6', 'm3sbq7_tmna__97',
        'm3sbq8_tmna__2', 'm3sbq8_tmna__3', 'm3sbq8_tmna__4','m3sbq8_tmna__5', 'm3sbq8_tmna__6', 'm3sbq8_tmna__7', 'm3sbq8_tmna__8', 'm3sbq8_tmna__97', 'm3sbq8_tmna__98',
        'm3sbq9_tmna__1', 'm3sbq9_tmna__2', 'm3sbq9_tmna__3', 'm3sbq9_tmna__4', 'm3sbq9_tmna__7', 'm3sbq9_tmna__97', 'm3sbq9_tmna__98',
        'm3bq10_tmna__1', 'm3bq10_tmna__2', 'm3bq10_tmna__3', 'm3bq10_tmna__4', 'm3bq10_tmna__7', 'm3bq10_tmna__97', 'm3bq10_tmna__98')

teacher_questionnaire_TEVL <- teacher_questionnaire_TMNA %>%
  dplyr::select(preamble_info, preamble_info_teacher, tevl, m3sbq6_tmna, m3sbq8_tmna__1)

teacher_questionnaire_TEVL<- teacher_questionnaire_TEVL %>%
  mutate(formally_evaluated=bin_var(m3sbq6_tmna,1),
         evaluation_knowledge_skill=if_else(m3sbq6_tmna==1,
                                            case_when(
                                              (m3sbq8_tmna__2==1 | m3sbq8_tmna__3==1) ~ 1,
                                              (m3sbq8_tmna__1==1 | m3sbq8_tmna__4==1 | m3sbq8_tmna__5==1 | m3sbq8_tmna__6==1 | m3sbq8_tmna__7==1 | m3sbq8_tmna__8==1) ~ 0.5,
                                              (m3sbq8_tmna__97==1 | m3sbq8_tmna__98==1 ) ~ 0,
                                              TRUE ~ 0
                                            ),
                                            0),
         negative_consequences=case_when(
           (m3sbq9_tmna__1==1 | m3sbq9_tmna__2==1 | m3sbq9_tmna__3==1 | m3sbq9_tmna__4==1 | m3sbq9_tmna__97==1) ~ 1,
           TRUE ~ 0),
         positive_consequences=case_when(
           (m3bq10_tmna__1==1 | m3bq10_tmna__2==1 | m3bq10_tmna__3==1 | m3bq10_tmna__4==1 | m3bq10_tmna__97==1) ~ 1,
           TRUE ~ 0)
         ) %>%
  mutate(teaching_evaluation=1+formally_evaluated+evaluation_knowledge_skill+negative_consequences+positive_consequences)


final_school_data_TEVL <- teacher_questionnaire_TEVL %>%
  mutate(n_mssing_TEVL=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)




#############################################
##### Teacher  Monitoring and Accountability ###########
#############################################

# School Survey. This policy lever measures the extent to which teacher presence is being monitored, whether attendance is rewarded, and whether there are consequences for chronic absence. Score is the sum of the following: 
#   - 1 Point. Teachers evaluated by some authority on basis of absence. 
# - 1.5 Points. Good attendance is rewarded. 
# - 1.5 Points. There are consequences for chronic absence (more than 30% absence).

teacher_questionnaire_TMNA2 <- teacher_questionnaire_TATT %>%
  select(interview__id, school_code, teacher_name, teacher_number, m3seq4_tatt, m3seq5_tatt__1, m3sbq1_other_tatt )

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
                                         (m3seq5_tatt__1==1) ~ 1.5,
                                         TRUE ~ 0
                                       ),
                                       0),
           attendence_sanctions=case_when(
             (m3sbq2_tmna__1==1 | m3sbq2_tmna__2==1 | m3sbq2_tmna__3==1 | m3sbq2_tmna__4==1 | m3sbq2_tmna__97==1) ~ 1.5,
             TRUE ~ 0
           )
           )  %>%
    mutate(teacher_monitoring=1+attendance_evaluated + attendance_rewarded + attendence_sanctions)

final_school_data_TMNA <- teacher_questionnaire_TMNA %>%
  mutate(n_mssing_TMNA=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)



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

final_school_data_TINM <- teacher_questionnaire_TINM %>%
  mutate(n_mssing_TINM=n_miss_row(.)) %>%
  mutate_at(intrinsic_motiv_q_rev, attitude_fun_rev ) %>%
  mutate_at(intrinsic_motiv_q, attitude_fun ) %>%
  mutate(intrinsic_motivation=rowSums(.[intrinsic_motiv_q_all], na.rm=TRUE)/10) %>%
  group_by(school_code) %>%
  summarise(n_mssing_teach_TINM=sum(n_mssing_TINM),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number),
            intrinsic_motivation=mean(intrinsic_motivation))


#############################################
##### School  Inputs and Infrastructure Standards ###########
#############################################




#############################################
##### School  Inputs and Infrastructure Monitoring ###########
#############################################

# School Survey. This lever measures the extent to which there is a monitoring system in place to ensure that the inputs that must be available at the schools are in fact available at the schools. This set of questions will include three aspects: 
#   - 1 Point. Are there standards in place to monitor blackboard and chalk, pens and pencils, basic classroom furniture, computers, textbooks, exercise books, toilets, electricity, drinking water, accessibility for those with disabilities? (partial credit available) 
# - 1 Point. Are all input items (functioning blackboard, chalk, pens, pencils, textbooks, exercise books in 4th grade classrooms, basic classroom furniture, and at least one computer in the schools) being monitored? (partial credit available) 
# - 1 Point. Are all infrastructure items (functioning toilets, electricity, drinking water, and accessibility for people with disabilities) being monitored? (partial credit available) 
# - 1 Point. Is the community involved in the monitoring?

school_data_IMON <- school_data_IMON %>%
  mutate(standards_monitoring_input=rowSums(.[grep(x=colnames(school_data_IMON), 
                                                   pattern="m1scq13_imon__")], na.rm=TRUE),
         standards_monitoring_infrastructure=rowSums(.[grep(x=colnames(school_data_IMON), 
                                                            pattern="m1scq14_imon__")], na.rm=TRUE) ) %>%
  mutate(standards_monitoring=(standards_monitoring_input+standards_monitoring_infrastructure)/10) %>%
  mutate(monitoring_inputs=if_else(m1scq1_imon==1,
                                   rowSums(.[grep(x=colnames(school_data_IMON), 
                                                  pattern="m1scq4_imon__")], na.rm=TRUE),
                                   0)/6,
         monitoring_infrastructure=if_else(m1scq1_imon==1,
                                   rowSums(.[grep(x=colnames(school_data_IMON), 
                                                  pattern="m1scq9_imon__")], na.rm=TRUE),
                                   0)/4,
  ) %>%
  mutate(parents_involved=if_else(m1scq3_imon==1,1,0,0)) %>%
  mutate(school_monitoring=standards_monitoring+monitoring_inputs+monitoring_infrastructure+parents_involved)
  


final_school_data_IMON <- school_data_IMON %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_IMON=n_miss_row(.)) 



#############################################
##### School School Management Attraction  ###########
#############################################

# This policy lever measures whether the right candidates are being attracted to the profession of school principals. The questions will aim to capture the provision of benefits to attract and maintain the best people to serve as principals. 
# 
# Scoring: 
#   - For now, score is between 1-5 based on how satisfied the principal is with status in community. We will also add in component based on Principal salaries.

school_data_SATT <- school_data_SATT %>%
  mutate(principal_satisfaction=attitude_fun_rev(m7shq1_satt)) %>%
  mutate(school_management_attraction=principal_satisfaction)
  
final_school_data_SATT <- school_data_SATT %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SATT=n_miss_row(.)) 


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
  mutate(school_selection_deployment=case_when(
    (m7sgq2_ssld==2 | m7sgq2_ssld==3 | m7sgq2_ssld==8) ~ 5,
    (m7sgq2_ssld==6 | m7sgq2_ssld==7) ~ 1,
    (!(m7sgq2_ssld==6 | m7sgq2_ssld==7) & (m7sgq1_ssld__2==1 | m7sgq1_ssld__3==1 | m7sgq1_ssld__8==1) ) ~ 4,
    (!(m7sgq2_ssld==6 | m7sgq2_ssld==7) & (m7sgq1_ssld__1==1 | m7sgq1_ssld__4==1 | m7sgq1_ssld__5==1 | m7sgq1_ssld__97==1) ) ~ 3,
    (m7sgq1_ssld__6==1 | m7sgq1_ssld__7==1 ) ~ 2, 
    TRUE ~ as.numeric(NA))
    )

final_school_data_SSLD <- school_data_SSLD %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.)) 


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
                                    rowSums(.[grep(x=colnames(school_data_SSUP), 
                                                   pattern="m7sgq4_ssup__")], na.rm=TRUE),
                                    0)/3,
         principal_used_skills=if_else(m7sgq3_ssup==1,
                                       bin_var(m7sgq5_ssup,1),
                                       0),
         principal_offered=if_else((m7sgq7_ssup==2 | m7sgq7_ssup==3 | m7sgq7_ssup==4 | m7sgq7_ssup==5),1,0)
  ) %>%
  mutate(school_support=1+prinicipal_trained+principal_training+principal_used_skills+principal_offered)

final_school_data_SSUP <- school_data_SSUP %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSUP=n_miss_row(.)) 

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

final_school_data_SEVL <- school_data_SEVL %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SEVL=n_miss_row(.)) 



#############################################
##### School Level Info ###########
#############################################

#Build school level database

#first create temp dataset with only required info (school_code + indicator info).  Main thing here is to drop enumerator code, interview ID, which mess up merges
#list additional info that will be useful to keep in each indicator dataframe
drop_info <- c('interview__id', 'enumerator_name_other', 'enumerator_number')
keep_info <-       c('school_code',
                     'school_name_preload', 'school_address_preload', 
                     'school_province_preload', 'school_district_preload', 'school_code_preload', 'school_emis_preload',
                     'school_info_correct', 'm1s0q2_name', 'm1s0q2_code', 'm1s0q2_emis',
                     'survey_time', 'lat', 'lon')

if (exists('final_school_data')) {
  rm('final_school_data')
  }

for (i in indicator_names ) {
  if (exists(paste("final_school_data_",i, sep=""))) {
    #form temp data frame with each schools data
    temp<-get(paste("final_school_data_",i, sep="")) 
    
    temp <- temp %>%
      select(-drop_info)
    print(i)
    #Merge this to overall final_school_data frame
    if (!exists('final_school_data')) {
      final_school_data<-temp
      print(i)
      write_dta(temp, path = file.path(paste(save_folder,"/Indicators", sep=""), paste(i,"_final_school_data.dta", sep="")), version = 14)
      if (backup_onedrive=="yes") {
        write_dta(temp, path = file.path(paste(save_folder_onedrive,"/Indicators", sep=""), paste(i,"_final_school_data.dta", sep="")), version = 14)
      }
      
    } else {
      final_school_data<-final_school_data %>%
        left_join(temp)
      
      write_dta(temp, path = file.path(paste(save_folder,"/Indicators", sep=""), paste(i,"_final_school_data.dta", sep="")), version = 14)
      if (backup_onedrive=="yes") {
        write_dta(temp, path = file.path(paste(save_folder_onedrive,"/Indicators", sep=""), paste(i,"_final_school_data.dta", sep="")), version = 14)
      }
    }
  }
}

#Create list of key indicators
ind_list<-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge',
            'absence_rate', 'school_absence_rate', 'student_attendance',
            'content_knowledge', 'math_content_knowledge', 'literacy_content_knowledge',
            'ecd_student_knowledge', 'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
            'inputs', 'blackboard_functional', 'pens_etc', 'share_desk', 'used_ict', 'access_ict',
            'infrastructure','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
            'operational_management', 'vignette_1', 'vignette_2', 'intrinsic_motivation', 'instructional_leadership','principal_management','teacher_attraction',
            'teacher_selection_deployment', 'teacher_support', 'teaching_evaluation', 'teacher_monitoring','school_monitoring',
            'school_management_attraction', 'school_selection_deployment', 'school_support', 'principal_evaluation'
)


final_school_data <- final_school_data %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(keep_info, ind_list, everything())



write.csv(final_school_data, file = file.path(save_folder, "final_complete_school_data.csv"))
write_dta(final_school_data, path = file.path(save_folder, "final_complete_school_data.dta"), version = 14)
if (backup_onedrive=="yes") {
  write.csv(final_school_data, file = file.path(save_folder_onedrive, "final_complete_school_data.csv"))
  write_dta(final_school_data, path = file.path(save_folder_onedrive, "final_complete_school_data.dta"), version = 14)
}
#If indicator in this list doesn't exists, create empty column with Missing values


for (i in ind_list ) {
  if(!(i %in% colnames(final_school_data))) {
    print(i)
    final_school_data[, i] <- NA
  }
}





school_dta_short <- final_school_data %>%
  select(keep_info, ind_list)

write.csv(school_dta_short, file = file.path(save_folder, "final_indicator_school_data.csv"))
write_dta(school_dta_short, path = file.path(save_folder, "final_indicator_school_data.dta"), version = 14)

if (backup_onedrive=="yes") {
  write.csv(school_dta_short, file = file.path(save_folder_onedrive, "final_indicator_school_data.csv"))
  write_dta(school_dta_short, path = file.path(save_folder_onedrive, "final_indicator_school_data.dta"), version = 14)
}

################################
#Store Key Created Datasets
################################

#saves the following in R and stata format

data_list <- c('school_dta', 'school_dta_short',  'school_data_preamble', 'final_school_data', 'teacher_questionnaire','teacher_absence_final', 'ecd_dta', 'teacher_assessment_dta', 'teacher_roster')

save(list=data_list, file = file.path(save_folder, "school_survey_data.RData"))

if (backup_onedrive=="yes") {
  save(list=data_list, file = file.path(save_folder_onedrive, "school_survey_data.RData"))
}

#loop and produce list of data tables
# 
# teacher_roster_list<-teacher_roster %>%
#   left_join(school_data_preamble) %>%
#   select(preamble_info,  teacher_number, teacher_name) 
# 
# 
# orphans_number_questionnaire <- teacher_questionnaire %>%
#   select(keep_info, teacher_name, teacher_number) %>%
#   anti_join(teacher_roster_list, by=c('teacher_name', 'teacher_number')) 
# 
# orphans_name_questionnaire <- teacher_questionnaire %>%
#   anti_join(teacher_roster) %>%
#   select(interview__id, teacher_name, teacher_number) %>%
#   datatable()
# 
# 
# orphans_number_assess <- teacher_questionnaire %>%
#   anti_join(teacher_roster) %>%
#   select(interview__id, teacher_name, teacher_number) %>%
#   datatable()
# 
# 
# orphans_name_assess <- teacher_questionnaire %>%
#   anti_join(teacher_roster) %>%
#   select(interview__id, teacher_name, teacher_number) %>%
#   datatable()
# 
# # 
# # 
# # for (i in indicator_names ) {
# #   if (exists(paste("final_school_data_",i, sep=""))) {
# #     temp<-get(paste("final_school_data_",i, sep="")) 
# #     skim(temp) %>%
# #       DT::datatable()
# #   }
# # }
# # 
# # 
# # for (i in indicator_names ) {
# #   if (exists(paste("final_school_data_",i, sep=""))) {
# #     temp<-get(paste("final_school_data_",i, sep="")) 
# #     skim(temp) %>%
# #       skimr::kable()
# #   }
# # }
# # 
# # skim(final_school_data_INFR) %>%
# #   skimr::kable()
# # 
# # skim(sumstats) %>%
# #   skimr::kable()