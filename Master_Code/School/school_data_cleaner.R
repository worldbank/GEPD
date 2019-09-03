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

teacher_roster<-read_dta(file.path(download_folder, "questionnaire_selected.dta"))

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
  select(preamble_info, everything())



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
    m2sbq6_efft==6  ~ 1,
    m2sbq6_efft!=6   ~ 0,
    is.na(m2sbq6_efft) ~ as.numeric(NA)))

#create indicator for whether each teacher was absent from classroom or school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(absent=case_when(
    m2sbq6_efft==6 | m2sbq6_efft==5 | m2sbq6_efft==2  ~ 1,
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
##### Teacher Knowledge ###########
#############################################

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
  mutate_at(vars(starts_with("m8saq"), starts_with("m8sbq")), ~bin_var(.,1)  )


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
            enumerator_number=first(enumerator_number))




#############################################
##### ECD Assessment ###########
#############################################



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
  summarise(used_ict=mean(m3sbq4_inpt))

#access to ICT
school_data_INPT <- school_data_INPT %>%
  mutate(access_ict=bin_var(m1sbq14_inpt,1))


inpt_list<-c('blackboard_functional', 'pens_etc', 'share_desk',  'used_ict', 'access_ict')

final_school_data_INPT <- school_data_INPT %>%
  left_join(school_teacher_questionnaire_INPT) %>%
  group_by(school_code) %>%
  select(preamble_info, inpt_list, contains('INPT')) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_INPT=n_miss_row(.)) %>%
  mutate(inputs=rowSums(select(.,blackboard_functional, pens_etc, share_desk,  used_ict, access_ict), na.rm=TRUE)) %>%
  select(preamble_info, inputs, everything())



#############################################
##### School Infrastructure ###########
#############################################



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
    m4scq10_inpt==1 &  m4scq8_inpt==1  ~ 1,
    m4scq10_inpt==0 & m4scq8_inpt==1 ~ 0)) 

#electricity
school_data_INFR <- school_data_INFR %>%
  mutate(class_electricity=bin_var(m1sbq11_infr,1)) 


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
                                disab_class_ramp+disab_class_entr+disab_screening)/5
  )


infr_list<-c('drinking_water', 'functioning_toilet', 'visibility',  'class_electricity', 'disability_accessibility')

final_school_data_INFR <- final_school_data_INFR %>%
  select(preamble_info, infr_list, contains('INFR'), contains('disab')) %>%
  mutate(n_mssing_INFR=n_miss_row(.)) %>%
  mutate(infrastructure=rowSums(select(.,drinking_water, functioning_toilet, visibility,  class_electricity, disability_accessibility), na.rm=TRUE)) %>%
  select(preamble_info, infrastructure, everything())





#############################################
##### Teacher Pedagogical Skill ###########
#############################################



#final_school_data_PEDG <- ''


#############################################
##### School Operational Management ###########
#############################################

final_school_data_OPMN <- school_data_OPMN %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(
         vignette_1_resp=bin_var(m7sbq1_opmn,1),
         vignette_1_address=bin_var(m7sbq3_opmn,3),
         #give total score for this vignette
         vignette_1=if_else(vignette_1_resp==1,vignette_1_address,as.numeric(NA), missing=as.numeric(NA)),
         vignette_2_resp=bin_var(m7scq1_opmn,2),
         #give partial credit based on how quickly it will be solved <1 month, 1-3, 3-6, 6-12, >1 yr
         vignette_2_address=case_when(
           m7scq2_opmn==1 ~ 1,
           m7scq2_opmn==2 ~ .75,
           m7scq2_opmn==3 ~ .5,
           m7scq2_opmn==4 ~ .25,
           m7scq2_opmn==5 ~ 0),
         vignette_2_textbook_access=bin_var(m7scq4_opmn,1),
         vignette_2=if_else(vignette_1_resp==1,(vignette_2_address+vignette_2_textbook_access)/2,as.numeric(NA), missing=as.numeric(NA)),
         #sum all components for overall score
         operational_management=(vignette_1+ vignette_2)
  )

final_school_data_OPMN <- final_school_data_OPMN %>%
  mutate(n_mssing_OPMN=n_miss_row(.)) 


#############################################
##### School Instructional Leadership ###########
#############################################



final_school_data_ILDR <- teacher_questionnaire_ILDR %>%
  mutate(n_mssing_ILDR=n_miss_row(.)) %>%
  group_by(school_code) %>%
  summarise(n_mssing_teach_ILDR=sum(n_mssing_ILDR),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number))




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



final_school_data_PMAN <- school_data_PMAN %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_PMAN=n_miss_row(.)) 

#############################################
##### Teacher Teaching Attraction ###########
#############################################


final_school_data_TATT <- teacher_questionnaire_TATT %>%
  mutate(n_mssing_TATT=n_miss_row(.)) %>%
  group_by(school_code) %>%
  summarise(n_mssing_teach_TATT=sum(n_mssing_TATT),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number))





#############################################
##### Teacher Teaching Selection and Deployment ###########
#############################################




final_school_data_TSDP <- teacher_questionnaire_TSDP %>%
  mutate(n_mssing_TSDP=n_miss_row(.)) %>%
  group_by(school_code) %>%
  summarise(n_mssing_teach_TSDP=sum(n_mssing_TSDP),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number))



#############################################
##### Teacher Teaching Support ###########
#############################################



final_school_data_TSUP <- teacher_questionnaire_TSUP %>%
  mutate(n_mssing_TSUP=n_miss_row(.)) %>%
  group_by(school_code) %>%
  summarise(n_mssing_teach_TSUP=sum(n_mssing_TSUP),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number))



#############################################
##### Teacher Teaching Evaluation ###########
#############################################




#list of teacher evluation questions
tevl<-c('m3sbq6_tmna', 
        'm3sbq7_tmna__1', 'm3sbq7_tmna__2', 'm3sbq7_tmna__3', 'm3sbq7_tmna__4','m3sbq7_tmna__5', 'm3sbq7_tmna__6', 'm3sbq7_tmna__97',
        'm3sbq8_tmna__1', 'm3sbq8_tmna__2', 'm3sbq8_tmna__3', 'm3sbq8_tmna__4','m3sbq8_tmna__5', 'm3sbq8_tmna__6', 'm3sbq8_tmna__7', 'm3sbq8_tmna__8', 'm3sbq8_tmna__97', 'm3sbq8_tmna__98',
        'm3sbq9_tmna__1', 'm3sbq9_tmna__2', 'm3sbq9_tmna__3', 'm3sbq9_tmna__4', 'm3sbq9_tmna__7', 'm3sbq9_tmna__97', 'm3sbq9_tmna__98',
        'm3bq10_tmna__1', 'm3bq10_tmna__2', 'm3bq10_tmna__3', 'm3bq10_tmna__4', 'm3bq10_tmna__7', 'm3bq10_tmna__97', 'm3bq10_tmna__98')

final_school_data_TEVL <- teacher_questionnaire_TMNA %>%
  dplyr::select(preamble_info, preamble_info_teacher, tevl) %>%
  mutate(n_mssing_TEVL=n_miss_row(.)) %>%
  group_by(school_code) %>%
  summarise(n_mssing_TEVL=sum(n_mssing_TEVL),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number))




#############################################
##### Teacher  Monitoring and Accountability ###########
#############################################


final_school_data_TMNA <- teacher_questionnaire_TMNA %>%
  dplyr::select(preamble_info, preamble_info_teacher, -tevl) %>%
  mutate(n_mssing_TMNA=n_miss_row(.)) %>%
  group_by(school_code) %>%
  summarise(n_mssing_TMNA=sum(n_mssing_TMNA),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number))


#############################################
##### Teacher  Intrinsic Motivation ###########
#############################################

final_school_data_TINM <- teacher_questionnaire_TINM %>%
  mutate(n_mssing_TINM=n_miss_row(.)) %>%
  group_by(school_code) %>%
  summarise(n_mssing_teach_TINM=sum(n_mssing_TINM),
            interview__id=first(interview__id), 
            enumerator_name_other=first(enumerator_name_other), 
            enumerator_number=first(enumerator_number))


#############################################
##### School  Inputs and Infrastructure Standards ###########
#############################################




#############################################
##### School  Inputs and Infrastructure Monitoring ###########
#############################################




final_school_data_IMON <- school_data_IMON %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_IMON=n_miss_row(.)) 



#############################################
##### School School Management Attraction  ###########
#############################################



final_school_data_SATT <- school_data_SATT %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SATT=n_miss_row(.)) 


#############################################
##### School School Management Selection and Deployment  ###########
#############################################



final_school_data_SSLD <- school_data_SSLD %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.)) 


#############################################
##### School School Management Support  ###########
#############################################



final_school_data_SSUP <- school_data_SSUP %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSUP=n_miss_row(.)) 

#############################################
##### School School Management Evaluation  ###########
#############################################




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
      
    } else {
      final_school_data<-final_school_data %>%
        left_join(temp)
      
      write_dta(temp, path = file.path(paste(save_folder,"/Indicators", sep=""), paste(i,"_final_school_data.dta", sep="")), version = 14)
      
    }
  }
}

#Create list of key indicators
ind_list<-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge',
            'absence_rate', 'school_absence_rate', 
            'content_knowledge', 'math_content_knowledge', 'literacy_content_knowledge',
            'ecd_student_knowledge', 'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
            'inputs', 'blackboard_functional', 'pens_etc', 'share_desk', 'used_ict', 'access_ict',
            'infrastructure','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
            'operational_management', 'vignette_1', 'vignette_2'
)


final_school_data <- final_school_data %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(keep_info, ind_list, everything())



write.csv(final_school_data, file = file.path(save_folder, "final_complete_school_data.csv"))
write_dta(final_school_data, path = file.path(save_folder, "final_complete_school_data.dta"), version = 14)


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

################################
#Store Key Created Datasets
################################

#saves the following in R and stata format

data_list <- c('school_dta', 'school_dta_short', 'final_school_data', 'teacher_questionnaire','teacher_absence_final', 'ecd_dta', 'teacher_assessment')
data_list <- c( 'school_dta_short', 'final_school_data')

save(data_list, file = file.path(save_folder, "school_survey_data.RData"))
#loop and produce list of data tables


# 
# 
# for (i in indicator_names ) {
#   if (exists(paste("final_school_data_",i, sep=""))) {
#     temp<-get(paste("final_school_data_",i, sep="")) 
#     skim(temp) %>%
#       DT::datatable()
#   }
# }
# 
# 
# for (i in indicator_names ) {
#   if (exists(paste("final_school_data_",i, sep=""))) {
#     temp<-get(paste("final_school_data_",i, sep="")) 
#     skim(temp) %>%
#       skimr::kable()
#   }
# }
# 
# skim(final_school_data_INFR) %>%
#   skimr::kable()
# 
# skim(sumstats) %>%
#   skimr::kable()