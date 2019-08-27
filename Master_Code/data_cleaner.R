#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
library(skimr)

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




#Country name
country <-'Mozambique'
year <- '2019'


# File paths
#You can enter your UPI and specify a path, or you can select manually in a new window
if (Sys.getenv("USERNAME") == "wb469649"){
  project_folder  <- "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work"
  download_folder <-file.path(paste(project_folder,country,year,"Data/raw/School", sep="/"))
} else {

download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")
}

###########################
#read in school level file
###########################
school_dta<-read_dta(file.path(download_folder, "EPDash.dta"))
vtable(school_dta)
#rename a few key variables up front
school_dta<- school_dta %>%
  mutate(enumerator_name=m1s0q1_name  ,
            enumerator_number=m1s0q1_number ,
            survey_time=m1s0q8,
            lat=m1s0q9__Latitude,
            lon=m1s0q9__Longitude
            )

#create school metadata frame
school_metadta<-makeVlist(school_dta)

#Read in list of indicators
indicators <- read_delim("./Indicators/indicators.md", delim="|", trim_ws=TRUE)
indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)

#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- indicators$indicator_tag

#list additional info that will be useful to keep in each indicator dataframe
preamble_info <- c('interview__id', 'school_name_preload', 'school_address_preload', 
                   'school_province_preload', 'school_district_preload', 'school_code_preload', 'school_emis_preload',
                   'school_info_correct', 'm1s0q2_name', 'm1s0q2_code', 'm1s0q2_emis',
                   'enumerator_name', 'enumerator_number', 'survey_time', 'lat', 'lon')

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
teacher_questionnaire
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
preamble_info_school <- c('interview__id', 'questionnaire_roster__id', 'teacher_name', 'teacher_number',
                   'available', 'teacher_position', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 'teacher_grd5',
                   'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'teacher_education', 'teacher_year_began',
                   'teacher_age')



#use dplyr select(contains()) to search for variables with select tags and create separate databases
for (i in indicator_names ) {
  temp_df<-teacher_questionnaire %>%
    select( contains(i))
  if (ncol(temp_df) > 0) {
    temp_df<-teacher_questionnaire %>%
      select(preamble_info_school, contains(i))
    assign(paste("teacher_questionnaire_",i, sep=""), temp_df )
  }
}



#############################################
##### Teacher Absence ###########
#############################################

#read in teacher absence file
teacher_absence_dta<-read_dta(file.path(download_folder, "questionnaire_selected.dta"))
teacher_absence_metadta<-makeVlist(teacher_absence_dta)

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
preamble_info <- c('interview__id', 'questionnaire_selected__id', 'teacher_name', 'teacher_number',
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
  select(preamble_info, contains('absent'))




#Build teacher absence practice indicator
final_school_data_EFFT <- teacher_absence_dta %>%
  group_by(interview__id) %>%
  summarise(school_absence_rate=mean(school_absent), 
            absence_rate=mean(absent),
            principal_absence_rate=mean(principal_absent)
            )


#############################################
##### Teacher Knowledge ###########
#############################################

teacher_assessment_dta<-read_dta(file.path(download_folder, "teacher_assessment.dta"))

#number missing
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(n_mssing_CONT=n_miss_row(.))


#rename a few key variables up front
teacher_assessment_dta<- teacher_assessment_dta %>%
  mutate(teacher_name=m5sb_troster  ,
         teacher_number=m5sb_tnumber ,
         consent=m5_consent
  )


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
  group_by(interview__id) %>%
  summarise(content_knowledge=mean(content_knowledge),
            math_content_knowledge=mean(math_content_knowledge),
            literacy_content_knowledge=mean(literacy_content_knowledge))



#############################################
##### 4th Grade Assessment ###########
#############################################

#read in 4th grade assessment level file
assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))

#number missing
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(n_mssing_LERN=n_miss_row(.))

#create indicator for % correct on student assessment
#Note:  in the future we could incorporate irt program like mirt


#rename a few key variables up front
assess_4th_grade_dta<- assess_4th_grade_dta %>%
  mutate(student_name=m8s1q1  ,
         student_number=fourth_grade_assessment__id,
         teacher_name=m8_teacher_name,
         teacher_number=m8_teacher_code,
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
  group_by(interview__id) %>%
  summarise(student_knowledge=mean(student_knowledge),
            math_student_knowledge=mean(math_student_knowledge),
            literacy_student_knowledge=mean(literacy_student_knowledge))




#############################################
##### ECD Assessment ###########
#############################################



#read in ecd level file
ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))

#create indicator for % correct on student assessment
#Note:  in the future we could incorporate irt program like mirt
#number of missing values
ecd_dta <- ecd_dta %>%
  mutate(n_mssing_ECD=n_miss_row(.))

#rename a few key variables up front
ecd_dta<- ecd_dta %>%
  mutate(student_name=m6s1q1  ,
         student_number=ecd_assessment__id,
         teacher_name=m6_teacher_name,
         student_age=m6s1q2,
         student_male=bin_var(m6s1q3,1),
         num_students=m6_class_count,
         reading_instruction_time=m6_instruction_time,
         consent=bin_var(m6s1q4)
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
            ends_with("countingproduce_set"),
            ends_with( "number_ident"),
            ends_with("number_compare"),
            ends_with("simple_add"),
            ends_with("backward_digit"),
            ends_with("perspective"),
            ends_with("conflict_resol")), ~bin_var(.,1)  ) %>%
  mutate_at(vars(ends_with("head_shoulders")), ~if_else(.x==2,1,0,missing=NULL)) %>%
  mutate_at(vars(ends_with("vocabn")), ~case_when(.x==98 ~ as.numeric(NA),
                                                  .x==99 ~ 0,
                                                  (.x!=98 & .x!=99 & .x>=10) ~ 1,
                                                  (.x!=98 & .x!=99 & .x<10) ~ as.numeric(.x)/10,
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

ecd_dta$soc_length<-length(exec_items)


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
  group_by(interview__id) %>%
  summarise(student_knowledge=mean(student_knowledge),
            math_student_knowledge=mean(math_student_knowledge),
            literacy_student_knowledge=mean(literacy_student_knowledge),
            exec_student_knowledge=mean(exec_student_knowledge),
            soc_student_knowledge=mean(soc_student_knowledge))


#############################################
##### School Inputs ###########
#############################################

#number of missing values
school_data_INPT <- school_data_INPT %>%
  mutate(n_mssing_INPT=n_miss_row(.))

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
  group_by(interview__id) %>%
  summarise(used_ict=mean(m3sbq4_inpt))
            
#access to ICT
school_data_INPT <- school_data_INPT %>%
  mutate(access_ict=bin_var(m1sbq14_inpt,1))

final_school_data_INPT <- school_data_INPT %>%
  left_join(school_teacher_questionnaire_INPT) %>%
  mutate(inputs=rowSums(select(.,blackboard_functional, pens_etc, share_desk,  used_ict, access_ict), na.rm=TRUE))


#############################################
##### School Infrastructure ###########
#############################################

#number of missing values
school_data_INFR <- school_data_INFR %>%
  mutate(n_mssing_INFR=n_miss_row(.))
  
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
school_data_INFR <- school_data_INFR %>%
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
                                            disab_class_ramp+disab_class_entr+disab_screening)
  )
  
final_school_data_INFR <- school_data_INFR 


#############################################
##### Teacher Pedagogical Skill ###########
#############################################



final_school_data_PEDG <- ''


#############################################
##### School Operational Management ###########
#############################################

school_data_OPMN <- school_data_OPMN %>%
  mutate(n_mssing_OPMN=n_miss_row(.),
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
final_school_data_OPMN <- school_data_OPMN


#############################################
##### School Instructional Leadership ###########
#############################################

school_data_ILDR <- school_data_ILDR %>%
  mutate(n_mssing_ILDR=n_miss_row(.))

teacher_questionnaire_ILDR <- teacher_questionnaire_ILDR %>%
  mutate(n_mssing_ILDR=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise(n_mssing_teach_ILDR=sum(n_miss_row))


final_school_data_ILDR <- school_data_ILDR %>%
  left_join(teacher_questionnaire_ILDR)


#############################################
##### School Principal School Knowledge ###########
#############################################

school_data_PKNW <- school_data_PKNW%>%
  mutate(n_mssing_PKNW=n_miss_row(.))


final_school_data_PKNW <- school_data_PKNW

#############################################
##### School Principal Management Skills ###########
#############################################

school_data_PMAN <- school_data_PMAN %>%
  mutate(n_mssing_ILDR=n_miss_row(.))


final_school_data_PMAN <- school_data_PMAN

#############################################
##### School Teaching Attraction ###########
#############################################

school_data_TATT <- school_data_TATT %>%
  mutate(n_mssing_TATT=n_miss_row(.))

teacher_questionnaire_TATT <- teacher_questionnaire_TATT %>%
  mutate(n_mssing_TATT=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise(n_mssing_teach_TATT=sum(n_miss_row))



final_school_data_TATT <- school_data_TATT %>%
  left_join(teacher_questionnaire_TATT)



#############################################
##### School Teaching Selection and Deployment ###########
#############################################


school_data_TSDP <- school_data_TSDP %>%
  mutate(n_mssing_TSDP=n_miss_row(.))

teacher_questionnaire_TSDP <- teacher_questionnaire_TSDP %>%
  mutate(n_mssing_TSDP=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise(n_mssing_teach_TSDP=sum(n_miss_row))


final_school_data_TSDP <- school_data_TSDP %>%
  left_join(teacher_questionnaire_TSDP)

#############################################
##### School Teaching Support ###########
#############################################
school_data_TSUP <- school_data_TSUP %>%
  mutate(n_mssing_TSUP=n_miss_row(.))


teacher_questionnaire_TSUP <- teacher_questionnaire_TSUP %>%
  mutate(n_mssing_TSUP=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise(n_mssing_teach_TSUP=sum(n_miss_row))


final_school_data_TSUP <- school_data_TSUP %>%
  left_join(teacher_questionnaire_TSUP)



#############################################
##### School Teaching Evaluation ###########
#############################################

school_data_TEVL <- school_data_TEVL %>%
  mutate(n_mssing_TEVL=n_miss_row(.))

final_school_data_TEVL <- school_data_TEVL


#############################################
##### School  Monitoring and Accountability ###########
#############################################
school_data_TMNA <- school_data_TMNA %>%
  mutate(n_mssing_TMNA=n_miss_row(.))

teacher_questionnaire_TMNA <- teacher_questionnaire_TMNA %>%
  mutate(n_mssing_TMNA=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise(n_mssing_teach_TMNA=sum(n_miss_row))



final_school_data_TMNA <- school_data_TMNA  %>%
  left_join(teacher_questionnaire_TMNA)

#############################################
##### School  Intrinsic Motivation ###########
#############################################

school_data_TINM <- school_data_TINM %>%
  mutate(n_mssing_TINM=n_miss_row(.))

teacher_questionnaire_TINM <- teacher_questionnaire_TINM %>%
  mutate(n_mssing_TINM=n_miss_row(.)) %>%
  group_by(interview__id) %>%
  summarise(n_mssing_teach_TINM=sum(n_miss_row))


final_school_data_TINM <- school_data_TINM %>%
  left_join(teacher_questionnaire_TINM)

#############################################
##### School  Inputs and Infrastructure Standards ###########
#############################################

school_data_ISTD <- school_data_ISTD %>%
  mutate(n_mssing_ISTD=n_miss_row(.))


final_school_data_ISTD <- school_data_ISTD

#############################################
##### School  Inputs and Infrastructure Monitoring ###########
#############################################

school_data_IMON <- school_data_IMON %>%
  mutate(n_mssing_IMON=n_miss_row(.))


final_school_data_IMON <- school_data_IMON



#############################################
##### School School Management Attraction  ###########
#############################################
school_data_SATT <- school_data_SATT %>%
  mutate(n_mssing_SATT=n_miss_row(.))


final_school_data_SATT <- school_data_SATT


#############################################
##### School School Management Selection and Deployment  ###########
#############################################

school_data_SSLD <- school_data_SSLD %>%
  mutate(n_mssing_SSLD=n_miss_row(.))

final_school_data_SSLD <- school_data_SSLD


#############################################
##### School School Management Support  ###########
#############################################

school_data_SSUP <- school_data_SSUP %>%
  mutate(n_mssing_SSUP=n_miss_row(.))


final_school_data_SSUP <- school_data_SSUP

#############################################
##### School School Management Evaluation  ###########
#############################################

school_data_SEVL <- school_data_SEVL %>%
  mutate(n_mssing_SEVL=n_miss_row(.))


final_school_data_SEVL <- school_data_SEVL



#############################################
##### School Level Info ###########
#############################################

#Build school level database



#merge on 4th grade student learning
school_dta <- school_dta %>%
  left_join(school_student_knowledge, by="interview__id")

#merge on teacher absence
school_dta <- school_dta %>%
  left_join(school_absence_rate, by="interview__id")

#merge on teacher content knowledge
school_dta <- school_dta %>%
  left_join(school_content_knowledge, by="interview__id")


######DELETE THIS########
#Fill with random numbers of missing
school_dta$rand1<-runif(nrow(school_dta))
school_dta$rand2<-runif(nrow(school_dta))
school_dta$rand3<-runif(nrow(school_dta))
school_dta$pedagogical_knowledge<-rbinom(nrow(school_dta),50,0.5)/10
school_dta$inputs<-rbinom(nrow(school_dta),40,0.5)/10
school_dta$infrastructure<-rbinom(nrow(school_dta),50,0.5)/10
school_dta$ecd<-runif(nrow(school_dta))
school_dta$operational_management<-rbinom(nrow(school_dta),50,0.5)/10
school_dta$instructional_leadership<-rbinom(nrow(school_dta),50,0.5)/10
school_dta$school_knowledge<-rbinom(nrow(school_dta),50,0.5)/10
school_dta$management_skills<-rbinom(nrow(school_dta),50,0.5)/10

#replace missing indicator values with random numbers
school_dta$student_knowledge[is.na(school_dta$student_knowledge)]<-school_dta$rand1[is.na(school_dta$student_knowledge)]
school_dta$absence_rate[is.na(school_dta$absence_rate)]<-school_dta$rand2[is.na(school_dta$absence_rate)]
school_dta$content_knowledge[is.na(school_dta$content_knowledge)]<-school_dta$rand3[is.na(school_dta$content_knowledge)]

school_dta$learning_outcome<-100*runif(nrow(school_dta))
school_dta$participation_outcome<-100*runif(nrow(school_dta))

#rename database as practice data and collapse to single country observation



write.csv(school_dta, file = "C:/Users/WB469649/OneDrive - WBG/Education Policy Dashboard/Survey Solutions/Peru/gepd_map_peru/school_dta.csv")


################################
#Store Key Created Datasets
################################

#saves the following in R and stata format

data_list <- c('school_dta', 'teacher_questionnaire','teacher_absence_final')

#loop and produce list of data tables




for (i in indicator_names ) {
  if (exists(paste("final_school_data_",i, sep=""))) {
  temp<-get(paste("final_school_data_",i, sep="")) 
    skim(temp) %>%
      DT::datatable()
  }
}


for (i in indicator_names ) {
  if (exists(paste("final_school_data_",i, sep=""))) {
    temp<-get(paste("final_school_data_",i, sep="")) 
    skim(temp) %>%
      skimr::kable()
  }
}

skim(final_school_data_INFR) %>%
  skimr::kable()

