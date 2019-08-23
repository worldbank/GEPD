#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries
library(tidyverse)
library(haven)
library(stringr)
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




#Load the data

###########################
#read in school level file
###########################
download_folder <- choose.dir(default = "", caption = "Select folder to open data downloaded from API")

school_dta<-read_dta(file.path(download_folder, "EPDash.dta"))

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




########################################
#read in 4th grade assessment level file
########################################
assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))

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
preamble_info <- c('interview__id', 'questionnaire_roster__id', 'teacher_name', 'teacher_number',
                   'available', 'teacher_position', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 'teacher_grd5',
                   'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'teacher_education', 'teacher_year_began',
                   'teacher_age')



#use dplyr select(contains()) to search for variables with select tags and create separate databases
for (i in indicator_names ) {
  temp_df<-teacher_questionnaire %>%
    select( contains(i))
  if (ncol(temp_df) > 0) {
    temp_df<-teacher_questionnaire %>%
      select(preamble_info, contains(i))
    assign(paste("teacher_questionnaire_",i, sep=""), temp_df )
  }
}



#############################################
##### Teacher Absence ###########
#############################################

#read in teacher absence file
teacher_absence_dta<-read_dta(file.path(download_folder, "questionnaire_selected.dta"))
teacher_absence_metadta<-makeVlist(teacher_absence_dta)

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
preamble_info <- c('interview__id', 'questionnaire_roster__id', 'teacher_name', 'teacher_number',
                   'available', 'teacher_position','teacher_gender', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 'teacher_grd5',
                   'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'teacher_education', 'teacher_year_began',
                   'teacher_age')

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


#Build teacher absence practice indicator
final_school_absence_rate <- teacher_absence_dta %>%
  group_by(interview__id) %>%
  summarise(school_absence_rate=mean(school_absent), 
            absence_rate=mean(absent),
            principal_absence_rate=mean(principal_absent)
            )


#############################################
##### Teacher Knowledge ###########
#############################################

teacher_assessment_dta<-read_dta(file.path(download_folder, "teacher_assessment.dta"))



#rename a few key variables up front
teacher_assessment_dta<- teacher_assessment_dta %>%
  mutate(teacher_name=m5sb_troster  ,
         teacher_number=m5sb_tnumber ,
         consent=m5_consent
  )

#recode assessment variables to be 1 if student got it correct and zero otherwise
teacher_assessment_dta<- teacher_assessment_dta %>%
  mutate_at(c(starts_with("m5s1q"), starts_with("m5s2q")), ~bin_var(.,1)  )


#create indicator for % correct on teacher assessment
#Note:  in the future we could incorporate irt program like mirt


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m5s1q, which is the prefix of literacy items
teacher_assessment_dta$literacy_length<-length(grep(x=colnames(teacher_assessment_dta), pattern="m5s1q"))

####NEED TO FIX THIS BECAUSE some items have non-binary responses####

lit_items<-colnames(teacher_assessment_dta[,grep(x=colnames(teacher_assessment_dta), pattern="m5s1q")])

#calculate teachers lit items correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(literacy_content_knowledge=rowSums(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q")]))

####Math####
#calculate # of math items
teacher_assessment_dta$math_length<-length(grep(x=colnames(teacher_assessment_dta), pattern="m5s2q"))

math_items<-colnames(teacher_assessment_dta[,grep(x=colnames(teacher_assessment_dta), pattern="m5s2q")])

#calculate teachers math items correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(math_content_knowledge=rowSums(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s2q")]))

####Total score####
#calculate teachers percent correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(content_knowledge=(math_content_knowledge+literacy_content_knowledge)/(literacy_length+math_length),
         math_content_knowledge=math_content_knowledge/math_length,
         literacy_content_knowledge=literacy_content_knowledge/literacy_length)


#calculate % correct for literacy, math, and total
final_school_content_knowledge <- teacher_assessment_dta %>%
  group_by(interview__id) %>%
  summarise(content_knowledge=mean(content_knowledge),
            math_content_knowledge=mean(math_content_knowledge),
            literacy_content_knowledge=mean(literacy_content_knowledge))



#############################################
##### 4th Grade Assessment ###########
#############################################

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
  mutate_at(c(starts_with("m8saq"), starts_with("m8sbq")), ~bin_var(.,1)  )


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items
assess_4th_grade_dta$literacy_length<-length(grep(x=colnames(assess_4th_grade_dta), pattern="m8saq"))

lit_items<-colnames(assess_4th_grade_dta[,grep(x=colnames(assess_4th_grade_dta), pattern="m8saq")])


#calculate students lit items correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(literacy_student_knowledge=rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq")]))

####Math####
#calculate # of math items
assess_4th_grade_dta$math_length<-length(grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq"))

math_items<-colnames(assess_4th_grade_dta[,grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq")])


#Do some temporary recoding of first few items
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(m8sbq1=ifelse(m8sbq1>0 & m8sbq1<98,1,0) )

#calculate students math items correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(math_student_knowledge=rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq")]))

####Total score####
#calculate students percent correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(student_knowledge=(math_student_knowledge+literacy_student_knowledge)/(literacy_length+math_length),
         math_student_knowledge=math_student_knowledge/math_length,
         literacy_student_knowledge=literacy_student_knowledge/literacy_length)


#calculate % correct for literacy, math, and total
school_student_knowledge <- assess_4th_grade_dta %>%
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


ender <- function(variables) {
  
}  
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
  mutate_at(vars(ends_with("head_shoulders")), ~if_else(.==2,1,0,missing=NULL)) %>%
  mutate_at(vars(ends_with("vocabn")), ~case_when(.==98 ~ as.numeric(NA),
                                                  .==99 ~ 0,
                                                  (.!=98 & .!=99) ~ as.numeric(.),
                                                  is.na(.) ~ as.numeric(NA)))


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items


lit_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "vocabn|comprehension|letters|words|sentence|name_writing|print")])

ecd_dta$literacy_length<-length(lit_items)

#calculate students lit items correct
ecd_dta <- ecd_dta %>%
  mutate(literacy_student_knowledge=rowSums(.[grep(x=colnames(ecd_dta), 
  pattern="vocabn|comprehension|letters|words|sentence|name_writing|print")]))

####Math####
#calculate # of math items

math_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "counting|produce_set|number_ident|number_compare|simple_add")])

ecd_dta$math_length<-length(math_items)


#calculate students math items correct
ecd_dta <- ecd_dta %>%
  mutate(math_student_knowledge=rowSums(.[grep(x=colnames(ecd_dta), 
                                                   pattern="counting|produce_set|number_ident|number_compare|simple_add")]))

####Executive Functioning####
#calculate # of Exec Function items

exec_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "backward_digit|head_shoulders")])

ecd_dta$exec_length<-length(exec_items)


#calculate students excec items correct
ecd_dta <- ecd_dta %>%
  mutate(exec_student_knowledge=rowSums(.[grep(x=colnames(ecd_dta), 
                                               pattern="backward_digit|head_shoulders")]))

####Socio-Emotional####
#calculate # of Exec Function items

soc_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "perspective$|conflict_resol$")])

ecd_dta$soc_length<-length(exec_items)


#calculate students excec items correct
ecd_dta <- ecd_dta %>%
  mutate(soc_student_knowledge=rowSums(.[grep(x=colnames(ecd_dta), 
                                               pattern="perspective$|conflict_resol$")]))


####Total score####
#calculate students percent correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(student_knowledge=(math_student_knowledge+literacy_student_knowledge)/(literacy_length+math_length),
         math_student_knowledge=math_student_knowledge/math_length,
         literacy_student_knowledge=literacy_student_knowledge/literacy_length)


#calculate % correct for literacy, math, and total
school_student_knowledge <- assess_4th_grade_dta %>%
  group_by(interview__id) %>%
  summarise(student_knowledge=mean(student_knowledge),
            math_student_knowledge=mean(math_student_knowledge),
            literacy_student_knowledge=mean(literacy_student_knowledge))


#############################################
##### School Inputs ###########
#############################################

#functioning blackboard and chalk
school_data_INPT <- school_data_INPT %>%
  mutate(blackboard_functional=case_when(
    blackboard_contrast=="Yes" & chalk=="Yes" & blackboard=="Yes"  ~ 1,
    blackboard_contrast=="No" | chalk=="No" | blackboard=="No" ~ 0)) 

#pens, pencils, textbooks, exercise books
school_data_INPT <- school_data_INPT %>%
  mutate(share_textbook=(books_boys+books_girls)/(num_boys+num_girls)) %>%
  mutate(share_pencil=(pen_boys+pen_girls)/(num_boys+num_girls)) %>%
  mutate(share_exbook=(booklet_boys+booklet_girls)/(num_boys+num_girls)) %>%
  mutate(pens_etc=case_when(
    share_textbook>=0.9 & share_pencil>=0.9 & share_exbook>=0.9  ~ 1,
    share_textbook<0.9 | share_pencil<0.9 | share_exbook<0.9 ~ 0)) 

#basic classroom furniture
school_data_INPT <- school_data_INPT %>%
  mutate(student_sit=if_else(student_sit=="Yes", 1,0, as.numeric(NA) ))

#Used ICT


#access to ICT


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