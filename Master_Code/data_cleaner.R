#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries
library(ggplot2)
library(dplyr)
library(haven)
library(readr)
library(tidyr)
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

raw_school_dta<-read_dta(file.path(download_folder, "EPDash.dta"))

#rename a few key variables up front
raw_school_dta<- raw_school_dta %>%
  mutate(enumerator_name=m1s0q1_name  ,
            enumerator_number=m1s0q1_number ,
            survey_time=m1s0q8,
            lat=m1s0q9__Latitude,
            lon=m1s0q9__Longitude
            )

#create school metadata frame
school_metadta<-makeVlist(raw_school_dta)

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
                   'enumerator_name', 'enumerator_number', 'survey_time', 'lat', 'lon')

#use dplyr select(contains()) to search for variables with select tags and create separate databases by indicator
#This will make the information for each indicator contained in an independent database
#Will need to join the school level information with teacher level questionnaire information for some indicators.  This will be done later.
for (i in indicator_names ) {
  temp_df<-raw_school_dta %>%
    select(preamble_info, contains(i))
  if (ncol(temp_df) > 0) {
    assign(paste("raw_school_data_",i, sep=""), temp_df )
  }
}


########################
#read in ecd level file
########################
raw_ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))

########################################
#read in 4th grade assessment level file
########################################
raw_assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))

#########################################
#read in teacher questionnaire level file
#########################################
raw_teacher_questionnaire<-read_dta(file.path(download_folder, "questionnaire_roster.dta"))
raw_teacher_questionnaire_metadta<-makeVlist(raw_teacher_questionnaire)

#rename a few key variables up front
raw_teacher_questionnaire<- raw_teacher_questionnaire %>%
  mutate(teacher_name=m3sb_troster  ,
         teacher_number=m3sb_tnumber ,
         available=m3s0q1,
         teacher_position=m3saq1,
         teacher_grd1=m3saq2__1==1,
         teacher_grd2=m3saq2__2==1,
         teacher_grd3=m3saq2__3==1,
         teacher_grd4=m3saq2__4==1,
         teacher_grd5=m3saq2__5==1,
         teacher_language=m3saq3__1==1,
         teacher_math=m3saq3__2==1,
         teacher_both_subj=m3saq3__3==1,
         teacher_other_subj=m3saq3__4==1,
         teacher_education=m3saq4,
         teacher_year_began=m3saq5,
         teacher_age=m3saq6,
  )

#list additional info that will be useful to keep in each indicator dataframe
preamble_info <- c('interview__id', 'questionnaire_roster_id', 'teacher_name', 'teacher_number',
                   'available', 'teacher_position', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 'teacher_grd5',
                   'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'teacher_education', 'teacher_year_began',
                   'teacher_age')


#use dplyr select(contains()) to search for variables with select tags and create separate databases
for (i in indicator_names ) {
  temp_df<-raw_teacher_questionnaire %>%
    select(contains(i))
  if (ncol(temp_df) > 0) {
    assign(paste("raw_teacher_questionnaire_",i, sep=""), temp_df )
  }
}



################################
#read in principal absence file
################################
raw_principal_absence_dta<-read_dta(file.path(download_folder, "principal_absence_roster2.dta"))


#############################################
##### Teacher Absence ###########
#############################################

raw_teacher_absence_dta<-read_dta(file.path(download_folder, "absence_roster2.dta"))



#create indicator for whether each teacher was absent from school
raw_teacher_absence_dta <- raw_teacher_absence_dta %>%
  mutate(school_absent=case_when(
    m2sbq6==6  ~ 1,
    m2sbq6!=6   ~ 0,
    is.na(m2sbq6) ~ as.numeric(NA)))

#create indicator for whether each teacher was absent from classroom or school
raw_teacher_absence_dta <- raw_teacher_absence_dta %>%
  mutate(absent=case_when(
    m2sbq6==6 | m2sbq6==5 | m2sbq6==2  ~ 1,
    m2sbq6!="Absent" & m2sbq6!=5 &  m2sbq6!=2 ~ 0,
    is.na(m2sbq6) ~ as.numeric(NA)) )


#Build teacher absence practice indicator
final_school_absence_rate <- raw_teacher_absence_dta %>%
  group_by(interview__id) %>%
  summarise(school_absence_rate=mean(school_absent), absence_rate=mean(absent))


#############################################
##### Teacher Knowledge ###########
#############################################

raw_teacher_assessment_dta<-read_dta(file.path(download_folder, "teacher_assessment.dta"))

#create indicator for % correct on teacher assessment
#Note:  in the future we could incorporate irt program like mirt


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m5s1q, which is the prefix of literacy items
raw_teacher_assessment_dta$literacy_length<-length(grep(x=colnames(raw_teacher_assessment_dta), pattern="m5s1q"))

####NEED TO FIX THIS BECAUSE some items have non-binary responses####

lit_items<-colnames(raw_teacher_assessment_dta[,grep(x=colnames(raw_teacher_assessment_dta), pattern="m5s1q")])

#calculate teachers lit items correct
raw_teacher_assessment_dta <- raw_teacher_assessment_dta %>%
  mutate(literacy_content_knowledge=rowSums(.[grep(x=colnames(raw_teacher_assessment_dta), pattern="m5s1q")]))

####Math####
#calculate # of math items
raw_teacher_assessment_dta$math_length<-length(grep(x=colnames(raw_teacher_assessment_dta), pattern="m5s2q"))

math_items<-colnames(raw_teacher_assessment_dta[,grep(x=colnames(raw_teacher_assessment_dta), pattern="m5s2q")])

#calculate teachers math items correct
raw_teacher_assessment_dta <- raw_teacher_assessment_dta %>%
  mutate(math_content_knowledge=rowSums(.[grep(x=colnames(raw_teacher_assessment_dta), pattern="m5s2q")]))

####Total score####
#calculate teachers percent correct
raw_teacher_assessment_dta <- raw_teacher_assessment_dta %>%
  mutate(content_knowledge=(math_content_knowledge+literacy_content_knowledge)/(literacy_length+math_length),
         math_content_knowledge=math_content_knowledge/math_length,
         literacy_content_knowledge=literacy_content_knowledge/literacy_length)


#calculate % correct for literacy, math, and total
final_school_content_knowledge <- raw_teacher_assessment_dta %>%
  group_by(interview__id) %>%
  summarise(content_knowledge=mean(content_knowledge),
            math_content_knowledge=mean(math_content_knowledge),
            literacy_content_knowledge=mean(literacy_content_knowledge))



#############################################
##### 4th Grade Assessment ###########
#############################################

#create indicator for % correct on student assessment
#Note:  in the future we could incorporate irt program like mirt


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items
assess_4th_grade_dta$literacy_length<-length(grep(x=colnames(assess_4th_grade_dta), pattern="m8saq"))

lit_items<-colnames(assess_4th_grade_dta[,grep(x=colnames(assess_4th_grade_dta), pattern="m8saq")])

#Do some temporary recoding of first few items
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(m8saq1=ifelse(m8saq1>0 & m8saq1<98,1,0) ,
         m8saq2=ifelse(m8saq2>0 & m8saq2<98,1,0) ,
         m8saq3=ifelse(m8saq3>0 & m8saq3<98,1,0) ,
         m8saq4=ifelse(m8saq4>0 & m8saq4<98,1,0) )

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
##### School Inputs ###########
#############################################

#functioning blackboard and chalk
raw_school_data_INPT <- raw_school_data_INPT %>%
  mutate(blackboard_functional=case_when(
    blackboard_contrast=="Yes" & chalk=="Yes" & blackboard=="Yes"  ~ 1,
    blackboard_contrast=="No" | chalk=="No" | blackboard=="No" ~ 0)) 

#pens, pencils, textbooks, exercise books
raw_school_data_INPT <- raw_school_data_INPT %>%
  mutate(share_textbook=(books_boys+books_girls)/(num_boys+num_girls)) %>%
  mutate(share_pencil=(pen_boys+pen_girls)/(num_boys+num_girls)) %>%
  mutate(share_exbook=(booklet_boys+booklet_girls)/(num_boys+num_girls)) %>%
  mutate(pens_etc=case_when(
    share_textbook>=0.9 & share_pencil>=0.9 & share_exbook>=0.9  ~ 1,
    share_textbook<0.9 | share_pencil<0.9 | share_exbook<0.9 ~ 0)) 

#basic classroom furniture
raw_school_data_INPT <- raw_school_data_INPT %>%
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