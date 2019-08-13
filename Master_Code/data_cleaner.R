#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries
library(ggplot2)
library(dplyr)
library(haven)
#NOTE:  The R script to pull the data from the API should be run before this file

#Load the data
#read in school level file
download_folder <- file.path("C:/Users/WB469649/OneDrive - WBG/Education Policy Dashboard/Survey Solutions/Peru/Data")

school_dta<-read_dta(file.path(download_folder, "EPDash.dta"))


##########Remove This##############
#temporary change of coordinates
school_dta$m1s0q9__Latitude[1]=-11.832258
school_dta$m1s0q9__Longitude[1]=-77.000996
school_dta$m1s0q9__Latitude[2]=-12.055666
school_dta$m1s0q9__Longitude[2]=-77.027924
school_dta$m1s0q9__Latitude[3]=-11.920157
school_dta$m1s0q9__Longitude[3]=-77.045572
school_dta$m1s0q9__Latitude[4]=-12.059618
school_dta$m1s0q9__Longitude[4]=-76.992199
###################################

#read in ecd level file
ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))

#read in 4th grade assessment level file
assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))

#read in teacher questionnaire level file
teacher_questionnaire<-read_dta(file.path(download_folder, "questionnaire_roster.dta"))

#read in teacher absence file
teacher_absence_dta<-read_dta(file.path(download_folder, "absence_roster2.dta"))

#read in principal absence file
principal_absence_dta<-read_dta(file.path(download_folder, "principal_absence_roster2.dta"))

#read in teacher assessment file
teacher_assessment_dta<-read_dta(file.path(download_folder, "teacher_assessment.dta"))

#Note:  Schools can be identified between modules by the identifier:  interview__id

#############################################
##### Teacher Absence ###########
#############################################
#create indicator for whether each teacher was absent from school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(school_absent=case_when(
    m2sbq6==6  ~ 1,
    m2sbq6!=6   ~ 0,
    is.na(m2sbq6) ~ as.numeric(NA)))

#create indicator for whether each teacher was absent from classroom or school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(absent=case_when(
    m2sbq6==6 | m2sbq6==5 | m2sbq6==2  ~ 1,
    m2sbq6!="Absent" & m2sbq6!=5 &  m2sbq6!=2 ~ 0,
    is.na(m2sbq6) ~ as.numeric(NA)) )


#Build teacher absence practice indicator
school_absence_rate <- teacher_absence_dta %>%
  group_by(interview__id) %>%
  summarise(school_absence_rate=mean(school_absent), absence_rate=mean(absent))


#############################################
##### Teacher Knowledge ###########
#############################################

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
school_content_knowledge <- teacher_assessment_dta %>%
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

#############################################
##### School Infrastructure ###########
#############################################

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