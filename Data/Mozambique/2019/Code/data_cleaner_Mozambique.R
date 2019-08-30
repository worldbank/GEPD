#Data files are read from Mozambique SDI data
#Written by Brian Stacy 7/8/2019

#load relevant libraries
library(ggplot2)
library(dplyr)
library(haven)

#Load the data
#read in school level file
download_folder <- file.path("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data")

#load R dataframe
load(file.path(download_folder, "all_modules.RData"))

school_dta<-rmod1


#read in ecd level file
#ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))

#read in 4th grade assessment level file
assess_4th_grade_dta<-rmod5

#read in teacher questionnaire level file
#teacher_questionnaire<-read_dta(file.path(download_folder, "rmod6.dta"))

#read in teacher absence file
teacher_absence_dta<-rmod2_abs

#read in principal absence file
#principal_absence_dta<-rmod2_abs

#read in teacher assessment file
teacher_assessment_dta<-rmod6

#read in classroom observation file
classroom_observation<-rmod4

#Note:  Schools can be identified between modules by the identifier:  sch_id

#############################################
##### Teacher Absence ###########
#############################################

#create indicator for whether each teacher was absent from school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(school_absent=case_when(
    visit_2=="Absent"  ~ 1,
    visit_2!="Absent"   ~ 0,
    is.na(visit_2) ~ as.numeric(NA)))

#create indicator for whether each teacher was absent from classroom or school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(absent=case_when(
    visit_2=="Absent" | visit_2=="Classroom - not teaching" | visit_2=="School - had class"  ~ 1,
    visit_2!="Absent" & visit_2!="Classroom - not teaching" &  visit_2!="School - had class" ~ 0,
    is.na(visit_2) ~ as.numeric(NA)) )

#create indicator for whether each teacher was absent from classroom or school in 1st visit
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(absent_1st=case_when(
    visit_1=="Absent" | visit_1=="Classroom - not teaching" | visit_1=="School - had class"  ~ 1,
    visit_1!="Absent" & visit_1!="Classroom - not teaching" &  visit_1!="School - had class" ~ 0,
    is.na(visit_1) ~ as.numeric(NA)) )


#Build teacher absence practice indicator
school_absence_rate <- teacher_absence_dta %>%
  group_by(sch_id) %>%
  summarise(school_absence_rate=mean(school_absent, na.rm=TRUE), absence_rate=mean(absent, na.rm=TRUE), absence_rate_1st=mean(absent_1st, na.rm=TRUE))

absent_table<-teacher_absence_dta %>%
  summarise(school_absence_rate=mean(school_absent, na.rm=TRUE), absence_rate=mean(absent, na.rm=TRUE), absence_rate_1st=mean(absent_1st, na.rm=TRUE))

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(absent_table)

#############################################
##### Teacher Knowledge ###########
#############################################

#create indicator for % correct on teacher assessment
#Note:  in the future we could incorporate irt program like mirt


####Literacy####
#calculate # of literacy items

lit_items<-c("p1a", "p1b", "p1c", "p1d", "p2a", "p2b", "p2c", "p2d", "p2e",
             "p2f", "p3a", "p3b", "p3c", "p3d",  "p3e", "p3f", "p3g", "p3h",
             "p3i", "p3j", "p3k", "p4")
teacher_assessment_dta$literacy_length<-41

colnames(teacher_assessment_dta[,lit_items])

#calculate teachers lit items correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(literacy_content_knowledge=rowSums(.[lit_items]))

####Math####
#calculate # of math items

math_items<-colnames(teacher_assessment_dta[,grep(x=colnames(teacher_assessment_dta), pattern="m")])
math_items<-math_items[1:23]

teacher_assessment_dta$math_length<-22

#calculate teachers math items correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(math_content_knowledge=rowSums(.[math_items]))

####Total score####
#calculate teachers percent correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(content_knowledge=(math_content_knowledge+literacy_content_knowledge)/(literacy_length+math_length),
         math_content_knowledge=math_content_knowledge/math_length,
         literacy_content_knowledge=literacy_content_knowledge/literacy_length)


#calculate % correct for literacy, math, and total
school_content_knowledge <- teacher_assessment_dta %>%
  group_by(sch_id) %>%
  summarise(content_knowledge=mean(content_knowledge, na.rm=TRUE),
            math_content_knowledge=mean(math_content_knowledge, na.rm=TRUE),
            literacy_content_knowledge=mean(literacy_content_knowledge, na.rm=TRUE))

teacher_assessment_dta %>%
  summarise(content_knowledge=mean(content_knowledge, na.rm=TRUE),
            math_content_knowledge=mean(math_content_knowledge, na.rm=TRUE),
            literacy_content_knowledge=mean(literacy_content_knowledge, na.rm=TRUE))

#############################################
##### 4th Grade Assessment ###########
#############################################

#create indicator for % correct on student assessment
#Note:  in the future we could incorporate irt program like mirt


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items
assess_4th_grade_dta$literacy_length<-78


####Math####
#calculate # of math items
assess_4th_grade_dta$math_length<-17


####Total score####
#calculate students percent correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(student_knowledge=(sum_mat+sum_por)/(literacy_length+math_length),
         math_student_knowledge=sum_mat/math_length,
         literacy_student_knowledge=sum_por/literacy_length)


#calculate % correct for literacy, math, and total at school level
school_student_knowledge <- assess_4th_grade_dta %>%
  group_by(sch_id) %>%
  summarise(student_knowledge=mean(student_knowledge, na.rm=TRUE),
            math_student_knowledge=mean(math_student_knowledge, na.rm=TRUE),
            literacy_student_knowledge=mean(literacy_student_knowledge, na.rm=TRUE))

assess_4th_grade_dta %>%
  filter(!is.na(student_knowledge)) %>%
  summarise(student_knowledge=mean(student_knowledge),
            math_student_knowledge=mean(math_student_knowledge),
            literacy_student_knowledge=mean(literacy_student_knowledge))



#############################################
##### School Inputs ###########
#############################################

#functioning blackboard and chalk
classroom_observation <- classroom_observation %>%
  mutate(blackboard_functional=case_when(
    blackboard_contrast=="Yes" & chalk=="Yes" & blackboard=="Yes"  ~ 1,
    blackboard_contrast=="No" | chalk=="No" | blackboard=="No" ~ 0)) 

#pens, pencils, textbooks, exercise books
classroom_observation <- classroom_observation %>%
  mutate(share_textbook=(books_boys+books_girls)/(num_boys+num_girls)) %>%
  mutate(share_pencil=(pen_boys+pen_girls)/(num_boys+num_girls)) %>%
  mutate(share_exbook=(booklet_boys+booklet_girls)/(num_boys+num_girls)) %>%
  mutate(pens_etc=case_when(
    share_textbook>=0.9 & share_pencil>=0.9 & share_exbook>=0.9  ~ 1,
    share_textbook<0.9 | share_pencil<0.9 | share_exbook<0.9 ~ 0)) 

#basic classroom furniture
classroom_observation <- classroom_observation %>%
  mutate(student_sit=if_else(student_sit=="Yes", 1,0, as.numeric(NA) ))


#access to ICT


  

#############################################
##### School Infrastructure ###########
#############################################


#drinking water
school_dta <- school_dta %>%
  mutate(drinking_water=if_else(drinking_water=="Yes", 1,0, as.numeric(NA) ))
  
#functioning toilets
school_dta <- school_dta %>%
  mutate(functioning_toilet=case_when(
    toilet=="Yes" & toilet_separate=="Yes" & toilet_clean!="Not very clean" & toilet_access=="Yes" & toilet_handwash=="Yes" ~ 1,
    toilet=="No"  ~ 0,
    toilet=="Yes" & ( toilet_separate=="No" | toilet_clean=="Not very clean"| toilet_access=="No" | toilet_handwash=="No") ~ 0)) 


#classroom visibility
classroom_observation <- classroom_observation %>%
  mutate(class_light=if_else(class_light=="Yes"  , 1,0, as.numeric(NA) )) 

#electricity
classroom_observation <- classroom_observation %>%
  mutate(class_electricity=if_else(class_electricity=="Yes"  , 1,0, as.numeric(NA) )) 

#accessibility for people with disabilities

#############################################
##### School Level Info ###########
#############################################

#Build school level database

#Classroom observation.  Start by creating list of variables to keep
class_keep_list<-c("blackboard_functional", "blackboard_contrast", "chalk", "blackboard",
                   "pens_etc", "share_textbook", "share_pencil", "share_exbook",
                   "student_sit",
                   "class_light",
                   "class_electricity",
                   "teach_score")

school_classroom_observation <- classroom_observation %>%
  group_by(sch_id) %>%
  summarise_at(vars(class_keep_list), mean, na.rm=TRUE)

#merge on classroom observation
school_dta <- school_dta %>%
  left_join(school_classroom_observation, by="sch_id")

#merge on 4th grade student learning
school_dta <- school_dta %>%
  left_join(school_student_knowledge, by="sch_id")

#merge on teacher absence
school_dta <- school_dta %>%
  left_join(school_absence_rate, by="sch_id")

#merge on teacher content knowledge
school_dta <- school_dta %>%
  left_join(school_content_knowledge, by="sch_id")


######Build Final Indicator for Practices######

#teacher ped skill
school_dta$pedagogical_knowledge<-school_dta$teach_score

#inputs
school_dta <- school_dta %>%
  mutate(inputs=blackboard_functional+pens_etc+student_sit)

#infrastructure
school_dta <- school_dta %>%
  mutate(infrastructure=drinking_water+functioning_toilet+class_light+class_electricity)

#prepared learners
school_dta$ecd<-as.numeric(NA)

#operational management
school_dta$operational_management<-as.numeric(NA)

#instructional leadership
school_dta$instructional_leadership<-as.numeric(NA)

#school knowledge
school_dta$school_knowledge<-as.numeric(NA)

#management skills
school_dta$management_skills<-as.numeric(NA)

#student assessment
#school_dta$student_knowledge

#teacher absence
#school_dta$absence_rate

#teacher content knowledge 
#school_dta$content_knowledge



#rename database as practice data and collapse to single country observation

write.csv(school_dta, file = "C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/school_dta.csv")

write.csv(school_dta, file = "C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Code/gepd_map_moz/school_dta.csv")

#create datatable with school level indicators
output_table_keeplist <- c("school", "province", "district", "sch_id", "student_knowledge", "absence_rate", 
                           "absence_rate_1st","content_knowledge", "pedagogical_knowledge","inputs", 
                           "infrastructure", "ecd", "operational_management", "instructional_leadership",
                           "school_knowledge", "management_skills", "lat", "lon")

school_indicator_data <- school_dta %>%
  select(output_table_keeplist) %>%
  mutate(school=structure(school, label="School Name")) %>%
  mutate(province=structure(province, label="Province")) %>%
  mutate(district=structure(district, label="District")) %>%
  mutate(sch_id=structure(sch_id, label="EMIS")) %>%
  mutate(student_knowledge=structure(100*round(student_knowledge, digits=2), label="Avg 4th Grade Learning Score")) %>%
  mutate(absence_rate=structure(100*round(absence_rate, digits=2), label="Teacher Absence Rate")) %>%
  mutate(absence_rate_1st=structure(100*round(absence_rate_1st, digits=2), label="Teacher Absence Rate")) %>%
  mutate(content_knowledge=structure(100*round(content_knowledge, digits=2), label="Teacher Content Knowledge Score")) %>%
  mutate(pedagogical_knowledge=structure(round(pedagogical_knowledge, digits=2), label="Teacher Pedagogical Skill")) %>%
  mutate(inputs=structure(round(inputs, digits=2), label="Basic Inputs")) %>%
  mutate(infrastructure=structure(round(infrastructure, digits=2), label="Basic Infrastructure")) %>%
  mutate(ecd=structure(100*round(ecd, digits=2), label="Capacity for Learning")) %>%
  mutate(operational_management=structure(round(operational_management, digits=2), label="Operational Management")) %>%
  mutate(instructional_leadership=structure(round(instructional_leadership, digits=2), label="Instructional Leadership")) %>%
  mutate(school_knowledge=structure(round(school_knowledge, digits=2), label="School Knowledge")) %>%
  mutate(management_skills=structure(round(management_skills, digits=2), label="Management Skills"))


save(school_indicator_data, school_dta, classroom_observation, assess_4th_grade_dta, 
     teacher_absence_dta, teacher_assessment_dta, region, strata, full_sample,
     file=file.path(download_folder, "clean/all_modules_clean.RData"))
