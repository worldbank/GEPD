*Clean data files for GEPD school indicators
*Written by Brian Stacy 11/19/2019

clear all

*Country name and year of survey
local country "PER"
local country_name  "Peru"
local year  "2019"

*Set working directory
gl wrk_dir "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/`country_name'/`year'/Data/raw/School"



********************************************
* Create indicators and necessary variables
********************************************

************
*School data
************
cap frame create school
frame change school
*Load the school data
use "${wrk_dir}/EPDash.dta"

************
*Teacher absence
************
cap frame create teacher_absence
frame change teacher_absence
*Load the teacher_absence data
use "${wrk_dir}/questionnaire_selected.dta"


************
*Teacher questionnaire
************
cap frame create teacher_questionnaire
frame change teacher_questionnaire
*Load the teacher_questionnaire data
use "${wrk_dir}/questionnaire_roster.dta"

************
*Teacher knowledge
************
cap frame create teacher_assessment
frame change teacher_assessment
*Load the teacher_assessment data
use "${wrk_dir}/teacher_assessment_answers.dta"

************
*4th grade assessment
************
cap frame create learning
frame change learning
*Load the 4th grade assessment data
use "${wrk_dir}/fourth_grade_assessment.dta"

************
*ECD assessment
************
cap frame create ecd
frame change ecd
*Load the ecd data
use "${wrk_dir}/ecd_assessment.dta"

*****************
* Read in Sampling Info (This will need to be modified by country)
*****************
cap frame create sample
frame change sample
*read in the sample info
import delimited "${wrk_dir}/sample_frame_updated.csv"
gen school_code = ïcodigomodular
destring weights, replace force

    *correct a few missing values in weights with departamento average
    *because stratification was at the departamento level, this is accurate correction.
egen weight_dept = mean(weights), by(departamento)

replace weights=weight_dept if missing(weights)

gen school_ipw=weights*total_4th
egen school_ipw_imp= median(school_ipw)
replace school_ipw=school_ipw_imp if missing(school_ipw)

**********************************
* Create school code variable in school frame
**********************************
frame change school

gen school_code=school_code_preload
replace school_code = m1s0q2_code if missing(school_code_preload) // replace school code with enumerator manually entered school code if missing 
destring school_code, replace
*fix a few mistakenly entered school codes
replace school_code=328328 if school_code==0
replace school_code=558163 if school_code==62181

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento school_ipw, from(sample)

***********************************
* Link the data frames to the school frame (similar to merge, but keeps dataframes separate) and attach school code and sample weights
***********************************
*****
*ecd
*****
frame change ecd

frlink m:1 interview__id, frame(school)
frget school_code, from(school)

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento school_ipw, from(sample)

*****
*4th grade assessment
*****
frame change learning
frlink m:1 interview__id, frame(school)
frget school_code, from(school)

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento school_ipw, from(sample)

*****
*teacher assessment
*****
frame change teacher_assessment
frlink m:1 interview__id, frame(school)
frget school_code, from(school)

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento school_ipw, from(sample)

*****
*teacher questionnaire
*****
frame change teacher_questionnaire
frlink m:1 interview__id, frame(school)
frget school_code, from(school)

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento school_ipw, from(sample)

*****
*teacher absence
*****
frame change teacher_absence
frlink m:1 interview__id, frame(school)
frget school_code, from(school)

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento school_ipw, from(sample)


*********************************************************
* Clean data and produce indicators
*********************************************************


*********************************************************
* Teacher Absence 
*********************************************************
* School survey. Percent of teachers absent. Teacher is coded absent if they are: 
* - not in school 
* - in school but absent from the class 

frame change teacher_absence

*create info on teacher background
gen 		teacher_name = m2saq2
gen 		teacher_number=questionnaireteachcode
gen 		teacher_position = m2saq4
gen	        teacher_permanent=m2saq5==1 if !missing(m2saq5)
gen         teacher_contract=m2saq5==2 if !missing(m2saq5)
gen         teacher_temporary=m2saq5==3 if !missing(m2saq5)
gen         teacher_volunteer=m2saq5==4 if !missing(m2saq5)
gen         teacher_ngo=m2saq5==5 if !missing(m2saq5)
gen         teacher_other=m2saq5_other if m2saq5==97 

gen         teacher_fulltime=m2saq6==1 if !missing(m2saq6)
gen         teacher_male=m2saq3==1 if !missing(m2saq3)
gen         teacher_grd1=m2saq7__1==1 if !missing(m2saq7__1)
gen         teacher_grd2=m2saq7__2==1 if !missing(m2saq7__2)
gen         teacher_grd3=m2saq7__3==1 if !missing(m2saq7__3)
gen         teacher_grd4=m2saq7__4==1 if !missing(m2saq7__4)
gen         teacher_grd5=m2saq7__5==1 if !missing(m2saq7__5)
gen         teacher_language=m2saq8__1==1 if !missing(m2saq8__1)
gen         teacher_math=m2saq8__2==1 if !missing(m2saq8__2)
gen         teacher_both_subj=m2saq8__3==1 if !missing(m2saq8__3)
gen         teacher_other_subj=m2saq8__97==1 if !missing(m2saq8__97)

*Generate school absence variable
gen school_absence_rate = (m2sbq6_efft==6 | teacher_available==2 ) if !missing(m2sbq6_efft)
replace school_absence_rate=100*school_absence_rate
*generate absence variables
gen absence_rate = 100 if m2sbq6_efft==6 | m2sbq6_efft==5 |  teacher_available==2 
replace absence_rate = 0 if m2sbq6_efft==1 | m2sbq6_efft==3 | m2sbq6_efft==2 | m2sbq6_efft==4 

*generate principal absence_rate
gen principal_absence = 100 if m2sbq3_efft==8
replace principal_absence = 0 if m2sbq3_efft!=8 & !missing(m2sbq3_efft)

*Fix absence rates, where in some cases the principal is the only one they could assess for absence (1 room schools type of situation?)
replace absence_rate = principal_absence if missing(absence_rate)

frame put *, into(final_teacher_absence)
frame change final_teacher_absence

collapse school_absence_rate absence_rate school_ipw, by(school_code)
su absence_rate [aw=school_ipw]

**********************************************************
* Teacher Questionnaire
**********************************************************

frame change teacher_questionnaire

*Drop teachers who would not consent to interview
drop if m3s0q1!=1

*Create teacher background variables
gen teacher_name=m3sb_troster  
gen teacher_number=m3sb_tnumber 
gen available=m3s0q1
gen teacher_position=m3saq1
gen teacher_grd1=m3saq2__1==1 if !missing(m3saq2__1)
gen teacher_grd2=m3saq2__2==1 if !missing(m3saq2__2)
gen teacher_grd3=m3saq2__3==1 if !missing(m3saq2__3)
gen teacher_grd4=m3saq2__4==1 if !missing(m3saq2__4)
gen teacher_grd5=m3saq2__5==1 if !missing(m3saq2__5)
gen teacher_language=m3saq3__1==1 if !missing(m3saq3__1)
gen teacher_math=m3saq3__2==1 if !missing(m3saq3__2)
gen teacher_both_subj=m3saq3__3==1 if !missing(m3saq3__3)
gen teacher_other_subj=m3saq3__97==1 if !missing(m3saq3__97)
gen teacher_education=m3saq4
gen teacher_year_began=m3saq5
gen teacher_age=m3saq6

*********************************************
***** Teacher Teaching Attraction ***********
*********************************************

* In the school survey, a number of De Facto questions on teacher attraction are asked. 0.8 points is awarded for each of the following: 
*   - 0.8 Points. Teacher satisfied with job 
* - 0.8 Points. Teacher satisfied with status in community 
* - 0.8 Points. Would better teachers be promoted faster? 
*   - 0.8 Points. Do teachers receive bonuses? 
*   - 0.8 Points. One minus the fraction of months in past year with a salary delay.


