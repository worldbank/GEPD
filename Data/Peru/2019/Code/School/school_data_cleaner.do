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
import delimited "${wrk_dir}/school_sample_2019-07-22.csv"
gen school_code = codigomodular

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
frget weights rural total_1st total_4th departamento, from(sample)

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
frget weights rural total_1st total_4th departamento, from(sample)

*****
*4th grade assessment
*****
frame change learning
frlink m:1 interview__id, frame(school)
frget school_code, from(school)

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento, from(sample)

*****
*teacher assessment
*****
frame change teacher_assessment
frlink m:1 interview__id, frame(school)
frget school_code, from(school)

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento, from(sample)

*****
*teacher questionnaire
*****
frame change teacher_questionnaire
frlink m:1 interview__id, frame(school)
frget school_code, from(school)

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento, from(sample)

*****
*teacher absence
*****
frame change teacher_absence
frlink m:1 interview__id, frame(school)
frget school_code, from(school)

frlink m:1 school_code, frame(sample)
frget weights rural total_1st total_4th departamento, from(sample)


*********************************************************
* Clean data and produce indicators
*********************************************************

