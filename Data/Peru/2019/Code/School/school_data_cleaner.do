*Clean data files for GEPD school indicators
*Written by Brian Stacy 11/19/2019

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
