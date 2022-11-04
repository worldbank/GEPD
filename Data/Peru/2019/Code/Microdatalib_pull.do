clear all

*set the paths
gl data_dir "C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\SLE\SLE_2022_GEPD\SLE_2022_GEPD_v01_Analysis\Data\"
gl save_dir



***************
***************
* School File
***************
***************

********
*read in the raw school file
********
frame create school
frame change school

use "${data_dir}raw\School\EPDash.dta" 

********
*read in the school weights
********

frame create weights
frame change weights
import delimited "${data_dir}sampling\school_weights_revised_2022-10-07.csv" 

gen school_emis_preload=idemis_code 
gen school_code=idemis_code
*create urban rural status
gen urban_rural="Rural"
replace urban_rural = "Urban" if accessibility=="Easily accessible"

gen private="Private"
replace private="Public" if sch_owner=="Government" | sch_owner=="Community"

keep school_code sch_owner idregion iddistrict accessibility school_districthq_distance  urban_rural  private ipw
destring school_code, replace force
destring ipw, replace force
duplicates drop school_code, force

********
*read in some fixes to the school codes
********
frame create emis_fixes
frame change emis_fixes

import delimited  "${data_dir}raw\School\emis_fixes.csv" 

keep interview__key interview__id school_code


********
*Merge emis fixes to school data
********

frame change school


frlink m:1 interview__key interview__id,  frame(emis_fixes)
frget school_code, from(emis_fixes)

drop if school_code=="Blank interview"
destring school_code, replace

******
* Merge the weights
*******

frlink m:1 school_code, frame(weights)
frget sch_owner idregion iddistrict accessibility school_districthq_distance  urban_rural  private ipw, from(weights)

*******
* collapse to school level
*******

*drop some unneeded info
drop name*
drop m6_teacher_name
drop m8_teacher_name
drop enumerators*
drop m2saq2*
drop m1saq1*
drop *phone*
drop m1saq2*
drop m7sb_troster*
drop m3sb_troster*
drop m3sb_etri*
drop m5sb_troster*
drop m6s1q1*
drop m4saq1
drop m8s1q1*
drop m4saq1_g2*
drop comments*
drop m1s0q1_name m1s0q1_name_other

order school_code
sort school_code
save "${save_dir}confidential\School\microdata\school_micro.dta" , replace



***************
***************
* Teacher Questionnaire File
***************
***************

frame create teachers
frame change teachers
use "${data_dir}raw\School\questionnaire_roster.dta" 

frlink m:1 interview__key interview__id, frame(school)
frget school_code sch_owner idregion iddistrict accessibility school_districthq_distance  urban_rural  private ipw, from(school)

*drop some unneeded info
drop m3sb_troster

order school_code
sort school_code

save "${save_dir}confidential\School\microdata\teacher_questionnaire_micro.dta" , replace

***************
***************
* Teacher Absence File
***************
***************

frame create absence
frame change absence
use "${data_dir}raw\School\TEACHERS.dta" 

frlink m:1 interview__key interview__id, frame(school)
frget school_code sch_owner idregion iddistrict accessibility school_districthq_distance  urban_rural  private ipw, from(school)

*drop some unneeded info
drop m2saq2

order school_code
sort school_code

save "${save_dir}confidential\School\microdata\teacher_absence_micro.dta" , replace


***************
***************
* Teacher Assessment File
***************
***************

frame create teacher_assessment
frame change teacher_assessment
use "${data_dir}raw\School\teacher_assessment_answers.dta" 

frlink m:1 interview__key interview__id, frame(school)
frget school_code sch_owner idregion iddistrict accessibility school_districthq_distance  urban_rural  private ipw, from(school)

*drop some unneeded info
drop m5sb_troster

order school_code
sort school_code

save "${save_dir}confidential\School\microdata\teacher_assessment_micro.dta" , replace




***************
***************
* Teacher Pedagogy File
***************
***************

frame create teachers_pedg
frame change teachers_pedg
import delimited "${data_dir}raw\School\teach_score_raw.csv" 

frlink m:1 school_code, frame(weights)
frget sch_owner idregion iddistrict accessibility school_districthq_distance  urban_rural  private ipw, from(weights)

order school_code
sort school_code




save "${save_dir}confidential\School\microdata\teacher_pedgagogy_micro.dta" , replace

***************
***************
* 4th grade file
***************
***************

frame create g4_assess
frame change g4_assess
use "${data_dir}raw\School\fourth_grade_assessment.dta" 

frlink m:1 interview__key interview__id, frame(school)
frget school_code sch_owner idregion iddistrict accessibility school_districthq_distance  urban_rural  private ipw, from(school)

order school_code
sort school_code

*drop some unneeded info
drop m8s1q1

save "${data_dir}confidential\School\microdata\fourth_grade_assessment_micro.dta" , replace

***************
***************
* 1st grade file
***************
***************

frame create g1_assess
frame change g1_assess
use "${data_dir}raw\School\ecd_assessment.dta" 

frlink m:1 interview__key interview__id, frame(school)
frget school_code sch_owner idregion iddistrict accessibility school_districthq_distance  urban_rural  private ipw, from(school)

order school_code
sort school_code

*drop some unneeded info
drop m6s1q1
drop notekid

save "${data_dir}confidential\School\microdata\first_grade_assessment_micro.dta" , replace

