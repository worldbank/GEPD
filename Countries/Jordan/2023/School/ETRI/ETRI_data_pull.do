clear all

*set the paths
gl data_dir "C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\JOR\JOR_2023_GEPD\JOR_2023_GEPD_v01_Analysis\Data\"




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
import delimited "${data_dir}sampling\GEPD_JOR_weights_2023-02-03.csv" 

keep school_code territory supervisory_authority sample_status strata ipw
destring school_code, replace force
destring ipw, replace force
duplicates drop school_code, force



******
* Merge the weights
*******
frame change school

gen school_code = school_emis_preload
destring school_code, replace force
drop if missing(school_code)

frlink m:1 school_code, frame(weights)
frget territory supervisory_authority sample_status strata ipw, from(weights)

*******
* collapse to school level
*******

*drop some unneeded info
drop enumerators*

order school_code
sort school_code
save "${data_dir}confidential\School\ETRI_school.dta" , replace



***************
***************
* Teacher File
***************
***************

frame create teachers
frame change teachers
use "${data_dir}raw\School\questionnaire_roster.dta" 


frlink m:1 interview__key interview__id, frame(school)
frget school_code territory supervisory_authority sample_status strata ipw, from(school)

order school_code
sort school_code

save "${data_dir}confidential\School\ETRI_teachers.dta" , replace


***************
***************
* Teacher G5 File
***************
***************

frame create teachers_g5
frame change teachers_g5
use "${data_dir}raw\School\etri_roster.dta" 



frlink m:1 interview__key interview__id, frame(school)
frget school_code territory supervisory_authority sample_status strata ipw, from(school)

order school_code
sort school_code

save "${data_dir}confidential\School\ETRI_G5_teachers.dta" , replace