clear all

*set the paths
gl data_dir "C:\Users\wb577189\OneDrive - WBG\GEPD-Confidential\CNT\JOR\JOR_2023_GEPD\JOR_2023_GEPD_v01_Analysis\Data\"




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

*drop region

********
*read in the school weights
********

frame create weights
frame change weights
import delimited "${data_dir}sampling\GEPD_JOR_weights_2023-02-03.csv" 

gen school_emis_preload=school_code 
*gen school_code=school_code

keep school_code directorate governorate schoolstatus property ipw areaclassification supervisory_authority
destring school_code, replace force
destring ipw, replace force
duplicates drop school_code, force

 

******
* Merge the weights
*******
frame change school

keep if   regexm(school_emis_preload, "^[0-9]")

rename school_emis_preload school_code
destring school_code, replace



frlink m:1 school_code, frame(weights)
frget directorate governorate schoolstatus property ipw areaclassification supervisory_authority, from(weights)

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

*drop school_name_preload school_province_preload school_district_preload school_emis_preload m1s0q2_name m1s0q2_code m1s0q2_emis school_code region

frlink m:1 interview__key interview__id, frame(school)
frget school_code directorate governorate schoolstatus property ipw areaclassification supervisory_authority, from(school)

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

*drop school_name_preload school_province_preload school_district_preload school_emis_preload m1s0q2_name m1s0q2_code m1s0q2_emis school_code region


frlink m:1 interview__key interview__id, frame(school)
frget school_code directorate governorate schoolstatus property ipw areaclassification supervisory_authority, from(school)

order school_code
sort school_code

save "${data_dir}confidential\School\ETRI_G5_teachers.dta" , replace