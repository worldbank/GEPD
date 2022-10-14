clear all

*set the paths
gl data_dir "C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\SLE\SLE_2022_GEPD\SLE_2022_GEPD_v01_Analysis\Data\"




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

keep school_code sch_owner idregion iddistrict accessibility urban_rural ipw
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
frget sch_owner idregion iddistrict accessibility urban_rural ipw, from(weights)

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
frget school_code sch_owner idregion iddistrict accessibility urban_rural ipw, from(school)

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
frget school_code sch_owner idregion iddistrict accessibility urban_rural ipw, from(school)

order school_code
sort school_code

save "${data_dir}confidential\School\ETRI_G5_teachers.dta" , replace