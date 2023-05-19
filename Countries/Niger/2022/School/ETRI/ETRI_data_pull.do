clear all

*set the paths
gl data_dir "C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\NER\NER_2022_GEPD\NER_2022_GEPD_v01_Analysis\Data\"




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

use "${data_dir}raw\School\INFRA\EPDash_STATA_Clean.dta" 

drop region

********
*read in the school weights
********

frame create weights
frame change weights
import delimited "${data_dir}sampling\school_weights_revised_2022-10-24.csv" 

gen school_emis_preload=code_etablissement 
gen school_code=code_etablissement

keep school_code region urban_rural lire urban_rural  ownership ipw
destring school_code, replace force
destring ipw, replace force
duplicates drop school_code, force



******
* Merge the weights
*******
frame change school

destring school_code, replace

frlink m:1 school_code, frame(weights)
frget region urban_rural lire urban_rural urban_rural  ownership ipw, from(weights)

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
use "${data_dir}raw\School\INFRA\questionnaire_roster_Clean.dta" 

drop school_name_preload school_province_preload school_district_preload school_emis_preload m1s0q2_name m1s0q2_code m1s0q2_emis school_code region

frlink m:1 interview__key interview__id, frame(school)
frget school_code region urban_rural lire  urban_rural  ownership ipw, from(school)

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
use "${data_dir}raw\School\INFRA\etri_roster_Clean.dta" 

drop school_name_preload school_province_preload school_district_preload school_emis_preload m1s0q2_name m1s0q2_code m1s0q2_emis school_code region


frlink m:1 interview__key interview__id, frame(school)
frget school_code region urban_rural lire  urban_rural  ownership ipw, from(school)

order school_code
sort school_code

save "${data_dir}confidential\School\ETRI_G5_teachers.dta" , replace