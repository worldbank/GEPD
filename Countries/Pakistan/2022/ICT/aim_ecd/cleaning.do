* Purpose: Clean and Aggregate AIM-ECD data from the GEPD pilot in ICT
* Author: Ahmed Raza
* Date: 19th Sept, 2022

include setup.do
	
	
* Supervisor approved datasets:

*v1:

********************************************************************************
use "$raw\aim_ecd\v1\supervisor_approved\AIM_ECD_PAK.dta", clear

gen approve = "Approved by Supervisor"
gen version = 1

tempfile AIM_ECD_Sup_v1
save `AIM_ECD_Sup_v1', replace

********************************************************************************
	
use "$raw\aim_ecd\v1\hq_approved\AIM_ECD_PAK.dta"

gen approve = "Approved by HQ"
gen version = 1

tempfile AIM_ECD_Hq_v1
save `AIM_ECD_Hq_v1', replace
	
********************************************************************************

use "$raw\aim_ecd\v1\supervisor_approved\ecd_assessment.dta", clear

gen approve = "Approved by Supervisor"
gen version = 1

tempfile ecd_assessment_Sup_v1
save `ecd_assessment_Sup_v1', replace

use "$raw\aim_ecd\v1\hq_approved\ecd_assessment.dta", clear

gen approve = "Approved by HQ"
gen version = 1

tempfile ecd_assessment_Hq_v1
save `ecd_assessment_Hq_v1', replace


********************************************************************************


	
*v2:
	
use "$raw\aim_ecd\v2\supervisor_approved\AIM_ECD_PAK.dta", clear

gen approve = "Approved by Supervisor"
gen version =2

tempfile AIM_ECD_Sup_v2
save `AIM_ECD_Sup_v2', replace

********************************************************************************

use "$raw\aim_ecd\v2\hq_approved\AIM_ECD_PAK.dta"

gen approve = "Approved by HQ"
gen version = 2

tempfile AIM_ECD_Hq_v2
save `AIM_ECD_Hq_v2', replace


********************************************************************************



use "$raw\aim_ecd\v2\supervisor_approved\ecd_assessment.dta", clear

gen approve = "Approved by Supervisor"
gen version = 2

tempfile ecd_assessment_Sup_v2
save `ecd_assessment_Sup_v2', replace

use "$raw\aim_ecd\v2\hq_approved\ecd_assessment.dta", clear

gen approve = "Approved by HQ"
gen version = 2

tempfile ecd_assessment_Hq_v2
save `ecd_assessment_Hq_v2', replace



********************************************************************************

* APPEND AIM_ECD_HQ

use `AIM_ECD_Hq_v1', clear

append using `AIM_ECD_Hq_v2'
append using `AIM_ECD_Sup_v1'
append using `AIM_ECD_Sup_v2'



* Clean emiscode 

gen emis_code = school_emis_preload 
replace emis_code = m1s0q2_emis if emis_code == ""

tab emis_code 

* Master School level dataset:

* isid emis_code   // should be unique but not due blank forms entered without emiscode 

save "$out/aim_ecd/AIM_ECD_PAK_master.dta", replace

********************************************************************************

* APPEND ecd_assessment 


use `ecd_assessment_Sup_v1', clear
append using `ecd_assessment_Sup_v1'
append using `ecd_assessment_Hq_v1'
append using `ecd_assessment_Hq_v2'


/*
* Clean emiscode - Add EMISCODE to this file: 

gen emis_code = school_emis_preload 
replace emis_code = m1s0q2_emis if emis_code == ""

tab emis_code 

*/


* Master School level dataset:

* isid emis_code   // should be added and should be unique  

save "$out/aim_ecd/ecd_assessment_master.dta", replace

********************************************************************************

* Create Master AIM Data 

use "$out/aim_ecd/ecd_assessment_master.dta", clear

merge m:1 interview__id using "$out/aim_ecd/AIM_ECD_PAK_master.dta"


tab emis_code _merge 


save "$out/aim_ecd/aim_ecd_master.dta", replace


exit

