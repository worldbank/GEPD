
/*******************************************************************************
 File : mz_educ_qc_m1.do
 Overview : performs standard checks on the Mozambique SDI education module data
	
 Update History:
 Vsn   mm/dd/yyyy Person Change
 ----- ---------- ------ ------
 2.001 08/25/2014 EzeM   -Excel output to Sections O, 1A, 1B, 1C, 1D
 2.002 09/13/2014 EzeM   -Add controls for day of the week the survey was done
 3.002 10/13/2014 EzeM   -Change paths on directories
*******************************************************************************/

clear

set more off

cd "C:\Users\WB317160\Documents\Current Stuff\SDI\SDI Work Program\Country SDI\Mozambique SDI\SCHOOLS AND FACILITIES\Schools\STATA"

cap program drop listExcel
cap program drop setExcelSheet
adopath + "./Do Files"
global excelFile = "$qcloc/checks_m1.xls"
cap rm "$excelFile"

global qcloc = "C:\Users\WB317160\Documents\Current Stuff\SDI\SDI Work Program\Country SDI\Mozambique SDI\SCHOOLS AND FACILITIES\Schools\STATA\qreport"
global mdloc = "C:\Users\WB317160\Documents\Current Stuff\SDI\SDI Work Program\Country SDI\Mozambique SDI\SCHOOLS AND FACILITIES\Schools\STATA\modules"
cap log close dataChecks_m1
log using "$qcloc/dataChecks_m1", replace text name(dataChecks_m1)


/*******************************************************************************
 MODULE 0: Correct Schools
*******************************************************************************/

setExcelSheet "Schools"
use "$qcloc/m1", clear
//check that we go to the right schools (compare to those sampled)
// Merge Sample Frame and Sampled Facilities
use "$mdloc/mz_edu_weights", clear
sort codigo
capture drop _merge
merge 1:1 codigo using "$mdloc/sample_school_sid", gen(_merge_weights)
tab  _merge_weights
merge 1:1 codigo using "$mdloc/info_weights", gen(_merge_info_weights)
tab  _merge_info_weights
drop _merge*
ren nomeesc orig_name
ren provincia orig_province
ren distrito orig_distrito
ren grade4 orig_n_students
ren pov_prov orig_pov_prov
ren cl4medPR orig_cl4medPR
ren sumofprof_hm orig_n_teachers
ren SizeStr orig_size
ren RuralStr orig_rural
ren schoolid school
*replace school = 90530 if codigo==90530
sort school
save "$qcloc/mz_school_sample_frame", replace

use "$qcloc/m1", clear

gen school=schid
*replace school=m1q6 if schid!=m1q6
*replace school=101061 if m1q5a=="Escola Primaria do Mosquito"
merge 1:1 school using "$qcloc/mz_school_sample_frame"
tab _merge

//check that all schools in the dataset are part of the sample frame
listExcel school m1q5a m1q5b m1q6 m1q3 m1q3a if _merge==1,  ///
	desc("This school was not part of the sample frame")
//check that all schools in the dataset are part of the selected sample
listExcel school m1q5a m1q5b m1q6 m1q3 m1q3a codigo orig_name orig_province ///
 last_replacement last_sampled if (_merge==1|_merge==3) & last_replacement2!=1,   ///
	desc("This school is in the dataset but was not part of the selected sample")
//check that all schools in the sample are in the dataset
/*Last replacement is a variable equal to one if the school was selected for the final sample
Those for which we use a replacement, the replacement, rather than the selected school are
equal to one*/
listExcel school codigo orig_name orig_province if _merge==2 & last_replacement2==1, ///
	desc("This school was sampled but it not part of the dataset")



/*******************************************************************************
 MODULE 1: Consent
*******************************************************************************/
setExcelSheet "Consent"
use "$qcloc/m1", clear
// check that reasons are given only if there is a refusal
listExcel schid m1siq1 m1siq2 if m1siq2~="" & m1siq1==1, ///
	desc("A reason should only be given if there is a refusal")
// check for duplicate facility ids
quietly bysort schid: gen dup = cond(_N==1,0,_n)
listExcel schid if dup==1, ///
	desc("These facility IDs appear more than once in the consent data")
drop dup


/*******************************************************************************
 MODULE 1: Section O: Cover Sheet
*******************************************************************************/
setExcelSheet "Section 0"
use "$qcloc/m1", clear
// check for missing States
listExcel schid m1q3 m1q3a if m1q3==. |m1q3a==. , ///
	desc("These schools have missing Provinces or States")
// check for missing Government Areas
listExcel schid m1q4 if m1q4==., ///
	desc("These schools have missing Government Areas")
// check for missing school names
listExcel schid m1q5a if m1q5a=="", ///
	desc("These schools have missing names")
// check for duplicate school names
quietly bysort m1q5a: gen dup = cond(_N==1,0,_n)
listExcel schid m1q5a if dup~=0 & m1q5a~="", ///
	desc("These schools have the same name")
drop dup
// check for missing school code
listExcel schid m1q5b if m1q5b==., ///
	desc("These schools have missing school codes")
// check for duplicate school code
quietly bysort m1q5b: gen dup = cond(_N==1,0,_n)
listExcel schid m1q5b if dup~=0 & m1q5b~=., ///
	desc("These schools have the same school id")
drop dup
// check for missing EMIS codes
listExcel schid m1q6 if m1q6==., ///
	desc("These schools have missing EMIS schools codes")
// check for duplicate EMIS school code
quietly bysort m1q6: gen dup = cond(_N==1,0,_n)
listExcel schid m1q6 if dup~=0 & m1q6~=., ///
	desc("These schools have the same school EMIS id")
drop dup
// check for missing GPS information
listExcel schid m1q7utm m1q7lat m1q7lon if m1q7utm==.| m1q7lat==.| m1q7lon==., ///
	desc("These schools have missing GPS information")
// check for missing dates
listExcel schid m1q8d m1q8m m1q8y if m1q8d==.| m1q8m==. | m1q8y==.,   /// first visit
	desc("The date of the first visit is missing")
listExcel schid m1q9d m1q9m m1q9y if m1q9d==.| m1q9m==. | m1q9y==.,  /// second visit
	desc("The date of the second visit is missing")

// check first visit date variables for consistency
qui gen fst_visit=mdy(m1q8m,m1q8d,m1q8y)
qui gen sec_visit=mdy(m1q9m,m1q9d,m1q9y)
listExcel schid m1q8* if m1q8d>31 | ///
	m1q8m>12 | ///
	m1q8y!=2014, ///
	desc("The first visit date appears to be recorded incorrectly")
	
// check second visit date variables for consistency
listExcel schid m1q9* if m1q9d>31 | ///
	m1q9m>12 | ///
	m1q9y!=2014, ///
	desc("The second visit date appears to be recorded incorrectly")

// check that the 2 visits were at least 2 days apart
format fst_visit sec_visit %d
listExcel schid fst_visit sec_visit if (sec_visit-fst_visit)<=2, ///
	desc("The 2 visit dates should be at leas 2 days apart")
*drop fst_visit sec_visit

//check second visit date for patterns: second visit must be on a separate day after the first visit
// and should not be concentrated around one day of the week
qui gen dow=dow(sec_visit)
qui gen dow2=dow(fst_visit)
tab dow
tab dow2
qui gen monday=(dow==1)/(dow)
qui gen tuesday=(dow==2)/(dow)
qui gen wednesday=(dow==3)/(dow)
qui gen thursday=(dow==4)/(dow)
qui gen friday=(dow==5)/(dow)

// check whether visits were done on Saturday or Sunday
listExcel schid fst_visit sec_visit dow if dow==0|dow==6|dow2==0|dow2==6, ///
	desc("One of the visits was done on Saturday (dow=6) or Sunday(dow=0)")


//check second visit time for patters: second visits times should be more or less evenly distributed around the school day
tostring m1q11h m1q11m m1q12h m1q12m m1q13h m1q13m m1q14h m1q14m, replace
foreach var of varlist m1q11h m1q11m m1q12h m1q12m m1q13h m1q13m m1q14h m1q14m  {
replace  `var' = "0" +  `var'  if length(`var' ) == 1
}

qui egen arrive=concat(m1q13h m1q13m )
qui gen  arrive_sec_visit= clock( arrive, "hm")
qui format arrive_sec_visit %tcHHMM
gen morning=cond( arrive_sec_visit < tc(11:00:00),1,0)
gen noon=cond( arrive_sec_visit <= tc(14:00:00) & arrive_sec_visit >= tc(11:00:00),1,0)
gen afternoon=cond( arrive_sec_visit > tc(14:00:00)  ,1,0)
gen hod=1 if morning==1
replace hod=2 if noon==1
replace hod=3 if afternoon==1
qui la var hod "Hour of the Day (Morning, Noon and Afternoon)"
tab hod


/*******************************************************************************
 MODULE 1: Section A-B: General Information
*******************************************************************************/
*setExcelSheet "Section 1B"


/*******************************************************************************
 MODULE 1: Section C: Infrastructure
*******************************************************************************/
setExcelSheet "Section 1C"
use "$qcloc/m1", clear

// check missing values in the toilets
listExcel schid m1scq1 if m1scq1==., ///
	desc("Missing values for the toilet variable")

/*******************************************************************************
 MODULE 1: Section D: Compute Duration of Teaching Day
*******************************************************************************/
setExcelSheet "Section 1D"
use "$qcloc/m1", clear

// Duration of Teaching per day
collapse (mean)  m1sdq7_1sh_2 m1sdq7_1sm_2  m1sdq7_1eh_2 m1sdq7_1em_2  m1sdq7_2sh_2 m1sdq7_2sm_2 ///
m1sdq7_2eh_2 m1sdq7_2em_2 m1sdq7_3sh_2 m1sdq7_3sm_2  m1sdq7_3eh_2 m1sdq7_3em_2 m1sdq7_1bh_2 m1sdq7_1bm_2 ///
m1sdq7_2bh_2 m1sdq7_2bm_2 m1sdq7_3bh_2 m1sdq7_3bm_2, by(schid)
tostring m1sdq7_1sh_2 m1sdq7_1sm_2  m1sdq7_1eh_2 m1sdq7_1em_2  m1sdq7_2sh_2 m1sdq7_2sm_2  m1sdq7_2eh_2 m1sdq7_2em_2 ///
m1sdq7_3sh_2 m1sdq7_3sm_2  m1sdq7_3eh_2 m1sdq7_3em_2 m1sdq7_1bh_2 m1sdq7_1bm_2 ///
m1sdq7_2bh_2 m1sdq7_2bm_2 m1sdq7_3bh_2 m1sdq7_3bm_2, replace force
foreach var of varlist m1sdq7_1sh_2  m1sdq7_1eh_2  m1sdq7_2sh_2   m1sdq7_2eh_2 m1sdq7_3sh_2 ///
m1sdq7_3eh_2 m1sdq7_1bh_2 m1sdq7_2bh_2 m1sdq7_3bh_2 {
replace  `var' = "0" +  `var'  if length(`var' ) == 1
}
foreach var of varlist m1sdq7_1sm_2  m1sdq7_1em_2  m1sdq7_2sm_2   m1sdq7_2em_2 m1sdq7_3sm_2 ///
m1sdq7_3em_2 m1sdq7_1bm_2 m1sdq7_2bm_2 m1sdq7_3bm_2 {
replace  `var' = "0" +  `var'  if length(`var' ) == 1
}

egen start1=concat(m1sdq7_1sh_2 m1sdq7_1sm_2  )
gen  starts1= clock( start1, "hm")
format starts1 %tcHHMM
egen start2=concat(m1sdq7_2sh_2 m1sdq7_2sm_2  )
gen  starts2= clock( start2, "hm")
format starts2 %tcHHMM
egen start3=concat(m1sdq7_3sh_2 m1sdq7_3sm_2  )
gen  starts3= clock( start3, "hm")
format starts3 %tcHHMM

egen end1=concat(m1sdq7_1eh_2 m1sdq7_1em_2  )
gen  ends1= clock( end1, "hm")
format ends1 %tcHHMM
egen end2=concat(m1sdq7_2eh_2 m1sdq7_2em_2  )
gen  ends2= clock( end2, "hm")
format ends2 %tcHHMM
egen end3=concat(m1sdq7_3eh_2 m1sdq7_3em_2  )
gen  ends3= clock( end3, "hm")
format ends3 %tcHHMM

egen break1=concat(m1sdq7_1bh_2 m1sdq7_1bm_2  )
gen  breaks1= clock( break1, "hm")
replace break1="1.00" if break1=="0.60"
//Note: The clock command does not work when we have exactly 60 mins
//Note: I have to adjust it so not to have a missing value. This is due to how the clock command works.
format breaks1 %tcHHMM
egen break2=concat(m1sdq7_2bh_2 m1sdq7_2bm_2  )
replace break2="1.00" if break2=="0.60"
//Note: I have to adjust it so not to have a missing value. This is due to how the clock command works.
gen  breaks2= clock( break2, "hm")
format breaks2 %tcHHMM
egen break3=concat(m1sdq7_3bh_2 m1sdq7_3bm_2  )
gen  breaks3= clock( break3, "hm")
replace break3="1.00" if break3=="0.60"
//Note: I have to adjust it so not to have a missing value. This is due to how the clock command works.
format breaks3 %tcHHMM

gen double schedule_shift1 =  minutes(ends1 - starts1 - breaks1)
replace  schedule_shift1=. if schedule_shift1==0
gen double schedule_shift2 =  minutes(ends2 - starts2 - breaks2)
replace schedule_shift2=. if schedule_shift2==0
gen double schedule_shift3 =  minutes(ends3 - starts3- breaks3)
replace schedule_shift3=. if schedule_shift3==0
gen double schedule_tot =  minutes(ends1 - starts1- breaks1) + minutes(ends2 - starts2- breaks2) + minutes(ends3 - starts3- breaks3)
egen school_duration = rowmean(schedule_shift1  schedule_shift2  schedule_shift3)
lab var school_duration "Schedule Teaching per day (4th grade class)"

save "$mdloc/m1_schoold", replace

collapse (mean)  school_duration (lastnm) starts1-breaks3 , by(schid)
// check if missing school duration
listExcel schid starts1-breaks3 if school_duration==., ///
	desc("Missing values for school duration")

// check if school duration is lees than 120 mins
listExcel schid starts1-breaks3 school_duration if school_duration<=120, ///
	desc("School duration last less than 120 mins")

use "$mdloc/m1", clear
// check incosistencies in school days I
listExcel schid m1sdq7a m1sdq8 if 190-m1sdq7a<m1sdq8, ///
	desc("Incosistent number of schools days in a given year I")
// Note: Quantos dias de escola foram canceladas em 2013? in questionaire but label says 2012. This error is also in another places

// check incosistencies in school days II
qui gen fst_tri_b=mdy(m1sdq9aom,m1sdq9aod,m1sdq9aoy)
qui gen fst_trim_e=mdy(m1sdq9acm,m1sdq9acd,m1sdq9acy)
qui gen scn_tri_b=mdy(m1sdq9bom,m1sdq9bod,m1sdq9boy)
qui gen scn_trim_e=mdy(m1sdq9bcm,m1sdq9bcd,m1sdq9bcy)
qui gen trd_tri_b=mdy(m1sdq9com,m1sdq9cod,m1sdq9coy)
qui gen trd_trim_e=mdy(m1sdq9ccm,m1sdq9ccd,m1sdq9ccy)
qui gen fst_trim=fst_trim_e-fst_tri_b
qui gen scn_trim=scn_trim_e-scn_tri_b
qui gen trd_trim=trd_trim_e-trd_tri_b
qui gen all_trim=fst_trim+ scn_trim + trd_trim
qui workdays fst_tri_b fst_trim_e  , gen(fst_trim_w)
qui workdays scn_tri_b scn_trim_e , gen(scn_trim_w)
qui workdays trd_tri_b trd_trim_e  , gen(trd_trim_w)
qui gen all_trim_w=fst_trim_w + scn_trim_w + trd_trim_w

listExcel schid m1sdq7a m1sdq8 all_trim all_trim_w if all_trim_w<m1sdq8-m1sdq7a, ///
	desc("Incosistent number of schools days in a given year II")



cap log close dataChecks

exit
