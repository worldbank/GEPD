  * Author: Ahmed Raza
  * Date: 22nd November, 2022
  * Purpose: Setting up data import pipeline:
 
********************************************************************************  
  
  *-----------------------------------------------------------------------------
  * 1) General program setup
  *-----------------------------------------------------------------------------
  clear               all
  capture log         close _all
  set more            off
  set varabbrev       off, permanently
  set emptycells      drop
  set maxvar          12000
  version             16

  
  *-----------------------------------------------------------------------------
  * 2) Define user-independant project paths:
  *-----------------------------------------------------------------------------

* Ahmed Raza 

  if inlist("`c(username)'","wb549384") {
    global folder "C:/Users/`c(username)'/WBG/Ezequiel Molina - Pakistan_all"
}

* Add in more users:
  
  if inlist("`c(username)'","<>", "<>") {
    global folder "C:/Users/`c(username)'/WBG/Ezequiel Molina - Pakistan_all"
}

  *-----------------------------------------------------------------------------
  * 3)  Set pathways:
  *-----------------------------------------------------------------------------

  * Scool survey final complete:
  
  use "$folder\2021\Data\02. Provinces\ICT\clean\final_complete_school_data.dta", clear
  
  clonevar emis_code_clean = school_emis_preload
  drop school_emis_preload  school_code_preload
  
  tempfile school_survey
  save `school_survey', replace
 
 
********************************************************************************  
  * AIM ECD data:
********************************************************************************
  
  use "$folder\2021\Data\02. Provinces\ICT\raw\aim_ecd\AIM_ECD_PAK.dta", clear
  
  * Generate clean emis_code variable: 
  
   clonevar emis_code_clean = school_emis_preload
   replace emis_code_clean = m1s0q2_emis if emis_code_clean == "Blank interview"
   
   drop school_emis_preload  school_code_preload
   
   destring emis_code_clean, replace

   
   merge 1:1 emis_code_clean using `school_survey'
   
   * Note : No merge errors found:
   tab _merge 
   drop _merge

   
********************************************************************************
 
 /*  
   * codebook total_enrolled student_number student_age student_male student_proficient student_proficient_70 student_proficient_75 literacy_student_proficient literacy_student_proficient_70 literacy_student_proficient_75 math_student_proficient math_student_proficient_70 math_student_proficient_75 student_knowledge math_student_knowledge literacy_student_knowledge, compact
   
   tabstat total_enrolled student_number student_age student_male student_proficient student_proficient_70 student_proficient_75 literacy_student_proficient literacy_student_proficient_70 literacy_student_proficient_75 math_student_proficient math_student_proficient_70 math_student_proficient_75 student_knowledge math_student_knowledge literacy_student_knowledge, by(foreign) stat(mean sd min max)

 */
 
********************************************************************************


/*  
 
	 use "$folder\2021\Data\02. Provinces\ICT\raw\aim_ecd\ecd_assessment.dta", clear
	 isid interview__key ecd_assessment__id
*/
	
	
	drop enumerators_preload*
  
  

  tempfile aim_ecd
  save `aim_ecd', replace
  
  
******************************************************************************** 
*  ECD assessment data:
********************************************************************************
  
  merge 1:m interview__id using "$folder\2021\Data\02. Provinces\ICT\raw\aim_ecd\ecd_assessment.dta"
  
  
  save "$folder\2021\Data\02. Provinces\ICT\clean\aim_ecd.dta", replace
  
  
********************************************************************************
  ta _merge
  
  list ecd_teacher_name ecd_class_count ecd_instruction_time m6s1q1__0 has__errors assignment__id m1s0q1_comments comments if _merge ==1 

  ta _merge
list ecd_teacher_name ecd_class_count ecd_instruction_time m6s1q1__0 has__errors assignment__id m1s0q1_comments comments if _merge ==1 
ta assent 
ta _merge if assent ==1

  
  
  
  
  exit
  
  
  