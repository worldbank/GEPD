*Output: merged teacher file

global dir "C:\Users\wb631589\OneDrive - WBG\GEPD-Confidential\General\LEGO_Teacher_Paper\"

*do some basic names cleaning 
foreach v in teacher_absence teacher_assessment teacher_questionnaire {
	
	if "`v'" == "teacher_absence" {
		local i m2saq2
	}
	if "`v'" == "teacher_assessment" {
		local i m5sb_troster
	}
	if "`v'" == "teacher_questionnaire" {
		local i m3sb_troster
	}
	
	use "$dir/3_input_data/MDG/MDG_`v'", clear 
	
	gen `i'_original = `i' // to be able to merge with our datasets to produce indicators
	
	*clean up the name
	replace `i' = strlower(`i')
	replace `i' = strtrim(`i')
	replace `i' = strtrim(ustrregexra(`i', "\s+", " "))
	replace `i' = strtrim(ustrregexra(`i', "^[0-9]+[.,]?", ""))

	tempfile `v'
	save ``v''
}


*******
*make sure all names and ids are unique and prepare the files for the fuzzy match
use `teacher_absence', clear // 1497

drop if school_code == 726 & teacher_number == 13 // seems to be a duplicate
isid m2saq2 school_code
isid school_code teacher_number

gen idusing = _n 
gen txtusing = m2saq2

tempfile teacher_absence
save `teacher_absence'

keep idusing txtusing school_code 

tempfile teacher_absence_for_fuzzy
save `teacher_absence_for_fuzzy'

*****
use `teacher_assessment', clear // 1212

isid school_code m5sb_troster // not unique at the assessment number but leaving this as is for now

gen idmaster = _n 
gen txtmaster = m5sb_troster

tempfile teacher_asmnt
save `teacher_asmnt'

keep idmaster txtmaster school_code 

tempfile teacher_asmnt_for_fuzzy
save `teacher_asmnt_for_fuzzy'



*****
use `teacher_questionnaire', clear // 1497

isid school_code m3sb_troster

gen idmaster = _n
gen txtmaster = m3sb_troster

tempfile teacher_quest
save `teacher_quest'

keep idmaster txtmaster school_code 

isid idmaster txtmaster school_code 

tempfile teacher_quest_for_fuzzy
save `teacher_quest_for_fuzzy'

******
*Matching 

*first merge using complete match of names 

use `teacher_absence_for_fuzzy', clear

levelsof school_code, local(schools) 

*preperare individual school level files for the roster
foreach v in `schools' {
	preserve
		keep if school_code == `v'
		
		tempfile teacher_absence_`v'
		save `teacher_absence_`v''
	restore
}

*do the same process for 
foreach v in `schools' {
	foreach i in teacher_asmnt teacher_quest {
		preserve
			use ``i'_for_fuzzy', clear
			keep if school_code == `v'
			
			tempfile `i'_`v'
			save ``i'_`v''
		restore
	}
}

*now match the all individual teacher files. for each of the files, keep the one with the maximum score 
foreach v in `schools' {
	*foreach i in teacher_asmnt teacher_quest {
	foreach i in teacher_asmnt  {

		preserve

			use ``i'_`v'', clear
			matchit idmaster txtmaster using `teacher_absence_`v'', idusing(idusing) txtusing(txtusing)
			
			*drop the ones below 0.75
			*qui drop if similscore < 0.7
			qui count 
			
			if `r(N)' != 0 {
				bysort idmaster txtmaster: egen double max = max(similscore)
				keep if max == similscore
			
				gen school_code = `v'
			}
			
			tempfile match_`i'_`v'
			save `match_`i'_`v''
		
		restore
	}
}

*now append them all
local k = 1

*foreach i in teacher_quest teacher_asmnt {
	foreach i in teacher_asmnt  {

	local k = 1
	
		foreach v in `schools' {
		
			if `k' == 1 {
				use `match_`i'_`v'', clear
			}
			if `k' != 1 {
				append using `match_`i'_`v''
			}
			
			local k = `k' + 1
		}
	
	di "`i'"
	count
	
	bysort school_code txtusing idusing: egen double max_using = max(similscore)
	*just making sure that there is only one roster entry being matched to the assessment 
	keep if max_using == similscore
	
	isid school_code txtmaster idmaster // only one name from each teacher dataset matched
	isid school_code txtusing idusing // only one name from roster matched
	
	tempfile append_`i'
	save `append_`i''
}


*now merge each of these files with the original dataset
*foreach i in teacher_asmnt teacher_quest {
	foreach i in teacher_asmnt  {

	*preserve
		use ``i'', clear
		
		merge 1:m idmaster txtmaster school_code using `append_`i'', gen(merge_`i')
		
		replace idusing = idmaster if idusing == .
		replace txtusing = txtmaster if txtusing == ""
		
		foreach v in idmaster txtmaster similscore {
			rename `v' `v'_`i'
		}
		
		drop max
		
		tempfile merged_`i'
		save `merged_`i''
	*restore
}

X:

*now merge them all together 
use `teacher_absence', clear

merge 1:1 idusing txtusing school_code using `merged_teacher_quest', gen(absence_teacher_quest)

merge 1:1 idusing txtusing school_code using `merged_teacher_asmnt', gen(absence_teacher_asmnt)

drop similscore* txtmaster* txtusing idmaster* idusing* absence* merge*

save "$dir/5_output_data/MDG/MDG_teacher_level_updated.dta", replace 
