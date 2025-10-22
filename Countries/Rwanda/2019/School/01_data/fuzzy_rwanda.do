*Output: merged teacher file

global dir "C:\Users\wb631589\OneDrive - WBG\GEPD-Confidential\General\LEGO_Teacher_Paper\"

*do some basic names cleaning 
foreach v in teacher_absence teacher_assessment teacher_pedagogy teacher_questionnaire {
	
	if "`v'" == "teacher_absence" {
		local i m2saq2
	}
	if "`v'" == "teacher_assessment" {
		local i m5sb_troster
	}
	if "`v'" == "teacher_pedagogy" {
		local i m4saq1
	}
	if "`v'" == "teacher_questionnaire" {
		local i m3sb_troster
	}
	
	use "$dir/3_input_data/RWA/RWA_`v'", clear 
	
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
use `teacher_absence', clear // 1586

*not sure if they are the same teacher or not but given that they have slightly different entries assume they are different 
replace m2saq2 = "NIZEYIMANA FELICIEN1" if teachers_id == 6 & school_code == 340306

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
use `teacher_assessment', clear // 948

*one teacher took both tests, combine them 
foreach v of varlist *number *geometric *data math_length math_content_knowledge arithmetic* geometry* interpret* {
    sort school_code m5sb_troster typetest
    replace `v' = `v'[_n-1] if missing(`v') & !missing(`v'[_n-1]) ///
        & school_code == 571308 & m5sb_troster == "callixte mutwarasibo"
}

drop if school_code == 571308 & m5sb_troster == "callixte mutwarasibo" & typetest == 1

*changing some names to facilitate the merge with the roster since they seem to be the best matches out of everything available
replace m5sb_troster = "uwitonze berthe" if m5sb_troster == "uwayisenga belthe" & school_code == 270203
replace m5sb_troster = "phocas nikwigize" if m5sb_troster == "twizerimana phocas" & school_code == 440710
replace m5sb_troster = "nsabimana donatien" if m5sb_troster == "nsabimana jean de dieu" & school_code == 510704

isid school_code m5sb_troster // not unique at the assessment number but leaving this as is for now

gen idmaster = _n 
gen txtmaster = m5sb_troster

tempfile teacher_asmnt
save `teacher_asmnt'

keep idmaster txtmaster school_code 

tempfile teacher_asmnt_for_fuzzy
save `teacher_asmnt_for_fuzzy'


*******

use `teacher_pedagogy', clear // 194

*correcting some names to be able to merge with the roster data 

*a lot of teachers do not have their names entered here. for the purposes of fuzzy matching, only keep the ones that have the names 
gen idmaster = _n
gen txtmaster = m4saq1

tempfile teacher_pedag
save `teacher_pedag'

drop if txtmaster == ""
keep school_code idmaster txtmaster

isid school_code txtmaster

tempfile teacher_pedag_for_fuzzy
save `teacher_pedag_for_fuzzy'

*****
use `teacher_questionnaire', clear // 951

*change some names to be able to merge with the roster since they seem to be the best matches
replace m3sb_troster = "uwitonze berthe" if m3sb_troster == "uwayisenga berthe" & school_code == 270203

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
	foreach i in teacher_asmnt teacher_pedag teacher_quest {
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
	foreach i in teacher_asmnt teacher_pedag teacher_quest {

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

foreach i in teacher_quest teacher_asmnt teacher_pedag {

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

	*for the ones that were never matched with the roster, just use the name that matches the corresponding module
	drop if school_code == 440710 & txtusing == "vincent twizerimana" & txtmaster == "twizerimana phocas"
	drop if school_code == 510704 & txtmaster == "nsabimana jean de dieu" & txtusing == "minani jean de dieu"
		
	isid school_code txtmaster idmaster // only one name from each teacher dataset matched
	isid school_code txtusing idusing // only one name from roster matched
	
	tempfile append_`i'
	save `append_`i''
}


*now merge each of these files with the original dataset
foreach i in teacher_asmnt teacher_pedag teacher_quest {

	preserve
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
	restore
}


*for teacher pedagogy, if many teachers do not have names, just merge them on id-s 
use `merged_teacher_pedag', clear

*first keep the ones that were already merged
preserve
	keep if merge_teacher_pedag == 3
	
	tempfile fully_merged
	save `fully_merged'
restore

keep if merge_teacher_pedag == 1
gen teachers_id = m4saq1_number

tempfile remaining 
save `remaining'


*now merge them all together 

use `teacher_absence', clear

merge 1:1 idusing txtusing school_code using `merged_teacher_quest', gen(absence_teacher_quest)

merge 1:1 idusing txtusing school_code using `merged_teacher_asmnt', gen(absence_teacher_asmnt)

merge 1:1 idusing txtusing school_code using `fully_merged', gen(absence_teacher_pedag)

/*
*there are three teachers that do not have ids because they are not merged, we can replace these ids manually
replace teachers_id = 11 if txtusing == "etienne habiyambere" & school_code == 570205
replace teachers_id = 12 if txtusing == "lydie mukashema" & school_code == 570205
replace teachers_id = 13 if txtusing == "aime pacifique mutuyumukiza" & school_code == 570205
*/


*in cases where we have missing teacher ids, replace with the manual one
replace teachers_id = m3sb_tnumber if teachers_id == . & m3sb_tnumber != .
replace teachers_id = m5sb_tnum if teachers_id == . & m5sb_tnum != .

duplicates tag teachers_id school_code, gen(dupl)

merge m:1 teachers_id school_code using `remaining', gen(absence_teacher_pedag_2) replace update

count if dupl == 1 & absence_teacher_pedag_2==5

drop similscore* txtmaster* txtusing idmaster* idusing* absence* merge* dupl

save "$dir/5_output_data/RWA/RWA_teacher_level_updated.dta", replace 
