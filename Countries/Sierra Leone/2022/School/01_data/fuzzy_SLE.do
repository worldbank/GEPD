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
	
	use "$dir/3_input_data/SLE/SLE_`v'", clear 
	
	if "`v'" == "teacher_pedagogy" {
		*SLE-specific given that videos were also filmed in the second grade there
		replace m4saq1 = m4saq1_g2 if m4saq1_g2 != "" & m4saq1 == ""
		replace m4saq1_number = m4saq1_number_g2 if m4saq1_number_g2 != . & m4saq1_number == .
	}
	
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
use `teacher_absence', clear // 2391

*not sure if these ones are duplicates or not because different 
replace m2saq2 = "ibrahim sesay1" if school_code == 219101202 & teachers_id == 4

*there is one school where we cannot find the school code, assign 99 for now
replace school_code = 99 if school_code == .

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
use `teacher_assessment', clear // 1116

*there is one school where we cannot find the school code, assign 99 for now
replace school_code = 99 if school_code == .

*replace some names to be able to merge with roster
replace m5sb_troster = "ishmael jalloh" if m5sb_troster == "jalloh bambara ishamel" & school_code == 120301220
replace m5sb_troster = "juliana a. conteh" if m5sb_troster == "jelicatu a conteh" & school_code == 240706340
replace m5sb_troster = "isah a conteh" if m5sb_troster == "aisha a. conteh" & school_code == 240706340 
replace m5sb_troster = "karim m. y koroma" if m5sb_troster == "karim mohamed yayah koroma" & school_code == 250102206
replace m5sb_troster = "esther fasoloko" if m5sb_troster == "esther fadulukuku" & school_code == 139102214 // the only reasonable match, not really sure about it 
replace m5sb_troster = "mbalusah m. koroma" if m5sb_troster == "mbaluza memunatu conteh" & school_code == 139102214 // the only reasonable match, not really sure about it 
replace m5sb_troster = "ishmeal b kavura" if m5sb_troster == "ishmael b. kamara" & school_code == 330401204 // the only reasonable match, not really sure about it 
replace m5sb_troster = "mr. kouta coker" if m5sb_troster == "abdulai b coker" & school_code == 410405207 // the only reasonable match, not really sure about it 
replace m5sb_troster = "joseph sama kargbo" if m5sb_troster == "sama kamara" & school_code == 3110202824 // the only reasonable match, not really sure about it 

isid school_code m5sb_troster // not unique at the assessment number but leaving this as is for now

gen idmaster = _n 
gen txtmaster = m5sb_troster

tempfile teacher_asmnt
save `teacher_asmnt'

keep idmaster txtmaster school_code 

tempfile teacher_asmnt_for_fuzzy
save `teacher_asmnt_for_fuzzy'

*******

use `teacher_pedagogy', clear // 520

*SLE-specific, two teachers have both g2 and g4 filmed, just keep g4 for our purposes to be able to properly merge with the data
drop if school_code == 340501205 & m4saq1 == "henry sandy" & grade == 2
drop if school_code == 110403206 & m4saq1 == "fayia j james" & grade == 2

rename grade teach_grade 
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
use `teacher_questionnaire', clear // 1114

*there is one school where we cannot find the school code, assign 99 for now
replace school_code = 99 if school_code == .

*a couple of schools have missing names, fill them in based on the names from the roster
replace m3sb_troster = "abdul t kamara" if m3sb_troster == "abdulai t kamara" & school_code == 211001206
replace m3sb_troster = "slyvanus gbla" if m3sb_troster == "sylvanu a. s. gblah" & school_code == 430201226
replace m3sb_troster = "francess m. kamara" if m3sb_troster == "francis m koroma" & school_code == 321001214
replace m3sb_troster = "joseph sama kargbo" if m3sb_troster == "sama kamara" & school_code == 3110202824 // not fully sure about it 

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

foreach i in teacher_asmnt teacher_pedag teacher_quest {

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
	
	*drop some observations that are clearly not matches
	drop if school_code == 120301220 & txtmaster == "jalloh bambara ishamel" &	txtusing == "joseph s bambara"
	drop if school_code == 240706340 & txtusing == "isah a conteh" & inlist(txtmaster, "juliana a. conteh", "aisha a. conteh") // not sure which one is supposed to be a match
	drop if school_code == 250102206 & txtusing == "mohamed yayah koroma" & txtmaster == "karim mohamed yayah koroma"
	drop if school_code == 3110202824 & txtmaster == "sama kamara" & txtusing == "abu kamara"
	drop if school_code == 230101211 & txtmaster == "mahmoud l mansaray" & txtusing == "manmoud k mansaray"
	drop if school_code == 410103207 & txtmaster == "abdulai mansaray" & inlist(txtusing, "moses mansaray", "abdulai kargbo")
	drop if school_code == 210501223 & txtmaster == "abass bangura" & txtusing == "osman bangura"
	drop if school_code == 211001206 & txtmaster == "abdulai t kamara" & txtusing == "abdulai m kamara"
	drop if school_code == 321001214 & txtmaster == "francis m koroma" & txtusing == "francis domingo"
	
	isid school_code txtmaster idmaster // only one name from each teacher dataset matched
	isid school_code txtusing idusing // only one name from roster matched
	
	tempfile append_`i'
	save `append_`i''
}


*now merge each of these files with the original dataset
foreach i in teacher_asmnt teacher_pedag teacher_quest {

	preserve
		use ``i'', clear
		
		merge 1:1 idmaster txtmaster school_code using `append_`i'', gen(merge_`i')
		
		*for the ones that were never matched with the roster, just use the name that matches the corresponding module
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

*looks like SLE has a lot of teachers with missing names and codes, drop them since we will not be able to merge them, can later append them
preserve
	drop if teachers_id == .
	isid school_code teachers_id
	
	tempfile remaining 
	save `remaining'
restore 

*now only keep the ones with missing info to append them 
keep if teachers_id == .
tempfile unmerged
save `unmerged'


use `teacher_absence', clear

merge 1:1 idusing txtusing school_code using `merged_teacher_quest', gen(absence_teacher_quest)

merge 1:1 idusing txtusing school_code using `merged_teacher_asmnt', gen(absence_teacher_asmnt)

merge 1:1 idusing txtusing school_code using `fully_merged', gen(absence_teacher_pedag)

*for the newly merged teachers, they will not have ids, so just assign them the ids in some kind of order that would make them unique
bysort school_code: gen id_new = _n if teachers_id == .

*to make sure it does not mess up the rest of ids, confirm that the count if below 15 (alternatively can check that these same observations are also using only in the merges above)
qui count if id_new != .
assert `r(N)' <= 15

replace teachers_id = id_new if id_new != . & teachers_id == .

*some cases where id and schools are not unique, manually address them here (confirmed that they are not affected by the next merge)
replace teachers_id = 5 if school_code == 210501223 & teacher_name == "GIBRILLA KAMARA"
replace teachers_id = 6 if school_code == 331201217 & teacher_name == ""
replace teachers_id = 11 if school_code == 410103207 & teacher_name == "Abdulai mansaray"
replace teachers_id = 12 if school_code == 99 & teacher_name == "Issa Ballah Samura"
replace teachers_id = 13 if school_code == 99 & teacher_name == ""
replace teachers_id = 9 if school_code == 250603210 & teacher_name == ""
replace teachers_id = 11 if school_code == 240706340 & teacher_name == ""
replace teachers_id = 6 if school_code == 210501223 & teacher_name == "Abass Bangura"

merge 1:1 teachers_id school_code using `remaining', gen(absence_teacher_pedag_2) replace update
append using `unmerged' // still quite a lot are unmerged because they do not seem to have identifying info

replace school_code = . if school_code == 99

drop similscore* txtmaster* txtusing idmaster* idusing* absence* merge* id_new

save "$dir/5_output_data/SLE/SLE_teacher_level_updated.dta", replace 
