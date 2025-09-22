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
	
	use "$dir/3_input_data/ETH/ETH_`v'", clear 
	
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
use `teacher_absence', clear

**# Bookmark #1
*drop the ones that seem to be duplicate observations
drop if school_code == 100957 & teacher_name == "Gebianesh Alemnew" & teachers_id == 11
drop if school_code == 202223 & teacher_name == "Mitiku Kebede" & teachers_id == 17
drop if school_code == 402606 & teacher_name == "Lubaba Ahemed" & teachers_id == 4
drop if school_code == 500077 & teacher_name == "RIEK LAP" & teachers_id == 7
drop if school_code == 500322 & teacher_name == "CHUOL TUT" & teachers_id != 6

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
use `teacher_assessment', clear

*some manual fixes to be able to merge with the roster 
replace m5sb_troster = "abebaw firdaweke" if m5sb_troster == "abenaw fradwork" & school_code == 106132
replace m5sb_troster = "alemayew teferi" if m5sb_troster == "alamaayyoo tafarii" & school_code == 201182
replace m5sb_troster = "masarat zawudoo" if m5sb_troster == "metered zewudu" & school_code == 202228
replace m5sb_troster = "fiqiree raggaasaa" if m5sb_troster == "finite regasa" & school_code == 202311
replace m5sb_troster = "hamziya abadaga" if m5sb_troster == "amiziya" & school_code == 202691
replace m5sb_troster = "fozee mohammed" if m5sb_troster == "fozeya" & school_code == 202691
replace m5sb_troster = "tirualem kaba" if m5sb_troster == "turuwek" & school_code == 202691 // not sure about this one
replace m5sb_troster = "juwar a/sanbi" if m5sb_troster == "jewar abasambi" & school_code == 203052
replace m5sb_troster = "asha kedir" if m5sb_troster == "ayisha" & school_code == 203057
replace m5sb_troster = "degife gisa" if m5sb_troster == "degefe" & school_code == 203057
replace m5sb_troster = "yesuf abdulla" if m5sb_troster == "yusuf" & school_code == 203057
replace m5sb_troster = "abeba ordofa" if m5sb_troster == "abebe" & school_code == 203144
replace m5sb_troster = "meseret shiferaw" if m5sb_troster == "masarat shifarahu" & school_code == 205801
replace m5sb_troster = "zawde tilahun" if m5sb_troster == "zawudu" & school_code == 206376
replace m5sb_troster = "umar daraje" if m5sb_troster == "umer derje" & school_code == 208402
replace m5sb_troster = "feruza adem" if m5sb_troster == "fruze adam" & school_code == 208560
replace m5sb_troster = "ayele geda" if m5sb_troster == "ayala gada" & school_code == 209198
replace m5sb_troster = "desibil gemmeda" if m5sb_troster == "dassibal gammadaa" & school_code == 211788
replace m5sb_troster = "emanuail abesa" if m5sb_troster == "amanuel ebisa" & school_code == 221451
replace m5sb_troster = "reda hussein" if m5sb_troster == "rida" & school_code == 232330
replace m5sb_troster = "maxamad dheesi muxumed" if m5sb_troster == "mohamed desi" & school_code == 301325
replace m5sb_troster = "fardawsa carab mataan" if m5sb_troster == "furdosa arab" & school_code == 301325
replace m5sb_troster = "mahad c/qaadir sheekh" if m5sb_troster == "mohamed abdulqadir" & school_code == 301325
replace m5sb_troster = "mifeta nasir" if m5sb_troster == "mifita nesru" & school_code == 400218
replace m5sb_troster = "jafar nesru" if m5sb_troster == "jafer naser" & school_code == 400219
replace m5sb_troster = "semano yuye" if m5sb_troster == "shumalo burka" & school_code ==  400957 // not sure about this one
replace m5sb_troster = "kefa washe" if m5sb_troster == "kafe wake" & school_code == 401185
replace m5sb_troster = "tsegrada ossie" if m5sb_troster == "tsigered hsea" & school_code == 405673
replace m5sb_troster = "abde ahemadin" if m5sb_troster == "abdi amedine" & school_code == 550254
replace m5sb_troster = "behari ali" if m5sb_troster == "bahir ali" & school_code == 550363
replace m5sb_troster = "natabara alex" if m5sb_troster == "matanboru alex" & school_code == 695750

isid school_code m5sb_troster // not unique at the assessment number but leaving this as is for now

gen idmaster = _n 
gen txtmaster = m5sb_troster

tempfile teacher_asmnt
save `teacher_asmnt'

keep idmaster txtmaster school_code 

tempfile teacher_asmnt_for_fuzzy
save `teacher_asmnt_for_fuzzy'

*******
use `teacher_pedagogy', clear

*correcting some names to be able to merge with the roster data 
replace m4saq1 = "dabs g/her" if m4saq1 == "debes gebrezgbher" & school_code == 10592
replace m4saq1 = "fozee mohammed" if m4saq1 == "foze" & school_code == 202691
replace m4saq1 = "jemal abanega" if m4saq1 == "jamal a /naga" & school_code == 203026
replace m4saq1 = "almaza tesfaye" if m4saq1 == "alima" & school_code == 206376
replace m4saq1 = "maxamad dheesi muxumed" if m4saq1 == "mohamed desi" & school_code == 301325
replace m4saq1 = "hasen mahamed shek abdulahi" if m4saq1 == "hassan" & school_code == 306215
replace m4saq1 = "yonase woled" if m4saq1 == "yonas welde" & school_code == 402487
replace m4saq1 = "hiri abdi" if m4saq1 == "hire abdulmejid" & school_code == 501491
replace m4saq1 = "g/yesus g/hiwet" if m4saq1 == "gebreyesus gebrehiwet" & school_code == 10458
replace m4saq1 = "degife gisa" if m4saq1 == "degefe" & school_code == 203057
replace m4saq1 = "umar daraje" if m4saq1 == "umer dereje" & school_code == 208402
replace m4saq1 = "reda hussein" if m4saq1 == "rediha" & school_code == 232330
replace m4saq1 = "erimyas barcho" if m4saq1 == "ermaysi" & school_code == 401351

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
use `teacher_questionnaire', clear

replace m3sb_troster = "frewyni kiday" if m3sb_troster == "freweyni ataklti" & school_code == 10592
replace m3sb_troster = "enyew derso" if m3sb_troster == "eniyew daresu" & school_code == 100883
replace m3sb_troster = "degife gisa" if m3sb_troster == "degefe" & school_code == 203057
replace m3sb_troster = "umar daraje" if m3sb_troster == "umer dereje" & school_code == 208402
replace m3sb_troster = "mahad c/qaadir sheekh" if m3sb_troster == "mahamed abdulqadir" & school_code == 301325
replace m3sb_troster = "fardawsa carab mataan" if m3sb_troster == "ferduwasa arab" & school_code == 301325
replace m3sb_troster = "mifeta nasir" if m3sb_troster == "mifita nesru" & school_code == 400218
replace m3sb_troster = "desta kasamo" if m3sb_troster == "dasete kasemo" & school_code == 400482
replace m3sb_troster = "amsale berele" if m3sb_troster == "sumaleo bureka" & school_code == 400957 // not sure about this one, just matching based on the id number
replace m3sb_troster = "kefa washe" if m3sb_troster == "kafe wake" & school_code == 401185
replace m3sb_troster = "tsegrada ossie" if m3sb_troster == "tsigered hsea" & school_code == 405673
replace m3sb_troster = "baqeleche shutaka" if m3sb_troster == "baqaleche suxuqa" & school_code ==  428903
replace m3sb_troster = "beza mokenen" if m3sb_troster == "beza" & school_code == 550234
replace m3sb_troster = "belte asfaw" if m3sb_troster == "belet asefa" & school_code == 550446
replace m3sb_troster = "natabara alex" if m3sb_troster == "matanboru alex" & school_code == 695750

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
	
	cap drop if txtmaster == "nasteho mahamed omer" & school_code == 300122 & txtusing == "halimo mahamed omer" // dropping this double matched observation from the questionnaire 
	cap drop if txtmaster == "belet asefa" & school_code == 550446 & txtusing == "tadese asefa" // dropping this double matched observation from the questionnaire 
	
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

*now merge them all together 

use `teacher_absence', clear

merge 1:1 idusing txtusing school_code using `merged_teacher_quest', gen(absence_teacher_quest)

merge 1:1 idusing txtusing school_code using `merged_teacher_pedag', gen(absence_teacher_pedag)

merge 1:1 idusing txtusing school_code using `merged_teacher_asmnt', gen(absence_teacher_asmnt)

drop similscore* txtmaster* txtusing idmaster* idusing* absence* merge*

save "$dir/5_output_data/ETH/ETH_teacher_level_updated.dta", replace 
