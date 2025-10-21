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
	
	use "$dir/3_input_data/PER/PER_`v'", clear 
	
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
use `teacher_assessment', clear // 766

*replace some names to be able to merge with roster
replace m5sb_troster = "ceyda Mónica Román" if m5sb_troster == "ceyda m�nica rom�n quintana" & school_code == 201129  
replace m5sb_troster = "cinthia elvira deza cuellar" if m5sb_troster == "cinthia" & school_code == 259358
replace m5sb_troster = "José González Garcia" if m5sb_troster == "jos� gonz�lez" & school_code == 281774
replace m5sb_troster = "Jaime Véliz Chumbiauca" if m5sb_troster == "jaime v�liz" & school_code == 281774
replace m5sb_troster = "Vilma Vargas Cochachin" if m5sb_troster == "vilma" & school_code == 361337
replace m5sb_troster = "Lidia Ramos Castillo" if m5sb_troster == "lidia" & school_code == 361337
replace m5sb_troster = "Beatriz Yolanda Huertas Murga" if m5sb_troster == "beatriz" & school_code == 361337
replace m5sb_troster = "Haydee Ríos" if m5sb_troster == "hayd�e r�os" & school_code == 370213
replace m5sb_troster =  "Carmen Sevillano Valverde" if m5sb_troster == "carmen" & school_code == 392753
replace m5sb_troster = "Selene Principe de la cruz" if m5sb_troster == "selene" & school_code == 392753
replace m5sb_troster = "Olivia Castillo romero" if m5sb_troster == "olivia" & school_code == 392753
replace m5sb_troster = "Elva Mestaza Bolo" if m5sb_troster == "elva" & school_code == 393009
replace m5sb_troster = "Lorgio Rodríguez Villajuan" if m5sb_troster == "lorgio" & school_code == 393322
replace m5sb_troster = "Rubér Vega Urtado" if m5sb_troster == "ruber" & school_code == 393322
replace m5sb_troster = "Jenny Rozaba Robles Chavez" if m5sb_troster == "jenny" & school_code == 415992
replace m5sb_troster = "Cecilia Isabel Huaney Tinoco" if m5sb_troster == "cecilia" & school_code == 415992
replace m5sb_troster = "reyner yatsupich sanchez" if m5sb_troster == "reyner" & school_code == 768036
replace m5sb_troster = "abel kapit yanua" if m5sb_troster == "abel" & school_code == 768036
replace m5sb_troster = "Raúl Antúnez" if m5sb_troster == "ra�l ant�nez" & school_code == 808147
replace m5sb_troster = "BERTHA KATRINA BORJE BRA" if m5sb_troster == "bertha" & school_code == 838565
replace m5sb_troster = "León Delci" if m5sb_troster ==  "delci antonia le�n ramos" & school_code == 1422534
replace m5sb_troster = "José Luis Rivera Levano" if m5sb_troster ==  "jos� luis" & school_code == 1731926
replace m5sb_troster = "Rogger William Junchaya Diaz" if m5sb_troster ==  "rogger" & school_code == 1731926

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
replace m4saq1 = "angela barrionuevo pardave" if m4saq1 == "angela" & school_code == 624247   
replace m4saq1 = "BERTHA KATRINA BORJE BRA" if m4saq1 == "bertha" & school_code == 838565   

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

*a couple of schools have missing names, fill them in based on the names from the roster
replace m3sb_troster = "NICOLÁS MORI GUIMARAEZ" if school_code == 272989 & m3sb_tnumber == 2
replace m3sb_troster = "Genaro Pérez Cárdenas" if school_code == 1238377 & m3sb_tnumber == 1
replace m3sb_troster = "ANDRES GONZALES PEREZ" if school_code == 512160 & m3sb_tnumber == 1
replace m3sb_troster = "violeta margarita amarillo rimari" if school_code == 715367 & m3sb_tnumber == 1

replace m3sb_troster = "cinthia elvira deza cuellar" if school_code == 259358 & m3sb_troster == "cinthia"
replace m3sb_troster = "nicolÁs mori guimaraez" if school_code == 272989 & m3sb_troster == "NICOLÁS MORI GUIMARAEZ"
replace m3sb_troster = "beatriz yolanda huertas murga" if school_code == 361337 & m3sb_troster == "beatriz"
replace m3sb_troster = "lidia ramos castillo" if school_code == 361337 & m3sb_troster == "lidia"
replace m3sb_troster = "vilma vargas cochachin" if school_code == 361337 & m3sb_troster == "vilma"
replace m3sb_troster = "olivia castillo romero" if school_code == 392753 & m3sb_troster == "olivia"
replace m3sb_troster = "carmen sevillano valverde" if school_code == 392753 & m3sb_troster == "carmen"
replace m3sb_troster = "selene principe de la cruz" if school_code == 392753 & m3sb_troster == "selene"
replace m3sb_troster = "elva mestaza bolo" if school_code == 393009 & m3sb_troster == "elva"
replace m3sb_troster = "lorgio rodríguez villajuan" if school_code == 393322 & m3sb_troster == "lorgio"
replace m3sb_troster = "rubér vega urtado" if school_code == 393322 & m3sb_troster == "ruber"
replace m3sb_troster = "jenny rozaba robles chavez" if school_code == 415992 & m3sb_troster == "jenny"
replace m3sb_troster = "cecilia isabel huaney tinoco" if school_code == 415992 & m3sb_troster == "cecilia"
replace m3sb_troster = "wilder romero" if school_code == 416990 & m3sb_troster == "416990"
replace m3sb_troster = "juan tinoco quiroz" if school_code == 416990 & m3sb_troster == "juan"
replace m3sb_troster = "andres gonzales perez" if school_code == 512160 & m3sb_troster == "ANDRES GONZALES PEREZ"
replace m3sb_troster = "reyner yatsupich sÁnchez" if school_code == 768036 & m3sb_troster == "reyner"
replace m3sb_troster = "abel katip yanua" if school_code == 768036 & m3sb_troster == "abel"
replace m3sb_troster = "bertha katrina borje bra" if school_code == 838565 & m3sb_troster == "bertha"
replace m3sb_troster = "rogger william junchaya diaz" if school_code == 1731926 & m3sb_troster == "rogger"

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

tempfile remaining 
save `remaining'


*now merge them all together 

use `teacher_absence', clear

merge 1:1 idusing txtusing school_code using `merged_teacher_quest', gen(absence_teacher_quest)

merge 1:1 idusing txtusing school_code using `merged_teacher_asmnt', gen(absence_teacher_asmnt)

*do some replacements to be able to merge in these file 
replace teachers_id = 3 if teacher_name == "MAGALY"
replace teachers_id = 4 if teacher_name == ""

merge 1:1 idusing txtusing school_code using `fully_merged', gen(absence_teacher_pedag)
merge 1:1 teachers_id school_code using `remaining', gen(absence_teacher_pedag_2) replace update

drop similscore* txtmaster* txtusing idmaster* idusing* absence* merge*

save "$dir/5_output_data/PER/PER_teacher_level_updated.dta", replace 
