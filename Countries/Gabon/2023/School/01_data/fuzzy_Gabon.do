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
	
	use "$dir/3_input_data/GAB/GAB_`v'", clear 
	
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

*not sure if these ones are duplicates or not because there are some differences between the two, for example, in the gender varable
replace m2saq2 = "mboumba ntombo2" if school_code == "ECOLE PUBLIQUE COMMUNALE D LEYIMADE MOANDA _ 0202402016X524" & m2saq2 == "mboumba ntombo" & teachers_id == 22
replace m2saq2 = "moussavou2" if school_code == "ECOLE CATHOLIQUE SAINTE THERESE _ 0801402018X507" & m2saq2 == "moussavou" & teachers_id == 12

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

*replace some names to be able to merge with roster
replace m5sb_troster = "ngomkom" if m5sb_troster == "ngomkam epse mounanga" & school_code == "COMPLEXE SCOLAIRE LES ELITES DE DEMAIN _ 0101306174X501" // confirm
replace m5sb_troster = "nziengui lionnel" if m5sb_troster == "rabila lionel" & school_code == "COMPLEXE SCOLAIRE LES COROLINES _ 0801401016X506" // confirm
replace m5sb_troster = "chaold arsene" if m5sb_troster == "charle arsÈne arsÈne" & school_code == "COMPLEXE SCOLAIRE LES PETITS VAINQUEURS _ 0106401017X501" // confirm
replace m5sb_troster = "sola" if m5sb_troster == "mounguembe sola" & school_code == "COMPLEXE SCOLAIRE PANAFRICAIN BAV MARIE GEVY _ 0101306125X524" 
replace m5sb_troster = "dinarucabu lÉa" if m5sb_troster == "dinamicambou lea" & school_code == "EAPEC _ 0101301130X505" 
replace m5sb_troster = "nzue menie christian" if m5sb_troster == "nzue" & school_code == "ECOLE CATHOLIQUE DON BOSCO _ 0901402009X512" 
replace m5sb_troster = "mezui engwe romial" if m5sb_troster == "mezui" & school_code == "ECOLE COMMUNALE DE FOUGAMOU CENTRE _ 0402201005X514" 
replace m5sb_troster = "evoso" if m5sb_troster == "evozo'o allogho" & school_code == "ECOLE PRIVEE BILINGUE LA BELLE PORTE ACADEMIQUE _ 0101306170X524" 
replace m5sb_troster = "moutsinga" if m5sb_troster == "moudjiga nancy" & school_code == "ECOLE PRIVEE BILINGUE LA BELLE PORTE ACADEMIQUE _ 0101306170X524" 
replace m5sb_troster = "malame aÏcha" if m5sb_troster == "malam axiliatrice" & school_code == "ECOLE PRIVEE ISLAMIQUE NOUROUL HOUDA AL ISLAM _ 0101302043X502" 
replace m5sb_troster = "matseya" if m5sb_troster == "matseva mouandja emilie" & school_code == "ECOLE PUBLIQUE D OYENANO _ 0402101003X506" 
replace m5sb_troster = "omondo" if m5sb_troster == "omondo bivouli guenolé" & school_code == "ECOLE PUBLIQUE D OYENANO _ 0402101003X506" 
replace m5sb_troster = "omondo" if m5sb_troster == "omondo bivouli guenolé" & school_code == "ECOLE PUBLIQUE D OYENANO _ 0402101003X506" 
replace m5sb_troster = "motho" if m5sb_troster == "motho paul arnauld" & school_code == "ECOLE PUBLIQUE DE BELLE VUE 3 _ 0101303213X502" 
replace m5sb_troster = "nze" if m5sb_troster == "nze mendome papynel" & school_code == "ECOLE PUBLIQUE DE BELLE VUE 3 _ 0101303213X502" 
replace m5sb_troster = "ngoua abagha benjamin" if m5sb_troster == "ngoua" & school_code == "ECOLE PUBLIQUE DE MEYO KYE _ 0902104002X512" 
replace m5sb_troster = "ekobi seck fatou" if m5sb_troster == "eboki" & school_code == "ECOLE PUBLIQUE RURALE DE SIAT 1 _ 0902102005X505" 
replace m5sb_troster = "ezeme ella nelly chimÈne" if m5sb_troster == "ezeme" & school_code == "ECOLE PUBLIQUE RURALE DE SIAT 1 _ 0902102005X505" 
replace m5sb_troster = "ezeme ella nelly chimÈne" if m5sb_troster == "ezeme" & school_code == "ECOLE PUBLIQUE RURALE DE SIAT 1 _ 0902102005X505" 
replace m5sb_troster = "ngui obiang diane" if m5sb_troster == "diare estelle ngui" & school_code == "MISSION BARAKA _ 0101304013X501" 
replace m5sb_troster = "mfono ebang harnestine epouse ngema" if m5sb_troster == "nfono" & school_code == "SAINTE FAMILLE DE FOUGAMOU _ 0402201007X514" 
replace m5sb_troster = "mafoumbi yves" if m5sb_troster == "mafoumbi yves" & school_code == "ECOLE FRANCO ARABE AS SALAM _ 0101303084X509" 
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

replace school_code = "99" if school_code == ""
*replace some teachers to be to merge them with the roster 
replace m3sb_troster = "temi adonsi" if m3sb_troster == "tembi" & school_code == "COMPLEXE SCOLAIRE BELLE FONTAINE _ 0101302087X502"
replace m3sb_troster = "abede yolande" if m3sb_troster == "abeghe obiang" & school_code == "COMPLEXE SCOLAIRE JEREMIE _ 0101306220X503"
replace m3sb_troster = "ngui arnold" if m3sb_troster == "ngui meye" & school_code == "COMPLEXE SCOLAIRE JEREMIE _ 0101306220X503"
replace m3sb_troster = "ngomkom" if m3sb_troster == "ngomkam epse mounanga" & school_code == "COMPLEXE SCOLAIRE LES ELITES DE DEMAIN _ 0101306174X501"
replace m3sb_troster = "mouloungui vladie marie" if m3sb_troster == "moussavou ep/yele" & school_code == "COMPLEXE SCOLAIRE LES PETITS COEURS _ 0101302008X503"
replace m3sb_troster = "sola" if m3sb_troster == "mounguembe sola" & school_code == "COMPLEXE SCOLAIRE PANAFRICAIN BAV MARIE GEVY _ 0101306125X524"
replace m3sb_troster = "mileage mi obame" if m3sb_troster == "mefeang-mi-obame astrid t" & school_code == "COMPLEXE SCOLAIRE PRIVE BILINGUE LE CEP _ 0101306150X524"
replace m3sb_troster = "koumba catherine rudy gipsy" if m3sb_troster == "koumba m.ėpouse koumba" & school_code == "COMPLEXE SCOLAIRE PRIVE IVARU _ 0801402090X524" // not fully sure about this one but thats the only one in this school that has not been merged
replace m3sb_troster = "evoso" if m3sb_troster == "evozo'o allogho" & school_code == "ECOLE PRIVEE BILINGUE LA BELLE PORTE ACADEMIQUE _ 0101306170X524"
replace m3sb_troster = "moutsinga" if m3sb_troster == "moudjiga nancy" & school_code == "ECOLE PRIVEE BILINGUE LA BELLE PORTE ACADEMIQUE _ 0101306170X524"
replace m3sb_troster = "omondo" if m3sb_troster == "omondo bivouli guenolé" & school_code == "ECOLE PUBLIQUE D OYENANO _ 0402101003X506"
replace m3sb_troster = "matseya" if m3sb_troster == "matseva mouandja emilie" & school_code == "ECOLE PUBLIQUE D OYENANO _ 0402101003X506"
replace m3sb_troster = "motho" if m3sb_troster == "monsieur motho angue paul arnauld" & school_code == "ECOLE PUBLIQUE DE BELLE VUE 3 _ 0101303213X502"
replace m3sb_troster = "ngoua abagha benjamin" if m3sb_troster == "ngoua" & school_code == "ECOLE PUBLIQUE DE MEYO KYE _ 0902104002X512"
replace m3sb_troster = "rembogo regina" if m3sb_troster == "pepe wora regina" & school_code == "ECOLE PUBLIQUE HENRI CLEMENT _ 0801402028X507"
replace m3sb_troster = "ezeme ella nelly chimÈne" if m3sb_troster == "ezeme" & school_code == "ECOLE PUBLIQUE RURALE DE SIAT 1 _ 0902102005X505"
replace m3sb_troster = "ekouma thÉo sandy" if m3sb_troster == "ekouma theo" & school_code == "LA PEPINIERE DES CHAMPIONS _ 0102502028X506"

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
*gabon-specific: given that school code is a string variable, to be able to use school codes in the name, create a file that would assign numeric values for each school code
preserve 
	keep school_code
	duplicates drop
	sort school_code 
	gen id = _n

	tempfile school_codes
	save `school_codes'
restore

merge m:1 school_code using `school_codes', nogen 

levelsof id, local(schools) 

*preperare individual school level files for the roster
foreach v in `schools' {
	preserve
		keep if id == `v'
		
		tempfile teacher_absence_`v'
		save `teacher_absence_`v''
	restore
}

*do the same process for 
foreach v in `schools' {
	foreach i in teacher_asmnt teacher_pedag teacher_quest {
		preserve
			use ``i'_for_fuzzy', clear
			merge m:1 school_code using `school_codes', nogen keep(1 3)
			
			tempfile ``i'_for_fuzzy
			save ``i'_for_fuzzy'
			
			keep if id == `v'
			
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
	drop if school_code == 43 & txtusing == "tsona nzamba doria epouse mouloungui" & (txtmaster == "koumba epouse koumba" | txtmaster == "koumba m.ėpouse koumba")
	isid school_code txtmaster idmaster // only one name from each teacher dataset matched
	isid school_code txtusing idusing // only one name from roster matched
	
	tempfile append_`i'
	save `append_`i''
}

*now merge each of these files with the original dataset
foreach i in teacher_asmnt teacher_pedag teacher_quest {

	*preserve
		use ``i'', clear
		
		merge 1:1 idmaster txtmaster school_code using ``i'_for_fuzzy', gen(merge_school_`i') // to be able to get the school codes and ids
		
		rename school_code school_code_proper
		rename id school_code
		
		merge 1:1 idmaster txtmaster school_code using `append_`i'', gen(merge_`i')
		
		*for the ones that were never matched with the roster, just use the name that matches the corresponding module
		replace idusing = idmaster if idusing == .
		replace txtusing = txtmaster if txtusing == ""
		
		foreach v in idmaster txtmaster similscore {
			rename `v' `v'_`i'
		}
		
		drop max
		drop school_code 
		rename school_code_proper school_code
		
		tempfile merged_`i'
		save `merged_`i''
	*restore
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

*in cases where we have missing teacher ids, replace with the manual one
replace teachers_id = m3sb_tnumber if teachers_id == . & m3sb_tnumber != .
replace teachers_id = m5sb_tnum if teachers_id == . & m5sb_tnum != .

*create a variable for duplicates
duplicates tag teachers_id school_code, gen(dupl)
merge m:1 teachers_id school_code using `remaining', gen(absence_teacher_pedag_2) replace update  

*combine these 2 observations into one since they are the same teacher
foreach v of varlist m5* typetest *content_knowledge {
	sort school_code teachers_id m5sb_tnum
	cap replace `v' = `v'[_n-1] if `v' == . & `v'[_n-1] != . & ((school_code == "LA PEPINIERE DES CHAMPIONS _ 0102502028X506" & teachers_id == 4) | ///
	school_code == "ECOLE FRANCO ARABE AS SALAM _ 0101303084X509" & teachers_id == 12)
	cap replace `v' = `v'[_n-1] if `v' == "" & `v'[_n-1] != "" & ((school_code == "LA PEPINIERE DES CHAMPIONS _ 0102502028X506" & teachers_id == 4) |  | ///
	school_code == "ECOLE FRANCO ARABE AS SALAM _ 0101303084X509" & teachers_id == 12)
}

drop if school_code == "LA PEPINIERE DES CHAMPIONS _ 0102502028X506" & m3sb_troster == ""
drop if school_code == "ECOLE FRANCO ARABE AS SALAM _ 0101303084X509" & m3sb_troster == ""

count if dupl == 1 & absence_teacher_pedag_2==5
drop similscore* txtmaster* txtusing idmaster* idusing* absence* merge* dupl

save "$dir/5_output_data/GAB/GAB_teacher_level_updated.dta", replace 
