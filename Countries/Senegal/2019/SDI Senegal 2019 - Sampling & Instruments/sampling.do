global snsdiedu "C:\Users\wb192076\OneDrive - WBG\WW Data Files\SDIProject\SDI_AFTHD\Senegal\2018"
use "$snsdiedu\effectifstot.dta", clear

*    DROP SCHOOLS WHICH DO NOT HAVE P4 STUDENTS
keep if total_ce2>0 & total_ce2<.
gen urban=urbainrural=="Urbain"
lab def urban 0 Rural 1 Urban, modify
lab val urban urban
lab var urban "Ecole urbaine"
drop if statutetablissement=="Com/Ass"
encode statutetablissement, gen(owner)

*    CREATING THE NUMBER OF SCHOOLS AND STUDENTS
*    BY INSPECTION, DEPARTMENT, COMMUNE, AND ARRONDISSEMENT 
encode ia, gen(inspection)
encode ief, gen(department)
bysort inspection department arrond: gen arondid=_n
bysort inspection department       : gen dptid=_n
bysort inspection                  : gen inspid=_n

qui for var arondid dptid inspid: gen x_X=cond(X==1,1,0)
bysort inspection: egen narond=sum(x_arondid)
bysort inspection: egen ndpt=sum(x_dptid)
lab var narond       "# of arrondissements in region"
lab var ndpt          "# of departments in region"

egen total=rsum(total_*)
lab var total "Total # of students in school"

bysort inspection department arrondissement: gen nscharond =_N
bysort inspection department               : gen nschief   =_N
bysort inspection                          : gen nschia    =_N
lab var nscharond   "# schools in arrondissement"
lab var nschief     "# schools in department"
lab var nschia      "# schools in region"

bysort inspection department arrondissement: egen totarond=sum(total)
bysort inspection department               : egen totief=sum(total)
bysort inspection                          : egen totia=sum(total)
lab var totarond   "# students in arrondissement"
lab var totief     "# students in department"
lab var totia      "# students in region"

bysort inspection department arrondissement: egen p4arond=sum(total_ce2)
bysort inspection department               : egen p4ief=sum(total_ce2)
bysort inspection                          : egen p4ia=sum(total_ce2)
lab var p4arond   "# P4 students in arrondissement"
lab var p4ief     "# P4 students in department"
lab var p4ia      "# P4 students in region"

drop x_*
*    CREATING THE STRATA
*gen strata=.
*replace strata=1 if sregion==1 & urban==0
*replace strata=2 if sregion==1 & urban==1
*replace strata=3 if sregion==2 & urban==0
*replace strata=4 if sregion==2 & urban==1
*replace strata=5 if sregion==3 & urban==0
*replace strata=6 if sregion==3 & urban==1
*replace strata=7 if sregion==4 & urban==0
*replace strata=8 if sregion==4 & urban==1
*replace strata=9 if sregion==5

*lab def strata 1 RurCentral 2 UrbCentral 3 RurEast 4 UrbEast 5 RurNorth 6 UrbNorth 7 RurWest 8 UrbWest 9 Kampala
*lab val strata strata

*    FIRST TABLES TO DESCRIBE SAMPLE FRAME
tabstat narond ndpt nschia p4ia totia, by(inspection) 

*tab strata if arondid==1
*tab strata if dptid==1
*tab strata if inspid==1
*tab strata if inspid==1, sum(p4ia)
*tab strata if inspid==1, sum(total)
*tab strata, sum(total)


*    MOVING TO GENERATE THE SAMPLES
*    1. RANDOM SAMPLING WITHIN STRATUM WITH A SAMPLE ALLOCATION OF
*    DAKAR-55/DIOURBEL-55/FATICK-45/KAFFRINE-45/KAOLACK-55/KEDOUGOU-45/KOLDA-45/LOUGA-45
*    MATAM-45/PIKINE-GUED-70/RUFISQUE-45/SAINT-LOUIS-55/SEDHIOU-45/TAMBA-45/THIES-100/ZIG-55

*gsample 800 if total_ce2!=0 [aw=total_ce2], gen(sample) wor 
*gsample 800 if total_ce2!=0 [aw=total_ce2], gen(sample) wor strata(strata)

sort inspection department arrondissement owner urban 
set seed 987361
qui for num 1/5 7/16 \ num 55 55 45 45 55 45 45 45 70 45 55 45 45 100 45: gsample Y [aw=total_ce2] if inspection==X, gen(sampleX) wor
gsample 44 [aw=total_ce2] if inspection==6 & total_ce2<118, gen(sample6) wor
replace sample6=1 if total_ce2>110 & total_ce2<. & inspection==6
egen finalsample=rsum(sample1-sample6)
drop sample1-sample6
lab var finalsample "Random sample of schools within region"

*    SELECTING REPLACEMENT SCHOOLS WHICH WILL BE CHOSEN WITHIN THE ARRONDISSEMENT
*    THREE SETS OF REPLACEMENTS WILL BE CHOSEN AT RANDOM FIRST AND THEN ASSIGNED
qui for num 1/16 \ num 55 55 45 45 55 45 45 45 45 70 45 55 45 45 100 45: gsample Y if inspection==X & finalsample==0, gen(replaceX_1) wor
qui for num 1/16 \ num 55 55 45 45 55 45 45 45 45 70 45 55 45 45 100 45: gsample Y if inspection==X & finalsample==0 & replaceX_1==0, gen(replaceX_2) wor
*qui for num 1/16 \ num 55 55 45 45 55 45 45 45 45 70 45 55 45 45 100 45: gsample Y if inspection==X & finalsample==0 & replaceX_1==0 & replaceX_2==0, gen(replaceX_3) wor
egen replace1=rsum(replace1_1- replace16_1)
egen replace2=rsum(replace1_2- replace16_2)
gen samplereplace=finalsample+2*replace1+3*replace2
lab def samprep 1 "Sampled" 2 "First replacement" 3 "Second replacement"
lab val samplereplace samprep 
lab var samplereplace "School is either in the sample or a replacement"
drop replace1_1- replace16_2
sort inspection department commune arrondissement communauterurale
compress
save "$snsdiedu\finalsample.dta", replace


**********  SAMPLING FOR DAARAS ***********************
***  INITIALLY 25 SUB-COUNTIES WILL BE CHOSEN BY REGION            ***
***  SAMPLING THE SUB-COUNTIES ONLY; SCHOOLS WILL BE CHOSEN LATER  *** 
**********************************************************************
 use "$snsdiedu\listedaarasfull.dta", clear
 
 *    CREATE A UNIQUE IDENTIFIER FOR EACH DAARA
sort ia ief commune arrondissement communaute_rurale village_quartier libelle_type_occupation libelle_type_statut_etablissemen nom_etablissement agegroup
bysort ia ief commune arrondissement communaute_rurale village_quartier libelle_type_occupation libelle_type_statut_etablissemen nom_etablissement: gen darid=_n
gen darn=darid==1
gen daraid=sum(darn)
lab var daraid "Daara's unique ID"
bysort daraid: egen nfilles=sum(filles)
bysort daraid: egen ngarcons=sum(garcons)
bysort daraid: egen ntalibes=sum(talibes)
gen shgrl=nfilles/ntalibes
lab var nfilles "Nombre de filles"
lab var ngarcons "Nombre de garcons"
lab var ntalibes "Nombre de talibes"
lab var shgrl "%age de filles"

drop if darid!=1
drop darid darn
sort ia ief commune arrondissement communaute_rurale village_quartier libelle_type_occupation libelle_type_statut_etablissemen nom_etablissement agegroup
set seed 297341
gsample 90 [aw=ntalibes], gen(sampleddaara) wor
gsample 90 if sampled==0, gen(replace1) wor
gsample 90 if sampled==0 & replace1==0, gen(replace2) wor
lab var replace1 "Daara is first replacement"
lab var replace2 "Daara is second replacement"
gen samplereplace=sampled+2*replace1+3*replace2
lab def samprep 1 "Sampled" 2 "First replacement" 3 "Second replacement"
lab val samplereplace samprep
lab var samplereplace "Daara is sampled or a replacement"
drop agegroup filles garcons talibes
compress
sort ia ief commune arrondissement communaute_rurale village_quartier libelle_type_occupation libelle_type_statut_etablissemen nom_etablissement 
save "$snsdiedu\daarasample.dta", replace

 
 
 
 **********  SAMPLING FOR PAQEEB'S DAARAS ***********************

 
use "$snsdiedu\daarapaqeeb.dta", clear
drop if nomdudaara==""
set seed 192846
gsample 90, gen(sampled) wor
lab var sampled "Sampled daara for SDI"
gsample 90 if sampled==0, gen(replace1) wor
gsample 90 if sampled==0 & replace1==0, gen(replace2) wor
lab var replace1 "Daara is first replacement"
lab var replace2 "Daara is second replacement"
gen samplereplace=sampled+2*replace1+3*replace2
lab def samprep 1 "Sampled" 2 "First replacement" 3 "Second replacement"
lab val samplereplace samprep
lab var samplereplace "Daara is sampled or a replacement"
save "$snsdiedu\daarapaqeebsample.dta", replace
 
 
