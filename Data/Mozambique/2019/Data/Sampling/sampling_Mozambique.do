# Sampling Mozambique
clear
use "C:\Users\WB469649\OneDrive - WBG\Education Policy Dashboard\Sampling\Sampling Data\Mozambique_SDI\samplingframe_weights.dta" 
cap drop _merge

merge m:1 statecode lgacode using "C:\Users\WB469649\OneDrive - WBG\Education Policy Dashboard\Sampling\Sampling Data\Mozambique_SDI\lga_vars.dta" 
drop if _merge==2
cap drop _merge

gen const=1
egen school_count=total(const), by(lgacode state)
save "C:\Users\WB469649\OneDrive - WBG\Education Policy Dashboard\Sampling\Sampling Data\Mozambique_SDI\samplingframe_details.dta" , replace

clear
gl path "C:/Users/WB469649/OneDrive - WBG/Older Files/SDI Microdata - Confidential - EDU/SDI Microdata - Confidential - EDU - Brian/"
gl sdi_data "./Mozambique/SDI Indicators/"
use "$path/$sdi_data/Mozambique_absence_data.dta", replace

cap drop _merge
merge m:1 schid using "C:\Users\WB469649\OneDrive - WBG\Older Files\SDI Microdata - Confidential - EDU\SDI Microdata - Confidential - EDU - Brian\Mozambique\raw\M1S1.dta" , keepusing(m1s1q9 m1s1q10)
gen schoolcode=m1s1q9
replace schoolcode=m1s1q10 if schoolcode==.
cap drop _merge

merge m:1 schoolcode using "C:\Users\WB469649\OneDrive - WBG\Education Policy Dashboard\Sampling\Sampling Data\Mozambique_SDI\samplingframe_details.dta" 



drop if _merge<3
drop _merge


svyset [pw=absteachwt], strata(cellpovrurown)
svy: mean absentfromclass
local mean= round(_b[absentfromclass],.1)
local se=round(_se[absentfromclass],.1)

svy: mean absentfromclass
bysort district_id:  gen teachid=_n


egen group_mn=mean(absentfromclass), by(district_id)
gen one=1
egen group_cnt=total(one), by(district_id)
egen group_var=mean((absentfromclass-group_mn)^2*group_cnt/(group_cnt-1)), by(district_id)
gen group_sd=sqrt(group_var)

collapse (first) schoolcode- group_sd, by(district_id)
hist group_sd


gl vars rural_share poor09 gvt_educ gvt_health nonprofit_educ nonprofit_health read write numeracy netenrolled dropout malaria diarrhoea rural_status population
foreach var in $vars {
*lowess group_sd `var'
*sleep 5000
}

reg group_sd school_count, robust
label var group_sd "Std Dev of Absenteeism"
label var school_count "# of Schools in LGA"
twoway scatter group_sd school_count || lfit group_sd school_count, title("Plot of Std Dev of Absenteeism on # of Schools in Mozambique Districts", size(medsmall)) xtitle("# of Schools in LGA") ytitle("Std Dev of Absenteeism")
graph export "C:\Users\WB469649\OneDrive - WBG\Education Policy Dashboard\Sampling\Sampling Data\Mozambique_SDI\school_size_plot.png", as(png) replace
reg group_sd read

twoway scatter group_sd read || lfit group_sd read, title("Plot of Std Dev of Absenteeism on Proportion Who Can Read in Mozambique Districts", size(medsmall)) xtitle("Proportion Who Can Read") ytitle("Std Dev of Absenteeism")
graph export "C:\Users\WB469649\OneDrive - WBG\Education Policy Dashboard\Sampling\Sampling Data\Mozambique_SDI\prop_read_plot.png", as(png)  replace



clear
gl path "C:/Users/WB469649/OneDrive - WBG/Older Files/SDI Microdata - Confidential - EDU/SDI Microdata - Confidential - EDU - Brian/"
gl sdi_data "./Mozambique/SDI Indicators/"
use "$path/$sdi_data/Mozambique_teacher_knowledge.dta", replace

cap drop _merge
merge m:1 schid using "C:\Users\WB469649\OneDrive - WBG\Older Files\SDI Microdata - Confidential - EDU\SDI Microdata - Confidential - EDU - Brian\Mozambique\raw\M1S1.dta" , keepusing(m1s1q9 m1s1q10)
gen schoolcode=m1s1q9
replace schoolcode=m1s1q10 if schoolcode==.
cap drop _merge

merge m:1 schoolcode using "C:\Users\WB469649\OneDrive - WBG\Education Policy Dashboard\Sampling\Sampling Data\Mozambique_SDI\samplingframe_details.dta" 


drop if _merge<3
drop _merge

keep if teachtestwt!=.
keep if scorecont!=.
gl items scorecont
su $items
egen overall_score=rowtotal($items)
su overall_score
gen overall_score_std=(overall_score-`r(mean)')/`r(sd)'


svyset [pw=teachtestwt], strata(cellpovrurown)
svy: mean overall_score_std
local mean= round(_b[overall_score_std],.1)
local se=round(_se[overall_score_std],.1)

svy: mean overall_score_std
bysort district_id:  gen teachid=_n


egen group_mn=mean(overall_score_std), by(district_id)
gen one=1
egen group_cnt=total(one), by(district_id)
egen group_var=mean((overall_score_std-group_mn)^2*group_cnt/(group_cnt-1)), by(district_id)
gen group_sd=sqrt(group_var)

collapse (first) schoolcode- group_sd, by(district_id)
hist group_sd


gl vars rural_share poor09 gvt_educ gvt_health nonprofit_educ nonprofit_health read write numeracy netenrolled dropout malaria diarrhoea rural_status population
foreach var in $vars {
*lowess group_sd `var'
*sleep 5000
}

reg group_sd school_count, robust
label var group_sd "Std Dev of Content Knowledge"
label var school_count "# of Schools in LGA"
twoway scatter group_sd school_count || lfit group_sd school_count, title("Plot of Std Dev of Content Knowledge on # of Schools in Mozambique Districts", size(medsmall)) xtitle("# of Schools in LGA") ytitle("Std Dev of Content Knowledge")
graph export "C:\Users\WB469649\OneDrive - WBG\Education Policy Dashboard\Sampling\Sampling Data\Mozambique_SDI\school_size_plot_cont.png", as(png)  replace
reg group_sd read
twoway scatter group_sd read || lfit group_sd read, title("Plot of Std Dev of Content Knowledge on Proportion Who Can Read in Mozambique Districts", size(medsmall)) xtitle("Proportion Who Can Read") ytitle("Std Dev of Absenteeism")
graph export "C:\Users\WB469649\OneDrive - WBG\Education Policy Dashboard\Sampling\Sampling Data\Mozambique_SDI\prop_read_plot_cont.png", as(png)  replace

reg group_sd write
lowess group_sd write

reg group_sd numeracy
lowess group_sd numeracy


stop
