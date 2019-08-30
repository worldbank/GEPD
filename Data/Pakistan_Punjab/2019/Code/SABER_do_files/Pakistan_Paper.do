*********************************************************************************
* TEACH Study
* Programming:
*   Stata Version:  Stata 14.2 
*   Original Authors: Syeda Farwa Fatima
*   Last Modified: July 1st, 2018
* 	Notes: Reliability Analyses for TEACH Pakistan 
*********************************************************************************

* Stata Version:
    version 13.0
  
* Clearing
    clear all
	set more off
  
* Local directory:
cd "/Users/Farwa/Documents/SABER_WB/Pakistan" 
*------------------------------------------------------------------------
* Input the data set, name and label the variables and their values
*------------------------------------------------------------------------
use "M4AB_Classroom Observation.dta"
describe //long format

*------------------------------------------------------------------------
* Check all variables for inconsistencies 
*------------------------------------------------------------------------
*setExcelSheet "Overall_seconddataset"

set more off, permanent 
rename enum_name enum_code
destring enum_code, replace
sort scode enum_code
tab1 scode enum_code

duplicates report scode subject // 86 sinlge rating obs, cannot calculate inter-rater reliability
duplicates tag scode subject, gen(tag)
tab tag

*listExcel scode subject enum_code if tag==0, ///
*desc("28 sinlge rating obs, cannot calculate inter-rater reliability")
	
*Check first 4 vars
tab1 enum_code-topic_of_lesson //coding slightly different than questionnaire/first dataset
assert regular_teacher==0 | regular_teacher==1

**Check range of overall element score
foreach x of varlist m4a_a1 m4a_a2 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
assert inlist(`x', 1, 2, 3, 4, 5, .)
}

*Check/fix range of individual behaviors
tab1 m4a_a1_1-m4a_a1_4 m4a_a2_1-m4a_a2_3 m4a_b4_1-m4a_b4_4 m4a_b5_1-m4a_b5_3 m4a_b6_1 m4a_b7_1-m4a_b7_3 m4a_c8_1-m4a_c8_3 m4a_c9_1-m4a_c9_3 m4a_c10_1 m4a_c10_2

//Item missing for element 3
*listExcel scode enum_code m4a_a3 if _n==1, ///
*desc("Item 3.1 missing from the dataset, but element 3 given")
	
foreach x of varlist m4a_a1_1 m4a_a1_2 m4a_a2_1-m4a_a2_3 m4a_b4_1-m4a_b4_4 m4a_b5_1 m4a_b5_3 m4a_b6_1 m4a_b7_1-m4a_b7_3 m4a_c8_1-m4a_c8_3 m4a_c9_1-m4a_c9_3 m4a_c10_1 m4a_c10_2 {
assert inlist(`x', 4, 3, 2, .)
}

tab1 m4a_a1_3 m4a_a1_4  m4a_b5_2
foreach x of varlist m4a_a1_3 m4a_a1_4 m4a_b5_2 {
assert inlist(`x', 4, 3, 2, 1)
}

save "M4AB_Classroom Observation.dta", replace
gen teach=(m4a_a1+m4a_a2+m4a_b4+m4a_b5+m4a_b6+m4a_b7+m4a_c8+m4a_c9+m4a_c10)/10

*----------------------------------
* Inter-Rater Reliability Stats
*----------------------------------
*a)For each element

*i)Percentage of exact agreement for non-missing values
*local c = 7
foreach x of varlist teach /*m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10*/ {
bys scode teacher_selected subject: gen agree_`x'= 1 if `x'[1]==`x'[2] & `x'[1]!=.
bys scode teacher_selected subject: replace agree_`x'= 0 if `x'[1]!=`x'[2] & `x'[1]!=. & `x'[2]!=.
tabstat agree_`x', stat(mean) save
*putexcel B`c'=matrix(r(StatTotal)) using Pakistan, sheet("Table A.3") modify
*loc ++c
}

tab1 agree_*

*ii)Percentage of exact agreement or within one (+/- one point) for non-missing values 
*local c = 7
foreach x of varlist teach /*m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10*/ {
bys scode teacher_selected subject: gen agree2_`x'= 1 if abs(`x'[1]-`x'[2])<=1 & `x'[1]!=. & `x'[2]!=.
bys scode teacher_selected subject: replace agree2_`x'= 0 if abs(`x'[1]-`x'[2])>1 & `x'[1]!=. & `x'[2]!=.
tabstat agree2_`x', stat(mean) save
*putexcel C`c'=matrix(r(StatTotal)) using Pakistan, sheet("Table A.3") modify
*loc ++c
}

tab1 agree2_*

*Logic check since true by definition
foreach x of varlist teach m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
assert agree_`x'<=agree2_`x'
}
drop agree*

/*
drop if teacher_selected=="" //missing teacher names
egen teachercode = group(teacher_selected)

*Create unique code for each school-teacher-subject
gen unq_tsubjcode = scode*1000+teachercode*10+subject*10
*/

gen unq_tsubjcode = scode*10+subject

*iii)Calculate ICC
preserve
local c = 7
keep if tag==1 //to ensure no missing values
foreach x of varlist teach m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
icc `x' unq_tsubjcode //one-way random effects model
disp as text "The ICC for Question ‘`x'’ = " _col(30) as result %5.4f r(icc_i)
putexcel D`c'=(r(icc_i)) using Pakistan, sheet("Table A.3") modify
loc ++c
}
restore

*iv)Calculate Krippalpha v)Gamma vi) Weighted Kappa
preserve
keep if tag==1 //to ensure no missing values
keep unq_tsubjcode enum_code m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10
bys unq_tsubjcode: replace enum_code = _n //since want to reshape for enum1 and enum2
reshape wide m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, i(unq_tsubjcode) j(enum_code)

local c = 7
foreach x in m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
krippalpha `x'1 `x'2, method(ordinal)
putexcel E`c'=(r(k_alpha)) using Pakistan, sheet("Table A.3") modify
tab `x'1 `x'2, all
putexcel F`c'=(r(gamma)) using Pakistan, sheet("Table A.3") modify
kap `x'1 `x'2, wgt(w)
putexcel G`c'=(r(kappa)) using Pakistan, sheet("Table A.3") modify
loc ++c
}

restore


*b)For each behavior

*viii)Percentage of exact agreement for non-missing values

foreach x of varlist m4a_a1_1-m4a_a1_4 m4a_a2_1-m4a_a2_3 m4a_b4_1-m4a_b4_4 m4a_b5_1-m4a_b5_3 m4a_b6_1 m4a_b7_1-m4a_b7_3 m4a_c8_1-m4a_c8_3 m4a_c9_1-m4a_c9_3 m4a_c10_1 m4a_c10_2 {
bys scode subject: gen agree_`x'= 1 if `x'[1]==`x'[2] & `x'[1]!=.
bys scode subject: replace agree_`x'= 0 if `x'[1]!=`x'[2] & `x'[1]!=. & `x'[2]!=.
}

tab1 agree_*

*ix) Krippalpha x) Gamma 
preserve
keep if tag==1 //to ensure no missing values
keep unq_tsubjcode scode enum_code m4a_a1_1-m4a_a1_4 m4a_a2_1-m4a_a2_3 m4a_b4_1-m4a_b4_4 m4a_b5_1-m4a_b5_3 m4a_b6_1 m4a_b7_1-m4a_b7_3 m4a_c8_1-m4a_c8_3 m4a_c9_1-m4a_c9_3 m4a_c10_1 m4a_c10_2

bys scode: replace enum_code = _n //since want to reshape for enum1 and enum2
reshape wide m4a_a1_1-m4a_a1_4 m4a_a2_1-m4a_a2_3 m4a_b4_1-m4a_b4_4 m4a_b5_1-m4a_b5_3 m4a_b6_1 m4a_b7_1-m4a_b7_3 m4a_c8_1-m4a_c8_3 m4a_c9_1-m4a_c9_3 m4a_c10_1 m4a_c10_2, i(unq_tsubjcode) j(enum_code)

foreach x of newlist m4a_a1_1-m4a_a1_4 m4a_a2_1-m4a_a2_3 m4a_b4_1-m4a_b4_4 m4a_b5_1-m4a_b5_3 m4a_b6_1 m4a_b7_1-m4a_b7_3 m4a_c8_1-m4a_c8_3 m4a_c9_1-m4a_c9_3 m4a_c10_1 m4a_c10_2 {
krippalpha `x'1 `x'2, method(ordinal)
tab `x'1 `x'2, all
kap `x'1 `x'2, wgt(w)
}

restore
*/
/*
*---------*
***GRAPHS
*---------*
*Overall*/
local c = 3
foreach x of varlist m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
tab `x', matcell(freq) matrow(names)
putexcel A`c'=matrix(names) B`c'=matrix(freq*100/r(N)) using Pakistan, sheet("Graphs") modify
local c = `c'+7
}

gen total= (m4a_a1+m4a_a2+m4a_a3+m4a_b4+m4a_b5+m4a_b6+m4a_b7+m4a_c8+m4a_c9+m4a_c10)/10

gen total1 = 100 if total>=1 & total<2
replace total1 = 0 if total1!=100 & total!=.
gen total2 = 100 if total>=2 & total<3
replace total2 = 0 if total2!=100 & total!=.
gen total3 = 100 if total>=3 & total<4
replace total3 = 0 if total3!=100 & total!=.
gen total4 = 100 if total>=4 & total<5
replace total4 = 0 if total4!=100 & total!=.

tabstat total1 total2 total3 total4, stat(mean) save
putexcel B72=matrix(r(StatTotal)') using Pakistan, sheet("Graphs") modify

*/
*********************************************************************************
* Tabs by behaviors keeping one obs if double rated for Segment 1 and 2 separately
*********************************************************************************

bysort scode subject: keep if _n==1

local c = 2
foreach x of varlist m4a_a1_1-m4a_a1_4 m4a_a2_1-m4a_a2_3 m4a_b4_1-m4a_b4_4 m4a_b5_1-m4a_b5_3 m4a_b6_1 m4a_b7_1-m4a_b7_3 m4a_c8_1-m4a_c8_3 m4a_c9_1-m4a_c9_3 m4a_c10_1 m4a_c10_2 {
tab `x', matcell(freq) matrow(names)
putexcel A`c'=matrix(names) B`c'=matrix(freq) using TEACHFactSheet, sheet("Pakistan") modify
local c = `c'+5
}



*--------------------------------------------------------------------------------
* PAPER
*-------------------------------------------------------------------------------- 
set more off, permanent 
*--------------------------------------------------------------------------------
* Classical test theory
*-------------------------------------------------------------------------------- 


alpha m4a_a1 m4a_a2 m4a_a3, asis item
alpha m4a_b4 m4a_b5 m4a_b6 m4a_b7, asis item
alpha m4a_c8 m4a_c9 m4a_c10, asis item
alpha m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, asis item

pwcorr m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, sig star(.05)
tabstat m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, stats (mean sd n min max) col(statistics)
	
*-------------------------------------------------------------------------------- 
* Exploratory Factor analysis to see how many factors might emerge, 
* 	as everything seems to be correlated with each other. 

	mean (m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10)
	factor m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, blanks (.3)
	** Exploratory Factor Analytic Scree Plot
	screeplot, lcolor(black) mcolor(black) yline(1, lcolor(black)) ///
			title("Scree Plot of Eigenvalues", color(black)) graphregion(color(white)) ///
			bgcolor(white) yla(0(1)6) xla(0(1)11) xtitle(Dimension Number) 
	factor m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, factors(3) blanks (.3)		
	factor m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, factors(1) blanks (.3)

** PCA Scree Plot	
	pca m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10
	screeplot, lcolor(black) mcolor(black) ///
		title("") graphregion(color(white)) ///
		bgcolor(white) yla(0(1)6, nogrid) xla(0(1)11) xtitle(Dimension Number) 
	
	
** PCA Scree Plot, Unstandardized
	pca m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, covar
	screeplot, lcolor(black) mcolor(black) ///
		title("") graphregion(color(white)) ///
		bgcolor(white) yla(, nogrid) xla(0(1)11) xtitle(Dimension Number)
		
*-------------------------------------------------------------------------------- 
*	Confirmatory Factor analysis	

sem (Classcul -> m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10)
sem, stand
estat gof, stats(all)


*3 factor CFA
sem (Classcul -> m4a_a1 m4a_a2 m4a_a3) (Inst -> m4a_b4 m4a_b5 m4a_b6 m4a_b7) (Socemo-> m4a_c8 m4a_c9 m4a_c10) 
sem, stand
estat gof, stats(all) // gof stats meet cutoffs to some extent, ok fit


*--------------------------------------------------------------------------------
* GRM Polytmous IRT for TEACH
*-------------------------------------------------------------------------------- 	
set more off
irt grm m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10
predict theta, latent // Theta estimates

ereturn list
matrix list e(b)
*spit out all coefficients
matrix define COEFS = e(b)'
*store in a matrix called coefficients, aposotrophe transposes 60 columns single row. 
svmat COEFS

putexcel set "IRT", replace

* Column 1, item labels
putexcel A1=("Item Code"), border(bottom)
local c = 2
foreach x of varlist m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
	putexcel A`c' = ("`x'")
	local c = `c'+1
}

* Column 2, discrimination estimates
putexcel B1 = ("Discrimination Parameter Estimates"), border(bottom)
putexcel B2 = matrix(COEFS[1..10,1]), nformat(number_d2)

* Columns 3-7, location estimates 10x1, 4x8, 3x2
putexcel C1 = ("Location Parameter Estimates"), border(bottom)
putexcel C2 = matrix(COEFS[11..14,1]'/COEFS[1]), nformat(number_d2)
putexcel C3 = matrix(COEFS[15..18,1]'/COEFS[1]), nformat(number_d2)
putexcel C4 = matrix(COEFS[19..22,1]'/COEFS[1]), nformat(number_d2)
putexcel C5 = matrix(COEFS[23..26,1]'/COEFS[1]), nformat(number_d2)
putexcel C6 = matrix(COEFS[27..30,1]'/COEFS[1]), nformat(number_d2)
putexcel C7 = matrix(COEFS[31..34,1]'/COEFS[1]), nformat(number_d2)
putexcel C8 = matrix(COEFS[35..38,1]'/COEFS[1]), nformat(number_d2)
putexcel C9 = matrix(COEFS[39..42,1]'/COEFS[1]), nformat(number_d2)
putexcel C10 = matrix(COEFS[43..46,1]'/COEFS[1]), nformat(number_d2)
putexcel C11 = matrix(COEFS[47..50,1]'/COEFS[1]), nformat(number_d2)


*difference betwen slope intercept form and discrimination/difficulty form. 
*IRT spits our discrimination and difficulty, but actually spitting out slope intercept. 
*each location paramater BY its descrimination parameter. 

**FIGURE 2
**Combined Category characteristic curves
irtgraph icc m4a_a1, lcolor(black) title("m4a_a1", color(black)) legend(off) graphregion(color(white)) name(m4a_a1,replace)
irtgraph icc m4a_a2, lcolor(black) title("m4a_a2", color(black)) legend(off) graphregion(color(white)) name(m4a_a2,replace)
irtgraph icc m4a_a3, lcolor(black) title("m4a_a3", color(black)) legend(off) graphregion(color(white)) name(m4a_a3,replace)
irtgraph icc m4a_b4, lcolor(black) title("m4a_b4", color(black)) legend(off) graphregion(color(white)) name(m4a_b4,replace)
irtgraph icc m4a_b5, lcolor(black) title("m4a_b5", color(black)) legend(off) graphregion(color(white)) name(m4a_b5,replace)
irtgraph icc m4a_b6, lcolor(black) title("m4a_b6", color(black)) legend(off) graphregion(color(white)) name(m4a_b6,replace)
irtgraph icc m4a_b7, lcolor(black) title("m4a_b7", color(black)) legend(off) graphregion(color(white)) name(m4a_b7,replace)
irtgraph icc m4a_c8, lcolor(black) title("m4a_c8", color(black)) legend(off) graphregion(color(white)) name(m4a_c8,replace)
irtgraph icc m4a_c9, lcolor(black) title("m4a_c9", color(black)) legend(off) graphregion(color(white)) name(m4a_c9,replace)
irtgraph icc m4a_c10, lcolor(black) title("m4a_c10", color(black)) legend(off) graphregion(color(white)) name(m4a_c10,replace)

gr combine m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, graphregion(color(white)) ///
	saving("Combined ICCs", replace)
	
**FIGURE 2
**Combined Boundary characteristic curves
irtgraph icc m4a_a1, bcc lcolor(black) title("m4a_a1", color(black)) legend(off) graphregion(color(white)) name(m4a_a1,replace)
irtgraph icc m4a_a2, bcc lcolor(black) title("m4a_a2", color(black)) legend(off) graphregion(color(white)) name(m4a_a2,replace)
irtgraph icc m4a_a3, bcc lcolor(black) title("m4a_a3", color(black)) legend(off) graphregion(color(white)) name(m4a_a3,replace)
irtgraph icc m4a_b4, bcc lcolor(black) title("m4a_b4", color(black)) legend(off) graphregion(color(white)) name(m4a_b4,replace)
irtgraph icc m4a_b5, bcc lcolor(black) title("m4a_b5", color(black)) legend(off) graphregion(color(white)) name(m4a_b5,replace)
irtgraph icc m4a_b6, bcc lcolor(black) title("m4a_b6", color(black)) legend(off) graphregion(color(white)) name(m4a_b6,replace)
irtgraph icc m4a_b7, bcc lcolor(black) title("m4a_b7", color(black)) legend(off) graphregion(color(white)) name(m4a_b7,replace)
irtgraph icc m4a_c8, bcc lcolor(black) title("m4a_c8", color(black)) legend(off) graphregion(color(white)) name(m4a_c8,replace)
irtgraph icc m4a_c9, bcc lcolor(black) title("m4a_c9", color(black)) legend(off) graphregion(color(white)) name(m4a_c9,replace)
irtgraph icc m4a_c10, bcc lcolor(black) title("m4a_c10", color(black)) legend(off) graphregion(color(white)) name(m4a_c10,replace)

gr combine m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10, graphregion(color(white)) ///
	saving("Combined ICCs", replace)

irtgraph iif
irtgraph tif, se

*-----------
*Item Map
*-----------

keep m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10
drop if _n>10
xpose, clear varname
drop if _n == 1


forvalues i=1/10 {
	gen num`i' = `i'
}

gen labels = ""
replace labels = "VeryLow" if _n == 1
replace labels = "Low" if _n == 2
replace labels = "Medium" if _n == 3
replace labels = "High" if _n == 4
replace labels = "VeryHigh" if _n == 5

#delimit ;
graph twoway
	(scatter num10 v1, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num9 v2, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num8 v3, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num7 v4, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num6 v5, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num5 v6, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num4 v7, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num3 v8, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num2 v9, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num1 v10, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5)),
	ylabel(1 "m4a_a1" 
	2 "m4a_a2"
	3 "m4a_a3"
	4 "m4a_b4"
	5 "m4a_b5"
	6 "m4a_b6"
	7 "m4a_b7"
	8 "m4a_c8"
	9 "m4a_c9"
	10 "m4a_c10", tlength(0) ang(h) labsize(v. Tiny))
	xlabel(1(1)5) legend(off) xtitle(Theta) graphregion(color(white))
	saving("Item Map", replace)
;
#delimit cr


*********************
*G-Study 
**********************

keep scode enum_code grade subject m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10
sort scode enum_code  

foreach x of numlist 1/3 {
rename m4a_a`x' e`x'
}
foreach x of numlist 4/7 {
rename m4a_b`x' e`x'
}
foreach x of numlist 8/10 {
rename m4a_c`x' e`x'
}

/*test if simplifying helps convergence
duplicates tag SchoolID TeacherID, gen(tag)
keep if tag==3
keep if segment==1
*/

gen row_no = _n
reshape long e, i(row_no) j(item)
drop row_no
rename e score
rename enum_code rater

rename scode person

sort person rater item
order person grade subject item rater score
export excel using "TEACH_Pakistan_Data_Long_GStudy", firstrow(variables) replace

capture egen pXi = group(person item)  
capture egen pXr = group(person rater)
capture egen iXr = group(item rater)

*mixed score || _all:R.person || _all:R.item || _all:R.rater || _all:R.pXi || _all:R.pXr || _all:R.iXr, variance reml


*-----------------------------------------------------------------------------------
*
* Analysis of Variance (ANOVA) approach to a Generalizability Study
*
*-----------------------------------------------------------------------------------	

*Exploratory data analysis
sum score
hist score

egen p_mscore= mean(score), by(person)
label variable p_mscore "Average Person Score"
sum p_mscore
hist p_mscore

egen i_mscore= mean(score), by(item)
label variable i_mscore "Average Item Score"
sum i_mscore
hist i_mscore

egen r_mscore= mean(score), by(rater)
label variable r_mscore "Average Rater Score"
sum r_mscore
hist r_mscore

sum p_mscore i_mscore r_mscore

/*


    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
    p_mscore |     17,490    2.688107    .5095764       1.05       4.25
    i_mscore |     17,490    2.688107    .9002215   1.582619   4.469983
    r_mscore |     17,490    2.688107    .2908656   2.078788   3.841667


*/
/*
* BRUTE FORCE TECHNIQUE - NEED MAXVAR >2048
anova score person item rater person#item person#rater item#rater

* Number of persons and items and raters
local n_p = e(df_1)+1
local n_i = e(df_2)+1
local n_r = e(df_3)+1

* Retrieving mean squares as SS/df
local MS_p = e(ss_1)/e(df_1)
local MS_i = e(ss_2)/e(df_2)
local MS_r = e(ss_3)/e(df_3)
local MS_pi = e(ss_4)/e(df_4)
local MS_pr = e(ss_5)/e(df_5)
local MS_ir = e(ss_6)/e(df_6)
local MS_pire = e(rss)/e(df_r)

* Estimating variance components from expected mean square equations
local sig2e = `MS_pire'
local sig2pi = (`MS_pi'-`MS_pire')/`n_r'
local sig2pr = (`MS_pr'-`MS_pire')/`n_i'
local sig2ir = (`MS_ir'-`MS_pire')/`n_p'
local sig2p = (`MS_p'-`MS_pi'-`MS_pr'+`MS_pire')/(`n_i'*`n_r')
local sig2i = (`MS_i'-`MS_pi'-`MS_ir'+`MS_pire')/(`n_p'*`n_r')
local sig2r = (`MS_r'-`MS_pr'-`MS_ir'+`MS_pire')/(`n_p'*`n_i')

di `sig2p'
di `sig2i'
di `sig2r'
di `sig2pi'
di `sig2pr'
di `sig2ir'
di `sig2e'
*/
*********************************************************************************
* Reliability Statistics with average of rater observations for double rated
*********************************************************************************
use "M4AB_Classroom Observation.dta"

rename m4a_a1 e1
rename m4a_a2 e2
rename m4a_a3 e3
rename m4a_b4 e4
rename m4a_b5 e5
rename m4a_b6 e6
rename m4a_b7 e7
rename m4a_c8 e8
rename m4a_c9 e9
rename m4a_c10 e10

foreach x of numlist 1/10 {
bysort scode subject: egen avg_elem`x'=mean(e`x')
}

save average_module2.dta, replace


*********************************************************************************
* Classical test theory
*********************************************************************************
sum avg_elem1 avg_elem2 avg_elem3 avg_elem4 avg_elem5 avg_elem6 avg_elem7 avg_elem8 avg_elem9 avg_elem10
alpha avg_elem1 avg_elem2 avg_elem3, asis item
alpha avg_elem4 avg_elem5 avg_elem6 avg_elem7, asis item
alpha avg_elem8 avg_elem9 avg_elem10, asis item
alpha avg_elem1 avg_elem2 avg_elem3 avg_elem4 avg_elem5 avg_elem6 avg_elem7 avg_elem8 avg_elem9 avg_elem10, asis item

pwcorr avg_elem1 avg_elem2 avg_elem3 avg_elem4 avg_elem5 avg_elem6 avg_elem7 avg_elem8 avg_elem9 avg_elem10, sig star(.05)
tabstat avg_elem1 avg_elem2 avg_elem3 avg_elem4 avg_elem5 avg_elem6 avg_elem7 avg_elem8 avg_elem9 avg_elem10, stats (mean sd n min max) col(statistics)
	
*-**********************************************************************
*Exploratory Factor analysis to see how many factors might emerge, 
*as everything seems to be correlated with each other
************************************************************************

	mean (avg_elem1 avg_elem2 avg_elem3 avg_elem4 avg_elem5 avg_elem6 avg_elem7 avg_elem8 avg_elem9 avg_elem10)
	factor avg_elem1 avg_elem2 avg_elem3 avg_elem4 avg_elem5 avg_elem6 avg_elem7 avg_elem8 avg_elem9 avg_elem10, blanks (.3)
	** Exploratory Factor Analytic Scree Plot
	screeplot, lcolor(black) mcolor(black) yline(1, lcolor(black)) ///
			title("Scree Plot of Eigenvalues", color(black)) graphregion(color(white)) ///
			bgcolor(white) yla(0(1)6) xla(0(1)11) xtitle(Dimension Number) 
	factor avg_elem1 avg_elem2 avg_elem3 avg_elem4 avg_elem5 avg_elem6 avg_elem7 avg_elem8 avg_elem9 avg_elem10, factors(3) blanks (.3)		
	factor avg_elem1 avg_elem2 avg_elem3 avg_elem4 avg_elem5 avg_elem6 avg_elem7 avg_elem8 avg_elem9 avg_elem10, factors(1) blanks (.3)

***********************************
*Confirmatory Factor analysis	
************************************

sem (Classcul -> avg_elem1 avg_elem2 avg_elem3 avg_elem4 avg_elem5 avg_elem6 avg_elem7 avg_elem8 avg_elem9 avg_elem10)
sem, stand
estat gof, stats(all)


*3 factor CFA
sem (Classcul -> avg_elem1 avg_elem2 avg_elem3) (Inst -> avg_elem4 avg_elem5 avg_elem6 avg_elem7) (Socemo-> avg_elem8 avg_elem9 avg_elem10) 
sem, stand
estat gof, stats(all) 
