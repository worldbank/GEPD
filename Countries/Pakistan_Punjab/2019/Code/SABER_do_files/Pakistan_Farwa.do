

*----------------------------------
* SECOND Dataset for TEACH Exercise 
* Task 1: Data Cleaning
*----------------------------------
version 13.0
clear
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

*----------------------------------
* SECOND Dataset for TEACH Exercise 
* Task 2: Data Analysis
*----------------------------------
*a)For each element

*i)Percentage of exact agreement for non-missing values
local c = 7
foreach x of varlist m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
bys scode teacher_selected subject: gen agree_`x'= 1 if `x'[1]==`x'[2] & `x'[1]!=.
bys scode teacher_selected subject: replace agree_`x'= 0 if `x'[1]!=`x'[2] & `x'[1]!=. & `x'[2]!=.
tabstat agree_`x', stat(mean) save
putexcel B`c'=matrix(r(StatTotal)) using Pakistan, sheet("Table A.3") modify
loc ++c
}

tab1 agree_*

*ii)Percentage of exact agreement or within one (+/- one point) for non-missing values 
local c = 7
foreach x of varlist m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
bys scode teacher_selected subject: gen agree2_`x'= 1 if abs(`x'[1]-`x'[2])<=1 & `x'[1]!=. & `x'[2]!=.
bys scode teacher_selected subject: replace agree2_`x'= 0 if abs(`x'[1]-`x'[2])>1 & `x'[1]!=. & `x'[2]!=.
tabstat agree2_`x', stat(mean) save
putexcel C`c'=matrix(r(StatTotal)) using Pakistan, sheet("Table A.3") modify
loc ++c
}

tab1 agree2_*

*Logic check since true by definition
foreach x of varlist m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
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
foreach x of varlist m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10 {
icc `x' unq_tsubjcode //one-way random effects model
disp as text "The ICC for Question Ô`x'Õ = " _col(30) as result %5.4f r(icc_i)
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

/*
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


alpha m4a_a1 m4a_a2 m4a_a3
putexcel C3=(r(alpha)) using Pakistan, sheet("Table A.4") modify
alpha m4a_b4 m4a_b5 m4a_b6 m4a_b7
putexcel C4=(r(alpha)) using Pakistan, sheet("Table A.4") modify
alpha m4a_c8 m4a_c9 m4a_c10
putexcel C5=(r(alpha)) using Pakistan, sheet("Table A.4") modify
alpha m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10
putexcel C6=(r(alpha)) using Pakistan, sheet("Table A.4") modify
corr m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10
putexcel B11=matrix(r(C)) using Pakistan, sheet("Table A.4") modify


*c)CFA
/*
sem (ClassInstSocemo -> m4a_a1 m4a_a2 m4a_a3 m4a_b4 m4a_b5 m4a_b6 m4a_b7 m4a_c8 m4a_c9 m4a_c10)
sem, stand
estat gof, stats(all)
*/

sem (Classcul -> m4a_a1 m4a_a2 m4a_a3) (Inst -> m4a_b4 m4a_b5 m4a_b6 m4a_b7) (Socemo-> m4a_c8 m4a_c9 m4a_c10) 
sem, stand
estat gof, stats(all) // gof stats meet cutoffs to some extent, ok fit

