*********************************************************************************
* TEACH Study
* Programming:
*   Stata Version:  Stata 14.2 
*   Original Authors: Syeda Farwa Fatima
*   Last Modified: October 15th, 2018
* 	Notes: Reliability Analyses for TEACH Pakistan 
*********************************************************************************
* Clearing
clear 
set more off
  
* Local directory:
cd "/Users/Farwa/Documents/SABER_WB/Pakistan" 
use TeacherData_M1-M2-M4AB-M6_ForTEACH
keep if teacherdata_module == 2 


*--------------------------------------------------------------------------------
* Data Cleaning
*-------------------------------------------------------------------------------- 
* for observer1
gen teach=(m4ab_a1_obs1+m4ab_a2_obs1+m4ab_a3_obs1+m4ab_b4_obs1+m4ab_b5_obs1+m4ab_b6_obs1+m4ab_b7_obs1+m4ab_c8_obs1+m4ab_c9_obs1+m4ab_c10_obs1)/10
gen F1=(m4ab_a1_obs1+m4ab_a2_obs1+m4ab_a3_obs1)/3
gen F2=(m4ab_b4_obs1+m4ab_b5_obs1+m4ab_b6_obs1+m4ab_b7_obs1)/4
gen F3=(m4ab_c8_obs1+m4ab_c9_obs1+m4ab_c10_obs1)/3
sum teach F1 F2 F3

* for observer2
gen teach_2=(m4ab_a1_obs2+m4ab_a2_obs2+m4ab_a3_obs2+m4ab_b4_obs2+m4ab_b5_obs2+m4ab_b6_obs2+m4ab_b7_obs2+m4ab_c8_obs2+m4ab_c9_obs2+m4ab_c10_obs2)/10
gen F1_2=(m4ab_a1_obs2+m4ab_a2_obs2+m4ab_a3_obs2)/3
gen F2_2=(m4ab_b4_obs2+m4ab_b5_obs2+m4ab_b6_obs2+m4ab_b7_obs2)/4
gen F3_2=(m4ab_c8_obs2+m4ab_c9_obs2+m4ab_c10_obs2)/3
sum teach_2 F1_2 F2_2 F3_2

* average of observer1 and observer 2
foreach x of numlist 1/3 {
gen avge`x'=(m4ab_a`x'_obs1+m4ab_a`x'_obs2)/2
}
foreach x of numlist 4/7 {
gen avge`x'=(m4ab_b`x'_obs1+m4ab_b`x'_obs2)/2
}
foreach x of numlist 8/10 {
gen avge`x'=(m4ab_c`x'_obs1+m4ab_c`x'_obs2)/2
}

gen teach_avg=(avge1+avge2+avge3+avge4+avge5+avge6+avge7+avge8+avge9+avge10)/10
gen F1_avg=(avge1+avge2+avge3)/3
gen F2_avg=(avge4+avge5+avge6+avge7)/4
gen F3_avg=(avge8+avge9+avge10)/3
sum teach_avg F1_avg F2_avg F3_avg

*Label variables 
label variable m4ab_a1_obs1 "Supportive Learning Environment"
label variable m4ab_a2_obs1 "Positive Behavioral Expectations"
label variable m4ab_a3_obs1 "Opportunities to Learn"
label variable m4ab_b4_obs1 "Lesson Facilitation"
label variable m4ab_b5_obs1 "Checks for Understanding"
label variable m4ab_b6_obs1 "Feedback"
label variable m4ab_b7_obs1 "Critical Thinking"
label variable m4ab_c8_obs1 "Autonomy"
label variable m4ab_c9_obs1 "Perseverance"
label variable m4ab_c10_obs1 "Social and Collaborative Skills"

*--------------------------------------------------------------------------------
* Item Analysis
*-------------------------------------------------------------------------------- 
*Descriptives
sum m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1
tabstat m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1, stats (mean sd n min max skewness kurtosis) col(statistics)

*Tabulations
putexcel set Tabs_Pak_1, replace
tab1 m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1
tab1 m4ab_a1_1_obs1 m4ab_a1_2_obs1 m4ab_a1_3_obs1 m4ab_a1_4_obs1  m4ab_a2_1_obs1 m4ab_a2_2_obs1 m4ab_a2_3_obs1  m4ab_b4_1_obs1 m4ab_b4_2_obs1 m4ab_b4_3_obs1 m4ab_b4_4_obs1  m4ab_b5_1_obs1 m4ab_b5_2_obs1 m4ab_b5_3_obs1  m4ab_b6_1_obs1  m4ab_b7_1_obs1 m4ab_b7_2_obs1 m4ab_b7_3_obs1  m4ab_c8_1_obs1 m4ab_c8_2_obs1 m4ab_c8_3_obs1  m4ab_c9_1_obs1 m4ab_c9_2_obs1 m4ab_c9_3_obs1  m4ab_c10_1_obs1 m4ab_c10_2_obs1, nolabel

*Areas
sum teach F1 F2 F3
tab1 teach F1 F2 F3

foreach x of varlist teach F1 F2 F3 {
gen `x'_c1=1 if `x'>=1 & `x'<=2
gen `x'_c2=1 if `x'>2 & `x'<=3
gen `x'_c3=1 if `x'>3 & `x'<=4
gen `x'_c4=1 if `x'>4 & `x'<=5
}

foreach x of varlist teach F1 F2 F3 {
egen `x'_cat1=sum(`x'_c1)
egen `x'_cat2=sum(`x'_c2)
egen `x'_cat3=sum(`x'_c3)
egen `x'_cat4=sum(`x'_c4)
}
foreach x in teach F1 F2 F3 {
drop `x'_c1 `x'_c2 `x'_c3 `x'_c4
}
foreach x of varlist teach F1 F2 F3 {
replace `x'_cat1=(`x'_cat1/845)*100
replace `x'_cat2=(`x'_cat2/845)*100
replace `x'_cat3=(`x'_cat3/845)*100
replace `x'_cat4=(`x'_cat4/845)*100
}

tabstat teach_cat1 teach_cat2 teach_cat3 teach_cat4, stat(mean)  
tabstat F1_cat1 F1_cat2 F1_cat3 F1_cat4, stat(mean)  
tabstat F2_cat1 F2_cat2 F2_cat3 F2_cat4, stat(mean)  
tabstat F3_cat1 F3_cat2 F3_cat3 F3_cat4, stat(mean)  

*Elements
local c = 3
foreach x of varlist m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1 {
tab `x', matcell(freq) matrow(names) 
putexcel D`c'=matrix(names) E`c'=matrix(freq/r(N)*100) 
local c=`c'+6
}
*Behaviors
local c = 3
foreach x of varlist m4ab_a1_1_obs1 m4ab_a1_2_obs1 m4ab_a1_3_obs1 m4ab_a1_4_obs1  m4ab_a2_1_obs1 m4ab_a2_2_obs1 m4ab_a2_3_obs1  m4ab_b4_1_obs1 m4ab_b4_2_obs1 m4ab_b4_3_obs1 m4ab_b4_4_obs1  m4ab_b5_1_obs1 m4ab_b5_2_obs1 m4ab_b5_3_obs1  m4ab_b6_1_obs1  m4ab_b7_1_obs1 m4ab_b7_2_obs1 m4ab_b7_3_obs1  m4ab_c8_1_obs1 m4ab_c8_2_obs1 m4ab_c8_3_obs1  m4ab_c9_1_obs1 m4ab_c9_2_obs1 m4ab_c9_3_obs1  m4ab_c10_1_obs1 m4ab_c10_2_obs1 {
tab `x', matcell(freq) matrow(names) 
putexcel G`c'=matrix(names) H`c'=matrix(freq/r(N)*100) 
local c=`c'+5
}

*Histograms		
foreach x of varlist m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1 teach {
hist `x', percent width(.8) color(grey*0.4) xscale(range(1 5)) yscale(range(0 65)) xlabel(1(1)5) ylabel(0(10)65) name(`x', replace)
}
gr combine m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1 teach, graphregion(color(white)) ///
	saving("Element_Overall_Distributions", replace)	
	
*--------------------------------------------------------------------------------
* Inter-Rater Reliability Stats
*--------------------------------------------------------------------------------
*Percentage of exact agreement within one (+/- one point) for non-missing values 
gen agree_1_teach= 1 if abs(teach-teach_2)<=1 & teach!=. & teach_2!=.
replace agree_1_teach= 0 if abs(teach-teach_2)>1 & teach!=. & teach_2!=.
*Percentage of exact agreement within 0.5 (+/- one point) for non-missing values 
gen agree_half_teach= 1 if abs(teach-teach_2)<=.5 & teach!=. & teach_2!=.
replace agree_half_teach= 0 if abs(teach-teach_2)>.5 & teach!=. & teach_2!=.
*Percentage of exact agreement within 0.25 (+/- one point) for non-missing values 
gen agree_quar_teach= 1 if abs(teach-teach_2)<=.25 & teach!=. & teach_2!=.
replace agree_quar_teach= 0 if abs(teach-teach_2)>.25 & teach!=. & teach_2!=.
*Percentage of exact agreement non-missing values 
gen agree_teach= 1 if abs(teach-teach_2)<=0 & teach!=. & teach_2!=.
replace agree_teach= 0 if abs(teach-teach_2)>0 & teach!=. & teach_2!=.

tab1 agree_teach agree_1_teach agree_half_teach agree_quar_teach agree_teach
*agree_1_teach (97.25) agree_half_teach (86.61) agree_quar_teach (57.13) agree_teach (15.61)
*--------------------------------------------------------------------------------
* Dimensionality
*-------------------------------------------------------------------------------- 	
	
*Correlations
pwcorr teach F1 F2 F3
pwcorr m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1, sig star(.05)

*Internal consistency
alpha m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1, asis item
alpha m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1, asis item
alpha m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1, asis item
alpha m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1, asis item

*-------------------------------------------------------------------------------- 
* Exploratory Factor analysis to see how many factors might emerge, 
* 	as everything seems to be correlated with each other. 
*-------------------------------------------------------------------------------- 
factor m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1, blanks (.3)
rotate, blanks(0.3)
rotate, promax blanks(0.3)
screeplot, lcolor(black) mcolor(black) /*yline(1, lcolor(black))*/ ///
			title("Scree Plot of Eigenvalues", color(black)) graphregion(color(white)) ///
			bgcolor(white) yla(0(1)6) xla(0(1)10) xtitle(Dimension Number) ///
			saving("ScreePlot_EFA", replace)
factor m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1, factor(1) blanks (.2)
rotate, blanks(0.3)
rotate, promax blanks(0.3)

pca m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1
	screeplot, lcolor(black) mcolor(black) ///
		title("") graphregion(color(white)) ///
		bgcolor(white) yla(0(1)6, nogrid) xla(0(1)11) xtitle(Dimension Number) 
 
*-------------------------------------------------------------------------------- 
*	Confirmatory Factor analysis
*-------------------------------------------------------------------------------- 
*1 factor CFA	
sem (Teach -> m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1) 
sem, stand
estat gof, stats(all) // gof stats meet cutoffs to some extent, ok fit  chi2(35)  =    145.91, Prob > chi2 = 0.0000

*3 factor CFA
sem (Classcul -> m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1) (Inst -> m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1) (Socemo-> m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1) 
sem, stand
estat gof, stats(all) // gof stats meet cutoffs to some extent, ok fit   chi2(32)  =     98.51, Prob > chi2 = 0.0000

*LR test= 47.4, 3
display chi2tail(47.4, 3) // gives p-value, is significant, go with complex model, 3-factor

*--------------------------------------------------------------------------------
* GRM Polytmous IRT for TEACH
*-------------------------------------------------------------------------------- 	
set more off
irt grm m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1
predict theta, latent


**FIGURE 3
**Combined Category characteristic curves
irtgraph icc m4ab_a1_obs1, lcolor(black) title("Supportive Learning Environment", color(black)) legend(off) graphregion(color(white)) name(m4ab_a1_obs1,replace)
irtgraph icc m4ab_a2_obs1, lcolor(black) title("Positive Behavioral Expectations", color(black)) legend(off) graphregion(color(white)) name(m4ab_a2_obs1,replace)
irtgraph icc m4ab_a3_obs1, lcolor(black) title("Opportunities to Learn", color(black)) legend(off) graphregion(color(white)) name(m4ab_a3_obs1,replace)
irtgraph icc m4ab_b4_obs1, lcolor(black) title("Lesson Facilitation", color(black)) legend(off) graphregion(color(white)) name(m4ab_b4_obs1,replace)
irtgraph icc m4ab_b5_obs1, lcolor(black) title("Checks for Understanding", color(black)) legend(off) graphregion(color(white)) name(m4ab_b5_obs1,replace)
irtgraph icc m4ab_b6_obs1, lcolor(black) title("Feedback", color(black)) legend(off) graphregion(color(white)) name(m4ab_b6_obs1,replace)
irtgraph icc m4ab_b7_obs1, lcolor(black) title("Critical Thinking", color(black)) legend(off) graphregion(color(white)) name(m4ab_b7_obs1,replace)
irtgraph icc m4ab_c8_obs1, lcolor(black) title("Autonomy", color(black)) legend(off) graphregion(color(white)) name(m4ab_c8_obs1,replace)
irtgraph icc m4ab_c9_obs1, lcolor(black) title("Perseverance", color(black)) legend(off) graphregion(color(white)) name(m4ab_c9_obs1,replace)
irtgraph icc m4ab_c10_obs1, lcolor(black) title("Social and Collaborative Skills", color(black)) legend(off) graphregion(color(white)) name(m4ab_c10_obs1,replace)

gr combine m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1, graphregion(color(white)) ///
	saving("Combined ICCs", replace)
	
**FIGURE 4
**Combined Boundary characteristic curves
irtgraph icc m4ab_a1_obs1, bcc lcolor(black) title("Supportive Learning Environment", color(black)) legend(off) graphregion(color(white)) name(m4ab_a1_obs1,replace)
irtgraph icc m4ab_a2_obs1, bcc lcolor(black) title("Positive Behavioral Expectations", color(black)) legend(off) graphregion(color(white)) name(m4ab_a2_obs1,replace)
irtgraph icc m4ab_a3_obs1, bcc lcolor(black) title("Opportunities to Learn", color(black)) legend(off) graphregion(color(white)) name(m4ab_a3_obs1,replace)
irtgraph icc m4ab_b4_obs1, bcc lcolor(black) title("Lesson Facilitation", color(black)) legend(off) graphregion(color(white)) name(m4ab_b4_obs1,replace)
irtgraph icc m4ab_b5_obs1, bcc lcolor(black) title("Checks for Understanding", color(black)) legend(off) graphregion(color(white)) name(m4ab_b5_obs1,replace)
irtgraph icc m4ab_b6_obs1, bcc lcolor(black) title("Feedback", color(black)) legend(off) graphregion(color(white)) name(m4ab_b6_obs1,replace)
irtgraph icc m4ab_b7_obs1, bcc lcolor(black) title("Critical Thinking", color(black)) legend(off) graphregion(color(white)) name(m4ab_b7_obs1,replace)
irtgraph icc m4ab_c8_obs1, bcc lcolor(black) title("Autonomy", color(black)) legend(off) graphregion(color(white)) name(m4ab_c8_obs1,replace)
irtgraph icc m4ab_c9_obs1, bcc lcolor(black) title("Perseverance", color(black)) legend(off) graphregion(color(white)) name(m4ab_c9_obs1,replace)
irtgraph icc m4ab_c10_obs1, bcc lcolor(black) title("Social and Collaborative Skills", color(black)) legend(off) graphregion(color(white)) name(m4ab_c10_obs1,replace)

gr combine m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1, graphregion(color(white)) ///
	saving("Combined BCCs", replace)

irtgraph iif, range(-6 6) n(10000) data("ItemInformationFunctions.dta",replace) saving("Item Information Functions", replace)
irtgraph tif, lcolor(black) se  (lcolor(grey*0.6)) range(-3 3) ylabel(0(0.25)1) saving("Test Information and Standard Error", replace)

/*
*Compare information and SE of 10 item scale with 9 item scale
use "ItemInformationFunctions.dta", clear

egen tif10 = rowtotal(iif1-iif10)
egen tif9 = rowtotal(iif1-iif9)

gen cse10 = 1/sqrt(tif10)
gen cse9 = 1/sqrt(tif9)

#delimit ;
graph twoway 
	(scatter tif10 theta, msymbol(i) connect(l) lcolor(black))
	(scatter tif9 theta, msymbol(i) connect(l) lcolor(gs8)),
	legend(order(1 2) label(1 "10 Items") label(2 "9 Items") row(1)) graphregion(color(white))
	plotregion(margin(none))
	ylabel(,nogrid)
	ytitle(Test Information Functions) xtitle(Theta (Score Scale))
	saving("10itemvs9item_TIF.gph", replace)
	;
#delimit cr


#delimit ;
graph twoway 
	(scatter cse10 theta if cse10 < 1 & theta > -3 & theta < 3, msymbol(i) connect(l) lcolor(black))
	(scatter cse9 theta if cse9 < 1 & theta > -3 & theta < 3, msymbol(i) connect(l) lcolor(gs8)),
	legend(order(1 2) label(1 "10 Items") label(2 "9 Items") row(1)) graphregion(color(white))
	plotregion(margin(none))
	ylabel(0(.2)1,nogrid) xla(-3(1)3)
	ytitle(Conditional Standard Error of Measurement) xtitle(Theta (Score Scale))
	saving("10itemvs9item_SE.gph", replace)
	;
#delimit cr
*/

*Predict theta_2 from second set of observations
irt grm m4ab_a1_obs2 m4ab_a2_obs2 m4ab_a3_obs2 m4ab_b4_obs2 m4ab_b5_obs2 m4ab_b6_obs2 m4ab_b7_obs2 m4ab_c8_obs2 m4ab_c9_obs2 m4ab_c10_obs2
predict theta_2, latent

*--------------------------------------------------------------------------------
* Concurrent Validity 
*-------------------------------------------------------------------------------- 	
*Clean up control variables
rename m1a_teacher_sex m1a_teacher_male
gen teacher_age=m1a_teacher_age
replace teacher_age=. if m1a_teacher_age==-98
gen teacher_educ_coll_uni=0 if m6c_c1==3 | m6c_c1==4 | m6c_c1==5
replace teacher_educ_coll_uni=1 if m6c_c1==6 | m6c_c1==7 | m6c_c1==8 | m6c_c1==9
gen teacher_experience=2018-m6c_a2
gen teacher_exp_morethan2=1 if teacher_experience >=2 & teacher_experience!=.
replace teacher_exp_morethan2=0 if teacher_experience<2
gen teacher_status_govtp=1 if m1a_teacher_contract==1 
replace teacher_status_govtp=0 if m1a_teacher_contract!=1 & m1a_teacher_contract!=.
gen teacher_status_govtc=1 if m1a_teacher_contract==2
replace teacher_status_govtc=0 if m1a_teacher_contract!=2 & m1a_teacher_contract!=.
gen teacher_status_privatec=1 if m1a_teacher_contract==3
replace teacher_status_privatec=0 if m1a_teacher_contract!=3 & m1a_teacher_contract!=.
gen teacher_status_temp=1 if m1a_teacher_contract==4
replace teacher_status_temp=0 if m1a_teacher_contract!=4 & m1a_teacher_contract!=.
rename m6c_num2 class_size
*Class environment
gen hygiene=1 if m4ab_1_obs1==1 | m4ab_1_obs1==2
replace hygiene=0 if m4ab_1_obs1==3 
rename m4ab_2_obs1 blackboard
rename m4ab_3_obs1 chalk_blackboard
rename m4ab_4_obs1 teacher_desk
rename m4ab_5_obs1 teacher_chair
rename m4ab_6_obs1 lights_fans
rename m4ab_7_obs1 student_desk_chair
rename m4ab_11_obs1 no_stud_textbook
destring no_stud_textbook, replace
gen stud_textbook=1 if no_stud_textbook/class_size>=0.8 & no_stud_textbook!=. & class_size!=.
replace stud_textbook=0 if no_stud_textbook/class_size<0.8
gen no_stud_pen=m4ab_14a_obs1+m4ab_14b_obs1
gen stud_pen=1 if no_stud_pen/class_size>=0.8 & no_stud_pen!=. & class_size!=.
replace stud_pen=0 if no_stud_pen/class_size<0.8
gen no_stud_exbook=m4ab_15a_obs1+m4ab_15b_obs1
gen stud_exbook=1 if no_stud_exbook/class_size>=0.8 & no_stud_exbook!=. & class_size!=.
replace stud_exbook=0 if no_stud_exbook/class_size<0.8
rename m4ab_16_obs1 loud_noise
rename m4ab_17_obs1 class_lighting
rename m4ab_18_obs1 blackboard_lighting


sum m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience teacher_exp_morethan2 m1a_teacher_contract teacher_status_govtp teacher_status_govtc teacher_status_privatec teacher_status_temp
tab1  m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience teacher_exp_morethan2 m1a_teacher_contract teacher_status_govtp teacher_status_govtc teacher_status_privatec teacher_status_temp

sum hygiene blackboard chalk_blackboard teacher_desk teacher_chair lights_fans student_desk_chair stud_textbook stud_pen stud_exbook loud_noise class_lighting blackboard_lighting


gen total_6A_perc_score=0.5*math_6A_perc_score+0.25*urdu_6A_perc_score+0.25*eng_6A_perc_score
gen total_6B_perc_score=0.5*math_6B_perc_score+0.25*urdu_6B_perc_score+0.25*eng_6B_perc_score

*Standardize teacher scores
foreach x of varlist total_6A_perc_score total_6B_perc_score math_6A_perc_score math_6B_perc_score urdu_6A_perc_score urdu_6B_perc_score eng_6A_perc_score eng_6B_perc_score {
egen z`x'=std(`x')
}

merge 1:m scode using StudentData_M1B_M2_M4C_M5_M7--M5StudentAssessment

*Descriptives
sum total_6A_perc_score total_6B_perc_score math_6A_perc_score math_6B_perc_score urdu_6A_perc_score urdu_6B_perc_score eng_6A_perc_score eng_6B_perc_score
sum m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience

*Correlations
pwcorr teach F1 F2 F3 math_6A_perc_score math_6B_perc_score if m4ab_subject_obs1==1, sig star(0.05)
pwcorr teach F1 F2 F3 urdu_6A_perc_score urdu_6B_perc_score if m4ab_subject_obs1==2, sig star(0.05)
pwcorr teach F1 F2 F3 eng_6A_perc_score eng_6B_perc_score if m4ab_subject_obs1==3, sig star(0.05)

* Correlations with controls
sum teach F1 F2 F3 m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1 ztotal_6A_perc_score ztotal_6B_perc_score class_size teacher_exp_morethan2 teacher_status_govtp teacher_status_temp hygiene blackboard chalk_blackboard teacher_desk teacher_chair lights_fans student_desk_chair stud_textbook stud_pen stud_exbook loud_noise class_lighting blackboard_lighting
pwcorr teach F1 F2 F3 m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1 ztotal_6A_perc_score ztotal_6B_perc_score class_size teacher_educ_coll_uni teacher_exp_morethan2 teacher_status_govtp teacher_status_temp hygiene blackboard chalk_blackboard teacher_desk teacher_chair lights_fans student_desk_chair stud_textbook stud_pen stud_exbook loud_noise class_lighting blackboard_lighting, sig star (0.05)
pwcorr teach theta F1 F2 F3 m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1 ztotal_6A_perc_score ztotal_6B_perc_score class_size teacher_educ_coll_uni teacher_exp_morethan2 teacher_status_govtp teacher_status_temp hygiene blackboard chalk_blackboard teacher_desk teacher_chair lights_fans student_desk_chair stud_textbook stud_pen stud_exbook loud_noise class_lighting blackboard_lighting, sig star (0.05)



*Regressions
erase "Concurrent.xls"
erase "Concurrent.txt"
foreach x of varlist teach theta F1 F2 F3 {
reg `x' ztotal_6A_perc_score ztotal_6B_perc_score 
outreg2 using "Concurrent.xls", append bdec(3) se label excel 

reg `x' zmath_6A_perc_score zmath_6B_perc_score if m4ab_subject_obs1==1
outreg2 using "Concurrent.xls", append bdec(3) se label excel 

reg `x' zurdu_6A_perc_score zurdu_6B_perc_score if m4ab_subject_obs1==2 
outreg2 using "Concurrent.xls", append bdec(3) se label excel 

reg `x' zeng_6A_perc_score zeng_6B_perc_score if m4ab_subject_obs1==3 
outreg2 using "Concurrent.xls", append bdec(3) se label excel 

reg `x' ztotal_6A_perc_score ztotal_6B_perc_score m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience teacher_exp_morethan2 teacher_status_govtp teacher_status_govtc teacher_status_privatec teacher_status_temp
outreg2 using "Concurrent.xls", append bdec(3) se label excel 

reg `x' zmath_6A_perc_score zmath_6B_perc_score m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience teacher_exp_morethan2 teacher_status_govtp teacher_status_govtc teacher_status_privatec teacher_status_temp if m4ab_subject_obs1==1
outreg2 using "Concurrent.xls", append bdec(3) se label excel 

reg `x' zurdu_6A_perc_score zurdu_6B_perc_score m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience teacher_exp_morethan2 teacher_status_govtp teacher_status_govtc teacher_status_privatec teacher_status_temp if m4ab_subject_obs1==2 
outreg2 using "Concurrent.xls", append bdec(3) se label excel 

reg `x' zeng_6A_perc_score zeng_6B_perc_score m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience teacher_exp_morethan2 teacher_status_govtp teacher_status_govtc teacher_status_privatec teacher_status_temp if m4ab_subject_obs1==3 
outreg2 using "Concurrent.xls", append bdec(3) se label excel 
}

*--------------------------------------------------------------------------------
* Predictive Validity 
*-------------------------------------------------------------------------------- 	

*Standardize student scores
foreach x of varlist M5_urdu_TestScore M5_math_TestScore M5_english_TestScore M5_listening_TestScore M5_AllTest_TestScore {
egen z`x'=std(`x')
}

*Clean up controls to include
rename M1B_student_sex M1B_student_male
gen student_age = M1B_student_age
replace student_age=. if M1B_student_age==-98
gen student_breakfast=M4C_c1
replace student_breakfast=. if M4C_c1==-97
rename m2_school_location_update m2_school_location_urban
gen school_private=1 if m2_a4!=1 & m2_a4!=.
replace school_private=0 if m2_a4==1


*Descriptives
sum zM5_urdu_TestScore zM5_math_TestScore zM5_english_TestScore zM5_listening_TestScore zM5_AllTest_TestScore
sum M1B_student_male student_age student_breakfast m2_school_location_urban school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience

*Correlations
pwcorr teach F1 F2 F3 zM5_math_TestScore if m4ab_subject_obs1==1, sig star(0.05)
pwcorr teach F1 F2 F3 zM5_urdu_TestScore if m4ab_subject_obs1==2, sig star(0.05)
pwcorr teach F1 F2 F3 zM5_english_TestScore if m4ab_subject_obs1==3, sig star(0.05)

*Regressions
*erase "Predictive.xls"
*erase "Predictive.txt" 

*Controls only
reg zM5_AllTest_TestScore  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size , cluster (scode)
outreg2 using "Predictive.xls", replace bdec(3) se label excel 

*reg zM5_math_TestScore  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_urdu_TestScore  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_english_TestScore  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 


foreach x of varlist teach theta F1 F2 F3 /*teach_avg F1_avg F2_avg F3_avg*/ {
reg zM5_AllTest_TestScore `x' M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_math_TestScore `x' M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_urdu_TestScore `x' M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_english_TestScore `x' M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 


*reg zM5_math_TestScore `x' M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==1, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_urdu_TestScore `x' M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==2, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_english_TestScore `x' M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==3, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 
}

* F1, F2, F3 in the same specification for unrestricted and restricted sample
reg zM5_AllTest_TestScore F1 F2 F3 M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_math_TestScore F1 F2 F3 M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_urdu_TestScore F1 F2 F3 M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_english_TestScore F1 F2 F3 M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_math_TestScore F1 F2 F3 M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==1, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_urdu_TestScore F1 F2 F3 M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==2, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_english_TestScore F1 F2 F3 M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==3, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

* F1_avg, F2_avg, F3_avg in the same specification for unrestricted and restricted sample
reg zM5_AllTest_TestScore F1_avg F2_avg F3_avg M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_math_TestScore F1_avg F2_avg F3_avg M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_urdu_TestScore F1_avg F2_avg F3_avg M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_english_TestScore F1_avg F2_avg F3_avg M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_math_TestScore F1_avg F2_avg F3_avg M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==1, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_urdu_TestScore F1_avg F2_avg F3_avg M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==2, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 

*reg zM5_english_TestScore F1_avg F2_avg F3_avg M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==3, cluster (scode)
*outreg2 using "Predictive.xls", append bdec(3) se label excel 



***IV
*erase "IV.xls" 
*erase "IV.txt"
foreach x in teach theta F1 F2 F3 {
ivreg2 zM5_AllTest_TestScore (`x'=`x'_2)  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, ffirst cluster (scode) 
outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_math_TestScore (`x'=`x'_2)  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_urdu_TestScore (`x'=`x'_2) M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_english_TestScore (`x'=`x'_2) M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_math_TestScore (`x'=`x'_2)  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==1, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_urdu_TestScore (`x'=`x'_2) M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==2, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_english_TestScore (`x'=`x'_2) M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==3, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 
}


* F1, F2, F3 in the same specification for unrestricted and restricted sample

ivreg2 zM5_AllTest_TestScore (F1 F2 F3=F1_2 F2_2 F3_2)  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, ffirst cluster (scode) 
outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_math_TestScore (F1 F2 F3=F1_2 F2_2 F3_2)  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_urdu_TestScore (F1 F2 F3=F1_2 F2_2 F3_2) M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_english_TestScore (F1 F2 F3=F1_2 F2_2 F3_2) M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_math_TestScore (F1 F2 F3=F1_2 F2_2 F3_2)  M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==1, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_urdu_TestScore (F1 F2 F3=F1_2 F2_2 F3_2) M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==2, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 

*ivreg2  zM5_english_TestScore (F1 F2 F3=F1_2 F2_2 F3_2) M1B_student_male student_age /*student_breakfast m2_school_location_urban*/ school_private m1a_teacher_male teacher_age teacher_educ_coll_uni teacher_experience class_size if m4ab_subject_obs1==3, ffirst cluster (scode)
*outreg2 using "IV.xls",  addstat("F Stat", e(cdf)) nor2 append bdec(3) se label excel 



************************
*Measurement invariance
************************
/*
*Model1 
sem (Teach -> m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1), group(m1a_teacher_male) ginvariant(none)
*Model2
sem (Teach -> m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1), group(m1a_teacher_male) gin(mcoef) //factor loadings the same; converge
*Model4
sem (Teach -> m4ab_a1_obs1 m4ab_a2_obs1 m4ab_a3_obs1 m4ab_b4_obs1 m4ab_b5_obs1 m4ab_b6_obs1 m4ab_b7_obs1 m4ab_c8_obs1 m4ab_c9_obs1 m4ab_c10_obs1), group(m1a_teacher_male) gin(mcoef mcons merrvar) //
*/
