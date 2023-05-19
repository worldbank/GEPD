
** Set directories

*** Individual Users ***

//Kabira
*cd "/Users/Kabira/Documents/Dropbox/Pakistan/Data"
*global data			"/Users/Kabira/Documents/Dropbox/Pakistan/Data"

//Quentin
cd "C:\Users\q.daviot\Quentin Daviot Dropbox\Quentin Daviot\Pakistan\Data"
global data			"C:\Users\q.daviot\Quentin Daviot Dropbox\Quentin Daviot\Pakistan\Data"

// Koen
cd "C:\Users\WB495813\Downloads\Data"
global data "C:\Users\WB495813\Downloads\Data"


**************************


global raw			"$data/1. Raw"

global processed	"$data/2. Processed"

global do			"$data/DO_Files"	

global figures		"$data/3. Figures"		

global tables		"$data/4. Tables"		

clear all	
set more off
capture log close
set linesize 120

log using "$do/Punjab_M5_LEAPS.usl", text replace 

/**
<h2> PUNJAB SABER ANALYSIS </h2>
*/

/**
<h2> STUDENT ANALYSIS </h2>
*/

*************************************************************************

*	Program:	Punjab SABER data (cleaning and analysis)

*	Author:		Kabira Namit and Ayesha Tahir           
    		
*	Email:		knamit@worldbank.org & ayesha.tahir44@gmail.com
	
*	Created:	July 2018	
			    	
*	Updated:	July 25, 2018
					 			
*************************************************************************



*** Install necessary code for new graphs

*ssc install blindschemes, replace all
*set scheme  plottig, permanently


*** Open the Urdu dataset directly through Dropbox

use "$raw/CSS_Urdu.dta"

*** Create a unique school and child_id combination

tostring school_code, 	gen(schoolcode)
tostring child_id, 		gen (childid) 

gen underscore = "_" 

gen unique_id = schoolcode + underscore + childid

*** Check for duplicates in unique_ids

duplicates tag unique_id, gen(dup)
tab dup


*** Check frequency distributons of all Urdu responses

foreach var of varlist urdu* {
		tab `var' 
		}

*** Trim string variables to remove extra spaces and convert all lower case string variables to upper case for consistency

foreach var of varlist urdu* {
		capture replace `var' = strtrim(`var') 
		capture replace `var' = strupper(`var') 
		}
		
*** Check for additional full stops or commas and remove if any

foreach var of varlist urdu* {
		capture list `var' if strpos(`var', ".")
		capture list `var' if strpos(`var', ",")
		capture list `var' if strpos(`var', "!")
		capture list `var' if strpos(`var', "?")
		capture replace `var' = subinstr(`var',".","",.)
		capture replace `var' = subinstr(`var',",","",.)
		capture replace `var' = subinstr(`var',"!","",.)
		capture replace `var' = subinstr(`var',"?","",.)
		}
		
		
		
/** Scoring Sheet for Urdu 

There are 48 questions in the Urdu test. 

Sone responses which were open ended were corrected to four options by the survey firm: correct, incorrect, invalid, unanswered. ///

Others were multiple choice. 

Questions which were open ended or where the correct response is 1 are: 

Urdu 1,	 Urdu 2,  Urdu 3,  Urdu 6,  Urdu 7,  Urdu 8,  Urdu 9,  Urdu 10, Urdu 11, Urdu 12, Urdu 13, Urdu 14, Urdu 15, Urdu 16
Urdu 20, Urdu 22, Urdu 26, Urdu 27, Urdu 28, Urdu 29, Urdu 30, Urdu 31, Urdu 32, Urdu 33, Urdu 34, Urdu 35, Urdu 36, Urdu 37
Urdu 40, Urdu 41, Urdu 43, Urdu 44, Urdu 45, Urdu 46, Urdu 47, Urdu 48

Questions where the correct response is 2 are: 

Urdu 5,  Urdu 18, Urdu 19, Urdu 24, Urdu 38, Urdu 42

Questions where the correst response is 3 are:

Urdu 4,  Urdu 17, Urdu 21, Urdu 23, Urdu 25, Urdu 39
*/
		
		
*** Create a new variable for correct scores in Urdu (*cs for 'correct score')

	*** Correct Responses where responses are either binary or the correct answer is 1


		foreach var of varlist 	urdu_1 - urdu_3 urdu_6 - urdu_16 urdu_20 urdu_22 urdu_26 - urdu_37	///
						urdu_40 urdu_41 urdu_43 - urdu_48 {
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  1
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}

	*** Correct Responses where the correct answer is 2


		foreach var of varlist 	urdu_5 urdu_18 urdu_19 urdu_24 urdu_38 urdu_42 {
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  2
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}

	*** Correct Responses where the correct answer is 3


		foreach var of varlist 	urdu_4 urdu_17 urdu_21 urdu_23 urdu_25 urdu_39   {
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  3
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}	

*** Check mean responses for all answers
			
codebook urdu*cs, compact

*** Replace missing values and invalid responses with zeroes

*recode *_cs (missing = 0) 

*** Generate a total score variable and see the frequency distribution for the same

egen urdu_total_score = rsum(urdu*_cs)
tab  urdu_total_score
sum  urdu_total_score, detail

*** Generate a percentage score 

gen urdu_perc_score = (urdu_total_score / 48) 
sum urdu_perc_score, detail

save "$processed/CSS_Urdu_with_cs.dta", replace


*** Merge Math data 

		clear all 
		use "$raw/CSS_Math.dta"

		*** Create a unique school and child_id combination to merge the two datasets

		tostring school_code, 	gen	(schoolcode)
		tostring child_id, 		gen (childid) 

		gen underscore = "_" 
		gen unique_id = schoolcode + underscore + childid
		
		*** Merge the Math dataset with the newly created Urdu dataset with correct scores directly through Dropbox

		merge 1:1 unique_id using "$processed/CSS_Urdu_with_cs.dta"

		tab  _merge
		drop _merge
		
		
		*** It's a perfect match. But the data is from the raw data folder. How is this possible? 
		
*** Destring variables (some Math responses are coded as string variables)

quietly destring, replace

*** Trim string variables to remove extra spaces and convert all lower case string variables to upper case for consistency

foreach var of varlist math* {
		capture replace `var' = strtrim(`var') 
		capture replace `var' = strupper(`var') 
		}
		
*** Check for additional commas and remove if any

			*** For Math, we would like to keep the full stops as we have responses like "10.2" 

foreach var of varlist math* {
		capture list `var' if strpos(`var', ".")
		capture list `var' if strpos(`var', ",")
		capture list `var' if strpos(`var', "!")
		capture list `var' if strpos(`var', "?")
		capture replace `var' = subinstr(`var',",","",.)
		capture replace `var' = subinstr(`var',"!","",.)
		capture replace `var' = subinstr(`var',"?","",.)
		}
		

*** Drop variables with no observations

drop 	 booklet_no

/** Scoring Sheet for Math

There are 42 questions in the Math test. 

Sone responses which were open ended were corrected to four options by the survey firm: correct, incorrect, invalid, unanswered. ///
However, the survey firm did not correct all open ended questions. 

Others were multiple choice. 

Questions which were open ended or where the correct response is 1 are: 

Math 3,  Math 4,  Math 11,  Math 12,  Math 13,  Math 14, Math 15, Math 16,  
Math 17, Math 18, Math 19,  Math 20,  Math 34,  Math 35, Math 36, Math 37, 
Math 38, Math 40, Math 41

Questions where the correct response is 2 are: 

Math 1 

Questions where the correst response is 3 are:

Math 33, Math 39,  Math 42

Questions where the correct reponse is 4 are:

Math 26, Math 32

Questions where the correct reponse is 5 are:

Math 2 

Questions that were not corrected by the survey firm (with the right answers as per the item bank) are:

Math 5  = 3,    Math 6 = 7,    Math 7 = 13,    Math 8 = 10,   Math 9 = 20,     Math 10 = 18,   Math 21 = 10.2,  
Math 22 = 1601, Math 23 = 97,  Math 24 = 109,  Math 25 = 43,  Math 27 = 11259, Math 28 = 128,  Math 29 = 17.6,  
Math 30 = 64,   Math 31 = 36.188


*/

		
*** Create a new variable for correct scores in Math (*cs for 'correct score')

	*** Correct Responses where responses are either binary or the correct answer is 1


		foreach var of varlist 	math_3 math_4 math_11 - math_20 math_35 - math_38	///
						math_40 math_41 {
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  1
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}
	
	*** Correct Responses where the correct answer is 2


		foreach var of varlist 	math_1 math_34	{
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  2
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}
	
	*** Correct Responses where the correct answer is 3


		foreach var of varlist 	math_26 math_33 math_39 math_42	{
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  3
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}
	
	*** Correct Responses where the correct answer is 4


		foreach var of varlist 	math_32	{
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  4
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}
	
	*** Correct Responses where the correct answer is 5


		foreach var of varlist 	math_2	{
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  5
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}
	
	*** For questions that were not corrected by the survey firm 
		
		*** Almost all of the variables include alphanumeric responses and so are string variables (apart from Math_6) 
	
		
		foreach var of varlist 	math_5 math_7 - math_10 math_21 - math_25 math_27 - math_31	{
		gen `var'_cs = 0 
		replace  `var'_cs =  .u if `var' == 	"-99"
		replace  `var'_cs =  .i if `var' == 	"-88"	
		}
	
		gen math_6_cs = 0
		replace  math_6_cs =  .u if math_6_cs == 	-99
		replace  math_6_cs =  .i if math_6_cs == 	-88
		
		replace math_5_cs =  1 if math_5 ==   "3"
		replace math_6_cs =  1 if math_6 ==    7
		replace math_7_cs =  1 if math_7 ==  "13"
		replace math_8_cs =  1 if math_8 ==  "10"
		replace math_9_cs =  1 if math_9 ==  "20"
		replace math_10_cs = 1 if math_10 == "18"
		replace math_21_cs = 1 if math_21 == "10.2"
		replace math_22_cs = 1 if math_22 == "1601"
		replace math_23_cs = 1 if math_23 == "97" 
		replace math_24_cs = 1 if math_24 == "109"
		replace math_25_cs = 1 if math_25 == "43"
		replace math_27_cs = 1 if math_27 == "11259"
		replace math_28_cs = 1 if math_28 == "128"
		replace math_29_cs = 1 if math_29 == "17.6"
		replace math_30_cs = 1 if math_30 == "64"
		replace math_31_cs = 1 if math_31 == "36.188"
		
		quietly destring, replace		

*** Check mean responses for all answers
			
codebook math*cs, compact


*** Replace missing values and invalid responses with zeroes

*recode *_cs (missing = 0) 


*** Generate a total score variable and see the frequency distribution for the same

egen math_total_score = rsum(math*_cs)
tab  math_total_score
sum  math_total_score, detail

*** Generate a percentage score 

gen math_perc_score = (math_total_score / 42) 
sum math_perc_score, detail

save "$processed/CSS_Math_with_cs.dta", replace

*** Merge English data 

		clear all 
		use "$raw/CSS_English.dta"

		*** Create a unique school and child_id combination to merge the two datasets

		tostring school_code, 	gen	(schoolcode)
		tostring child_id, 		gen (childid) 
		gen underscore = "_" 
		gen unique_id = schoolcode + underscore + childid
		drop schoolcode underscore childid

		*** Merge the English dataset with the newly created Math & Urdu dataset with correct scores directly through Dropbox
		
		merge 1:1 unique_id using "$processed/CSS_Math_with_cs.dta"

		tab  _merge
		drop _merge
		
		
		*** It's a perfect match. But the data is from the raw data folder. How is this possible? 


*** Destring variables (some English responses are coded as string variables)

quietly destring, replace

*** Trim string variables to remove extra spaces and convert all lower case string variables to upper case for consistency

foreach var of varlist english* {
		capture replace `var' = strtrim(`var') 
		capture replace `var' = strupper(`var') 
		}
		
*** Check for additional full stops or commas and remove if any

foreach var of varlist english* {
		capture list `var' if strpos(`var', ".")
		capture list `var' if strpos(`var', ",")
		capture list `var' if strpos(`var', "!")
		capture list `var' if strpos(`var', "?")
		capture replace `var' = subinstr(`var',".","",.)
		capture replace `var' = subinstr(`var',",","",.)
		capture replace `var' = subinstr(`var',"!","",.)
		capture replace `var' = subinstr(`var',"?","",.)
		}
		
		
*** Drop variables with no observations

drop 	 booklet_no

/** Scoring Sheet for English

There are 44 questions in the English test. 

Sone responses which were open ended were corrected to four options by the survey firm: correct, incorrect, invalid, unanswered. ///
However, the survey firm did not correct all open ended questions. 

Others were multiple choice. 

Questions which were open ended or where the correct response is 1 are: 

English 5,  English 8,  English 17,  English 19,  English 22,  English 23, English 27, English 30,  
English 33, English 34, English 35,  English 36,  English 37,  English 38, English 40, English 42, 
English 44 

Questions where the correct response is 2 are: 

English 7,  English 20,  English 24,  English 26,  English 29, English 31, English 39 

Questions where the correst response is 3 are:

English 6,  English 18,  English 21,  English 25,  English 28, English 32, English 41, English 43 

Questions that were not corrected by the survey firm (with the right answers as per the item bank) are:

English 1  = "E",    English 2  = "M",    English 3  = "ST",    English 4  = "N",   English 9  = "LL",       
English 10 = "WO",   English 11 = "A",	  English 12 = "G", 	English 13 = "PH", 	English 14 = "EA",
English 15 = "UI", 	 English 16 = "OU" 

*/

*** Create a new variable for correct scores in English (*cs for 'correct score')

	*** Correct Responses where responses are either binary or the correct answer is 1


		foreach var of varlist 	english_5  english_8  english_17 english_19 english_22	  ///
						        english_23 english_27 english_30 english_33 - english_38 ///
								english_40 english_42 english_44 {
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  1
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}

	*** Correct Responses where the correct answer is 2


		foreach var of varlist 	english_7 english_20 english_24 english_26 english_29 ///
								english_31 english_39 {
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  2
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}
	
	*** Correct Responses where the correct answer is 3


		foreach var of varlist 	english_6 english_18 english_21 english_25 english_28 ///
								english_32 english_41 english_43 {
		gen `var'_cs = 0 
		replace  `var'_cs =   1 if `var' == 	  3
		replace  `var'_cs =  .u if `var' == 	-99
		replace  `var'_cs =  .i if `var' == 	-88	
		}
	

	*** For questions that were not corrected by the survey firm 
		
		*** All variables include alphanumeric responses and so are string variables
	
		
		foreach var of varlist 	english_1 - english_4 english_9 - english_16	{
		gen `var'_cs = 0 
		replace  `var'_cs =  .u if `var' == 	"-99"
		replace  `var'_cs =  .i if `var' == 	"-88"	
		}

		replace english_1_cs =  1 if english_1 ==   "E"
		replace english_2_cs =  1 if english_2 ==   "M"
		replace english_3_cs =  1 if english_3 ==   "ST"
		replace english_4_cs =  1 if english_4 ==   "N"
		replace english_9_cs =  1 if english_9 ==   "LL"
		replace english_10_cs = 1 if english_10 ==  "WO"
		replace english_11_cs = 1 if english_11 ==  "A"
		replace english_12_cs = 1 if english_12 ==  "G"
		replace english_13_cs = 1 if english_13 ==  "PH"
		replace english_14_cs = 1 if english_14 ==  "EA"
		replace english_15_cs = 1 if english_15 ==  "UI"
		replace english_16_cs = 1 if english_16 ==  "OU"

		quietly destring, replace	
			
		
*** Check mean responses for all answers
			
codebook english*cs, compact

*** Replace missing values and invalid responses with zeroes

*recode *_cs (missing = 0) 

*** Generate a total score variable and see the frequency distribution for the same

egen eng_total_score = rsum(eng*_cs)
tab  eng_total_score
sum  eng_total_score, detail

*** Generate a percentage score 

gen eng_perc_score = (eng_total_score / 44) 
sum eng_perc_score, detail

save "$processed/CSS_English_with_cs.dta", replace

*** Merge Listening data 

		clear all 
		use "$raw/CSS_Title.dta"

		*** Create a unique school and child_id combination to merge the two datasets

		tostring school_code, 	gen	(schoolcode)
		tostring child_id, 		gen (childid) 
		gen underscore = "_" 
		gen unique_id = schoolcode + underscore + childid
		drop schoolcode underscore childid

		*** Merge the Listening dataset with the newly created English, Math & Urdu dataset with correct scores directly through Dropbox
		
		merge 1:1 unique_id using "$processed/CSS_English_with_cs.dta"

		tab  _merge
		drop _merge
		
		
		*** It's a perfect match. 
		
*** Trim string variables to remove extra spaces and convert all lower case string variables to upper case for consistency

foreach var of varlist listening* {
		capture replace `var' = strtrim(`var') 
		capture replace `var' = strupper(`var') 
		}

*** Check for additional full stops or commas and remove if any

foreach var of varlist listening* {
		capture list `var' if strpos(`var', ".")
		capture list `var' if strpos(`var', ",")
		capture list `var' if strpos(`var', "!")
		capture list `var' if strpos(`var', "?")
		capture replace `var' = subinstr(`var',".","",.)
		capture replace `var' = subinstr(`var',",","",.)
		capture replace `var' = subinstr(`var',"!","",.)
		capture replace `var' = subinstr(`var',"?","",.)
		}
		
*** Identify different booklet numbers

tab booklet_no

	*** Evenly distributed across booklet 1, 2 and 3. But what are the implications? We should check later!

/** Scoring Sheet for Listening

There are 7 questions in the listening component.

All responses are open ended. The answer key for these responses are: 

Listening 1 = "L",      Listening 2 = "R",    Listening 3 = "Y",    Listening 4 = "BOY",   Listening 5 = "GIRL",    
Listening 6 = "PAPER",	Listening 7 = "UNIFORM"

*/

*** Create a new variable for correct scores in Listening (*cs for 'correct score')
	
		
		*** All variables include alphanumeric responses and so are string variables
	
		
		foreach var of varlist 	listening_1 - listening_7	{
		gen `var'_cs = 0 
		replace  `var'_cs =  .u if `var' == 	"-99"
		replace  `var'_cs =  .i if `var' == 	"-88"	
		}

		replace listening_1_cs =  1 if listening_1 ==   "L"
		replace listening_2_cs =  1 if listening_2 ==   "R"
		replace listening_3_cs =  1 if listening_3 ==   "Y"
		replace listening_4_cs =  1 if listening_4 ==   "BOY"
		replace listening_5_cs =  1 if listening_5 ==   "GIRL"
		replace listening_6_cs =  1 if listening_6 ==   "PAPER"
		replace listening_7_cs =  1 if listening_7 ==   "UNIFORM"

		quietly destring, replace
		
		
*** Check mean responses for all answers
			
codebook listening*cs, compact

*** Replace missing values and invalid responses with zeroes

*recode *_cs (missing = 0) 

*** Generate a total score variable and see the frequency distribution for the same

egen listen_total_score = rsum(listening*_cs)
tab  listen_total_score
sum  listen_total_score, detail

*** Generate a percentage score 

gen listen_perc_score = (listen_total_score / 7) 
sum listen_perc_score, detail

*** Look at performance across all tests (Urdu, Math, English and Listening) 

pwcorr listen_perc_score eng_perc_score math_perc_score urdu_perc_score, star(0.01)
egen test_total_score = rsum(*_cs) 
gen  test_perc_score  = (test_total_score / 141)

*** Ordering variables per test for the analysis
foreach test in urdu math english listening {
order `test'*cs, sequential after(dup)
}

save "$processed/CSS_Title_with_cs.dta", replace






*** Assign variable and value labels for Urdu, Math, English, Listening and the Total Score (across all components) 

label define correct_score 1 "Correct" 0 "Incorrect" .u "Unanswered" .i "Invalid"
label values *_cs correct_score

label variable math_5_cs					"G1: Addition & Subtraction"
label variable math_6_cs 					"G1: Addition & Subtraction"
label variable math_7_cs 					"G1: Addition & Subtraction"
label variable math_10_cs 					"G1: Addition & Subtraction"
label variable math_25_cs 					"G1: Addition & Subtraction"
label variable math_22_cs 					"G2: Addition & Subtraction"
label variable math_23_cs 					"G2: Addition & Subtraction"
label variable math_24_cs 					"G2: Addition & Subtraction"
label variable math_21_cs 					"G4: Addition & Subtraction"
label variable urdu_1_cs 					"G1: Alphabet & Word Construction"
label variable urdu_2_cs 					"G1: Alphabet & Word Construction"
label variable urdu_3_cs 					"G1: Alphabet & Word Construction"
label variable urdu_7_cs 					"G2: Alphabet & Word Construction"
label variable urdu_8_cs 					"G2: Alphabet & Word Construction"
label variable urdu_9_cs 					"G2: Alphabet & Word Construction"
label variable urdu_10_cs 					"G2: Alphabet & Word Construction"
label variable urdu_11_cs 					"G2: Alphabet & Word Construction"
label variable urdu_12_cs 					"G2: Alphabet & Word Construction"
label variable urdu_13_cs 					"G2: Alphabet & Word Construction"
label variable urdu_14_cs 					"G2: Alphabet & Word Construction"
label variable urdu_15_cs 					"G2: Alphabet & Word Construction"
label variable urdu_16_cs 					"G2: Alphabet & Word Construction"
label variable math_1_cs 					"G1: Counting & Numbers"
label variable math_2_cs 					"G1: Counting & Numbers"
label variable math_3_cs 					"G2: Counting & Numbers"
label variable math_13_cs 					"G2: Counting & Numbers"
label variable math_15_cs 					"G2: Counting & Numbers"
label variable math_4_cs 					"G3: Counting & Numbers"
label variable math_11_cs 					"G3: Counting & Numbers"
label variable math_12_cs 					"G3: Counting & Numbers"
label variable math_14_cs 					"G3: Counting & Numbers"
label variable math_34_cs 					"G2: Fractions"
label variable math_33_cs 					"G3: Fractions"
label variable math_35_cs 					"G3: Fractions"
label variable math_36_cs 					"G4: Fractions"
label variable math_37_cs 					"G4: Fractions"
label variable math_40_cs 					"G4: Fractions"
label variable math_41_cs 					"G4: Fractions"
label variable math_38_cs 					"G4: Fractions, Percentages"
label variable math_39_cs 					"G4: Fractions, Percentages"
label variable urdu_37_cs 					"G2: Grammar"
label variable urdu_38_cs 					"G2: Grammar"
label variable urdu_39_cs 					"G2: Grammar"
label variable urdu_40_cs 					"G2: Grammar"
label variable urdu_41_cs 					"G2: Grammar"
label variable english_26_cs 				"G2: Grammar"
label variable english_27_cs 				"G2: Grammar"
label variable english_28_cs 				"G2: Grammar"
label variable english_29_cs 				"G2: Grammar"
label variable english_30_cs 				"G2: Grammar"
label variable english_31_cs 				"G2: Grammar"
label variable english_32_cs 				"G2: Grammar"
label variable urdu_26_cs 					"G3: Grammar"
label variable urdu_27_cs 					"G3: Grammar"
label variable urdu_28_cs 					"G3: Grammar"
label variable urdu_29_cs 					"G3: Grammar"
label variable urdu_30_cs 					"G3: Grammar"
label variable urdu_31_cs 					"G3: Grammar"
label variable math_9_cs 					"G2: Multiplication & Division"
label variable math_28_cs 					"G3: Multiplication & Division"
label variable math_27_cs 					"G4: Multiplication & Division"
label variable math_29_cs 					"G4: Multiplication & Division"
label variable math_30_cs 				 	"G4: Multiplication & Division"
label variable math_31_cs 					"G5: Multiplication & Division"
label variable math_26_cs 					"G1: Problem Solving"
label variable math_42_cs 					"G1: Problem Solving"
label variable math_8_cs 					"G2: Problem Solving"
label variable math_16_cs 					"G2: Problem Solving"
label variable math_17_cs 					"G2: Problem Solving"
label variable math_18_cs 					"G2: Problem Solving"
label variable math_19_cs 					"G2: Problem Solving"
label variable math_32_cs 					"G2: Problem Solving"
label variable math_20_cs 					"G3: Problem Solving"
label variable urdu_42_cs 					"G2: Reading Comprehension"
label variable urdu_43_cs					"G2: Reading Comprehension"
label variable urdu_44_cs 					"G2: Reading Comprehension"
label variable urdu_45_cs 					"G2: Reading Comprehension"
label variable urdu_46_cs 					"G2: Reading Comprehension"
label variable urdu_47_cs 					"G2: Reading Comprehension"
label variable urdu_48_cs 					"G2: Reading Comprehension"
label variable english_38_cs 				"G3: Reading Comprehension"
label variable english_39_cs 				"G3: Reading Comprehension"
label variable english_40_cs 				"G3: Reading Comprehension"
label variable english_41_cs 				"G3: Reading Comprehension"
label variable english_42_cs 				"G3: Reading Comprehension"
label variable english_43_cs 				"G3: Reading Comprehension"
label variable english_44_cs 				"G3: Reading Comprehension"
label variable urdu_17_cs 					"G2: Sentence Construction"
label variable urdu_18_cs 					"G2: Sentence Construction"
label variable urdu_19_cs 					"G2: Sentence Construction"
label variable urdu_32_cs 					"G2: Sentence Construction"
label variable urdu_33_cs 					"G2: Sentence Construction"
label variable urdu_34_cs 					"G2: Sentence Construction"
label variable urdu_35_cs 					"G2: Sentence Construction"
label variable urdu_36_cs 					"G2: Sentence Construction"
label variable english_21_cs 				"G2: Sentence Construction"
label variable english_22_cs 				"G2: Sentence Construction"
label variable english_23_cs	 			"G2: Sentence Construction"
label variable english_24_cs 				"G2: Sentence Construction"
label variable english_25_cs 				"G2: Sentence Construction"
label variable english_33_cs 				"G2: Sentence Construction"
label variable english_34_cs 				"G2: Sentence Construction"
label variable english_37_cs 				"G2: Sentence Construction"
label variable english_35_cs 				"G3: Sentence Construction"
label variable english_36_cs 				"G3: Sentence Construction"
label variable english_5_cs 				"G1: Word Recognition"
label variable english_6_cs 				"G1: Word Recognition"
label variable english_7_cs 				"G1: Word Recognition"
label variable english_8_cs 				"G1: Word Recognition"
label variable urdu_4_cs 					"G2: Word Recognition"
label variable urdu_5_cs 					"G2: Word Recognition"
label variable urdu_6_cs 					"G2: Word Recognition"
label variable urdu_20_cs 					"G3: Word Recognition"
label variable urdu_21_cs 					"G3: Word Recognition"
label variable urdu_22_cs 					"G3: Word Recognition"
label variable urdu_23_cs 					"G3: Word Recognition"
label variable urdu_24_cs 					"G3: Word Recognition"
label variable urdu_25_cs 					"G3: Word Recognition"
label variable english_17_cs 				"G3: Word Recognition"
label variable english_18_cs 				"G3: Word Recognition"
label variable english_19_cs 				"G3: Word Recognition"
label variable english_20_cs 				"G3: Word Recognition"
label variable english_1_cs 				"G1: Write Alphabet/Word"
label variable english_2_cs 				"G1: Write Alphabet/Word"
label variable english_3_cs 				"G1: Write Alphabet/Word"
label variable english_4_cs 				"G1: Write Alphabet/Word"
label variable english_9_cs 				"G1: Write Alphabet/Word"
label variable english_10_cs 				"G1: Write Alphabet/Word"
label variable english_11_cs 				"G1: Write Alphabet/Word"
label variable english_12_cs 				"G1: Write Alphabet/Word"
label variable english_14_cs 				"G1: Write Alphabet/Word"
label variable english_15_cs 				"G1: Write Alphabet/Word"
label variable english_13_cs 				"G3: Write Alphabet/Word"
label variable english_16_cs 				"G3: Write Alphabet/Word"
label variable listening_1_cs 				"G1: Listen & Write"
label variable listening_2_cs 				"G1: Listen & Write"
label variable listening_3_cs 				"G1: Listen & Write"
label variable listening_4_cs 				"G1: Listen & Write"
label variable listening_5_cs 				"G1: Listen & Write"
label variable listening_6_cs 				"G2: Listen & Write"
label variable listening_7_cs 				"G2: Listen & Write"


label variable urdu_total_score				"Total Score in Urdu"
label variable urdu_perc_score	 			"Percentage Score in Urdu"

label variable math_total_score				"Total Score in Mathematics"
label variable math_perc_score	 			"Percentage Score in Mathematics"

label variable eng_total_score				"Total Score in English"
label variable eng_perc_score	 			"Percentage Score in English"

label variable listen_total_score			"Total Score in Listening Component"
label variable listen_perc_score	 		"Percentage Score in Listening Component"

label variable test_total_score				"Total Score (Overall)"
label variable test_perc_score	 			"Percentage Score (Overall)"




*** Missing Values Analysis

//Total Missing Values per test
foreach test in urdu math english listening {
	egen `test'_perc_MV_TOT = rcount(`test'*cs), c(@==.)
}

	egen TotalScore_perc_MV_TOT = rcount(*cs), c(@==.)

//Share of Missing Values per test
replace urdu_perc_MV_TOT 		= urdu_perc_MV_TOT/48
replace math_perc_MV_TOT 		= math_perc_MV_TOT/42
replace english_perc_MV_TOT 	= english_perc_MV_TOT/44
replace listening_perc_MV_TOT 	= listening_perc_MV_TOT/7
replace TotalScore_perc_MV_TOT	= TotalScore_perc_MV_TOT/141

	label variable urdu_perc_MV_TOT 			"Share of MV: Urdu"
	label variable math_perc_MV_TOT 			"Share of MV: Math"
	label variable english_perc_MV_TOT 			"Share of MV: English"
	label variable listening_perc_MV_TOT 		"Share of MV: Listening"
	label variable TotalScore_perc_MV_TOT 		"Share of MV: All tests"


//Share of Missing Values per item per test
foreach var of varlist urdu_1_cs - urdu_48_cs {
quiet: mdesc `var'
gen `var'_MV = r(percent)
}

foreach var of varlist math_1_cs - math_42_cs {
quiet: mdesc `var'
gen `var'_MV = r(percent)
}

foreach var of varlist english_1_cs - english_44_cs {
quiet: mdesc `var'
gen `var'_MV = r(percent)
}

foreach var of varlist listening_1_cs - listening_7_cs {
quiet: mdesc `var'
gen `var'_MV = r(percent)
}

//Graphical representation: Total MV per test
#delimit ;
	graph bar (mean) urdu_perc_MV_TOT math_perc_MV_TOT english_perc_MV_TOT listening_perc_MV_TOT, 
	ylabel(0 0.3 0.5)
	title(Share of Missing Values per Test)
	blabel(bar, position(inside) format(%9.2f) color(white) size(medium))
	yvaroptions(relabel(1 "Share of MV: Urdu" 2 "Share of MV: Math" 3 "Share of MV: English" 4 "Share of MV: Listening"))
	ascategory
	scheme(plotplain);
#delimit cr

graph export "$figures/ShareMV_OverallTests.png", width(2000) replace

//Table: Total MV per test
estpost sum urdu_perc_MV_TOT math_perc_MV_TOT english_perc_MV_TOT listening_perc_MV_TOT TotalScore_perc_MV_TOT
esttab, cell("mean(label(Mean)fmt(%9.3f)) sd(label(S.D)) min(label(Min)fmt(%9.0f)) max(label(Max)fmt(%9.0f)) count(label(Obs.)fmt(%9.0f))") label nonumber noobs


//Graphical representation: Total MV per item, per test
#delimit ;
	graph bar (mean) urdu_*_cs_MV,
	ylabel(0 10 20 30 40 50 60 70 80 90 100)
	title(Share of Missing Values per item for Urdu (48 questions))
	legend(off)
	scheme(plotplain);
#delimit cr
graph export "$figures/ShareMV_UrduItems.png", width(2000) replace


#delimit ;
	graph bar (mean) math_*_cs_MV,
	ylabel(0 10 20 30 40 50 60 70 80 90 100)
	title(Share of Missing Values per item for Math (42 questions))
	legend(off)
	scheme(plotplain);
#delimit cr
graph export "$figures/ShareMV_MathItems.png", width(2000) replace

#delimit ;
	graph bar (mean) english_*_cs_MV,
	ylabel(0 10 20 30 40 50 60 70 80 90 100)
	title(Share of Missing Values per item for English (44 questions))
	legend(off)
	scheme(plotplain);
#delimit cr
graph export "$figures/ShareMV_EnglishItems.png", width(2000) replace

#delimit ;
	graph bar (mean) listening_*_cs_MV,
	ylabel(0 5 10 15 20)
	title(Share of Missing Values per item for Listening (7 questions))
	legend(off)
	scheme(plotplain);
#delimit cr
graph export "$figures/ShareMV_ListeningItems.png", width(2000) replace


drop *_MV



/* NECESSARY ?

//Recoding Missing values into 0
foreach test in urdu math english listening {
recode `test'*cs (missing = 0)
}
*/




*** A few basic graphs to visualize the data for Urdu, Math, English, Listening and the Total Score (across all components) 

*** First, without merging student identification data from the student roster, we are checking for student performance on the test (by quartiles)


//Table of global scores
estpost sum urdu_perc_score math_perc_score eng_perc_score listen_perc_score test_perc_score
esttab, cell("mean(label(Mean)fmt(%9.3f)) sd(label(S.D)) min(label(Min)fmt(%9.0f)) max(label(Max)fmt(%9.0f)) count(label(Obs.)fmt(%9.0f))") label nonumber noobs


	*** Urdu
	
	egen  quar_urdu =  cut(urdu_total_score), at(0,13,25,37,48)
	table quar_urdu,   contents(min urdu_total_score max urdu_total_score) 

	label define quarter 0 "25% or lower" 13 "Between 25% & 50%" 25 "Between 50% and 75%" 37 "Higher than 75%"
	label values quar_urdu quarter

	graph pie, over(quar_urdu) pie(_all, explode) plabel(_all percent, size(*2.5)) title(Student Performance: Urdu) 
	graph export "$figures/pie_urdu.png", width(2000) replace
	
	sum				urdu_perc_score,mean
	histogram 		urdu_perc_score, discrete title(Student Performance: Urdu) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
	graph export 	"$figures/hist_urdu.png", width(2000) replace
	
	
	*alpha urdu*cs , item
	
	
	*** Math

	egen  quar_math =  cut(math_total_score), at(0,11,22,32,42)
	table quar_math,   contents(min math_total_score max math_total_score) 

	label define quarter2 0 "25% or lower" 11 "Between 25% & 50%" 22 "Between 50% & 75%" 32 "Higher than 75%"
	label values quar_math quarter2

	graph pie, over(quar_math) pie(_all, explode) plabel(_all percent, size(*2.5)) title(Student Performance: Math) 
	graph export "$figures/pie_math.png", width(2000) replace

	sum				math_perc_score,mean
	histogram 		math_perc_score, discrete title(Student Performance: Math) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
	graph export 	"$figures/hist_math.png", width(2000) replace
	
	
	*alpha math*cs , item

	
	*** English

	egen  quar_eng =  cut(eng_total_score), at(0,12,23,34,44)
	table quar_eng,   contents(min eng_total_score max eng_total_score) 

	label define quarter3 0 "25% or lower" 12 "Between 25% & 50%" 23 "Between 50% & 75%" 34 "Higher than 75%"
	label values quar_eng quarter3

	graph pie, over(quar_eng) pie(_all, explode) plabel(_all percent, size(*2.5)) title(Student Performance: English) 
	graph export "$figures/pie_eng.png", width(2000) replace

	sum				eng_perc_score,mean
	histogram 		eng_perc_score, discrete title(Student Performance: English) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
	graph export 	"$figures/hist_eng.png", width(2000) replace
	
	
	*alpha english*cs , item

	
	
	*** Listening

	egen  quar_listen =  cut(listen_total_score), at(0,2,4,6,7)
	table quar_listen,   contents(min listen_total_score max listen_total_score) 

	label define quarter4 0 "25% or lower" 2 "Between 25% & 50%" 4 "Between 50% & 75%" 6 "Higher than 75%"
	label values quar_listen quarter4

	graph pie, over(quar_listen) pie(_all, explode) plabel(_all percent, size(*2.5)) title(Student Performance: Listening Component) 
	graph export "$figures/pie_listen.png", width(2000) replace

	sum				listen_perc_score,mean
	histogram 		listen_perc_score, discrete title(Student Performance: Listening Component) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
	graph export 	"$figures/hist_listen.png", width(2000) replace
	
	
	*alpha listening*cs , item
	
	
	
	*** Total Score

	egen  quar_test =  cut(test_total_score), at(0,36,72,107,141)
	table quar_test,   contents(min test_total_score max test_total_score) 

	label define quarter5 0 "25% or lower" 36 "Between 25% & 50%" 72 "Between 50% & 75%" 107 "Higher than 75%"
	label values quar_test quarter5

	graph pie, over(quar_test) pie(_all, explode) plabel(_all percent, size(*2.5)) title(Student Performance: Overall) 
	graph export "$figures/pie_test.png", width(2000) replace

	sum				test_perc_score,mean
	histogram 		test_perc_score, discrete title(Student Performance: Overall) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
	graph export 	"$figures/hist_test.png", width(2000) replace
	

	*alpha urdu*cs math*cs english*cs listening*cs , item
	

	
	
*** Score for each item

//Creation of the variables per item	
foreach item of varlist urdu_1_cs - urdu_48_cs math_1_cs - math_42_cs ///
english_1_cs - english_44_cs listening_1_cs - listening_7_cs {
quiet: sum `item'
gen  `item'_Score=`r(mean)'
}	

//Graphical representations
#delimit ;
	graph bar (mean) urdu_*_cs_Score,
	ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1)
	title(Average score for each item (Urdu))
	legend(off)
	scheme(plotplain);
#delimit cr
graph export "$figures/ItemScoring_Urdu.png", width(2000) replace
	
#delimit ;
	graph bar (mean) math_*_cs_Score,
	ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1)
	title(Average score for each item (Math))
	legend(off)
	scheme(plotplain);
#delimit cr
graph export "$figures/ItemScoring_Math.png", width(2000) replace
	
#delimit ;
	graph bar (mean) english_*_cs_Score,
	ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1)
	title(Average score for each item (English))
	legend(off)
	scheme(plotplain);
#delimit cr
graph export "$figures/ItemScoring_English.png", width(2000) replace
	
#delimit ;
	graph bar (mean) listening_*_cs_Score,
	ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1)
	title(Average score for each item (Listening))
	legend(off)
	scheme(plotplain);
#delimit cr
graph export "$figures/ItemScoring_Listening.png", width(2000) replace
	
drop *Score	
	


	
*** Create summary variables for various question groups ('Fractions', 'Comprehension' etc.)

sum 		*_cs




*Urdu

		*** Alphabet and Word Construction in Urdu

egen alpha_word_urdu            = rsum(urdu_1_cs urdu_2_cs urdu_3_cs urdu_7_cs urdu_8_cs urdu_9_cs urdu_10_cs urdu_11_cs urdu_12_cs urdu_13_cs urdu_14_cs urdu_15_cs urdu_16_cs) 
gen alpha_word_urdu_perc        = alpha_word_urdu/13

label variable alpha_word_urdu         "Alphabet and Word Construction in Urdu"
label variable alpha_word_urdu_perc    "Percentage Score in Alphabet and Word Construction in Urdu"

sum 		                     alpha_word_urdu_perc,mean
histogram 	                     alpha_word_urdu_perc, discrete title(Student Performance: Alphabet & Word Construction in Urdu) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/alpha_word_urdu_perc.png", width(2000) replace


alpha urdu_1_cs urdu_2_cs urdu_3_cs urdu_7_cs urdu_8_cs urdu_9_cs urdu_10_cs urdu_11_cs urdu_12_cs urdu_13_cs urdu_14_cs urdu_15_cs urdu_16_cs, item 
 
		*** Urdu Grammar

egen gram_urdu                 = rsum(urdu_26_cs urdu_27_cs urdu_28_cs urdu_29_cs urdu_30_cs urdu_31_cs urdu_37_cs urdu_38_cs urdu_39_cs urdu_40_cs urdu_41_cs)
gen gram_urdu_perc             = gram_urdu/11

label variable gram_urdu           	    "Urdu Grammar"
label variable gram_urdu_perc           "Percentage Score in Urdu Grammar"

sum 		                     gram_urdu_perc,mean
histogram 	                     gram_urdu_perc, discrete title(Student Performance: Urdu Grammar) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/gram_urdu_perc.png", width(2000) replace


alpha urdu_26_cs urdu_27_cs urdu_28_cs urdu_29_cs urdu_30_cs urdu_31_cs urdu_37_cs urdu_38_cs urdu_39_cs urdu_40_cs urdu_41_cs, item


		*** Urdu Reading Comprehension

egen comp_urdu                 = rsum(urdu_42_cs urdu_43_cs urdu_44_cs urdu_45_cs urdu_46_cs urdu_47_cs urdu_48_cs)
gen comp_urdu_perc             = comp_urdu /7

label variable comp_urdu               "Urdu Reading Comprehension"
label variable comp_urdu_perc     	   "Percentage Score in Urdu Reading Comprehension"

sum 		                     comp_urdu_perc,mean
histogram 	                     comp_urdu_perc, discrete title(Student Performance: Urdu Reading Comprehension) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/comp_urdu_perc.png", width(2000) replace

alpha urdu_42_cs urdu_43_cs urdu_44_cs urdu_45_cs urdu_46_cs urdu_47_cs urdu_48_cs, item

		*** Sentence Construction in Urdu
		
egen sentences_urdu            = rsum(urdu_17_cs urdu_18_cs urdu_19_cs urdu_32_cs urdu_33_cs urdu_34_cs urdu_35_cs urdu_36_cs) 
gen sentences_urdu_perc        = sentences_urdu/8

label variable sentences_urdu          "Sentence Construction in Urdu"
label variable sentences_urdu_perc     "Percentage Score in Sentence Construction in Urdu"

sum 		                     sentences_urdu_perc,mean
histogram 	                     sentences_urdu_perc, discrete title(Student Performance: Sentence Construction in Urdu) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/sentences_urdu_perc.png", width(2000) replace

alpha urdu_17_cs urdu_18_cs urdu_19_cs urdu_32_cs urdu_33_cs urdu_34_cs urdu_35_cs urdu_36_cs, item


		*** Word Recognition in Urdu 
			
egen word_recog_urdu            = rsum(urdu_4_cs urdu_5_cs urdu_6_cs urdu_20_cs urdu_21_cs urdu_22_cs  urdu_23_cs  urdu_24_cs  urdu_25_cs)
gen word_recog_urdu_perc        = word_recog_urdu/9
 
label variable word_recog_urdu          "Word Recognition in Urdu"
label variable word_recog_urdu_perc     "Percentage Score in Word Recognition in Urdu"

sum 		                     word_recog_urdu_perc,mean
histogram 	                     word_recog_urdu_perc, discrete title(Student Performance: Word Recognition in Urdu) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/word_recog_urdu_perc.png", width(2000) replace


alpha urdu_4_cs urdu_5_cs urdu_6_cs urdu_20_cs urdu_21_cs urdu_22_cs  urdu_23_cs  urdu_24_cs  urdu_25_cs, item



//Table: Subtests Urdu
estpost sum alpha_word_urdu_perc gram_urdu_perc comp_urdu_perc sentences_urdu_perc word_recog_urdu_perc
esttab, cell("mean(label(Mean)fmt(%9.3f)) sd(label(S.D)) min(label(Min)fmt(%9.0f)) max(label(Max)fmt(%9.0f)) count(label(Obs.)fmt(%9.0f))") label nonumber noobs

//Missing Values per Subtest



*Maths 

		*** Addition and Subtraction 
 
egen add_sub 		         = rsum(math_5_cs math_6_cs math_7_cs math_10_cs math_21_cs math_22_cs math_23_cs math_24_cs math_25_cs)
gen  add_sub_perc 	         = add_sub/9

label variable add_sub 		       "Addition and Subtraction" 
label variable add_sub_perc        "Percentage Score in Addition and Subtraction" 

sum 		                 add_sub_perc,mean
histogram 	                 add_sub_perc, discrete title(Student Performance: Addition and Subtraction) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                 "$figures/add_sub_perc.png", width(2000) replace

alpha math_5_cs math_6_cs math_7_cs math_10_cs math_21_cs math_22_cs math_23_cs math_24_cs math_25_cs, item


		*** Counting and Numbers

egen count_num               = rsum(math_1_cs math_2_cs math_3_cs math_4_cs math_11_cs math_12_cs math_13_cs math_14_cs math_15_cs)
gen  count_num_perc          = count_num/9

label variable count_num  	        "Counting and Numbers"
label variable count_num_perc       "Percentage Score in Counting and Numbers"  
 
sum 		                 count_num_perc,mean
histogram 	                 count_num_perc, discrete title(Student Performance: Counting and Numbers) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                 "$figures/count_num_perc.png", width(2000) replace

alpha math_1_cs math_2_cs math_3_cs math_4_cs math_11_cs math_12_cs math_13_cs math_14_cs math_15_cs, item


		*** Fractions

egen fractions                = rsum(math_33_cs math_34_cs math_35_cs math_36_cs math_37_cs math_40_cs math_41_cs)
gen  fractions_perc           = fractions/7

label variable  fractions  	        "Fractions"
label variable  fractions_perc      "Percentage Score in Fractions"  

sum 		                  fractions_perc,mean
histogram 	                  fractions_perc, discrete title(Student Performance: Fractions) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                  "$figures/fractions_perc.png", width(2000) replace

alpha math_33_cs math_34_cs math_35_cs math_36_cs math_37_cs math_40_cs math_41_cs, item


		*** Fractions and Percentages

egen fraction_percent         = rsum(math_38_cs math_39_cs)
gen  fraction_percent_perc    = fraction_percent/2

label variable fraction_percent      "Fractions and Percentages"
label variable fraction_percent_perc "Percentage Score in Fractions and Percenatges"

sum 		                   fraction_percent_perc,mean
histogram 	                   fraction_percent_perc, discrete title(Student Performance: Fractions and Percentages) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                   "$figures/fraction_percent_perc.png", width(2000) replace

alpha math_38_cs math_39_cs, item


		*** Multiplication and Division

egen mul_div       			   = rsum(math_9_cs math_27_cs math_28_cs math_29_cs math_30_cs math_31_cs)
gen  mul_div_perc   		   = mul_div/6

label variable mul_div      		 "Multiplication and Division"
label variable mul_div_perc 		 "Percentage Score in Multiplication and Division"

sum 		                    mul_div_perc,mean
histogram 	                    mul_div_perc, discrete title(Student Performance: Multiplication and Division) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                    "$figures/mul_div_perc.png", width(2000) replace

alpha math_9_cs math_27_cs math_28_cs math_29_cs math_30_cs math_31_cs, item


		*** Problem Solving

egen problem_solv              = rsum(math_8_cs math_16_cs math_17_cs math_18_cs math_19_cs math_20_cs math_26_cs math_32_cs math_42_cs)
gen  problem_solv_perc         = problem_solv/9
 
label variable problem_solv          "Probem Solving"
label variable problem_solv_perc     "Percentage Score in Problem Solving"

sum 		                     problem_solv_perc,mean
histogram 	                     problem_solv_perc, discrete title(Student Performance: Problem Solving) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/problem_solv_perc.png", width(2000) replace

alpha math_8_cs math_16_cs math_17_cs math_18_cs math_19_cs math_20_cs math_26_cs math_32_cs math_42_cs, item


//Table
estpost sum add_sub_perc count_num_perc fractions_perc fraction_percent_perc mul_div_perc problem_solv_perc
esttab, cell("mean(label(Mean)fmt(%9.3f)) sd(label(S.D)) min(label(Min)fmt(%9.0f)) max(label(Max)fmt(%9.0f)) count(label(Obs.)fmt(%9.0f))") label nonumber noobs




*English

		*** Write Alphabet/ Word in English

egen alpha_word_english        = rsum(english_1_cs english_2_cs english_3_cs english_4_cs english_9_cs english_10_cs english_11_cs english_12_cs english_13_cs english_14_cs english_15_cs english_16_cs) 
gen alpha_word_english_perc    = alpha_word_english/12

label variable alpha_word_english       "Write Alphabet/Word in English"
label variable alpha_word_english_perc  "Percentage Score in Write Alphabet/Word in English"

sum 		                     alpha_word_english_perc,mean
histogram 	                     alpha_word_english_perc, discrete title(Student Performance: Write Alphabet/Word in English) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/alpha_word_english_perc.png", width(2000) replace
 
alpha english_1_cs english_2_cs english_3_cs english_4_cs english_9_cs english_10_cs english_11_cs english_12_cs english_13_cs english_14_cs english_15_cs english_16_cs, item


		*** English Grammar
		
egen gram_english               =rsum(english_26_cs english_27_cs english_28_cs english_29_cs english_30_cs english_31_cs english_32_cs) 
gen gram_english_perc           =gram_english/7

label variable gram_english              "English Grammar"
label variable gram_english_perc         "Percentage Score in English Grammar"

sum 		                     gram_english_perc,mean
histogram 	                     gram_english_perc, discrete title(Student Performance: English Grammar) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/gram_english_perc.png", width(2000) replace

alpha english_26_cs english_27_cs english_28_cs english_29_cs english_30_cs english_31_cs english_32_cs, item


		*** English Reading Comprehension

egen comp_english   			=rsum(english_38_cs english_39_cs english_40_cs english_41_cs english_42_cs english_43_cs english_44_cs)
gen comp_english_perc 			= comp_english/7

label variable comp_english              "English Reading Comprehension"
label variable comp_english_perc         "Percentage Score in English Reading Comprehension"

sum 		                     comp_english_perc,mean
histogram 	                     comp_english_perc, discrete title(Student Performance: English Reading Comprehension) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/comp_english_perc.png", width(2000) replace

alpha english_38_cs english_39_cs english_40_cs english_41_cs english_42_cs english_43_cs english_44_cs, item


		*** Sentence Construction in English

egen sentences_english       	=rsum(english_21_cs english_22_cs english_23_cs english_24_cs english_25_cs english_33_cs english_34_cs english_35_cs english_36_cs english_37_cs)
gen sentences_english_perc  	=sentences_english/10

label variable  sentences_english         "Sentence Construction in English"
label variable  sentences_english_perc    "Percentage Score in Sentence Construction in English"

sum 		                     sentences_english_perc,mean
histogram 	                     sentences_english_perc, discrete title(Student Performance: Sentence Construction in English) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/sentences_english_perc.png", width(2000) replace

alpha english_21_cs english_22_cs english_23_cs english_24_cs english_25_cs english_33_cs english_34_cs english_35_cs english_36_cs english_37_cs, item


		*** Word Recognition in English

egen word_recog_english      	= rsum(english_5_cs english_6_cs english_7_cs english_8_cs english_17_cs english_18_cs  english_19_cs  english_20_cs) 
gen word_recog_english_perc 	= word_recog_english/8

label variable  word_recog_english        "Word Recognition in English"
label variable  word_recog_english_perc   "Percentage Score in Word Recognition in English"

sum 		                     word_recog_english_perc,mean
histogram 	                     word_recog_english_perc, discrete title(Student Performance: Word Recognition in English) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/word_recog_english_perc.png", width(2000) replace

alpha english_5_cs english_6_cs english_7_cs english_8_cs english_17_cs english_18_cs  english_19_cs  english_20_cs, item


//Table
estpost sum alpha_word_english_perc gram_english_perc comp_english_perc sentences_english_perc word_recog_english_perc
esttab, cell("mean(label(Mean)fmt(%9.3f)) sd(label(S.D)) min(label(Min)fmt(%9.0f)) max(label(Max)fmt(%9.0f)) count(label(Obs.)fmt(%9.0f))") label nonumber noobs
 





*Listening

		*** Listen and Write

egen lis_write 		             = rsum(listening_1_cs listening_2_cs listening_3_cs listening_4_cs listening_5_cs listening_6_cs listening_7_cs)
gen  lis_write_perc 	         = lis_write/7

label variable 	lis_write 		       "Listen and Write" 
label variable  lis_write_perc         "Percentage Score in Listen and Write" 

sum 		                     lis_write_perc,mean
histogram 	                     lis_write_perc,discrete title(Student Performance: Listen and Write) subtitle(Mean = `: di %3.2f r(mean)') xline(`r(mean)', lpattern(solid) lcolor(black) extend) ytitle("") scheme(plotplain)
graph export                     "$figures/lis_write_perc.png", width(2000) replace










*** Create summary variables for various grade levels

		*** Urdu (Grade 1) 


egen g1_urdu             = rsum(urdu_1_cs urdu_2_cs urdu_3_cs) 
gen  g1_urdu_perc        = g1_urdu/3

label variable  g1_urdu               "Urdu Grade 1 Questions"
label variable  g1_urdu_perc          "Percentage Score in Urdu, Grade 1 Questions "

sum 		                    g1_urdu_perc,mean
#delimit ;
histogram 	                    g1_urdu_perc, 
								discrete title(Student Performance: Percentage Score in Urdu: Grade 1 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/1.g1_urdu_perc.png", width(2000) replace as(png)			

		*** Urdu (Grade 2) 
		
egen g2_urdu             = rsum(urdu_4_cs urdu_5_cs urdu_6_cs urdu_7_cs urdu_8_cs urdu_9_cs urdu_10_cs urdu_11_cs urdu_12_cs urdu_13_cs urdu_14_cs urdu_15_cs urdu_16_cs urdu_17_cs urdu_18_cs  ///
                           urdu_19_cs urdu_32_cs urdu_33_cs urdu_34_cs urdu_35_cs urdu_36_cs urdu_37_cs urdu_38_cs urdu_39_cs urdu_40_cs urdu_41_cs urdu_42_cs urdu_43_cs urdu_44_cs urdu_45_cs) 
gen  g2_urdu_perc        = g2_urdu/30

label variable g2_urdu                 "Urdu Grade 2 Questions"
label variable g2_urdu_perc             "Percentage Score in Urdu, Grade 2 Questions "

sum 		                    g2_urdu_perc,mean
#delimit ;
histogram 	                    g2_urdu_perc, 
								discrete title(Student Performance: Percentage Score in Urdu: Grade 2 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/2.g2_urdu_perc.png", width(2000) replace as(png)			

	*** Urdu (Grade 3) 
	
egen g3_urdu             = rsum(urdu_20_cs urdu_21_cs urdu_22_cs urdu_23_cs urdu_24_cs urdu_25_cs urdu_26_cs urdu_27_cs urdu_28_cs urdu_29_cs urdu_30_cs urdu_31_cs urdu_46_cs urdu_47_cs urdu_48_cs) 
gen  g3_urdu_perc        = g3_urdu/15

label variable g3_urdu                   "Urdu Grade 3 Questions"
label variable g3_urdu_perc             "Percentage Score in Urdu, Grade 3 Questions "

sum 		                    g3_urdu_perc,mean
#delimit 
histogram 	                    g3_urdu_perc, 
								discrete title(Student Performance: Percentage Score in Urdu: Grade 3 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/3.g3_urdu_perc.png", width(2000) replace as(png)

	*** English (Grade 1) 
	
egen g1_english             = rsum(english_1_cs english_2_cs english_3_cs english_4_cs english_5_cs english_6_cs english_7_cs english_8_cs english_9_cs english_10_cs english_11_cs english_12_cs ///
                               english_14_cs english_15_cs) 
gen g1_english_perc            = g1_english/14

label variable  g1_english                "English Grade 1 Questions"
label variable  g1_english_perc          "Percentage Score in English, Grade 1 Questions "

sum 		                    g1_english_perc,mean
#delimit ;
histogram 	                    g1_english_perc, 
								discrete title(Student Performance: Percentage Score in English: Grade 1 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/4.g1_english_perc.png", width(2000) replace as(png)	


		*** English (Grade 2) 
		
		
egen g2_english              = rsum(english_21_cs english_22_cs english_23_cs english_24_cs english_25_cs english_26_cs english_27_cs english_28_cs english_29_cs english_30_cs english_31_cs english_32_cs english_33_cs english_34_cs english_37_cs) 
gen  g2_english_perc         = g2_english/15

label variable  g2_english                "English Grade 2 Questions"
label variable  g2_english_perc           "Percentage Score in English, Grade 2 Questions "

sum 		                    g2_english_perc,mean
#delimit ;
histogram 	                    g2_english_perc, 
								discrete title(Student Performance: Percentage Score in English: Grade 2 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/5.g2_english_perc.png", width(2000) replace as(png)	

	 *** English (Grade 3) 
	
egen g3_english              = rsum(english_13_cs english_16_cs english_17_cs english_18_cs english_19_cs english_20_cs english_35_cs english_36_cs english_38_cs english_39_cs english_40_cs english_41_cs english_42_cs english_43_cs ///
                                english_44_cs) 
gen  g3_english_perc         = g3_english/15

label variable  g3_english                "English Grade 3 Questions"
label variable  g3_english_perc          "Percentage Score in English, Grade 3 Questions "

sum 		                    g3_english_perc,mean
#delimit ;
histogram 	                    g3_english_perc, 
								discrete title(Student Performance: Percentage Score in English: Grade 3 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/6.g3_english_perc.png", width(2000) replace as(png)	

		
				*** Math (Grade 1) 
				
egen g1_math                = rsum(math_1_cs math_2_cs math_5_cs math_6_cs math_7_cs math_10_cs english_25_cs english_26_cs english_42_cs) 
gen  g1_math_perc          = g1_math/9

label variable  g1_math                "Math Grade 1 Questions"
label variable  g1_math_perc          "Percentage Score in Math, Grade 1 Questions "

sum 		                    g1_math_perc,mean
#delimit ;
histogram 	                    g1_math_perc, 
								discrete title(Student Performance: Percentage Score in Math: Grade 1 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/7.g1_math_perc.png", width(2000) replace as(png)
			

			*** Math (Grade 2) 
			
egen g2_math                = rsum(math_3_cs math_8_cs math_9_cs math_13_cs math_15_cs math_16_cs english_17_cs english_18_cs english_19_cs english_22_cs english_23_cs english_24_cs english_32_cs english_34_cs) 
gen  g2_math_perc           = g2_math/14

label variable  g2_math                "Math Grade 2 Questions"
label variable  g2_math_perc          "Percentage Score in Math, Grade 2 Questions "

sum 		                    g2_math_perc,mean
#delimit ;
histogram 	                    g2_math_perc, 
								discrete title(Student Performance: Percentage Score in Math: Grade 2 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/8.g2_math_perc.png", width(2000) replace as(png)

			*** Math (Grade 3) 
			
egen g3_math                = rsum(math_4_cs math_11_cs math_12_cs math_14_cs math_20_cs math_28_cs math_33_cs math_35_cs) 
gen  g3_math_perc           = g3_math/8

label variable  g3_math                "Math Grade 3 Questions"
label variable  g3_math_perc          "Percentage Score in Math, Grade 3 Questions "

sum 		                    g3_math_perc,mean
#delimit ;
histogram 	                    g3_math_perc, 
								discrete title(Student Performance: Percentage Score in Math: Grade 3 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/9.g3_math_perc.png", width(2000) replace as(png)	


			*** Math (Grade 4) 
			
				
egen g4_math                = rsum(math_21_cs math_27_cs math_29_cs math_30_cs math_31_cs math_36_cs math_37_cs math_38_cs math_39_cs math_40_cs math_41_cs) 
gen  g4_math_perc           = g4_math/11

label variable  g4_math                "Math Grade 4 Questions"
label variable  g4_math_perc          "Percentage Score in Math, Grade 4 Questions "

sum 		                    g4_math_perc,mean
#delimit ;
histogram 	                    g4_math_perc, 
								discrete title(Student Performance: Percentage Score in Math: Grade 4 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/10.g4_math_perc.png", width(2000) replace	as(png)

	     	*** Listening (Grade 1) 
			
egen g1_listening	             = rsum(listening_1_cs listening_2_cs listening_3_cs listening_4_cs listening_5_cs)
gen  g1_listening_perc 	         = g1_listening/5

label variable  g1_listening	        "Listening Grade 1 Questions"
label variable  g1_listening_perc       "Percentage Score in Listening, Grade 1 Questions "

sum 		                    g1_listening_perc,mean
#delimit ;
histogram 	                    g1_listening_perc, 
								discrete title(Student Performance: Percentage Score in Listening: Grade 1 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/11.g1_listening_perc.png", width(2000) replace as(png)	

			
			*** Listening (Grade 2) 
						
egen g2_listening	             = rsum(listening_6_cs listening_7_cs)
gen  g2_listening_perc 	         = g2_listening/2

label variable  g2_listening	        "Listening Grade 2 Questions"
label variable  g2_listening_perc       "Percentage Score in Listening, Grade 2 Questions "

sum 		                    g2_listening_perc,mean
#delimit ;
histogram 	                    g2_listening_perc, 
								discrete title(Student Performance: Percentage Score in Listening: Grade 2 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/12.g2_listening_perc.png", width(2000) replace as(png)	


 
 			*** Total (Grade 1)
						
egen g1_total	               = rsum(g1_urdu g1_english g1_math g1_listening)
gen  g1_total_perc 	           = g1_total/31

label variable g1_total 	      "Total Score in Grade 1 Questions"
label variable g1_total_perc      "Percentage Score in Grade 1 Questions "

sum 		                    g1_total_perc,mean
#delimit ;
histogram 	                    g1_total_perc, 
								discrete title(Student Performance: Percentage Score in Grade 1 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/13.g1_total_perc.png", width(2000) replace as (png)	
 	
		   *** Total (Grade 2)
		   
egen g2_total	               = rsum(g2_urdu g2_english g2_math g2_listening)
gen  g2_total_perc 	           = g2_total/61

label variable g2_total 	      "Total Score in Grade 2 Questions"
label variable g2_total_perc      "Percentage Score in Grade 2 Questions "

sum 		                    g2_total_perc,mean
#delimit ;
histogram 	                    g2_total_perc, 
								discrete title(Student Performance: Percentage Score in Grade 2 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/14.g2_total_perc.png", width(2000) replace as (png)	   

           *** Total (Grade 3)
		   
egen g3_total	               = rsum(g3_urdu g3_english g3_math)
gen  g3_total_perc 	           = g3_total/38

label variable g3_total 	      "Total Score in Grade 3 Questions"
label variable g3_total_perc      "Percentage Score in Grade 3 Questions "

sum 		                    g3_total_perc,mean
#delimit ;
histogram 	                    g3_total_perc, 
								discrete title(Student Performance: Percentage Score in Grade 3 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/15.g3_total_perc.png", width(2000) replace as (png)  

           *** Total (Grade 4)
		   
gen g4_total	               = g4_math
gen  g4_total_perc 	           = g4_math/11

label variable g4_total 	      "Total Score in Grade 4 Questions"
label variable g4_total_perc      "Percentage Score in Grade 4 Questions "

sum 		                    g4_total_perc,mean
#delimit ;
histogram 	                    g4_total_perc, 
								discrete title(Student Performance: Percentage Score in Grade 4 Questions) 
								subtitle(Mean = `: di %3.2f r(mean)') 
								xline(`r(mean)', lpattern(solid) lcolor(black) extend)
								ytitle("")
								scheme(plotplain);

#delimit cr
graph export                     "$figures/16.g4_total_perc.png", width(2000) replace as (png)	
			




*** Save combined correct scoring sheet data and delete temporary folders

save "$processed/CSS_all.dta", replace

erase 	"$processed/CSS_Urdu_with_cs.dta"
erase 	"$processed/CSS_Math_with_cs.dta"
erase 	"$processed/CSS_English_with_cs.dta"
erase 	"$processed/CSS_Title_with_cs.dta"

clear all 

*** Merge data with school information and student roster

	*** We don't have school codes in the 'school confirmed student roster'. However, we do have it in M1B. 

	*** The unique identifier is called 'key' in the student roster with school codes and 'parent_key' in the school confirmed student roster.

use "$raw/M1B_Student Roster.dta", clear

* Rename the key variable

rename 		key parent_key
sort 		parent_key		

* Merge in the student level data

merge 1:m parent_key using "$raw/M1B_Student Roster-school_confirmed-student_roster.dta"

*** Perfect merge 

tab  _merge
drop _merge

/*
*** Check for duplicates (QD: code problem) 

destring student_id, replace

gen  indid = scode*1000+student_id
duplicates 	report indid
*/

		*** Strategy: keep only the latest observation per school

order scode student_id indid
sort  scode student_id

by 		scode student_id (submissiondate), sort: keep if _n == _N
duplicates report 	scode student_id

rename 	scode school_code
rename 	student_id child_id

merge 1:1 school_code child_id using "$processed/CSS_all.dta"

tab  	_merge
list 	school_code if _merge==2
keep 	if _merge==3
tab		_merge
drop 	_merge


/** Issues identified: 

1. There is no exam data for 4332 children. Absent?

2. School code 70050 doesn't have roster information. 

*/

save "$processed/CSS_all.dta", replace

clear all

*** Merge school characteristics

use 	"$raw/M2_nodup.dta", clear
rename 	scode school_code
sort	school_code

keep 	school_code m2_a4 m2_a6 m2_a10_1 m2_a10_2 
merge 	1:m school_code using "$processed/CSS_all.dta"

list school_code if _merge == 2
list school_code if _merge == 1

/** Issues identified  

1. We don't have information for the school with school code '51964' 

2. Schools with no roster information
		
		11042	
		11078	
		21126	
		21334	
		31592	
		41600	
		41671	
		51892	
		51970	
		70050	
		70137 
*/


keep if _merge == 3
drop 	_merge

save "$processed/CSS_all.dta", replace





*** New graphs

*** District Graphs

		encode 	district, 		gen(district2)
		egen 	tag 		= 	tag(district2) 
		label 	variable district2 	"Districts" 

foreach var of varlist *perc_score {
		capture drop   		mean
		egen 	mean 		= 	mean(`var'), by(district2) 
		replace mean 		= 	round(mean,0.01)
		label variable mean  		"Mean Scores by District" 
		sum				`var',mean
		#delimit ;
		twoway 	(bar mean district2 if tag)  
				(scatter mean district2, msymbol (i) mlabel (mean) mlabsize(*1.1) mlabpos(6) mlabcolor(white)   ),
				yscale(range(0.25 0.45))
				xlabel(1 "Attock" 2 " Faisalabad" 3 "Lahore" 4 "Muzaffargarh" 5 "Rahimyar" 6 "Sargodha" ,noticks) 
				title(`: var la `var'' by District) 
				subtitle(Mean = `: di %3.2f r(mean)') 
				yline(`r(mean)', lpattern(solid) lcolor(black) extend)
				legend (off)
				scheme(plotplain);
				#delimit cr
				graph export    "$figures/district_`var'.png", width(2000) replace
				
		}
		

*** Gender Graphs

label variable student_sex "Student Gender" 

foreach var of varlist urdu_perc_score eng_perc_score test_perc_score {
		capture drop   			mean
		egen 	mean 		= 	mean(`var'), by(student_sex) 
		replace mean 		= 	round(mean,0.01)
		label variable mean  		"Mean Scores by Gender" 
		sum				`var',mean
		#delimit ;
		twoway 	(bar mean student_sex)  
				(scatter mean student_sex, msymbol (i) mlabel (mean) mlabsize(*1.1) mlabpos(6) mlabcolor(white)   ),
				ylabel(0.30 0.35 0.45)
				xlabel(1 "Boy" 0 "Girl" ,noticks) 
				title(`: var la `var'' by Gender) 
				subtitle(Mean = `: di %3.2f r(mean)') 
				yline(`r(mean)', lpattern(solid) lcolor(black) extend)
				legend (off)
				scheme(plotplain);
				#delimit cr
				graph export    "$figures/gender_`var'.png", width(2000) replace
				
		}

foreach var of varlist listen_perc_score {
		capture drop   			mean
		egen 	mean 		= 	mean(`var'), by(student_sex) 
		replace mean 		= 	round(mean,0.01)
		label variable mean  		"Mean Scores by Gender" 
		sum				`var',mean
		#delimit ;
		twoway 	(bar mean student_sex)  
				(scatter mean student_sex, msymbol (i) mlabel (mean) mlabsize(*1.1) mlabpos(6) mlabcolor(white)   ),
				ylabel(0.65 0.75)
				xlabel(1 "Boy" 0 "Girl" ,noticks) 
				title(`: var la `var'' by Gender) 
				subtitle(Mean = `: di %3.2f r(mean)') 
				yline(`r(mean)', lpattern(solid) lcolor(black) extend)
				legend (off)
				scheme(plotplain);
				#delimit cr
				graph export    "$figures/gender_`var'.png", width(2000) replace
				
		}
		
foreach var of varlist math_perc_score {
		capture drop   			mean
		egen 	mean 		= 	mean(`var'), by(student_sex) 
		replace mean 		= 	round(mean,0.01)
		label variable mean  		"Mean Scores by Gender" 
		sum				`var',mean
		#delimit ;
		twoway 	(bar mean student_sex)  
				(scatter mean student_sex, msymbol (i) mlabel (mean) mlabsize(*1.1) mlabpos(6) mlabcolor(white)   ),
				ylabel(0.35 0.4)
				xlabel(1 "Boy" 0 "Girl" ,noticks) 
				title(`: var la `var'' by Gender) 
				subtitle(Mean = `: di %4.3f r(mean)') 
				yline(`r(mean)', lpattern(solid) lcolor(black) extend)
				legend (off)
				scheme(plotplain);
				#delimit cr
				graph export    "$figures/gender_`var'.png", width(2000) replace
				
		}
		

save "$processed/CSS_all.dta", replace


***

/**

<h2> Appendix 1: All Graphs </h2>

<h2> Urdu </h2>

<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/pie_urdu.png"/>


<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/hist_urdu.png"/>


<h2> Math </h2>

<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/pie_math.png"/>


<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/hist_math.png"/>


<h2> English </h2>

<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/pie_eng.png"/>


<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/hist_eng.png"/>


<h2> Listening Component </h2>

<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/pie_listen.png"/>


<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/hist_listen.png"/>

	
<h2> Overall Test Results </h2>

<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/pie_test.png"/>


<img class="center" src="/Users/Kabira/Documents/Dropbox/Pakistan/Data/3. Figures/hist_test.png"/>

*/
	
	
log close

*** Install weave command to create a pdf of the Log file 

net from http://data.princeton.edu/wws509/stata
net install weave

weave using "/Users/Kabira/Documents/Dropbox/Pakistan/Data/DO_Files/Punjab_M5_LEAPS.usl"


