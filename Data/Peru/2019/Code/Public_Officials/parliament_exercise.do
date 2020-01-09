*code to examine differences in responses before and after Sept 30, 2019, at which time Peru congress dissolved
log using "C:\Users\wb469649\WBG\Ezequiel Molina - Dashboard (Team Folder)\Country_Work\Peru\2019\Data\clean\Public_Officials\public_officials_parliament_exercise.log", text replace


*load data
use "C:\Users\wb469649\WBG\Ezequiel Molina - Dashboard (Team Folder)\Country_Work\Peru\2019\Data\clean\Public_Officials\public_officials_survey_data.dta" , replace

*keep just the public officials that are not director of HR, who are asked different questions
keep if director_hr==0

*creat indicator for whether survey took place before or after sept 30
split survey_time, parse(T) generate(td)
gen date=date(td1, "YMD")
gen date_dissolved=td(30sep2019)

gen after_dissolved=date>=date_dissolved



*calculate summary statistics for sample before and after dissolved


gl national_learning national_learning_goals targeting monitoring incentives community_engagement
gl mandates mandates_accountability  coherence transparency accountability 
gl quality quality_bureaucracy knowledge_skills work_environment merit motivation_attitudes motivation_relative_start			
gl impartial impartial_decision_making pol_personnel_management pol_policy_making pol_policy_implementation employee_unions_as_facilitators	

su $national_learning $mandates $quality $impartial

foreach var in $national_learning $mandates $quality $impartial {
di "`var'"
ttest `var' , by(after_dissolved)

}

*do just for mineduc staff
foreach var in $national_learning $mandates $quality $impartial {
di "`var'"
ttest `var' if govt_tier==1, by(after_dissolved)

}