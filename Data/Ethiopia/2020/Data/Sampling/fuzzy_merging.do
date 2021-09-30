*fuzzy linking ethiopia sampling frame
*Brian Stacy 10/15/2019

*go to working directory
cd "C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Ethiopia/2019/Data/Sampling/"

*******************
*Open up sampling frame with more complete info, rename some variables for easier merging
*******************
clear
use Sampling_Frame_2009.dta
rename *, lower
gen sid=_n
save Sampling_Frame_2009_updated.dta, replace

***********************
*open master sampling frame with admin school codes
***********************
use ETH_sampling_data.dta

*do some renaming for easier merging
rename *, lower
rename school school_name

*Do one bit of pre-processing where two schools have same name 
replace school_name = "Seattle Academy 2" if  _n==35649

*replace school names as lower case to match using dataset
replace school_name=strlower(school_name)

gen mid=_n

*attempt to merge onto database with school size using reclink2 fuzzy merging
reclink2 school_name region zone woreda using Sampling_Frame_2009_updated.dta, gen(matchscore) idmaster(mid) idusing(sid) wmatch(10 2 2 4)  orblock(region zone woreda) npairs(1) minscore(0.5)