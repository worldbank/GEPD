  * Author: Ahmed Raza
  * Date: 29th June, 2021
  * Purpose: Setting up data import pipeline:
 
********************************************************************************  
  
  *-----------------------------------------------------------------------------
  * 1) General program setup
  *-----------------------------------------------------------------------------
  clear               all
  capture log         close _all
  set more            off
  set varabbrev       off, permanently
  set emptycells      drop
  set maxvar          12000
  version             16

  
  *-----------------------------------------------------------------------------
  * 2) Define user-independant project paths:
  *-----------------------------------------------------------------------------

* Ahmed Raza 

  if inlist("`c(username)'","wb549384") {
    global folder "C:/Users/`c(username)'/WBG/Ezequiel Molina - Dashboard (Team Folder)"
}

* Add in more users:
  if inlist("`c(username)'","<>", "<>") {
    global folder "C:/Users/`c(username)'/WBG/Ezequiel Molina - Dashboard (Team Folder)"
}

  *-----------------------------------------------------------------------------
  * 3)  Set pathways:
  *-----------------------------------------------------------------------------
          
		  
  local input "Country_Work\Pakistan_all\2021\Data\01. Pilot\raw"
  
  * local code "02_programs"
  
  local output "Country_Work\Pakistan_all\2021\Data\01. Pilot\clean"
			
  * whereis gedp "$folder"
		
  
  global raw "$folder/`input'"
  global out "$folder/`output'"
  global do "$folder/`code'"

********************************************************************************



