########################################################################################################
#                                                                                                      #
#                         MADAGASCAR - SCHOOL SURVEY                                                   #
#                       FIRST GRADE ASSESSMENT + FOURTH GRADE + SCHOOL LVL                             #
#                                                                                                      #
#                         Adrien Ciret - aciret@worldbank.org                                          #
########################################################################################################

### PURPOSE : Creating indicators for the 1st grade assessment based on the data collected by the SDI team

### INPUTS : 
    ## DATASETS : M5.dta (1st grade assessment), 01-SECABD24DE.dta (school level infos)
    ## DOCUMENTS : Evaluation directe de la petite enfance (https://worldbankgroup-my.sharepoint.com/:w:/r/personal/molina_worldbank_org/_layouts/15/Doc.aspx?sourcedoc=%7B1166E903-5607-432B-AC32-22A81047DEB2%7D&file=SchoolSurvey_EvaluationDirecte_Madagascar%20FR.docx&action=default&mobileredirect=true)

## NOTES : 

## LATEST UPDATE: August 31, 2021

#------------------------------------------------------------------------------#
### 1. Packages, settings and data----

## Switches

  Reset <- 1
  backup_onedrive <- "no"

## Cleaning
  
  if(Reset == 1){
    rm(list = ls())
  } 
  
## Packages
  if(!require("pacman")) install.packages("pacman")
  
  pacman::p_load(
    
    httr, haven, dplyr, Hmisc, tidyr, here, vtable, stringr, naniar, purr, readr
    
  )
  
# Here you need to indicate the path where you replicated the folder structures on your own computer
here::here() 

#AC - Modification to the folder paths to be able to run the code localy
  if(Sys.info()["user"] == "AdrianoCiretto"){
    
    setwd(dir = "/Users/AdrianoCiretto/Documents/Github/GEPD")
    download_folder <- "/Users/AdrianoCiretto/Desktop/Education GP/02. Country_work/MAD/Technical/Data/Raw/School"
    confidential_folder <- "/Users/AdrianoCiretto/Desktop/Education GP/02. Country_work/MAD/Technical/Data/Clean/School"
  }


#------------------------------------------------------------------------------#
## 2. Calling in datasets ----  

## 1st grade assessment ----
first_grade <- read_dta(file.path(download_folder, "/astata/21-M5.dta"))

## 4th grade assessment ----
fourth_grade <- read_dta(file.path(download_folder, "/astata/20-M5_liste.dta"))

## School level data ----
school_dta<-read_dta(file.path(download_folder, "/astata/01-SECABD24DE.dta"))
vtable(school_dta)

## Infrastructure school ----
infrastructure_dta <- read_dta(file.path(download_folder, "/astata/06-M1SECC.dta"))
vtable(infrastructure_dta)

## Infrastructure school : blackboard, light, cholk ----
#input_dta <- read_dta(file.path(download_folder, "/astata/17-M4BC..dta"))
input_dta<-read_dta(file.path(download_folder, "/astata/01-SECABD24DE.dta"))

vtable(input_dta)

## Management school  ----
management_dta <- read_dta(file.path(download_folder, "/astata/12-M3A.dta"))
vtable(management_dta)

## Teacher roster ----
roster_dta <- read_dta(file.path(download_folder, "/astata/08-M2AB1AB.dta"))
vtable(roster_dta)


#rename a few key variables up front
school_dta<- school_dta %>%
  mutate(enumerator_name_other= s_enqueteur_name  ,
         enumerator_number=s_enqueteur_code ,
         survey_time=s_date_heure_fin	,
         school_code=s_ecole_code,
         lat=s_latitude,
         lon=s_longitude,
  ) %>%
  mutate(school_code=if_else(school_code==0, 328328, school_code)) %>%
  mutate(school_code=if_else(school_code==62181, 558163, school_code))  #fix an error where the school code was loaded incorrectly



#------------------------------------------------------------------------------#
## 3. Lists of variables ----  

#list additional info that will be useful to keep in each indicator dataframe
preamble_info <- c( "s_ecole_code", "s_eleve_code",
                   "s_region_code", "s_cisco_code", "s_commune_name",   
                   "s_commune_code", "s_ecole_name", "s_zap_name", "s_zap_code",
                   "s_urbain", "s_date_visite", "s_enqueteur_code", "s_type_ecole", "s_public", "s_etab_code", "d_lng_instr"
                   )

# List of topics to facilitate the cleaning later
list_topics<-c("vocabn", "comprehension","letters","words","sentence","nm_writing","print",
               "countingproduce_set","number_ident","number_compare","simple_add",
               "backward_digit","head_shoulders",
               "perspective","conflict_resol")

#------------------------------------------------------------------------------#
## 4. Functions ----  

## Function to save metadata for each question in each module
  makeVlist <- function(dta) { 
    varlabels <- sapply(dta, function(x) attr(x,"label"))
    vallabels <- sapply(dta, function(x) attr(x,"labels"))
    tibble(name = names(varlabels),
           varlabel = varlabels, vallabel = vallabels) 
  }

## Function to save the number of NAs per variable
  makeNAlist <- function(dta) { 
    varlabels <- sapply(dta, function(x) attr(x,"label"))
    vallabels <- sapply(dta, function(x) attr(x,"labels"))
    varmissing <- sapply(dta, function(x) sum(is.na(x)))
    tibble(name = names(varlabels),
           varlabel = varlabels, vallabel = vallabels, varmissing=varmissing) 
  }


## Function to create binaries
bin_var <- function(var, val) {
  case_when(
    var==val  ~ 1,
    var!=val   ~ 0,
    is.na(var) ~ as.numeric(NA))
}

#create function to clean teacher attitudes questions.  Need to reverse the order for scoring for some questions.  
#Should have thought about this, when programming in Survey Solutions and scale 1-5.

attitude_fun  <- function(x) {
  case_when(
    x==99 ~ as.numeric(NA),
    x==4 ~ 5,
    x==3 ~ 3.67,
    x==2 ~ 2.33,
    x==1 ~ 1
  )
}

attitude_fun_rev  <- function(x) {
  case_when(
    x==99 ~ as.numeric(NA),
    x==1 ~ 5,
    x==2 ~ 3.67,
    x==3 ~ 2.33,
    x==4 ~ 1
  )
}

#------------------------------------------------------------------------------#
## 5. Cleaning : 1st grade Level ----  

#create school database with just preamble info.  This will be useful for merging on school level info to some databases
school_data_preamble_temp <- first_grade %>%
  group_by(s_ecole_code) %>%
  select( preamble_info) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 

school_data_preamble <- first_grade %>%
  group_by(s_enqueteur_code, s_date_heure_fin) %>%
  select(s_ecole_code, s_date_heure_fin) %>%
  left_join(school_data_preamble_temp)



# School survey. Fraction correct on the Early Childhoold Assessment given to students in school.
first_grade_metadata <- makeVlist(first_grade)


#Add school preamble info
first_grade <- first_grade %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything())  


#create indicator for % correct on student assessment
#Note:  in the future we could incorporate irt program like mirt
#number of missing values
first_grade <- first_grade %>%
  mutate(n_mssing_ECD=n_miss_row(.))

#rename a few key variables up front
first_grade<- first_grade %>%
  mutate(ecd_student_name=d_m5e3_01  ,
         ecd_student_age=d_m5e3_02,
         ecd_student_male=bin_var(d_m5e3_03,1),
         ecd_consent=bin_var(d_m5e3_00,1)
         
  ) %>% # Rename the variables to have them describe clearly the type of assessment being conducted
  
  rename(
    # Vocabulary
    m6s2q1_vocabn = d_m5e3_04a,
    m6s2q1b_vocabn = d_m5e3_04b,
    
    # Comprehension
    m6s2q5a_comprehension = d_m5e3_05a,
    m6s2q5b_comprehension = d_m5e3_05b,
    m6s2q5c_comprehension = d_m5e3_05c, 
    m6s2q5d_comprehension = d_m5e3_05d, 
    m6s2q5e_comprehension = d_m5e3_05e, 
    
    # Letters
    m6s2q2a_letters = d_m5e3_06a,	
    m6s2q2b_letters = d_m5e3_06b,
    m6s2q2c_letters = d_m5e3_06c,
    m6s2q2d_letters = d_m5e3_06d,
    m6s2q2e_letters = d_m5e3_06e,
    m6s2q2f_letters = d_m5e3_06f,
    m6s2q2g_letters  = d_m5e3_06g,
    m6s2q2h_letters = d_m5e3_06h,
    
    # Words
    m6s2q3a_words = d_m5e3_07a,
    m6s2q3b_words = d_m5e3_07b,
    m6s2q3c_words= d_m5e3_07c,
    
    # Sentences
    m6s2q4a_sentence = d_m5e3_08a,
    m6s2q4b_sentence = d_m5e3_08b,
    m6s2q4c_sentence = d_m5e3_08c,	
    
    
    # Name writing
    m6s2q6a_name_writing = d_m5e3_09a,
    m6s2q6b_name_writing = d_m5e3_09b,
    
    # Print
    m6s2q7a_print = d_m5e3_10a,
    m6s2q7b_print = d_m5e3_10b,
    m6s2q7c_print = d_m5e3_10c,
    
    # Counting produce set
    m6s2q8_counting = d_m5e3_11,
    m6s2q9a_produce_set = d_m5e3_12a,
    m6s2q9b_produce_set = d_m5e3_12b,
    m6s2q9c_produce_set = d_m5e3_12c, # NOTE, it should be noted that in the ETH data there are only two while there are three for MAD
    
    # Number identical
    m6s2q10a_number_ident = d_m5e3_13a,
    m6s2q10b_number_ident = d_m5e3_13b,
    m6s2q10c_number_ident= d_m5e3_13c,
    m6s2q10d_number_ident= d_m5e3_13d,
    m6s2q10e_number_ident= d_m5e3_13e,
    m6s2q10f_number_ident= d_m5e3_13f,
    m6s2q10g_number_ident= d_m5e3_13g,
    m6s2q10h_number_ident= d_m5e3_13h,
    m6s2q10i_number_ident= d_m5e3_13i,
    m6s2q10j_number_ident= d_m5e3_13j,
    
    # Number comparison
    m6s2q11a_number_compare = d_m5e3_14a,	
    m6s2q11b_number_compare = d_m5e3_14b,
    m6s2q11c_number_compare = d_m5e3_14c,
    
    # Simple addition
    m6s2q12a_simple_add = d_m5e3_15a,
    m6s2q12b_simple_add = d_m5e3_15b,
    m6s2q12c_simple_add = d_m5e3_15c,
    
    # Backward digits
    m6s2q13a_backward_digit = d_m5e3_16c,
    m6s2q13b_backward_digit = d_m5e3_16d,
    m6s2q13c_backward_digit = d_m5e3_16e,
    m6s2q13d_backward_digit = d_m5e3_16f, # NOTE, should be noted that we do not take into account the first two obs as they are trials
    
    # Head and shoulders
    m6s2q14a_head_shoulders = d_m5e3_17a,
    m6s2q14b_head_shoulders = d_m5e3_17b,
    m6s2q14c_head_shoulders = d_m5e3_17c,
    m6s2q14d_head_shoulders = d_m5e3_17d,
    m6s2q14e_head_shoulders = d_m5e3_17e,
    m6s2q14f_head_shoulders = d_m5e3_18d,
    m6s2q14g_head_shoulders = d_m5e3_18e,
    m6s2q14h_head_shoulders = d_m5e3_18f,
    m6s2q14i_head_shoulders = d_m5e3_18g,
    m6s2q14j_head_shoulders = d_m5e3_18h,
    m6s2q14k_head_shoulders = d_m5e3_18i,
    m6s2q14l_head_shoulders= d_m5e3_18j,
    m6s2q14m_head_shoulders= d_m5e3_18k,
    m6s2q14n_head_shoulders= d_m5e3_18l,
    m6s2q14o_head_shoulders= d_m5e3_18m, # NOTE, should be noted that trials are again in the data. To ensure a correct matching between names and var, a verification was done with the M_6 DirectAssessement document.
    
    # Perspective
    m6s2q15a_perspective = d_m5e3_19a,
    m6s2q15b_perspective  = d_m5e3_19b,
    m6s2q15c_perspective = d_m5e3_19c, # NOTE, should be noted that the response provided by the students are not present in the data shared by the SDI team

    # Conflict resolution
    m6s2q16a_conflict_resol = d_m5e3_20a,
    m6s2q16b_conflict_resol = d_m5e3_20b # NOTE, should be noted that the response provided by the students are not present in the data shared by the SDI team
    
  )


# Create one copy of each dataframe that never gets touched and is carried forward to public folder
  first_grade_raw <- first_grade

# Recode ECD variables to be 1 if student got it correct and zero otherwise
  first_grade<- first_grade %>%
    mutate_at(vars(ends_with("comprehension"),
                   ends_with("letters"),
                   ends_with("words"),
                   ends_with("sentence"),
                   ends_with("nm_writing"),
                   ends_with("print"),
                   ends_with("produce_set"),
                   ends_with( "number_ident"),
                   ends_with("number_compare"),
                   ends_with("simple_add"),
                   ends_with("backward_digit"),
                   ends_with("perspective"),
                   ends_with("conflict_resol")), ~bin_var(.,1)  ) %>%
    mutate_at(vars(ends_with("head_shoulders")), ~if_else(.x==2,1,0,missing=NULL)) %>%
    mutate_at(vars(ends_with("vocabn")), ~case_when(.x==97 ~ as.numeric(NA),
                                                    .x==99 ~ 0,
                                                    .x==77 ~ 0,
                                                    (.x!=97 & .x!=99 & .x>=10) ~ 1,
                                                    (.x!=97 & .x!=99 & .x<10) ~ as.numeric(.x)/10,
                                                    is.na(.x) ~ as.numeric(NA))) %>%
    mutate_at(vars(ends_with("counting")), ~case_when(.x==97 ~ as.numeric(NA),
                                                      .x==99 ~ 0,
                                                      .x==77 ~ 0,
                                                      (.x!=97 & .x!=99 & .x>=30) ~ 1,
                                                      (.x!=97 & .x!=99 & .x<30) ~ as.numeric(.x)/30,
                                                      is.na(.x) ~ as.numeric(NA)))



    
    ## Modify manually the answers to questions 7 and 8 based on Vania's email from the 03/09/2021
    
   first_grade <- first_grade %>% 
     
     mutate(Q7_Q8_delete=rowSums(.[grep(x=colnames(first_grade), pattern="a_letters|b_letters|c_letters|d_letters|e_letters")], na.rm=TRUE)) %>% 
     
     mutate(
       
       #Question 7
       m6s2q7a_print = if_else(str_detect(s_ecole_code, "623|646|647|650|719|721|728|738|749|766|799") & Q7_Q8_delete ==0, NA_integer_, as.integer(m6s2q7a_print)),
       m6s2q7b_print = if_else(str_detect(s_ecole_code, "623|646|647|650|719|721|728|738|749|766|799") & Q7_Q8_delete ==0, NA_integer_,as.integer(m6s2q7b_print)),
       m6s2q7c_print = if_else(str_detect(s_ecole_code, "623|646|647|650|719|721|728|738|749|766|799") & Q7_Q8_delete ==0, NA_integer_, as.integer(m6s2q7c_print)),
       
       # Question 8
       m6s2q8_counting = if_else(str_detect(s_ecole_code, "623|646|647|650|719|721|728|738|749|766|799") & Q7_Q8_delete ==0, NA_integer_, as.integer(m6s2q8_counting))
       
       )
  
 
#------------------------------------------------------------------------------#
## 6. Constructing indicator : 1st grade level----  

  #### A. Literacy ----  
  #calculate # of literacy items
  #NOTE: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items
  
  
  lit_items<-colnames(first_grade[,str_detect(
    colnames(first_grade), "vocabn|comprehension|letters|words|sentence|nm_writing$|print")])
  
  first_grade$literacy_length<-length(lit_items)
  
  #calculate students lit items correct
  first_grade <- first_grade %>%
    mutate(ecd_literacy_student_knowledge=100*rowMeans(.[grep(x=colnames(first_grade), 
                                                              pattern="vocabn|comprehension|letters|words|sentence|nm_writing$|print")], na.rm=TRUE))
  
  #### B. Math ----  
  #calculate # of math items
  
  math_items<-colnames(first_grade[,str_detect(
    colnames(first_grade), "counting|produce_set|number_ident|number_compare|simple_add")])
  
  first_grade$math_length<-length(math_items)
  
  
  #calculate students math items correct
  first_grade <- first_grade %>%
    mutate(ecd_math_student_knowledge=100*rowMeans(.[grep(x=colnames(first_grade), 
                                                          pattern="counting|produce_set|number_ident|number_compare|simple_add")], na.rm=TRUE))
  
  #### C. Executive Functioning ----  
  #calculate # of Exec Function items
  
  exec_items<-colnames(first_grade[,str_detect(
    colnames(first_grade), "backward_digit|head_shoulders")])
  
  first_grade$exec_length<-length(exec_items)
  
  
  #calculate students excec items correct
  first_grade <- first_grade %>%
    mutate(ecd_exec_student_knowledge=100*rowMeans(.[grep(x=colnames(first_grade), 
                                                          pattern="backward_digit|head_shoulders")], na.rm=TRUE))
  
  #### D. Socio-Emotional ----  
  #calculate # of Exec Function items
  
  #NOTE:  Ending persepectives and conflict resolution in $ is a grep trick to make sure columns with trailing characters aren't included.  
  #This means specifically the perspetive_responses conflict_resol_responses columns, which are text and just for quality control.
  soc_items<-colnames(first_grade[,str_detect(
    colnames(first_grade), "perspective$|conflict_resol$")])
  
  first_grade$soc_length<-length(soc_items)
  
  
  #calculate students excec items correct
  first_grade <- first_grade %>%
    mutate(ecd_soc_student_knowledge=100*rowMeans(.[grep(x=colnames(first_grade), 
                                                         pattern="perspective$|conflict_resol$")], na.rm=TRUE))
  
  
  #### E. Total score ----  
  #calculate students percent correct
  first_grade <- first_grade %>%
    mutate(ecd_student_knowledge=(ecd_math_student_knowledge+ecd_literacy_student_knowledge+
                                    ecd_exec_student_knowledge + ecd_soc_student_knowledge)/4) %>%
    mutate(ecd_student_proficiency=100*as.numeric(ecd_student_knowledge>=80),
           ecd_math_student_proficiency=100*as.numeric(ecd_math_student_knowledge>=80),
           ecd_literacy_student_proficiency=100*as.numeric(ecd_literacy_student_knowledge>=80),
           ecd_exec_student_proficiency=100*as.numeric(ecd_exec_student_knowledge>=80),
           ecd_soc_student_proficiency=100*as.numeric(ecd_soc_student_knowledge>=80)
    ) 
  #save ecd data at student level anonymized
  first_grade_anon <- first_grade %>%
    select(s_ecole_code, s_date_heure_fin, ecd_student_age, ecd_student_male, 
           ecd_student_knowledge, ecd_math_student_knowledge, ecd_literacy_student_knowledge, ecd_soc_student_knowledge, ecd_exec_student_knowledge,
           ecd_student_proficiency, ecd_math_student_proficiency, ecd_literacy_student_proficiency, ecd_soc_student_proficiency, ecd_exec_student_proficiency,
           math_items, lit_items, soc_items, exec_items)
  
  
  save(first_grade_anon, first_grade_metadata, 
       file = file.path(confidential_folder, "dashboard_ecd_data.RData"))
  
  #calculate % correct for literacy, math, and total
  final_indicator_data_LCAP <- first_grade_anon %>%
    left_join(school_dta[,c('s_ecole_code',  's_enqueteur_code', 's_date_heure_fin')]) %>%
    group_by(s_ecole_code) %>%
    summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
    select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'))
  
  #Breakdowns of Male/Female
  final_indicator_data_LCAP_M <- first_grade_anon %>%
    left_join(school_dta[,c('s_ecole_code', 's_enqueteur_code', 's_date_heure_fin')]) %>%
    filter(ecd_student_male==1) %>%
    group_by(s_ecole_code) %>%
    summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
    select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'))
  
  final_indicator_data_LCAP_F <- first_grade_anon %>%
    left_join(school_dta[,c('s_ecole_code', 's_enqueteur_code', 's_date_heure_fin')]) %>%
    filter(ecd_student_male==0) %>%
    group_by(s_ecole_code) %>%
    summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
    select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'))
  
  

  
  #------------------------------------------------------------------------------#
  ## 7. Notes ----
  
  ## It is important to highlight the fact that for the moment, the school data with which the student one is being merged does not 
  # contains the necessary information to do a proper merging. 
  
  # Generally speaking, there is a need to create a variable that uniquely identifies each interview. In the other data, this variable is usually called "interview__key". 
  
  # Furthermore, there is no uniquely identifying variable that allow for a quick and rigorous identification of each students. 
  # Previously, this variable was called "ecd_assessment__id". 
  
  # Finally, the school data used at the beginning does not contain 1) the number of teacher and 2) the 
  # number of students per grade. These informations can be useful to do the aggregations necessary.
  
  
  #------------------------------------------------------------------------------#
  ## 8. Cleaning : 4th grade Level ----  
  
  ## Create the metadata for the 4th grade
  fourth_grade_metadata <- makeVlist(fourth_grade)
  
  #Add school preamble info
  fourth_grade <- fourth_grade %>%
    left_join(school_data_preamble) %>%
    select(preamble_info, everything()) 
  
  
  #number missing
  fourth_grade <- fourth_grade %>%
    mutate(n_mssing_LERN=n_miss_row(.))
  
  #create indicator for % correct on student assessment
  #Note:  in the future we could incorporate irt program like mirt
  
  
  #rename a few key variables up front
  fourth_grade<- fourth_grade %>%
    mutate(student_name=m5sa1q2  ,
           student_number=m5sa1q1,
           student_age=m5sa1q3,
           student_male=bin_var(m5sa1q4,1),
    )
  
  # Create one copy of each dataframe that never gets touched and is carried forward to public folder
  fourth_grade_raw <- fourth_grade
  
  
  # create a function to score questions m8saq2 and m8saq3, in which students identify letters/words that enumerator calls out.
  # This question is tricky, because enumerators would not always follow instructions to say out loud the same letters/words
  # In order to account for this, will assume if 80% of the class has a the exact same response, then this is the letter/word called out
  # Score this so that if there is a deviation from what 80% of the class says, then it is wrong.
  call_out_scorer <- function(var, pctl) {
    1-abs(var - quantile(var, pctl, na.rm=T))
  }
  
  
  fourth_grade <- fourth_grade %>%  # Rename the variables to have them match the standard ones
   
     rename(
      # Maths
       m8sbq3a_arithmetic = m5sabq3a,
       m8sbq3b_arithmetic = m5sabq3b,
       m8sbq3c_arithmetic = m5sabq3c,
       m8sbq3d_arithmetic = m5sabq3d,
       m8sbq3e_arithmetic = m5sabq3e,
       m8sbq3f_arithmetic = m5sabq3f,
       m8sbq3g_arithmetic = m5sabq3g,
       m8sbq3h_arithmetic = m5sabq3h,
       m8sbq3i_arithmetic = m5sabq3i,
       m8sbq3j_arithmetic = m5sabq3j,
       
       m8sbq4_arithmetic = m5sabq4, # corresponds to the question S_M5E2_23 in the questionnaire about the smallest result
       m8sbq5_word_problem = m5sabq5, # corresponds to the question S_M5E2_24 about oranges
       m8sbq6_sequences = m5sabq6, # corresponds to the question S_M5E2_25 about the sequence of number
       m8sbq1_number_sense = m5sabq1, # identification of numbers indicated by the enumerator
       # NOTE - on this one we assume that 0 means that no answers were correct while 3 means all 3 numbers were correctly identified
       m8sbq2_number_sense = m5sabq2, # order number from smallest to highest

       
       m8saq2_id = m5sa2q1, #corresponds to the number of letters correctly identified 
       m8saq3_id = m5sa2q2, #corresponds to the number of words correctly identified 
       m8saq4_id = m5sa2q3, # corresponds to the number of images correctly identified (from 0 to 4)
       
       m8saq5_story = m5sa2q6a, # first question on the text
       m8saq6_story = m5sa2q6b # second question on the text
       # NOTE - the third question corresponding to the variable m5sa2q6c in the SDI questionnaire does not seem to have an equivalent in the GEPD one
       
    )
  
  ## Recoding of literacy and maths variables
  
  #recode assessment variables to be 1 if student got it correct and zero otherwise
  fourth_grade<- fourth_grade %>%
    mutate_at(vars(starts_with("m8saq5"), 
                   starts_with("m8saq6"),
                   starts_with("m8sbq2"),
                   starts_with("m8sbq3"),
                   starts_with("m8sbq4"),
                   starts_with("m8sbq5"),
                   starts_with("m8sbq6"),
    ), ~bin_var(.,1)  ) %>% #now handle the special cases
    mutate(m8saq4_id=if_else(m8saq4_id==5,4, as.numeric(m8saq4_id))) %>% #fix case where some enumerators recorded the pre-filled answer.
    # mutate(m8saq7a_gir=bin_var(m8saq7a_gir, 3),
    #        m8saq7b_gir=bin_var(m8saq7b_gir, 3),
    #        m8saq7c_gir=bin_var(m8saq7c_gir, 2),
    #        m8saq7d_gir=bin_var(m8saq7d_gir, 3),
    #        m8saq7e_gir=bin_var(m8saq7e_gir, 4),
    #        m8saq7f_gir=bin_var(m8saq7f_gir, 1),
    #        m8saq7g_gir=bin_var(m8saq7g_gir, 2),
    #        m8saq7h_gir=bin_var(m8saq7h_gir, 2),
    #        m8saq7i_gir=bin_var(m8saq7i_gir, 4),
    #        m8saq7j_gir=bin_var(m8saq7j_gir, 1),
    #        m8saq7k_gir=bin_var(m8saq7k_gir, 3)) %>% #grade lonely giraffe question
  
  ## NOTE - no question on the lonely giraffe from the SDI team
    group_by(s_ecole_code) %>%
    mutate_at(vars(starts_with("m8saq2_id"),starts_with("m8saq3_id"), starts_with("m8sbq1_number_sense")),
              ~call_out_scorer(.,0.8)) %>%
    ungroup() %>%
    # mutate(m8saq2_id=(rowSums(.[grep(x=colnames(fourth_grade), pattern="m8saq2_id")])-7)/3, #subtract some letters not assessed and make out of 3 points
    #        m8saq3_id=(rowSums(.[grep(x=colnames(fourth_grade), pattern="m8saq3_id")])-7)/3) %>%
    mutate(m8saq2_id=if_else(m8saq2_id<0,0,m8saq2_id), #subtract some letters not assessed and make out of 3 points
           m8saq3_id=if_else(m8saq3_id<0,0,m8saq3_id)) %>%
    mutate(m8saq2_id=if_else(m8saq2_id>1,1,m8saq2_id), #subtract some letters not assessed and make out of 3 points
           m8saq3_id=if_else(m8saq3_id>1,1,m8saq3_id)) %>%
    mutate(m8saq4_id=if_else(m8saq4_id!=99, m8saq4_id/4,0)) %>% 
           
           #m8saq7_word_choice=bin_var(m8saq7_word_choice,2),
           # NOTE - not found in SDI dataset
           #m8sbq1_number_sense=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq1_number_sense")])-7)/3)         
          
    mutate( 
      m8sbq1_number_sense=if_else(m8sbq1_number_sense<0,0,m8sbq1_number_sense)) %>%
    mutate(
      m8sbq1_number_sense=if_else(m8sbq1_number_sense>1,1,m8sbq1_number_sense)) %>%
    select(-starts_with("m8saq2_id__"),-starts_with("m8saq3_id__"),-starts_with("m8sbq1_number_sense__"))
  
  
  #------------------------------------------------------------------------------#
  ## 9. Constructing indicators - Fourth grade ---    
  
  ####Literacy####
  #calculate # of literacy items
  #note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items
  fourth_grade$literacy_length<-length(grep(x=colnames(fourth_grade), pattern="m8saq"))
  
  lit_items<-colnames(fourth_grade[,grep(x=colnames(fourth_grade), pattern="m8saq")])
  
  
  #calculate students lit items correct
  fourth_grade <- fourth_grade %>%
    mutate(literacy_student_knowledge=100*rowMeans(.[grep(x=colnames(fourth_grade), pattern="m8saq")], na.rm=TRUE),
           literacy_student_knowledge_nogiraffe=100*rowMeans(.[c('m8saq4_id', 'm8saq5_story', 'm8saq6_story', 
                                                                 #'m8saq7_word_choice', '
                                                                 'm8saq2_id', 'm8saq3_id')], na.rm=TRUE))
  
  ####Math####
  #calculate # of math items
  fourth_grade$math_length<-length(grep(x=colnames(fourth_grade), pattern="m8sbq"))
  
  math_items<-colnames(fourth_grade[,grep(x=colnames(fourth_grade), pattern="m8sbq")])
  
  
  #calculate students math items correct
  fourth_grade <- fourth_grade %>%
    mutate(math_student_knowledge=100*rowMeans(.[grep(x=colnames(fourth_grade), pattern="m8sbq")], na.rm=TRUE))
  
  ####Total score####
  #calculate students percent correct
  fourth_grade <- fourth_grade %>%
    mutate(student_knowledge=(math_student_knowledge+literacy_student_knowledge)/2) %>%
    mutate(student_proficient=100*as.numeric(student_knowledge>=82.9), #34/41
           student_proficient_nogiraffe=100*as.numeric((literacy_student_knowledge_nogiraffe+math_student_knowledge)/2>=86.6), #12/13 points
           student_proficient_70=100*as.numeric(student_knowledge>=70),
           student_proficient_75=100*as.numeric(student_knowledge>=75),
           literacy_student_proficient_nogiraffe=100*as.numeric(literacy_student_knowledge_nogiraffe>=92), #12/13 points
           literacy_student_proficient=100*as.numeric(literacy_student_knowledge>=83.3), #20/24 points
           literacy_student_proficient_70=100*as.numeric(literacy_student_knowledge>=70),
           literacy_student_proficient_75=100*as.numeric(literacy_student_knowledge>=75),
           math_student_proficient=100*as.numeric(math_student_knowledge>=82), #14/17 points
           math_student_proficient_70=100*as.numeric(math_student_knowledge>=70),
           math_student_proficient_75=100*as.numeric(math_student_knowledge>=75))
  
  
  #save  4th grade data at student level anonymized
  assess_4th_grade_anon <- fourth_grade %>%
    select(s_ecole_code, s_eleve_code, student_age, student_male, 
           contains('student_proficient'),
           contains('student_knowledge'),
           contains('ses'),
           math_items, lit_items)
  
  
  assess_4th_grade_metadata <- makeVlist(fourth_grade)
  
  
  save(assess_4th_grade_anon, assess_4th_grade_metadata, 
       file = file.path(confidential_folder, "dashboard_4th_grade_assessment_data.RData"))
  
  
  
  
  
  #calculate % correct for literacy, math, and total
  final_indicator_data_LERN <- assess_4th_grade_anon %>%
    left_join(school_dta[,c('interview__key', 'm8_teacher_name', 'm8_teacher_code')]) %>%
    group_by(school_code) %>%
    mutate(n_students=n()) %>%
    summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
    select(-ends_with('length'), -ends_with('items') , -starts_with('interview'), -starts_with('enumerator'))
  
  
  #Breakdowns by Male/Female
  final_indicator_data_LERN_M <- assess_4th_grade_anon %>%
    left_join(school_dta[,c('interview__key', 'm8_teacher_name', 'm8_teacher_code')]) %>%
    filter(student_male==1) %>%
    group_by(school_code) %>%
    mutate(n_students=n()) %>%
    summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
    select(-ends_with('length'), -ends_with('items') , -starts_with('interview'), -starts_with('enumerator'))
  
  final_indicator_data_LERN_F <- assess_4th_grade_anon %>%
    left_join(school_dta[,c('interview__key', 'm8_teacher_name', 'm8_teacher_code')]) %>%
    filter(student_male==0) %>%
    group_by(school_code) %>%
    mutate(n_students=n()) %>%
    summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
    select(-ends_with('length'), -ends_with('items') , -starts_with('interview'), -starts_with('enumerator'))
  
  
  #------------------------------------------------------------------------------#
  ## 10. NOTE - Fourth grade ---  
  
  # In the end, most variables have been retrieved from the SDI dataset. 
  
  # It is however important to highlight that for instance 1) some questions like Non Verbal reasoning have been identified as belonging to
  # SDI questionnaire but not GEPD's one, while 2) question on the "lonely giraff" are not in the SDI questionnaire.
  
  # Aside from renaming the variables, the modification mainly constited in desactivating the lines of codes that were doing operation on variable
  # that existed initially in the wide format in Survey Solution. These were not necessary for the SDI one.
  
  
  
  #------------------------------------------------------------------------------#
  ## 11. Cleaning and creating indicators: Infrastructure school Level ----  
  
  # School survey. Total score starts at 1 and points added are the sum of whether a school has: 
  #   - Access to adequate drinking water 
  # -Functional toilets.  Extra points available if are separate for boys/girls, private, useable, and have hand washing facilities 
  # - Electricity  in the classroom 
  # - Internet
  # - School is accessible for those with disabilities (road access, a school ramp for wheelchairs, an entrance wide enough for wheelchairs, ramps to classrooms where needed, accessible toilets, and disability screening for seeing, hearing, and learning disabilities with partial credit for having 1 or 2 or the 3).)
  
  ## Create the metadata for the infrastructure
  infrastructure_metadata <- makeVlist(infrastructure_dta)

  
  #drinking water
  infrastructure_dta <- infrastructure_dta %>%
    #
    mutate(drinking_water=if_else(d_m12c_08a==1, 1,0, as.numeric(NA) ))
  
  #functioning toilets
  infrastructure_dta <- infrastructure_dta %>%
    mutate(toilet_exists=if_else(s_m1c_01==1 ,1,0),
           toilet_separate=if_else((s_m1c_02==1 | s_m1_c01a==3),1,0),#by separate here we mean between boys and girls as well as teachers and students
           toilet_private=as.numeric(s_m1c_03), #here it means having a door on the toilets for instance
           toilet_usable=as.numeric(s_m1c_04),
           toilet_handwashing=as.numeric(s_m1c_07)
           #toilet_soap=as.numeric(m1sbq8_infr)
           ) %>% # This one is included in the one above
    mutate(functioning_toilet=case_when(
      # exist, separate for boys/girls, clean, private, useable,  handwashing available
      toilet_exists==1 & toilet_usable==1 & toilet_separate==1  & toilet_private==1  & toilet_handwashing==1 ~ 1,
      toilet_exists==0 | toilet_usable==0 | toilet_separate==0  | toilet_private==0  | toilet_handwashing==0 ~ 0
    )) 
  
  #visibility ## ATTENTION TO MODIFY
  input_dta <- input_dta %>%
    left_join(infrastructure_dta %>% select(s_ecole_code, s_date_heure_fin, d_m1c_11c) %>% rename(s_date_heure_fin_f09=s_date_heure_fin)) %>% 
    select(s_ecole_code, s_date_heure_fin_f09, s_m4ab_15, s_m4ab_21, d_m1c_11c) %>%
    mutate(visibility=case_when(
      s_m4ab_15==1 &  s_m4ab_21==1  ~ 1,
      s_m4ab_15==2 & s_m4ab_21==1 ~ 0)) 
  
  #electricity
  infrastructure_dta <- infrastructure_dta %>%
    mutate(class_electricity=if_else(d_m1c_10==1,1,0)) 
  
  
  #accessibility for people with disabilities
  final_indicator_data_INFR <- infrastructure_dta %>%
    filter(s_type_ecole == 3) %>% # schools from sample
    group_by(s_ecole_code) %>%
    summarise_all(~first(na.omit(.))) %>%
    mutate(
      disab_road_access=if_else((s_m1c_09 == 1 | s_m1c_09 == 2 | s_m1c_09 == 3), 1, 0), #here the walking, boat and other were considered as no road access
      
    disab_school_ramp=case_when(
        d_m1c_15a==0 ~ 1,
        (d_m1c_15b	==1 & d_m1c_15a	==1) ~ 1,
        (d_m1c_15b	==2 & d_m1c_15a	==1) ~ 0,
        is.na(d_m1c_15b) ~ as.numeric(NA)),
  
      disab_school_entr=if_else(d_m1c_15c ==1, 1, 0),
  
      disab_class_ramp=case_when(
        d_m1c_15b==0 ~ 1,
        (d_m1c_15b==1 & d_m1c_15b==1) ~ 1,
        (d_m1c_15b==2 & d_m1c_15b==1) ~ 0,
        is.na(d_m1c_15b) ~ as.numeric(NA)),
  
     ##  disab_class_entr -THIS ONE DOES NOT EXIST IN THE MADAGASCAR DATA
  
      disab_screening=rowMeans(select(.,d_m1c_13_01,d_m1c_13_02,d_m1c_13_03), na.rm = TRUE),
  
      #sum up all components for overall disability accessibility score
      disability_accessibility=(disab_road_access+disab_school_ramp+disab_school_entr+
                                  disab_class_ramp+
                                  #disab_class_entr+ # DOES NOT EXIST HERE
                                  if_else(s_m1c_01==2,0,as.numeric(d_m1c_06a))+
                                  disab_screening)/6
    ) %>%
    mutate(internet=case_when(
      d_m1c_11c==1  ~ 1,
      d_m1c_11c==2   ~ 0,
      is.na(as.numeric(d_m1c_11c)) ~ 0,
      TRUE ~ 0) ) # 
  
  
  infr_list<-c('drinking_water', 'functioning_toilet', 'internet',  'class_electricity', 'disability_accessibility')
  
  final_indicator_data_INFR <- final_indicator_data_INFR %>%
    mutate(n_mssing_INFR=n_miss_row(.)) %>%
    mutate(infrastructure=(drinking_water+ functioning_toilet+ internet + class_electricity+ disability_accessibility)) %>%
    select(s_ecole_code, s_region_code, s_urbain, s_cisco_code, s_commune_code, s_ecole_name, s_zap_name, s_date_visite,s_zap_code,
           s_enqueteur_code, s_type_ecole, infrastructure, everything()) %>%
    select( -starts_with('interview'), -starts_with('enumerator'))  
  
  
  ##  Filtering out the schools to only keep the ones sampled for GEPD
  schools_sampled <- readxl::read_xlsx(here::here('Data/Raw','Echantillons SDI V11 éducation.xlsx')) %>% 
    filter(`Choisie (Dashboard)` == 1) %>% #keep the sample GEPD
    rename(s_ecole_name = `NOM ETAB`) %>% #rename for the matching
    select(s_ecole_name, `Choisie (Dashboard)`, sn)
  
  
  ## Apply filter
  final_indicator_data_INFR <- final_indicator_data_INFR %>% 
    left_join(schools_sampled) %>% 
    filter(`Choisie (Dashboard)`==1)
  
  
## Missing schools from dataset
  subset(schools_sampled, !(sn %in% final_indicator_data_INFR$sn)) %>% 
    writexl::write_xlsx(., path = paste0(download_folder, "Missing_schools_MAD.xlsx") , col_names = T, format_headers = T)

 ## Number of NAs for the schools in the samples 
  makeNAlist(final_indicator_data_INFR) %>% 
    writexl::write_xlsx(., path = paste0(download_folder, "NAs_schools_MAD.xlsx") , col_names = T, format_headers = T)
  
  
  #------------------------------------------------------------------------------#
  ## 12. Cleaning and creating indicators: Management school Level ---- 
  
  ############################################
  ##### School Operational Management ###########
  #############################################
  
  #Principals/head teachers are given two vignettes:
  #- One on solving the problem of a hypothetical leaky roof 
  #- One on solving a problem of inadequate numbers of textbooks.  
  #Each vignette is worth 2 points.  
  #
  #The indicator will measure two things: presence of functions and quality of functions. In each vignette: 
  #- 0.5 points are awarded for someone specific having the responsibility to fix 
  #- 0.5 point is awarded if the school can fully fund the repair, 0.25 points is awarded if the school must get partial help from the community, and 0 points are awarded if the full cost must be born by the community 
  #- 1 point is awarded if the problem is fully resolved in a timely manner, with partial credit given if problem can only be partly resolved.
  

  
  ## Create the metadata for the infrastructure
  management_metadata <- makeVlist(management_dta)
  
  ## Rename the variable in accordance with the matrix
  management_dta <- management_dta %>% 
    rename(
      
      # Vignette 1
      m7sbq1_opmn = d_m3ab_01, # do the school fix the problem
      m7sbq2_opmn= d_m3ab_02, # how
      m7sbq3_opmn = d_m3ab_03, # within one year
      m7sbq4_opmn = d_m3ab_04, # who if not the school 
      m7sbq5_opmn = d_m3ab_05, # still within a year?
      
      # Vignette 2
      m7scq1_opmn = d_m3ac_01, #responsible to provide textbook
      m7scq2_opmn = d_m3ac_02, 
      m7scq3_opmn =d_m3ac_03,
      m7scq4_opmn = d_m3ac_04,
      
      
      # Goal setting
      m7sdq1_pman = d_m3ad_01,
      m7sdq2_pman = d_m3ad_02,
      m7sdq4_pman = d_m3ad_04,
      m7sdq5_pman = d_m3ad_05,
      
      # Problem solving 
      m7seq1_pman = d_m3ae_01, 
      m7seq3_pman = d_m3ae_03,
      
      
      # Principal knowledge of the school
      m7sfq1_pknw = d_m3af_02,
      m7sfq2_pknw = d_m3af_03,
      m7sfq3_pknw = d_m3af_04, 
      m7sfq4_pknw = d_m3af_05,
      m7sfq5_pknw = d_m3af_06,
      m7sfq6_pknw = d_m3af_07,
      m7sfq7_pknw = d_m3af_08,
      m7sfq10_pknw = d_m3af_1103,
      m7sfq11_pknw = d_m3af_12,
      m7sfq12_pknw = d_m3af_13, 
      m7sfq13_pknw = d_m3af_14, 
      m7sfq14_pknw = d_m3af_15, 
      
      ## NOTE - the questions like m7sfq15(a)_pknw about the policy knowledge of the principal appear to be missing
      
      # Recruitement, training and evaluation
      m7sgq2_ssld = d_m3ag_02 ,
      m7sgq3_ssup = d_m3ag_03,
      m7sgq5_ssup = d_m3ag_05,
      m7sgq7_ssup = d_m3ag_07,
      m7sgq8_sevl = d_m3ag_08,
      m7sgq9_sevl = d_m3ag_09,
      
      #Job satisfaction
      m7shq1_satt = d_m3ah_01, #satisfy 
      m7shq2_satt = d_m3ah_02 # public net salary
      
    ) %>% 

    # clean variable names in wide format
    setNames(tolower(gsub("d_m3ac_05","m7scq5_opmn",names(.)))) %>% 
    setNames(tolower(gsub("d_m3ad_03","m7sdq3_pman",names(.)))) %>% # d_m3ad_03 01 to 05
    setNames(tolower(gsub("d_m3ae_02","m7seq2_pman",names(.)))) %>% # d_m3ae_02a to f
    setNames(tolower(gsub("d_m3ag_01","m7sgq1_ssld",names(.)))) %>% #d_m3ag_01 01 06 08 97
    setNames(tolower(gsub("d_m3ag_04","m7sgq4_ssup",names(.)))) %>% # d_m3ag_04 01  to 03 99
    setNames(tolower(gsub("d_m3ag_06","m7sgq6_ssup",names(.)))) %>% #d_m3ag_06a to t
    setNames(tolower(gsub("d_m3ag_10","m7sgq10_sevl",names(.)))) %>% # d_m3ag_10 01 to 09 99
    setNames(tolower(gsub("d_m3ag_11","m7sgq11_sevl",names(.)))) %>% # d_m3ag_11 01 06 99 98
    setNames(tolower(gsub("d_m3ag_12","m7sgq12_sevl",names(.)))) # d_m3ag_12 01 05 99 98
  
  
  
  #############################################
  ##### Problem solving - vignettes ###########
  #############################################  
  final_indicator_data_OPMN <- management_dta %>%
    group_by(s_ecole_code) %>%
    summarise_all(~first(na.omit(.))) %>%
    mutate(
      vignette_1_resp=if_else((m7sbq1_opmn==0 & (m7sbq4_opmn==4 | m7sbq4_opmn==98)), 0, 0.5),
      vignette_1_finance=case_when(
        m7sbq2_opmn==1 ~ 0.5,
        (m7sbq2_opmn==2 | m7sbq2_opmn==97) ~ 0.25,
        m7sbq2_opmn==3 ~ 0,
        m7sbq1_opmn==0 & !(m7sbq4_opmn==4 | m7sbq4_opmn==98) ~ 0.5),
      vignette_1_address=if_else(m7sbq1_opmn==1, case_when(
        m7sbq3_opmn==1 ~ 0,
        (m7sbq3_opmn==2 | m7sbq3_opmn==97) ~ .5,
        m7sbq3_opmn==3 ~ 1),
        case_when(
          m7sbq5_opmn==1 ~ 0,
          m7sbq5_opmn==2 ~ .5,
          m7sbq5_opmn==3 ~ 1))) %>% 
    #give total score for this vignette
    mutate(vignette_1=vignette_1_resp+vignette_1_finance+vignette_1_address) %>% 
    mutate(vignette_2_resp=if_else(m7scq1_opmn==98, 0, 0.5), # no one responsible that is known
           vignette_2_finance=if_else(m7scq1_opmn==1,0,0.5),      #parents are forced to buy textbooks          
           #give partial credit based on how quickly it will be solved <1 month, 1-3, 3-6, 6-12, >1 yr
           vignette_2_address=case_when(
             m7scq2_opmn==1 ~ 1,
             m7scq2_opmn==2 ~ .75,
             m7scq2_opmn==3 ~ .5,
             m7scq2_opmn==4 ~ .25,
             (m7scq2_opmn==5 |m7scq2_opmn==98) ~ 0)) %>%
    mutate(vignette_2=vignette_2_resp+vignette_2_finance+vignette_2_address) %>%          #sum all components for overall score
    mutate(operational_management=1+vignette_1+vignette_2 )
  
  
  final_indicator_data_OPMN <- final_indicator_data_OPMN %>%
    mutate(n_mssing_OPMN=n_miss_row(.))  %>%
    select( -starts_with('interview'), -starts_with('enumerator'))  
  
  #Breakdowns by Male/Female
  # final_indicator_data_OPMN_M <- final_indicator_data_OPMN %>%
  #   filter(m7saq10==1) %>%
  #   mutate(n_mssing_OPMN=n_miss_row(.))  %>%
  #   select( -starts_with('interview'), -starts_with('enumerator'))  
  # 
  # final_indicator_data_OPMN_F <- final_indicator_data_OPMN %>%
  #   filter(m7saq10==2) %>%
  #   mutate(n_mssing_OPMN=n_miss_row(.))  %>%
  #   select( -starts_with('interview'), -starts_with('enumerator'))  
  
 
  
  #############################################
  ##### School Principal School Knowledge ###########
  #############################################
  # The aim of this indicator is to measure the extent to which principals have the knowledge about their own schools that is necessary for them to be effective managers. A score from 1 to 5 capturing the extent to which the principal is familiar with certain key aspects of the day-to-day workings of the school (in schools that have principals). Principal receives points in the following way: 
  #   - 5 points. Principal gets all 90-100% of questions within accuracy bounds (defined below). 
  # - 4 points. Principal gets 80-90% of question within accuracy bounds. 
  # - 3 points. Principal gets 70-80% of question within accuracy bounds. 
  # - 2 points. Principal gets 60-70% of question within accuracy bounds. 
  # - 1 points. Principal gets under 60% of question within accuracy bounds. 
  # 
  # Accuracy bounds for each question. 
  # Within 1 teacher/student for each of the following: 
  #   - Out of these XX teachers, how many do you think would be able to correctly add triple digit numbers (i.e. 343+215+127)? 
  #   - Out of these XX teachers, how many do you think would be able to correctly to multiply double digit numbers (i.e. 37 x 13)? 
  #   - Out of these XX teachers, how many do you think would be able to complete sentences with the correct world (i.e. The accident _____ (see, saw, had seen, was seen) by three people)? 
  #   - Any of these XX teachers have less than 3 years of experience? 
  #   - Out of these XX teachers, which ones have less than 3 years of experience as a teacher? 
  # Within 3 teacher/student for each of the following: 
  #   - In the selected 4th grade classroom, how many of the pupils have the relevant textbooks? 
  #   Must identify whether or not blackboard was working in a selected 4th grade classroom.
  
  #------------------------------------------------------------------------------#
  ## NOTE - from here the code no longer works because it relies on the teacher assessment to have the number of teacher and other indicators
  

                    # #first create a database containing actual values for each question for the principal
                    # pknw_actual_cont <- management_dta %>%
                    #   select(school_code, m5_teach_count, m5_teach_count_math, m5s2q1c_number, m5s2q1e_number, m5s1q1f_grammer ) 
                    # 
                    # pknw_actual_exper <- teacher_questionnaire %>%
                    #   select(school_code, m3sb_tnumber, m3sb_troster,m3saq5, m3saq6 ) %>%
                    #   mutate(experience=(2019-m3saq5)) %>%
                    #   filter(experience <3) %>% 
                    #   group_by(school_code) %>%
                    #   summarise(teacher_count_experience_less3=n())
                    # 
                    # pknw_actual_school_inpts <- final_indicator_data_INPT %>%
                    #   select(school_code, blackboard_functional, m4scq5_inpt, m4scq4_inpt)
                    # 
                    # pknw_actual_combined <- pknw_actual_school_inpts %>%
                    #   left_join(pknw_actual_cont) %>%
                    #   left_join(pknw_actual_exper) %>%
                    #   mutate(teacher_count_experience_less3=if_else(is.na(teacher_count_experience_less3), as.numeric(0), as.numeric(teacher_count_experience_less3)),
                    #          m5s2q1c_number=m5s2q1c_number*m5_teach_count,
                    #          m5s2q1e_number=m5s2q1e_number*m5_teach_count,
                    #          m5s1q1f_grammer=m5s1q1f_grammer*m5_teach_count)
                    # 
                    # 
                    # #create function to compare principal responses to actual
                    # # if principal is within 1 student/teacher, then score as 1, 0 otherwise
                    # principal_scorer <- function(var_guess, var_actual, var_total, margin1, margin2) {
                    #   if_else(
                    #     ((1-abs(var_guess-var_actual)/var_total>= as.numeric(margin1)) | (var_guess-var_actual <= as.numeric(margin2))),
                    #     1,
                    #     0)
                    # }
                    # 
                    # 
                    # 
                    # final_indicator_data_PKNW <- school_data_PKNW %>%
                    #   group_by(school_code) %>%
                    #   select(school_code, starts_with('m7sfq5_pknw'), starts_with('m7sfq6_pknw'), starts_with('m7sfq7_pknw'), m7sfq9_pknw_filter, m7sfq10_pknw, m7sfq11_pknw, m7_teach_count_pknw, m7saq10) %>%
                    #   summarise_all(~first(na.omit(.))) %>%
                    #   mutate(n_mssing_PKNW=n_miss_row(.))  %>%
                    #   select( -starts_with('interview'), -starts_with('enumerator'))  %>%
                    #   left_join(pknw_actual_combined) %>%
                    #   mutate_at(vars(starts_with('m7sfq5_pknw'), starts_with('m7sfq6_pknw'), starts_with('m7sfq7_pknw')), ~if_else(is.na(.),as.numeric(NA),1)) %>%
                    #   mutate(add_triple_digit_pknw=principal_scorer(rowSums(select(.,starts_with('m7sfq5_pknw')), na.rm=T), m5s2q1c_number, m7_teach_count_pknw,0.8,1),
                    #          multiply_double_digit_pknw=principal_scorer(rowSums(select(.,starts_with('m7sfq6_pknw')), na.rm=T), m5s2q1e_number, m7_teach_count_pknw,0.8,1),
                    #          complete_sentence_pknw=principal_scorer(rowSums(select(.,starts_with('m7sfq7_pknw')), na.rm=T), m5s1q1f_grammer, m7_teach_count_pknw,0.8,1),
                    #          experience_pknw=principal_scorer(m7sfq9_pknw_filter, teacher_count_experience_less3, m7_teach_count_pknw,0.8,1),
                    #          textbooks_pknw=principal_scorer(m7sfq10_pknw, m4scq5_inpt, m4scq4_inpt,0.8,3),
                    #          blackboard_pknw=if_else(m7sfq11_pknw==blackboard_functional,1,0)) %>%
                    #   mutate(principal_knowledge_avg=rowMeans(select(.,add_triple_digit_pknw, multiply_double_digit_pknw, complete_sentence_pknw, experience_pknw, textbooks_pknw, blackboard_pknw), na.rm=TRUE)) %>%
                    #   mutate(principal_knowledge_score=case_when(
                    #     principal_knowledge_avg ==1 ~ 5,
                    #     (principal_knowledge_avg >=5/6 & principal_knowledge_avg<1) ~ 4,
                    #     (principal_knowledge_avg >=4/6 & principal_knowledge_avg<5/6) ~ 3,
                    #     (principal_knowledge_avg >=3/6 & principal_knowledge_avg<4/6) ~ 2,
                    #     (principal_knowledge_avg <3/6 ) ~ 1  )
                    #   ) %>%
                    #   select(school_code, starts_with('m7sfq5_pknw'),m5s2q1c_number, starts_with('m7sfq6_pknw'), m5s2q1e_number, starts_with('m7sfq7_pknw'), m5s1q1f_grammer, m7sfq9_pknw_filter, teacher_count_experience_less3,  m7sfq10_pknw,m4scq5_inpt,  m7sfq11_pknw, blackboard_functional, principal_knowledge_score, add_triple_digit_pknw, 
                    #          multiply_double_digit_pknw, complete_sentence_pknw, experience_pknw, textbooks_pknw, blackboard_pknw, m7_teach_count_pknw,m7saq10) %>%
                    #   select(school_code, starts_with('m7sfq5_pknw'), starts_with('m7sfq6_pknw'), starts_with('m7sfq7_pknw'), m7sfq9_pknw_filter, m7sfq10_pknw, m7sfq11_pknw, principal_knowledge_score, add_triple_digit_pknw, 
                    #          multiply_double_digit_pknw, complete_sentence_pknw, experience_pknw, textbooks_pknw, blackboard_pknw, m7_teach_count_pknw, m7saq10)
                    # 
                    # 
                              # #Breakdowns by Male/Female
                              # final_indicator_data_PKNW_M <- final_indicator_data_PKNW %>%
                              #   filter(m7saq10==1) %>%
                              #   select( -starts_with('interview'), -starts_with('enumerator'))  
                              # 
                              # final_indicator_data_PKNW_F <- final_indicator_data_PKNW %>%
                              #   filter(m7saq10==2) %>%
                              #   select( -starts_with('interview'), -starts_with('enumerator'))  
  
  #############################################
  ##### School Principal Management Skills ###########
  #############################################
  
  
  # Score of 1-5 based on sum of following: 
  #   - 1 Point. School Goals Exists 
  # - 1 Point. School goals are clear to school director, teachers, students, parents, and other members of community (partial credit available) 
  # - 1 Point. Specific goals related to improving student achievement ( improving test scores, improving pass rates, reducing drop out, reducing absenteeism, improving pedagogy, more resources for infrastructure, more resources for inputs) 
  # - 1 Point. School has defined system to measure goals (partial credit available)
  
  #Create variables for whether school goals exists, are clear, are relevant to learning, and are measured in an appropriate way.
  
  
  final_indicator_data_PMAN <- management_dta %>%
    mutate(sch_goals_exist=bin_var(m7sdq1_pman,1),
           sch_goals_clear=if_else(m7sdq1_pman==1, 
                                   rowMeans(select(.,m7sdq3_pman01, m7sdq3_pman02, m7sdq3_pman03, m7sdq3_pman04, m7sdq3_pman05), na.rm=TRUE),
                                   0) ,
           sch_goals_relevant_total=rowSums(select(.,d_m3ad_04a, d_m3ad_04b, d_m3ad_04d, d_m3ad_04e, d_m3ad_04f, d_m3ad_04g, d_m3ad_04h, d_m3ad_04i, d_m3ad_04j), na.rm=TRUE),
           sch_goals_relevant=if_else(m7sdq1_pman==1, 
                                      case_when(
                                        (sch_goals_relevant_total > 0) ~ 1,
                                        (sch_goals_relevant_total == 0) ~ 0),
                                      0),
           sch_goals_measured=if_else((m7sdq1_pman==1), 
                                      case_when(
                                        (m7sdq5_pman==1) ~ 0,
                                        (m7sdq5_pman==2 | m7sdq5_pman==97 ) ~ 0.5,
                                        (m7sdq5_pman==3) ~ 1),
                                      0)) %>%
    mutate(goal_setting=1+sch_goals_exist+sch_goals_clear+sch_goals_relevant+sch_goals_measured) %>%
    
    # Now for problem solving
    mutate(
      problem_solving_proactive=case_when(
        (m7seq1_pman==4 ) ~ 1,
        (m7seq1_pman==2 | m7seq1_pman==3 ) ~ 0.5,
        (m7seq1_pman==1 | m7seq1_pman==98 ) ~ 0,
        TRUE ~ 0),
      problem_solving_info_collect=(m7seq2_pmana+m7seq2_pmanb + m7seq2_pmanc + m7seq2_pmand)/4,
      problem_solving_stomach=case_when(
        (m7seq3_pman==4 ) ~ 1,
        (m7seq1_pman==3 ) ~ 0.5,
        (m7seq1_pman==1 | m7seq1_pman==2 | m7seq1_pman==98 ) ~ 0.25,
        TRUE ~ 0)
      
    ) %>%
    mutate(problem_solving=1+(4/3)*problem_solving_proactive+(4/3)*problem_solving_info_collect+(4/3)*problem_solving_stomach) %>%
    
    mutate(principal_management=(goal_setting+problem_solving)/2)
  
  final_indicator_data_PMAN <- final_indicator_data_PMAN %>%
    group_by(s_ecole_code	) %>%
    summarise_all(~first(na.omit(.))) %>%
    mutate(n_mssing_PMAN=n_miss_row(.))  %>%
    select( -starts_with('interview'), -starts_with('enumerator'))  
  
  
  
  #------------------------------------------------------------------------------#
  ## NOTE - this does not work because in the School Management dataset (number 11),
  ## there is no question on the gender of the respondent.
  
  
                        #Breakdowns by Male/Female
                        # final_indicator_data_PMAN_M <- final_indicator_data_PMAN %>%
                        #   filter(m7saq10==1) %>%
                        #   select( -starts_with('interview'), -starts_with('enumerator'))  
                        # 
                        # final_indicator_data_PMAN_F <- final_indicator_data_PMAN %>%
                        #   filter(m7saq10==2) %>%
                        #   select( -starts_with('interview'), -starts_with('enumerator'))    
                        # 
                        # 

  
  
  
  
  #------------------------------------------------------------------------------#
  ##### NOTE - the codes below does not work because I could not find the data about policy knowledge from the principal
  
  #############################################
  ##### School School Management Clarity of Functions  ###########
  #############################################
  
                          # school_data_SCFN <- management_dta %>%
                          #   mutate(infrastructure_scfn=if_else((m7sfq15a_pknw__0==1 | m7sfq15a_pknw__98==1),0,1),
                          #          materials_scfn=if_else((m7sfq15b_pknw__0==1 | m7sfq15b_pknw__98==1),0,1),
                          #          hiring_scfn=if_else((m7sfq15c_pknw__0==1 | m7sfq15c_pknw__98==1),0,1),
                          #          supervision_scfn=if_else((m7sfq15d_pknw__0==1 | m7sfq15d_pknw__98==1),0,1),
                          #          student_scfn=if_else((m7sfq15e_pknw__0==1 | m7sfq15e_pknw__98==1),0,1),
                          #          principal_hiring_scfn=if_else((m7sfq15f_pknw__0==1 | m7sfq15f_pknw__98==1),0,1),
                          #          principal_supervision_scfn=if_else((m7sfq15g_pknw__0==1 | m7sfq15g_pknw__98==1),0,1)
                          #   ) %>%
                          #   mutate(sch_management_clarity=1+
                          #            (infrastructure_scfn+materials_scfn)/2+
                          #            (hiring_scfn + supervision_scfn)/2 +
                          #            student_scfn +
                          #            (principal_hiring_scfn+ principal_supervision_scfn)/2
                          #   )
                          # final_indicator_data_SCFN <- school_data_SCFN %>%
                          #   group_by(school_code) %>%
                          #   summarise_all(~first(na.omit(.))) %>%
                          #   mutate(n_mssing_SCFN=n_miss_row(.))  %>%
                          #   select( -starts_with('interview'), -starts_with('enumerator'))  
  
  #############################################
  ##### School School Management Attraction  ###########
  #############################################
  
  # This policy lever measures whether the right candidates are being attracted to the profession of school principals. The questions will aim to capture the provision of benefits to attract and maintain the best people to serve as principals. 
  # 
  # Scoring: 
  #   -score is between 1-5 based on how satisfied the principal is with status in community. We will also add in component based on Principal salaries.
  # For salary, based GDP per capita from 2018 World Bank  https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=JO.  
  
  school_data_SATT <- management_dta %>%
    mutate(principal_satisfaction=attitude_fun_rev(m7shq1_satt),
           principal_salary=12*m7shq2_satt/3011.67	) %>%
    mutate(
      principal_salary_score=case_when(
        between(principal_salary,0,0.5) ~ 1,
        between(principal_salary,0.5,0.75) ~ 2,
        between(principal_salary,0.75,1) ~ 3,
        between(principal_salary,1,1.5) ~ 4,
        between(principal_salary,1.5,5) ~ 5)) %>%
    mutate(sch_management_attraction=(principal_satisfaction+principal_salary_score)/2) %>% 
    rename(school_code = s_ecole_code)
  
  final_indicator_data_SATT <- school_data_SATT %>%
    group_by(school_code) %>%
    summarise_all(~first(na.omit(.))) %>%
    mutate(n_mssing_SATT=n_miss_row(.))  %>%
    select( -starts_with('interview'), -starts_with('enumerator'))  
  
  
  
  #############################################
  ##### School School Management Selection and Deployment  ###########
  #############################################
  
  
  # This policy lever measures whether the right candidates being selected. These questions will probe what the recruitment process is like to 
  # ensure that these individuals are getting the positions. The question would ultimately be based on: 1) there is a standard approach for selecting principals,
  # 2) that approach relies on professional/academic requirements, and 3) those requirements are common in practice. 
  # 
  # Scoring: 
  #   - 1 (lowest score) Most important factor is political affiliations or ethnic group. 
  # - 2 Political affiliations or ethnic group is a consideration, but other factors considered as well. 
  # - 3 Most important factor is years of experience, good relationship with owner/education department, and does not factor in quality teaching, demonstrated management qualities, or knowledge of local community. 
  # - 4 Quality teaching, demonstrated management qualities, or knowledge of local community is a consideration in hiring, but not the most important factor 
  # - 5 Quality teaching, demonstrated management qualities, or knowledge of local community is the most important factor in hiring.
  
  school_data_SSLD <- management_dta %>%
    mutate(sch_selection_deployment=case_when(
      (m7sgq2_ssld==2 | m7sgq2_ssld==3 | m7sgq2_ssld==8) ~ 5,
      (m7sgq2_ssld==6 | m7sgq2_ssld==7) ~ 1,
      (!(m7sgq2_ssld==6 | m7sgq2_ssld==7) & (m7sgq1_ssld02==1 | m7sgq1_ssld03==1 | m7sgq1_ssld08==1) ) ~ 4,
      (!(m7sgq2_ssld==6 | m7sgq2_ssld==7) & (m7sgq1_ssld01==1 | m7sgq1_ssld04==1 | m7sgq1_ssld05==1 | m7sgq1_ssld99==1) ) ~ 3,
      (m7sgq1_ssld06==1 | m7sgq1_ssld07==1 ) ~ 2, 
      TRUE ~ as.numeric(NA))
    )%>% 
    rename(school_code = s_ecole_code)
  
  final_indicator_data_SSLD <- school_data_SSLD %>%
    group_by(school_code) %>%
    summarise_all(~first(na.omit(.))) %>%
    mutate(n_mssing_SSLD=n_miss_row(.))  %>%
    select( -starts_with('interview'), -starts_with('enumerator'))  
  
  
  
  #############################################
  ##### School School Management Support  ###########
  #############################################
  
  
  # This policy lever measures the extent to which principals receive training and/or exposure to other professional opportunities that could help them be better school leaders. 
  # The questions aim to figure out if such programs are provided, and if they are, their level of quality. 
  # 
  # Scoring (sum of components below): 
  #   - 1 Point. Principal has received formal training on managing school. 
  # - 1/3 Point. Had management training for new principals. 
  # - 1/3 Point. Had management in-service training. 
  # - 1/3 Point. Had mentoring/coaching by experienced principals. 
  # - 1 Point. Have used skills gained at training. 
  # - 1 Point. Principals offered training at least once per year
  
  school_data_SSUP <- management_dta %>%
    mutate(prinicipal_trained=bin_var(m7sgq3_ssup,1),
           principal_training=if_else(m7sgq3_ssup==1,
                                      rowMeans(.[grep(x=colnames(management_dta), 
                                                      pattern="^m7sgq4_ssup[0-9]{2}$")], na.rm=TRUE),
                                      0),
           principal_used_skills=if_else(m7sgq3_ssup==1,
                                         bin_var(m7sgq5_ssup,1),0),
           principal_offered=if_else((m7sgq7_ssup==2 | m7sgq7_ssup==3 | m7sgq7_ssup==4 | m7sgq7_ssup==5),1,0)
    ) %>%
    mutate(sch_support=1+prinicipal_trained+principal_training+principal_used_skills+principal_offered)%>% 
    rename(school_code = s_ecole_code)
  
  final_indicator_data_SSUP <- school_data_SSUP %>%
    group_by(school_code) %>%
    summarise_all(~first(na.omit(.))) %>%
    mutate(n_mssing_SSUP=n_miss_row(.))  %>%
    select( -starts_with('interview'), -starts_with('enumerator'))  
  
  
  #############################################
  ##### School School Management Evaluation  ###########
  #############################################
  
  # School Survey. This policy lever measures the extent to which principal performance is being monitored and enforced via accountability measures. 
  # The idea is that the indicator will be based on: 1) there is a legislation outlining the need to monitor, 2) principals are being evaluated, 3) 
  # principals are being evaluated on multiple things, and 4) there the accountability mechanisms in place.
  
  
  
  school_data_SEVL<- management_dta %>%
    mutate(principal_formally_evaluated=bin_var(m7sgq8_sevl,1),
           principal_eval_tot=rowSums(.[grep(x=colnames(management_dta), 
                                             pattern="m7sgq10_sevl[0-9]{2}$")], na.rm=TRUE)-m7sgq10_sevl98) %>%
    mutate(principal_evaluation_multiple=if_else(m7sgq8_sevl==1,
                                                 case_when(
                                                   principal_eval_tot>=5 ~ 1,
                                                   (principal_eval_tot>1 & principal_eval_tot<5) ~ 0.666667,
                                                   principal_eval_tot==1 ~ 0.3333333,
                                                   TRUE ~ 0
                                                 ),
                                                 0),
           principal_negative_consequences=case_when(
             (m7sgq11_sevl01==1 | m7sgq11_sevl02==1 | m7sgq11_sevl03==1 | m7sgq11_sevl04==1 | m7sgq11_sevl99==1) ~ 1,
             TRUE ~ 0),
           principal_positive_consequences=case_when(
             (m7sgq12_sevl01==1 | m7sgq12_sevl02==1 | m7sgq12_sevl03==1 | m7sgq12_sevl04==1 | m7sgq12_sevl99==1) ~ 1,
             TRUE ~ 0)
    ) %>%
    mutate(principal_evaluation=1+principal_formally_evaluated+principal_evaluation_multiple+principal_negative_consequences+principal_positive_consequences)%>% 
    rename(school_code = s_ecole_code)
  
  final_indicator_data_SEVL <- school_data_SEVL %>%
    group_by(school_code) %>%
    summarise_all(~first(na.omit(.))) %>%
    mutate(n_mssing_SEVL=n_miss_row(.))  %>%
    select( -starts_with('interview'), -starts_with('enumerator'))  
  
  
  #------------------------------------------------------------------------------#
  ## 13. School  Inputs and Infrastructure Standards ###########
  
  # - 1 Point. Are there standards in place to monitor blackboard and chalk, pens and pencils, basic classroom furniture, computers, textbooks, exercise books, toilets, electricity, drinking water, accessibility for those with disabilities? (partial credit available) 
  
  ## Filtering
  school_data_IMON <- input_dta %>% 
    
    # Only var of interest
    select(-starts_with("s_m")) %>% 
    
    # Only schools from sample
    filter(s_type_ecole ==3)
    
    
  
  ## Metadata
  meta_IMON <- makeVlist(school_data_IMON)
  
  ## Computation of indicators
  school_data_ISTD <- school_data_IMON %>%
    
    # Rename for matching
    setNames(tolower(gsub("d_m1_e09","m1scq9_imon__",names(.)))) %>% 
    setNames(tolower(gsub("d_m1e_04","m1scq4_imon__",names(.)))) %>% 
    setNames(tolower(gsub("d_m1e_13","m1scq13_imon__",names(.)))) %>%
    setNames(tolower(gsub("d_m1e_14","m1scq14_imon__",names(.)))) %>%
  
    
    # Computation as previously established
    mutate(standards_monitoring_input=rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                      pattern="m1scq13_imon__")], na.rm=TRUE),
           standards_monitoring_infrastructure=rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                               pattern="m1scq14_imon__")], na.rm=TRUE) ) %>%
    mutate(standards_monitoring=(standards_monitoring_input*6+standards_monitoring_infrastructure*4)/2) %>% 
    
    rename(school_code = s_ecole_code)
  
  
  final_indicator_data_ISTD <- school_data_ISTD %>%
    group_by(school_code) %>%
    summarise_all(~first(na.omit(.))) %>%
    mutate(n_mssing_ISTD=n_miss_row(.))  %>%
    select( -starts_with('interview'), -starts_with('enumerator'))  
  
  
  #############################################
  ##### School  Inputs and Infrastructure Monitoring ###########
  #############################################
  
  # School Survey. A score of 1-5 based on 3 factors. Each factor has received an equal weight in terms of points. The factors are the following: 
  #-	Someone is monitoring 
  #-	System for monitoring available (E.g. inventory)
  #-	Community involved in monitoring 
  
  
  school_data_IMON <- school_data_IMON %>%
    
    rename(
      m1scq3_imon = d_m1e_10, 
      m1scq5_imon = d_m1e_05, # inventory for school intrants
      m1scq1_imon = d_m1e_01,
      m1scq7_imon = d_m1e_07
      ) %>% 
    
    setNames(tolower(gsub("d_m1_e09","m1scq9_imon__",names(.)))) %>% 
    setNames(tolower(gsub("d_m1e_04","m1scq4_imon__",names(.)))) %>% 
    setNames(tolower(gsub("d_m1e_13","m1scq13_imon__",names(.)))) %>%
    setNames(tolower(gsub("d_m1e_14","m1scq14_imon__",names(.)))) %>%
    
    mutate(m1scq3_imon=bin_var(m1scq3_imon,1),
           system_in_place=case_when(
             m1scq5_imon==0 ~ 0,
             m1scq5_imon==1 ~ 1,
             m1scq5_imon==2 ~ 0.5,
             TRUE ~ 0
           )) %>%
    mutate(monitoring_inputs=if_else(m1scq1_imon==1,
                                     rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                     pattern="m1scq4_imon__")], na.rm=TRUE),
                                     0),
           monitoring_infrastructure=if_else(m1scq7_imon==1,
                                             rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                             pattern="m1scq9_imon__")], na.rm=TRUE),
                                             0),
    ) %>%
    mutate(parents_involved=if_else(m1scq3_imon==1,1,0,0)) %>%
    mutate(sch_monitoring=1+(monitoring_inputs+monitoring_infrastructure)/2+system_in_place+parents_involved) %>% 
    rename(school_code = s_ecole_code)
  
  
  
  
  final_indicator_data_IMON <- school_data_IMON %>%
    group_by(school_code) %>%
    summarise_all(~first(na.omit(.))) %>%
    mutate(n_mssing_IMON=n_miss_row(.))  %>%
    select( -starts_with('interview'), -starts_with('enumerator'))  
  

  
  #------------------------------------------------------------------------------#
  ## FINAL - SAVING VARIOUS DATASETS IN CLEAN FOLDER FOR THE COUNTRY ###########
  
  # One last renaming for matching below
  final_indicator_data_INFR <- final_indicator_data_INFR %>% rename(school_code = s_ecole_code)
  final_indicator_data_LCAP <- final_indicator_data_LCAP %>% rename(school_code = s_ecole_code)
  final_indicator_data_OPMN <- final_indicator_data_OPMN %>% rename(school_code = s_ecole_code)
  final_indicator_data_PMAN <- final_indicator_data_PMAN %>% rename(school_code = s_ecole_code)
  
  
  #Read in list of indicators
  indicators <- read_delim(here::here('Indicators','indicators.md'), delim="|", trim_ws=TRUE)
  indicators <- indicators %>%
    filter(Series!="---") %>%
    separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE) %>% 
    select(-contains("X1"))
  
  #Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
  indicator_names <- indicators$indicator_tag
  ind_dta_list<-c()
  
  
  for (i in indicator_names ) {
    if (exists(paste("final_indicator_data_",i, sep=""))) {
      #form temp data frame with each schools data
      temp<-get(paste("final_indicator_data_",i, sep="")) 
      
      #add element to list
      ind_dta_list<-c(ind_dta_list, paste("final_indicator_data_",i, sep=""))
      
      
      print(i)
      #Merge this to overall final_school_data frame
      if (!exists('final_school_data')) {
        final_school_data<-temp
        print(i)
        write.csv(temp, file = file.path(paste(confidential_folder,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
        # if (backup_onedrive=="yes") {
        # write.csv(temp, file = file.path(paste(save_folder_onedrive,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
        # }

      } else {
        final_school_data<-final_school_data %>%
          left_join(temp, by='school_code') %>%
          select(-ends_with(".x"), -ends_with(".y"))
        
        write.csv(temp, file = file.path(paste(confidential_folder,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
        # if (backup_onedrive=="yes") {
        #   write.csv(temp, file = file.path(paste(save_folder_onedrive,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
        # }
      }
    }
  }
  
