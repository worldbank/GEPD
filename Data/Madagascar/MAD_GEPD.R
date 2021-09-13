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

## Cleaning
  
  if(Reset == 1){
    rm(list = ls())
  } 
  
## Packages
  if(!require("pacman")) install.packages("pacman")
  
  pacman::p_load(
    
    httr, haven, dplyr, Hmisc, tidyr, here, vtable, stringr, naniar, purr
    
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
infrastructure_board_dta <- read_dta(file.path(download_folder, "/astata/17-M4BC..dta"))
vtable(infrastructure_board_dta)

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
    m6s2q9c_produce_set = d_m5e3_12c, # HERE, it should be noted that in the ETH data there are only two while there are three for MAD
    
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
    m6s2q13d_backward_digit = d_m5e3_16f, # HERE, should be noted that we do not take into account the first two obs as they are trials
    
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
    m6s2q14o_head_shoulders= d_m5e3_18m, # HERE, should be noted that trials are again in the data. To ensure a correct matching between names and var, a verification was done with the M_6 DirectAssessement document.
    
    # Perspective
    m6s2q15a_perspective = d_m5e3_19a,
    m6s2q15b_perspective  = d_m5e3_19b,
    m6s2q15c_perspective = d_m5e3_19c, # HERE, should be noted that the response provided by the students are not present in the data shared by the SDI team

    # Conflict resolution
    m6s2q16a_conflict_resol = d_m5e3_20a,
    m6s2q16b_conflict_resol = d_m5e3_20b # HERE, should be noted that the response provided by the students are not present in the data shared by the SDI team
    
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
  #note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items
  
  
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
  ## 11. Cleaning and creating indicators: infrastructure school Level ----  
  
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
  infrastructure_board_dta <- infrastructure_board_dta %>%
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
  schools_sampled <- readxl::read_xlsx(here::here('Desktop/Education GP/02. Country_work/MAD/Technical/Data/Raw','Echantillons SDI V11 éducation.xlsx')) %>% 
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
  
  
  
  
  

  

