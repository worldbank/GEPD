# Packages
library(miceadds) # To open *.RData files in R.
library(psych) # For psychometric analyses.
library(car)
library(plyr)
library(lavaan)
library(ltm)
library(TAM)
library(mirt)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Get Data
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

setwd("C:/Users/WB552232/OneDrive - WBG/Project FY 20 21/Accelerator countries/Dashboard Data")

load("dashboard_4th_grade_assessment_data_jordan.RData")
JOR <- assess_4th_grade_anon
load("dashboard_4th_grade_assessment_data_peru.RData")
PER <- assess_4th_grade_anon
load("dashboard_4th_grade_assessment_data_rwanda.RData")
RWA <- assess_4th_grade_anon

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Data exploration
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

dim(JOR) # 2681   49
dim(PER) # 2681   28
dim(RWA) # 5215   64

View(JOR) #
View(PER) #
View(RWA) #


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Recode
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

JOR$m8sbq1_number_sense <- ifelse(JOR$m8sbq1_number_sense == 1, 1, 0)
PER$m8saq2_id <- ifelse(PER$m8saq2_id > 0.6, 1, 0)
PER$m8saq3_id <- ifelse(PER$m8saq3_id > 0.6, 1, 0)
PER$m8sbq1_number_sense <- ifelse(PER$m8sbq1_number_sense > 0.6, 1, 0)
RWA$m8saq2_id <- ifelse(RWA$m8saq2_id > 0.55, 1, 0)
RWA$m8saq3_id <- ifelse(RWA$m8saq3_id > 0.55, 1, 0)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Generate dataframes
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

JOR_Read <- data.frame(JOR$m8saq5_story, JOR$m8saq6_story, JOR$m8saq7_word_choice,
                       JOR$m8saq7a_gir, JOR$m8saq7b_gir, JOR$m8saq7c_gir,	  
                       JOR$m8saq7d_gir, JOR$m8saq7e_gir,	JOR$m8saq7f_gir,	  
                       JOR$m8saq7g_gir, JOR$m8saq7h_gir, JOR$m8saq7i_gir,
                       JOR$m8saq7j_gir, JOR$m8saq7k_gir)

JOR_Math <- data.frame(JOR$m8sbq1_number_sense, JOR$m8sbq2_number_sense, JOR$m8sbq3a_arithmetic,
                       JOR$m8sbq3b_arithmetic, JOR$m8sbq3c_arithmetic, JOR$m8sbq3d_arithmetic,
                       JOR$m8sbq3e_arithmetic, JOR$m8sbq3f_arithmetic, JOR$m8sbq3g_arithmetic,
                       JOR$m8sbq3h_arithmetic, JOR$m8sbq3i_arithmetic, JOR$m8sbq3j_arithmetic, 
                       JOR$m8sbq4_arithmetic, JOR$m8sbq5_word_problem, JOR$m8sbq6_sequences)

PER_Read <- data.frame(PER$m8saq5_story, PER$m8saq6_story, PER$m8saq7_word_choice,
                       PER$m8saq2_id, PER$m8saq3_id)

PER_Math <- data.frame(PER$m8sbq1_number_sense, PER$m8sbq2_number_sense, PER$m8sbq3a_arithmetic,
                       PER$m8sbq3b_arithmetic, PER$m8sbq3c_arithmetic, PER$m8sbq3d_arithmetic,
                       PER$m8sbq3e_arithmetic, PER$m8sbq3f_arithmetic, PER$m8sbq3g_arithmetic,
                       PER$m8sbq3h_arithmetic, PER$m8sbq3i_arithmetic, PER$m8sbq3j_arithmetic,
                       PER$m8sbq4_arithmetic, PER$m8sbq5_word_problem, PER$m8sbq6_sequences)

RWA_Read <- data.frame(RWA$m8saq5_story, RWA$m8saq6_story, RWA$m8saq7_word_choice,
                       RWA$m8saq7a_gir, RWA$m8saq7b_gir, RWA$m8saq7c_gir,
                       RWA$m8saq7d_gir, RWA$m8saq7e_gir, RWA$m8saq7f_gir,
                       RWA$m8saq7g_gir, RWA$m8saq7h_gir, RWA$m8saq7i_gir,
                       RWA$m8saq7j_gir, RWA$m8saq7k_gir, RWA$m8saq2_id,
                       RWA$m8saq3_id)

RWA_Math <- data.frame(RWA$m8sbq2_number_sense, RWA$m8sbq3a_arithmetic, RWA$m8sbq3b_arithmetic,
                       RWA$m8sbq3c_arithmetic, RWA$m8sbq3d_arithmetic, RWA$m8sbq3e_arithmetic,
                       RWA$m8sbq3f_arithmetic, RWA$m8sbq3g_arithmetic, RWA$m8sbq3h_arithmetic,
                       RWA$m8sbq3i_arithmetic, RWA$m8sbq3j_arithmetic, RWA$m8sbq4_arithmetic,
                       RWA$m8sbq5_word_problem, RWA$m8sbq6_sequences)



sapply(JOR_Read, function(x) prop.table(table(x)))
sapply(JOR_Math, function(x) prop.table(table(x)))
sapply(PER_Read, function(x) prop.table(table(x)))
sapply(PER_Math, function(x) prop.table(table(x)))
sapply(RWA_Read, function(x) prop.table(table(x)))
sapply(RWA_Math, function(x) prop.table(table(x)))



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Difficulty
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
write.csv(describe(JOR_Read), "Descriptive_JOR_Read.csv")
write.csv(describe(JOR_Math), "Descriptive_JOR_Math.csv")
write.csv(describe(PER_Read), "Descriptive_PER_Read.csv")
write.csv(describe(PER_Math), "Descriptive_PER_Math.csv")
write.csv(describe(RWA_Read), "Descriptive_RWA_Read.csv")
write.csv(describe(RWA_Math), "Descriptive_RWA_Math.csv")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# 2. CTT item discrimination and reliability
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

write.csv(data.frame(alpha(JOR_Read)$total[1], alpha(JOR_Read)$alpha.drop[1], 
                     alpha(JOR_Read)$item.stats[3]), "Alpha_JOR_Read.csv")

write.csv(data.frame(alpha(JOR_Math)$total[1], alpha(JOR_Math)$alpha.drop[1], 
                     alpha(JOR_Math)$item.stats[3]), "Alpha_JOR_Math.csv")

write.csv(data.frame(alpha(PER_Read)$total[1], alpha(PER_Read)$alpha.drop[1], 
                     alpha(PER_Read)$item.stats[3]), "Alpha_PER_Read.csv")

write.csv(data.frame(alpha(PER_Math)$total[1], alpha(PER_Math)$alpha.drop[1], 
                     alpha(PER_Math)$item.stats[3]), "Alpha_PER_Math.csv")

write.csv(data.frame(alpha(RWA_Read)$total[1], alpha(RWA_Read)$alpha.drop[1], 
                     alpha(RWA_Read)$item.stats[3]), "Alpha_RWA_Read.csv")

write.csv(data.frame(alpha(RWA_Math)$total[1], alpha(RWA_Math)$alpha.drop[1], 
                     alpha(RWA_Math)$item.stats[3]), "Alpha_RWA_Math.csv")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# 3. Confirmatory Factor Analysis Model
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# Define models

ModJOR_Read <- 'LV1 =~ NA*JOR.m8saq5_story +       JOR.m8saq6_story +       JOR.m8saq7_word_choice + JOR.m8saq7a_gir +        JOR.m8saq7b_gir +        JOR.m8saq7c_gir +        JOR.m8saq7d_gir +       
  JOR.m8saq7e_gir +        JOR.m8saq7f_gir +        JOR.m8saq7g_gir +        JOR.m8saq7h_gir +        JOR.m8saq7i_gir +        JOR.m8saq7j_gir +        JOR.m8saq7k_gir 
LV1 ~~ 1*LV1
'

ModJOR_Math <- 'LV1 =~ NA*JOR.m8sbq1_number_sense + JOR.m8sbq2_number_sense + JOR.m8sbq3a_arithmetic +  JOR.m8sbq3b_arithmetic +  JOR.m8sbq3c_arithmetic +  JOR.m8sbq3d_arithmetic + 
  JOR.m8sbq3e_arithmetic +  JOR.m8sbq3f_arithmetic +  JOR.m8sbq3g_arithmetic +  JOR.m8sbq3h_arithmetic +  JOR.m8sbq3i_arithmetic +  JOR.m8sbq3j_arithmetic  +
  JOR.m8sbq4_arithmetic +   JOR.m8sbq5_word_problem + JOR.m8sbq6_sequences 
LV1 ~~ 1*LV1
'

ModPER_Read <- 'LV1 =~ NA*PER.m8saq5_story +       PER.m8saq6_story +       PER.m8saq7_word_choice + PER.m8saq2_id +          PER.m8saq3_id    
LV1 ~~ 1*LV1
'

ModPER_Math <- 'LV1 =~ NA*PER.m8sbq1_number_sense + PER.m8sbq2_number_sense + PER.m8sbq3a_arithmetic +  PER.m8sbq3b_arithmetic +  PER.m8sbq3c_arithmetic +  PER.m8sbq3d_arithmetic + 
  PER.m8sbq3e_arithmetic +  PER.m8sbq3f_arithmetic +  PER.m8sbq3g_arithmetic +  PER.m8sbq3h_arithmetic +  PER.m8sbq3i_arithmetic +  PER.m8sbq3j_arithmetic + 
  PER.m8sbq4_arithmetic +   PER.m8sbq5_word_problem + PER.m8sbq6_sequences 
LV1 ~~ 1*LV1
'

ModRWA_Read <- 'LV1 =~ NA*RWA.m8saq5_story +       RWA.m8saq6_story +       RWA.m8saq7_word_choice + RWA.m8saq7a_gir +        RWA.m8saq7b_gir +        RWA.m8saq7c_gir +        RWA.m8saq7d_gir +       
  RWA.m8saq7e_gir +        RWA.m8saq7f_gir +        RWA.m8saq7g_gir +        RWA.m8saq7h_gir +        RWA.m8saq7i_gir +        RWA.m8saq7j_gir +        RWA.m8saq7k_gir +       
RWA.m8saq2_id +          RWA.m8saq3_id       
LV1 ~~ 1*LV1
'

ModRWA_Math <- 'LV1 =~ NA*RWA.m8sbq2_number_sense + RWA.m8sbq3a_arithmetic +  RWA.m8sbq3b_arithmetic +  RWA.m8sbq3c_arithmetic +  RWA.m8sbq3d_arithmetic +  RWA.m8sbq3e_arithmetic + 
  RWA.m8sbq3f_arithmetic +  RWA.m8sbq3g_arithmetic +  RWA.m8sbq3h_arithmetic +  RWA.m8sbq3i_arithmetic +  RWA.m8sbq3j_arithmetic +  RWA.m8sbq4_arithmetic +  
  RWA.m8sbq5_word_problem + RWA.m8sbq6_sequences 
LV1 ~~ 1*LV1
'

# Estimate models
CFA_JOR_Read <- cfa(model = ModJOR_Read, sample.cov = tetrachoric(JOR_Read)$rho, 
                sample.nobs = dim(JOR_Read)[1])

CFA_JOR_Math <- cfa(model = ModJOR_Math, sample.cov = tetrachoric(JOR_Math)$rho, 
                    sample.nobs = dim(JOR_Math)[1])

CFA_PER_Read <- cfa(model = ModPER_Read, sample.cov = tetrachoric(PER_Read)$rho, 
                    sample.nobs = dim(PER_Read)[1])

CFA_PER_Math <- cfa(model = ModPER_Math, sample.cov = tetrachoric(PER_Math)$rho, 
                    sample.nobs = dim(PER_Math)[1])

CFA_RWA_Read <- cfa(model = ModRWA_Read, sample.cov = tetrachoric(RWA_Read)$rho, 
                    sample.nobs = dim(RWA_Read)[1])

CFA_RWA_Math <- cfa(model = ModRWA_Math, sample.cov = tetrachoric(RWA_Math)$rho, 
                    sample.nobs = dim(RWA_Math)[1])


# Report results
summary(CFA_JOR_Read, fit.measures = TRUE, standardized = TRUE)
summary(CFA_JOR_Math, fit.measures = TRUE, standardized = TRUE)
summary(CFA_PER_Read, fit.measures = TRUE, standardized = TRUE)
summary(CFA_PER_Math, fit.measures = TRUE, standardized = TRUE)
summary(CFA_RWA_Read, fit.measures = TRUE, standardized = TRUE)
summary(CFA_RWA_Math, fit.measures = TRUE, standardized = TRUE)

write.csv(parameterEstimates(CFA_JOR_Read), "CFA_JOR_Read.csv")
write.csv(parameterEstimates(CFA_JOR_Math), "CFA_JOR_Math.csv")
write.csv(parameterEstimates(CFA_PER_Read), "CFA_PER_Read.csv")
write.csv(parameterEstimates(CFA_PER_Math), "CFA_PER_Math.csv")
write.csv(parameterEstimates(CFA_RWA_Read), "CFA_RWA_Read.csv")
write.csv(parameterEstimates(CFA_RWA_Math), "CFA_RWA_Math.csv")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# 6. Item Response Theory models
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# 1PL models
JOR_Read_Rasch <- mirt::mirt(JOR_Read, 1, itemtype="Rasch", technical = list(removeEmptyRows=TRUE))
write.csv(coef(JOR_Read_Rasch, simplify = TRUE, IRTpars = TRUE)$items, "1PL_JOR_Read.csv")

# 2PL models
JOR_Read_2PL <- mirt::mirt(JOR_Read, 1, itemtype="2PL", technical = list(removeEmptyRows=TRUE))
write.csv(coef(JOR_Read_2PL, simplify = TRUE, IRTpars = TRUE)$items, "2PL_JOR_Read.csv")


# 1PL models
JOR_Math_Rasch <- mirt::mirt(JOR_Math, 1, itemtype="Rasch", technical = list(removeEmptyRows=TRUE))
write.csv(coef(JOR_Math_Rasch, simplify = TRUE, IRTpars = TRUE)$items, "1PL_JOR_Math.csv")

# 2PL models
JOR_Math_2PL <- mirt::mirt(JOR_Math, 1, itemtype="2PL", technical = list(removeEmptyRows=TRUE))
write.csv(coef(JOR_Math_2PL, simplify = TRUE, IRTpars = TRUE)$items, "2PL_JOR_Math.csv")




# 1PL models
PER_Read_Rasch <- mirt::mirt(PER_Read, 1, itemtype="Rasch", technical = list(removeEmptyRows=TRUE))
write.csv(coef(PER_Read_Rasch, simplify = TRUE, IRTpars = TRUE)$items, "1PL_PER_Read.csv")

# 2PL models
PER_Read_2PL <- mirt::mirt(PER_Read, 1, itemtype="2PL", technical = list(removeEmptyRows=TRUE))
write.csv(coef(PER_Read_2PL, simplify = TRUE, IRTpars = TRUE)$items, "2PL_PER_Read.csv")


# 1PL models
PER_Math_Rasch <- mirt::mirt(PER_Math, 1, itemtype="Rasch", technical = list(removeEmptyRows=TRUE))
write.csv(coef(PER_Math_Rasch, simplify = TRUE, IRTpars = TRUE)$items, "1PL_PER_Math.csv")

# 2PL models
PER_Math_2PL <- mirt::mirt(PER_Math, 1, itemtype="2PL", technical = list(removeEmptyRows=TRUE))
write.csv(coef(PER_Math_2PL, simplify = TRUE, IRTpars = TRUE)$items, "2PL_PER_Math.csv")






# 1PL models
RWA_Read_Rasch <- mirt::mirt(RWA_Read, 1, itemtype="Rasch", technical = list(removeEmptyRows=TRUE))
write.csv(coef(RWA_Read_Rasch, simplify = TRUE, IRTpars = TRUE)$items, "1PL_RWA_Read.csv")

# 2PL models
RWA_Read_2PL <- mirt::mirt(RWA_Read, 1, itemtype="2PL", technical = list(removeEmptyRows=TRUE))
write.csv(coef(RWA_Read_2PL, simplify = TRUE, IRTpars = TRUE)$items, "2PL_RWA_Read.csv")


# 1PL models
RWA_Math_Rasch <- mirt::mirt(RWA_Math, 1, itemtype="Rasch", technical = list(removeEmptyRows=TRUE))
write.csv(coef(RWA_Math_Rasch, simplify = TRUE, IRTpars = TRUE)$items, "1PL_RWA_Math.csv")

# 2PL models
RWA_Math_2PL <- mirt::mirt(RWA_Math, 1, itemtype="2PL", technical = list(removeEmptyRows=TRUE))
write.csv(coef(RWA_Math_2PL, simplify = TRUE, IRTpars = TRUE)$items, "2PL_RWA_Math.csv")


  





  
