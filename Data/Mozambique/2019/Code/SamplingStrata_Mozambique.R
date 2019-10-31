library(SamplingStrata)
library(haven)
library(wbstats)
library(ggmap)
library(leaflet)
library(maps)
library(maptools)
library(rgeos)
library(RANN)
library(sp)
library(rgdal)
library(naniar)
library(raster)
library(tidyr)
library(tidyverse)
library(readxl)
library(mice)
library(dplyr)


set.seed(54351324)


#load SDI Mozambique data
load("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/clean/all_modules_clean.RData")

#produce data file with strata info from SDI
full_sample<- full_sample %>%
  left_join(strata) %>%
  left_join(school_indicator_data)

#generate indicator for whether school was selected for sample, but not visited
full_sample<-full_sample %>%
  mutate(stratum_gepd=if_else(condition = (is.na(student_knowledge) & is.na(absence_rate) & is.na(content_knowledge) & 
                                             is.na(pedagogical_knowledge) & is.na(inputs) & is.na(infrastructure)),
                              "no data", stratum)) 
  

#Summary stats of strata from SDI
tabs_strata<-full_sample %>%
  group_by(stratum_gepd) %>%
  summarise(n=n())

##################################################################
#Game Plan: There are 81 schools that were supposed to be visited but weren't (24%)
#           There are 338 schools with at least some information
#           We need to choose 150 schools
# Plan A:  Choose 36 schools at random from schools not chosen.  Choose 114 schools optimally stratifying
# Plan B:  Optimally stratify on basis of provinces, urban/rural, large/small schools, identifying number in each strata.  
#          Then choose 150 out of 403 using these stratification numbers.
# Plan B seems better, because many schools lack complete info, so aggregating can be a way of handling missing info at school level.
#          This approach then mirrors more closely orginal SDI sampling, since based on same stratum.  
#          Our innovation is that we choose stratum numbers optimally using data collected from SDI.
##################################################################

#keep only schools that were visited for training the optimal stratification algorithm
data_set<-full_sample %>%
  filter(stratum_gepd!="no data")


#create variable ID for strata
data_set<-data_set %>%
    mutate(domains=as.numeric(factor(rural))) %>%
    unite("stratum_type", c("province", "rural", "size"), sep=" ", remove=FALSE)



#impute missing values for model using mice
data_set_imp<-mice(data_set, m=5, maxit=40)

#Get average dataset
data_set_filled <- complete(data_set_imp, "long")

data_set_filled <- data_set_filled %>%
  group_by(sch_id) %>%
  summarise_all(list(if(is.numeric(.)) ~mean(., na.rm = TRUE) else ~first(.))) %>%
  dplyr::select(-ecd, -operational_management, - instructional_leadership, -school_knowledge, -management_skills)



#produce max coefficients of variation we will tolerate
cv <- as.data.frame(list(DOM=rep("DOM1",2),
                         CV1=c(0.1, 0.01),
                         CV2=c(0.1, 0.01),
                         CV3=c(0.1, 0.01),
                         CV4=c(0.1, 0.01),
                         CV5=c(0.1, 0.01),
                         CV6=c(0.1, 0.01),
                         domainvalue=c(1:2)
))
cv

#See https://cran.r-project.org/web/packages/SamplingStrata/vignettes/SamplingStrata.html

data_set_filled$province_factor<-as.numeric(factor(data_set_filled$province))
data_set_filled$rural_factor<-as.numeric(factor(data_set_filled$rural))
data_set_filled$size_factor<-as.numeric(factor(data_set_filled$size))


frame <- buildFrameDF(df = data_set_filled,
                             id = "sch_id",
                             X = c("province_factor", "size_factor"),
                             Y = c("student_knowledge", "absence_rate", "content_knowledge", "pedagogical_knowledge", "inputs","infrastructure"),
                             domainvalue = "domains")
str(frame)



strata <- buildStrataDF(frame, progress = FALSE, verbose=TRUE)

#set cost to be double in rural areas in Mozambique (as an approximation)
strata <- strata %>%
  mutate(COST=ifelse(DOM1==1, 2,1 ))
  
#   Once the strata and the constraints dataframes have been prepared, it is possible to apply the function that optimises the stratification of the frame, that is optimizeStrata. This function operates on all subdomains, identifying the best solution for each one of them. The fundamental parameters to be passed to optimizeStrata are:
#     
#     errors: the (mandatory) dataframe containing the precision levels expressed in terms of maximum allowable coefficients of variation that regard the estimates on target variables of the survey;
#   strata: the (mandatory) dataframe containing the information related to 'atomic' strata, i.e. the strata obtained by the Cartesian product of all auxiliary variables X's. Information concerns the identifiability of strata (values of X's) and variability of Y's (for each Y, mean and standard deviation in strata);
#   cens: the (optional) dataframe containing the 'take-all' strata, those strata whose units must be selected in whatever sample. It has same structure than *strata} dataframe;
# strcens: flag (TRUE/FALSE) to indicate if 'take-all' strata do exist or not. Default is FALSE;
# initialStrata: the initial limit on the number of strata for each solution. Default is NA, and in this case it is set equal to the number of atomic strata in each domain. If the parameter addStrataFactor is equal to zero, then initialStrata is equivalent to the maximum number of strata to be obtained in the final solution;
# addStrataFactor: indicates the probability that at each mutation the number of strata may increase with respect to the current value. Default is 0.0;
# minnumstr: indicates the minimum number of units that must be allocated in each stratum. Default is 2;
# iter: indicates the maximum number of iterations (= generations) of the genetic algorithm. Default is 50
# pops: dimension of each generations in terms of number 0f individuals to be generated. Default is 20;
# mut_chance (mutation chance): for each new individual, the probability to change each single chromosome, i.e. one bit of the solution vector. High values of this parameter allow a deeper exploration of the solution space, but a slower convergence, while low values permit a faster convergence, but the final solution can be distant from the optimal one. Default is NA, in correspondence of which it is computed as 1/(vars+1) where vars is the length of elements in the solution;
# elitism_rate: indicates the rate of better solutions that must be preserved from one generation to another. Default is 0.2;
# suggestions: indicates one possible solution (from kmeans clustering or from previous runs) that will be introduced in the initial population. Default is NULL;
# realAllocation : if FALSE, the allocation is based on INTEGER values; if TRUE, the allocation is based on REAL values. Default is TRUE;
# writeFiles : indicates if the various dataframes and plots produced during the execution have to be written in the working directory /output. Default is FALSE;
# showPlot : indicates if the plot showing the trend in the value of the objective function has to be shown or not. In parallel = TRUE, this defaults to FALSE, otherwise default is TRUE.
# parallel : Should the analysis be run in parallel. Default is TRUE
# cores : if the analysis is run in parallel, how many cores should be used. If not specified n-1 of total available cores are used OR if number of domains < (n-1) cores, then number of cores equal to number of domains are used
  
  checkInput(errors = checkInput(errors = cv, 
                                 strata = strata, 
                                 sampframe = frame))
  
  
  solution1 <- optimizeStrata(
    errors = cv, 
    strata = strata,
    parallel = FALSE,
    writeFiles = FALSE,
    showPlot = TRUE)
  
  #We can calculate the expected CV's by executing the function:
    
  strata_comp<-solution1$aggr_strata
  
  expected_CV(solution1$aggr_strata)
  
  #Can update sample size if too large or too small
  # Adjustment of the final sampling size
  # After the optimization step, the final sample size is the result of the allocation of units in final strata. This allocation is such that the precision constraints are expected to be satisfied. Actually, three possible situations may occur:
  #   
  #   the resulting sample size is acceptable;
  # the resulting sample size is to high, it is not affordable with respect to the available budget;
  # the resulting sample size is too low, the available budget permits to increase the number of units.
  # In the first case, no action is required. In the second case, it is necessary to reduce the number of units, by equally applying the same reduction rate in each stratum. In the third case, we could either to set more tight precision constraints, or proceed to increase the sample size by applying the same increase rate in each stratum. This increase/reduction process is iterative, as by applying the same rate we could find that in some strata there are not enough units to increase or to reduce. The function adjustSize permits to obtain the desired final sample size. Let us suppose that the obtained sample size is not affordable. We can reduce it by executing the following code:
  #   

    adjustedStrata <- adjustSize(size=176,strata=solution1$aggr_strata,cens=NULL)

  sum(adjustedStrata$SOLUZ)
  # The difference between the desired sample size and the actual adjusted size depends on the number of strata in the optimized solution. Consider that the adjustment is performed in each stratum by taking into account the relative difference between the current sample size and the desired one: this produces an allocation that is expressed by a real number, that must be rounded, while taking into account the requirement of the minimum number of units in the strata. The higher the number of strata, the higher the impact on the final adjusted sample size.
  

  #   
    newstrata <- updateStrata(strata, 
                              solution1, 
                              writeFiles = FALSE)
  # Now, the atomic strata are associated to the aggregate strata defined in the optimal solution, by means of the variable LABEL. If we want to analyse in detail the new structure of the stratification, we can look at the strata_aggregation.txt file:
    

  
  
  
  # Once the optimal stratification has been obtained, to be operational we need to accomplish the following two steps:
  #   
  #   to update the frame units with new stratum labels (combination of the new values of the auxiliary variables X's);
  # to select the sample from the frame.
  # As for the first, we execute the following command:
  #   
  

  
    framenew <- updateFrame(frame, newstrata, writeFiles=FALSE)
    

     #update original database with stratum labels
    dataset<- full_sample %>%
      mutate(ID=sch_id) %>%
      left_join(framenew)
    
    #Add in strata ids for schools that didn't get surveyed 
    dataset<- dataset %>%
      unite("strata_id", c("province", "rural", "size"), sep=" ", remove=FALSE ) %>%
      group_by(strata_id) %>%
      fill(c("STRATUM", "X1", "X2", "Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "LABEL"))
    
    
    sample_optimal <- selectSample(dataset, adjustedStrata, writeFiles=FALSE)
    colnames(sample_optimal)<-tolower(colnames(sample_optimal))
    
    #Summary stats of strata from SDI
    tabs_strata_samp<-sample_optimal %>%
      group_by(province, rural, size) %>%
      summarise(n=n())
    

  sample_random <- full_sample %>%
    group_by(province, rural, size) %>%
    sample_frac(size=0.3722 )
  
  tabs_strata_rand_samp<-sample_random %>%
    group_by(province, rural, size) %>%
    summarise(n=n())
  
  
  #Examine how many urban/rural we have
  sample_optimal %>%
    group_by(rural) %>%
    summarise(n=n())
  
sample_final <- sample_optimal %>%
  mutate(sample_dashboard=1)

data_set_updated <- full_sample %>%
  left_join(sample_final) %>%
  mutate(sample_dashboard=ifelse(is.na(sample_dashboard),0,sample_dashboard))

data_set_updated %>%
  group_by(sample_dashboard) %>%
  summarise(n=n())

#Save sample file
currentDate<-Sys.Date()
sample_file_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/sampling/school_sample_",currentDate,".csv", sep="")
write.csv(sample_final,sample_file_name)

sample_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/sampling/school_sample_",currentDate,".RData", sep="")
save(sample_final, data_set_updated,
     file=sample_frame_name)   


##################################
#Select Replacement Schools
###############################

#read in full sample frame
df<-read.csv("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/sampling/mozambique_sampling_frame.csv")

#drop extra columns at end
df<-df %>% 
  dplyr::select(1:73)

#update orginal dataframe with sampled schools.
data_set_updated <- df %>%
  dplyr::select(-rural) %>%
  mutate(sch_id=codigo) %>% 
  left_join(sample_final) %>%
  mutate(sample_dashboard=ifelse(is.na(sample_dashboard),0,sample_dashboard))
  dplyr::select(-n_prov)
#This will be done so that 2 replacements in every province are chosen  
#get a list of  districts sampled

districts <- sample_final %>%
  group_by(district) %>%
  summarise(n_prov=n()) %>%
  mutate(orig_distrito=district) %>%
  dplyr::select(-district)

district_list<-as.vector(districts$orig_distrito)

#produce table with randomly replacement selected schools
sample_replacement1<- data_set_updated %>%
  left_join(districts) %>%
  filter(orig_distrito %in% district_list) %>%
  filter(sample_dashboard==0) %>%
  group_by(orig_distrito) %>%
  sample_n(n_prov, weight=ipw ) %>%
  mutate(sample_replacement1=1) %>%
  dplyr::select(-n_prov)

data_set_updated <- data_set_updated %>%
  left_join(sample_replacement1) %>%
  mutate(sample_replacement1=ifelse(is.na(sample_replacement1),0,sample_replacement1))


sample_replacement2<- data_set_updated %>%
  left_join(districts) %>%
  filter(orig_distrito %in% district_list) %>%
  filter(sample_dashboard==0 & sample_replacement1==0) %>%
  group_by(orig_distrito) %>%
  sample_n(n_prov, weight=ipw ) %>%
  mutate(sample_replacement2=1) %>% 
  dplyr::select(-n_prov)

data_set_updated <- data_set_updated %>%
  left_join(sample_replacement2) %>%
  mutate(sample_replacement2=ifelse(is.na(sample_replacement2),0,sample_replacement2))

sample_maputo<- data_set_updated %>%
  filter(orig_province == "Maputo" | orig_province == "Cidade de Maputo") %>%
  filter(sample_dashboard==0 & sample_replacement1==0 & sample_replacement2==0) %>%
  ungroup() %>% 
  sample_n(5, weight=ipw ) %>%
  mutate(sample_maputo=1)

data_set_updated <- data_set_updated %>%
  left_join(sample_maputo) %>%
  mutate(sample_maputo=ifelse(is.na(sample_maputo),0,sample_maputo)) %>%
  mutate(sample=case_when(
    sample_dashboard==1  ~ 1,
    sample_replacement1==1 ~ 2,
    sample_replacement2==1 ~ 3,
    sample_maputo==1 ~ 4)) %>%
  mutate(sample=factor(sample, levels=c(1,2,3,4), labels=c('Sampled School', "Replacement 1", "Replacement 2", 'Maputo Pilot Schools')))

sample_updated<-data_set_updated  %>%
  filter(!is.na(sample)) %>%
  dplyr::select(-(74:115)) %>%
  dplyr::select(codigo, sample, everything())

#Save sample file
currentDate<-Sys.Date()
sample_file_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/sampling/school_sample_",currentDate,".csv", sep="")
write.csv(sample_updated,sample_file_name)

sample_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/sampling/school_sample_",currentDate,".RData", sep="")
save(sample_updated, data_set_updated,
     file=sample_frame_name)   


#################################################
#Get list of teachers for each school selected based on previous SDI visit
################################################

#load SDI data
load("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/all_modules.Rdata")

#create dataset with the teachers selected for teacher assessment
teacher_dta<-teacher_assessment_dta %>%
  left_join(rmod2a_2) %>%
  mutate(missing_name=ifelse(is.na(teacher_name), 1,0 ))

teacher_dta %>%
  group_by(missing_name) %>%
  summarise(n=n())


#################################################
###### Survey of Public Officials Sampling #####
#################################################

#This could also be split into a separate file.
currentDate<-Sys.Date()
sample_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/sampling/school_sample_",currentDate,".RData", sep="")
load(file=sample_frame_name) 

#Choose district offices to visit randomly among the list of districts we are already visiting for the schools
#We will choose 20 of these district offices from the Regional level
dist_list<- sample_final %>%
  group_by(district) %>%
  summarise(n=n() )




#Run an iteration where we choose 10 districts linked to schools and link up to provinces
#columns to keep
cols<-c( "district")

prov_list<- sample_final %>%
  group_by(province) %>%
  summarise(n=n() )


province_office_sample <- prov_list 

dist_list_alt3<- sample_final %>%
  filter(province %in% as.vector(province_office_sample$province)) %>%
  group_by(district) %>%
  summarise_all(first) %>%
  group_by(province) %>%
  mutate(n_districts=if_else((province=="Maputo Cidade" | province=="Maputo Provincia" | province=="Niassa" | province=="Sofala"), 1,2) )%>% #Select 2 districts from each province, but 1 in the smallest provinces
  
  sample_n(n_districts) %>%
  dplyr::select(province, district)

#save as csv
dist_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/sampling/district_sample_10region_18ugel",currentDate,".csv", sep="")

write.csv(dist_list_alt2,dist_frame_name) 

###save our sample  
dist_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/sampling/district_sample_",currentDate,".RData", sep="")

save(dist_list_alt2,
     file=dist_frame_name) 

