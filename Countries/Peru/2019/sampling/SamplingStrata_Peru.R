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
library(mirt)
library(dplyr)

set.seed(54351324)

#################################
#read in sampling frame from Peru
#################################
df=read.csv("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/Base SIAGIE_Primaria_2019 para Policy Dashboard.csv")

#make column names lower case 
colnames(df)<-tolower(colnames(df))

samp_cut <- df %>%
  mutate(rural=(area=="Rural")) %>%
  mutate(total_4th=x4to.boys+x4to.girls) %>%
  mutate(total_1st=x1ro.boys+x1ro.girls) %>%
  filter(total_4th>=1 & total_1st>=1 ) %>%
  mutate(cut=(total_4th>=3 & total_1st>=3)) %>%
  group_by(cut) %>%
  summarise(schools=n(),
            g4_stud=sum(total_4th),
            g1_stud=sum(total_1st))

#create variable ID for strata and keep schools with more than 1 4th grade student
df<-df %>%
  mutate(rural=(area=="Rural")) %>%
  mutate(total_4th=x4to.boys+x4to.girls) %>%
  mutate(total_1st=x1ro.boys+x1ro.girls) %>%
  filter(total_4th>=3 & total_1st>=3 ) %>%
  filter(anexo==0)

domains<-df %>% group_by(departamento) %>% summarise(n=n())


######################################################
#Read in MELQO data to use for optimal stratification
######################################################

df_melqo<-read_dta("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/PER_2017_MELQO_v02_M_v01_A_GECD_DRA.dta")

#Bring in department information from caregiver report
df_melqo_cgv<-read_dta("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/PER_2017_MELQO_v01_M_v01_A_GECD_CGV.dta")

#rename the child_id variable in the caregiver report
df_melqo_cgv <- df_melqo_cgv %>%
  mutate(child_id=child_id_10) %>%
  mutate(school_id=school_id_10) %>%
  mutate(child_name=child_name_11) %>%
  mutate(class_id=class_id_11) %>%
  select(child_id, class_id, school_id,  subnat_1_11, subnat_2_11, subnat_3_11)

#merge caregiver information with subnational variables
df_melqo <- df_melqo %>%
  left_join(df_melqo_cgv)

#Bring in identifiers for MELQO items used in dashboard
lit_preferred_names=c("lit_exp_a1", "lit_exp_a2", "lit_alp_a1","lit_lan_a1",
                      "lit_lan_a2", "lit_lan_a3", "lit_lan_a4", "lit_lan_a5",
                      "lit_pnt_a1")
#math item names
math_preferred_names=c("mth_num_f1","mth_num_c1", "mth_num_c4", "mth_num_c5",
                       "mth_num_b1","mth_num_a1", "mth_num_a2", "mth_num_a3",
                       "mth_num_e1", "mth_num_e2", "mth_num_e3")
#social-emotional development item names
soc_preferred_names=c("exe_inh_a1", "exe_inh_a2", "exe_inh_a3", "exe_inh_a4",
                      "exe_inh_a5", "exe_inh_a6", "exe_inh_a7", "exe_inh_a8",
                      "exe_inh_a9", "exe_inh_a10", "exe_inh_a11", "exe_inh_a12",
                      "exe_inh_a13", "exe_inh_a14", "exe_inh_a15",
                      "exe_mem_a1", "exe_mem_a2", "exe_mem_a3", "exe_mem_a4",
                      "exe_mem_a5", "exe_mem_a6", "exe_mem_a7",
                      "soc_emo_a1", "soc_emo_a2", "soc_emo_a3", "soc_emo_b1")
comb_preferred_names=c(lit_preferred_names,math_preferred_names, soc_preferred_names)
comb_items=df_melqo[, comb_preferred_names]  
row.not.all.na <- apply(comb_items, 1, function(x){any(!is.na(x))})
sum(row.not.all.na)
comb_items<- comb_items[row.not.all.na,] 

#estimate IRT d-scores
#Estimate IRT parameters ignoring missing values
irt_comb_gpcm <- mirt(comb_items, 1, itemtype='gpcm', SE=FALSE, technical=list(removeEmptyRows=TRUE))
comb_scores<-as.data.frame(fscores(irt_comb_gpcm, method='EAP', response.pattern = comb_items))

df_melqo <- df_melqo %>%
  left_join(comb_scores)

df_melqo$d_score <- scales::rescale(df_melqo$F1, to=c(0,100))

#Need to clean up some departmento names
domains_melqo<-df_melqo %>% group_by(subnat_1_11) %>% summarise(n=n())

df_melqo <- df_melqo %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Arequipa", "AREQUIPA" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Cajamarca", "CAJAMARCA" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Callao", "CALLAO" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Cusco", "CUSCO" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Hu!nuco", "HUANUCO" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Jun!n", "JUNIN" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="La Libertad", "LA LIBERTAD" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Lambayeque", "LAMBAYEQUE" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Lima", "LIMA" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Piura", "PIURA" )) %>%
  mutate(subnat_1_11=replace(subnat_1_11, 	subnat_1_11=="Tacna", "TACNA" ))

domains_melqo<-df_melqo %>% group_by(subnat_1_11) %>% summarise(n=n())

df_melqo_merge <- df_melqo %>%
  mutate(departamento=subnat_1_11) %>%
  group_by(departamento) %>%
  summarise( d_score_sd=sd(d_score, na.rm=TRUE), d_score_mean=mean(d_score, na.rm=TRUE), d_score_q25=quantile(d_score, 0.25, na.rm=TRUE))
  
#merge on melqo scores by department
df<- df %>%
  left_join(df_melqo_merge) 

#fill in missings with country avg
df <- df %>%
  mutate(d_score=if_else(is.na(d_score_mean), mean(d_score_mean, na.rm=TRUE), d_score_mean)) %>%
  mutate(d_score_sd=if_else(is.na(d_score_sd), mean(d_score_sd, na.rm=TRUE), d_score_sd))


df_stats<-df %>%
  group_by(departamento, rural) %>%
  summarise(M1_Mean=mean(d_score), M1_SD=mean(d_score_sd))
df_stats$X1=as.numeric(factor(df_stats$departamento))
df_stats$DOM1<-as.numeric(factor(df_stats$rural))
#########################################
#Prep database for Optimal Stratification
#########################################

#turn string variables into factor variables
df$departamento_factor=as.numeric(factor(df$departamento))
df$rural_factor<-as.numeric(factor(df$rural))

#produce max coefficients of variation we will tolerate
cv <- as.data.frame(list(DOM=rep("DOM1",),
                         CV1=rep(0.01,2),
                         CV2=rep(0.1,2),
                         CV3=rep(0.1,2),                         
                         domainvalue=c(1:2)
))
cv

#See https://cran.r-project.org/web/packages/SamplingStrata/vignettes/SamplingStrata.html


df$id <- c(1:nrow(df))


frame <- buildFrameDF(df = df,
                      id = "codigo.modular",
                      X = c("departamento_factor"),
                      Y = c("d_score", "total_4th", "total_1st"),
                      domainvalue = "rural_factor")
str(frame)

#need to convert to discrete for sampling strata
#frame$X1 <- var.bin(data_set$inspection, bins=16)
#frame$X2 <- var.bin(data_set$narond, bins=10)

#need to convert to discrete for sampling strata



strata <- buildStrataDF(frame, progress = FALSE)
#adjust values of strata for our X variables, d_score std deviations need to be adjusted for estimation
strata <- strata %>%
  left_join(df_stats) %>%
  mutate(S1=M1_SD) %>%
  mutate(CENS=1) %>%
  select(-c("departamento", "rural", "M1_Mean", "M1_SD"))
#strata$CENS<-1

  
  
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
    parallel = TRUE,
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

    adjustedStrata <- adjustSize(size=200,strata=solution1$aggr_strata,cens=NULL)

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
   df<- df %>%
      mutate(ID=codigo.modular) %>%
      left_join(framenew, by="ID" )
    
    #create variable so that sorting is done reversed
  df$rev_total_4th=-df$total_4th
    
    #Note: This reverse order sampling approach is identical to TIMSS.  See https://timssandpirls.bc.edu/methods/pdf/Sampling_Schools.pdf
    
   sample_optimal <- selectSampleSystematic(df, adjustedStrata, sortvariable = "rev_total_4th", writeFiles=FALSE) 
  
  colnames(sample_optimal)<-tolower(colnames(sample_optimal))

  region_selected<-sample_optimal %>% group_by(departamento) %>% summarise(n=n())
#############################    
#Summary Stats on Selected schools
###################################    
    #Tabulate by rural variable
   samp_stats<- sample_optimal %>%
      group_by(rural) %>%
      summarise(label=first(rural), n=n()) 
  
  samp_stats   
  samp_stats %>%
      summarise(tot=sum(n))

#Compare optimal solution to random selection based on stratification
  adjustedStrata_actual<- df %>%
    group_by(departamento, rural) %>%
    summarise(N=n()) 
  
  
  adjustedStrata_actual$SOLUZ<-round(adjustedStrata_actual$N*0.008282945)
  adjustedStrata_actual %>%
    summarise(tot=sum(SOLUZ))
  
  df<- df %>%
    left_join( adjustedStrata_actual, c("departamento", "rural") )
  
  #produce table with randomly selected schools
  sample_random<- df %>%
    group_by(departamento, rural) %>%
    sample_n(SOLUZ, weight=total_4th )

#summary stats to compare optimally stratified schools with random schools
  sumstats_rand<-sample_random %>%
    group_by(departamento, rural) %>%
    summarise(count=n(), frac_rural=mean(rural), mean_4th_grade=mean(total_4th)) %>%
    arrange(departamento, rural)

  sumstats_opt<-sample_optimal %>%
    group_by(departamento, rural) %>%
    summarise(count=n(), frac_rural=mean(rural), mean_4th_grade=mean(total_4th)) %>%
    arrange(departamento, rural) 
  
##################################
  #Select Replacement Schools
###############################

  #create new datatable with new name based on sample.  This is done to just be 100% our raw sampled schools are never touched again by code.
  sample_final<-sample_optimal %>%
    mutate(sample_dashboard=1)
  
  #update orginal dataframe with sampled schools.
data_set_updated <- df %>%
  left_join(sample_final) %>%
  mutate(sample_dashboard=ifelse(is.na(sample_dashboard),0,sample_dashboard))
  
#This will be done so that 2 replacements in every province are chosen  
  #get a list of provincial districts sampled

provinces <- sample_final %>%
  group_by(provincia) %>%
  summarise(n_prov=n())
  
province_list<-as.vector(provinces$provincia)

#produce table with randomly replacement selected schools
sample_replacement1<- data_set_updated %>%
  left_join(provinces) %>%
  filter(provincia %in% province_list) %>%
  filter(sample_dashboard==0) %>%
  group_by(provincia) %>%
  sample_n(n_prov, weight=total_4th ) %>%
  mutate(sample_replacement1=1) %>%
  select(-n_prov)

data_set_updated <- data_set_updated %>%
  left_join(sample_replacement1) %>%
  mutate(sample_replacement1=ifelse(is.na(sample_replacement1),0,sample_replacement1))


sample_replacement2<- data_set_updated %>%
  left_join(provinces) %>%
  filter(provincia %in% province_list) %>%
  filter(sample_dashboard==0 & sample_replacement1==0) %>%
  group_by(provincia) %>%
  sample_n(n_prov, weight=total_4th ) %>%
  mutate(sample_replacement2=1)

data_set_updated <- data_set_updated %>%
  left_join(sample_replacement2) %>%
  mutate(sample_replacement2=ifelse(is.na(sample_replacement2),0,sample_replacement2)) %>%
  mutate(sample=case_when(
    sample_dashboard==1  ~ 1,
    sample_replacement1==1 ~ 2,
    sample_replacement2==1 ~ 3))

#get nearest neighbor replacement for each sampled school
sample_optimal_match<-sample_optimal[,c("latitude", "longitude")]
rownames(sample_optimal_match) <- sample_optimal$codigo.modular
sample_replacement1_match<-sample_replacement1[,c("latitude", "longitude")]
rownames(sample_replacement1_match) <- sample_replacement1$codigo.modular

neighbor1<-nn2(sample_optimal_match, sample_replacement1_match, k=1)

sample_updated<-data_set_updated  %>%
  filter(!is.na(sample))
#Save sample file
   currentDate<-Sys.Date()
   sample_file_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/school_sample_",currentDate,".csv", sep="")
   write.csv(sample_updated,sample_file_name)

sample_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/school_sample_",currentDate,".RData", sep="")
save(sample_updated, data_set_updated,
    file=sample_frame_name)   
   
######################################################
#### Map the selected schools #####
######################################################
sample_map <- data_set_updated %>%
     filter(!is.na(sample)) %>%
    mutate(longitude=as.character(longitude)) %>%
    mutate(latitude=as.character(latitude)) %>%
  mutate(longitude=as.numeric(longitude)) %>%
    mutate(latitude=as.numeric(latitude)) 

  sample_map_chosen <- data_set_updated %>%
     filter(sample==1) %>%
     mutate(longitude=as.character(longitude)) %>%
     mutate(latitude=as.character(latitude)) %>%
     mutate(longitude=as.numeric(longitude)) %>%
     mutate(latitude=as.numeric(latitude)) 
   
   
   pal <- colorFactor(
     palette = c("red", "green", "blue"),
     domain = sample_map$sample
   )
   
leaflet(data=sample_map_chosen) %>%
    addTiles()  %>%
    addMarkers( lng=~longitude,  lat=~latitude ) 
    
  
#####################################################
  #May want to split code off into new file here
#####################################################
  
currentDate<-c("2019-07-22")
sample_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/school_sample_",currentDate,".RData", sep="")
load(file=sample_frame_name) 
  #prepare data for upload to API
  
    vars<-c("SCHOOL", "REGION", "WOREDA", "ADMINCODE")
  
  assignments <- sample_updated %>%
    filter(sample_dashboard==1) %>%
    select( school_name_preload=denominacion.ie, 
            school_province_preload=provincia, school_district_preload=distrito, 
            school_emis_preload=codigo.modular) %>%
    mutate(school_code_preload=9000+1:n()) %>%
    mutate(interviewer="")
  
  assign_file_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/assignments_schools.csv", sep="")
  write.csv(assignments,assign_file_name)
  
  #now do same for public officials
  dist_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/district_sample_",currentDate,".RData", sep="")

  load(file=dist_frame_name) 
 dist_list_alt2<- dist_list_alt2 %>%
   mutate(interviewer="")
 
 assign_file_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/assignments_offices.csv", sep="")
 write.csv(dist_list_alt2,assign_file_name)
 
  ## Load the libraries
  library("httr")
  library("xml2")
  library("jsonlite")

  #create some values
  user<-"bstacy_api"
password <- rstudioapi::askForPassword()
url_base<-"https://gepdsen.mysurvey.solutions"
  QUID<-"06756cace6d24cc996ffccbfc26a2264"
  version<-"1"

      #Get list of questionnaires available  (if needed)
  q<-GET(paste0(url_base,"/api/v1/questionnaires"),
         authenticate(user, password))

  #Produce dataset with supervisors
  #path and folder where the .zip file will be stored
  url=paste0(url_base, "/api/v1/supervisors")
  
  df<-GET(url=url,
          authenticate(user, password))  
  str(content(df))
  
  #convert to useful dataframe
  df<-fromJSON(txt=content(df, as="text"))
  supervisors<-df$Users  
  
  #Now merge list of supervisors to identifying data of schools
  #randomly assign supervisors to districts
  
  #first collapse assignments to district level
  assign_dist<- assignments %>%
    select(school_province_preload, school_district_preload) %>%
    group_by(school_district_preload) %>%
    summarise( school_province_preload=first(school_province_preload))

  #Now randomly select a supervisor from list and assign
  assign_dist$supervisor=""
  for(i in 1:nrow(assign_dist)){
    #print(assign_dist$school_district_preload[i])
    rand_super<-sample_n(supervisors,1)
    print(rand_super$UserName[1])
    assign_dist$supervisor[i]=rand_super$UserName[1]
  }
  
  
  #now merge back onto full list of schools
  assignments <- assignments %>%
    left_join(assign_dist, by="school_district_preload")
  
  
  #################################################
  ###### Survey of Public Officials Sampling #####
  #################################################
  
  #This could also be split into a separate file.
  currentDate<-c("2019-07-22")
  sample_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/school_sample_",currentDate,".RData", sep="")
  load(file=sample_frame_name) 
  
  #Choose district offices to visit randomly among the list of districts we are already visiting for the schools
  #We will choose 20 of these district offices from the Regional level
  dist_list<- sample_updated %>%
    group_by(departamento) %>%
    summarise( )
  
  district_office_sample <- dist_list %>%
    sample_n(20)
  
  #save as csv
  dist_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/district_sample_",currentDate,".csv", sep="")
  
  write.csv(district_office_sample,dist_frame_name) 
  
###Run an iteration where we choose 20 provinces (UGEL) linked to schools and link up to regions
    #columns to keep
  cols<-c( "provincia", "dsc.ugel")
  
  dist_list_alt1<- sample_updated %>%
    filter(departamento %in% as.vector(district_office_sample$departamento)) %>%
    group_by(departamento) %>%
    sample_n(1) %>%
    summarise_at(cols, first)
  #save as csv
  dist_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/district_sample_20region_1ugel",currentDate,".csv", sep="")
  
  write.csv(dist_list_alt1,dist_frame_name) 
  
  #Run an iteration where we choose 10 provinces (UGEL) linked to schools and link up to regions
  #columns to keep
  cols<-c( "provincia", "dsc.ugel")
  
  district_office_sample <- dist_list %>%
    sample_n(10)
  
  dist_list_alt2<- sample_updated %>%
    filter(departamento %in% as.vector(district_office_sample$departamento)) %>%
    group_by(departamento) %>%
    sample_n(1) %>%
    summarise_at(cols, first)
  
  #save as csv
  dist_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/district_sample_10region_10ugel",currentDate,".csv", sep="")
  
  write.csv(dist_list_alt2,dist_frame_name) 
  
  ###save our sample  
  dist_frame_name <- paste("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Peru/2019/Data/sampling/district_sample_",currentDate,".RData", sep="")
  
  save(district_office_sample, dist_list_alt1, dist_list_alt2,
       file=dist_frame_name) 

  
  #################################################################
  ###### Upload Assignments to Survey Solutions using API ########
  #################################################################
  

   ##  API parameters
   url=paste0(url_base, "/api/v1/assignments")
   quid=paste0(QUID,"$", version)
   ##  the post
   resp<-assignments$supervisor
   #resp<-rep("bstacy1_inter",length(resp))
   status_list<-list()
  
   #drop some stray variables
   assignments_clean <- assignments %>%
     select(-c(school_province_preload.x , school_province_preload.y, supervisor ))
   
   for(i in 1:nrow(assignments)){
     print(resp[i])
     js_ch<-list(Responsible=unbox(resp[i]),
                 Quantity=unbox(1),
                 QuestionnaireId=unbox(quid),
                 IdentifyingData=data.frame(Variable=names(assignments_clean),
                                            Answer=unlist(assignments_clean[i,],use.names = F)))
     test_post<-httr::POST(url = url,
                           accept_json(),add_headers(charset="utf-8"),
                           authenticate(user, password, type = "basic"),
                           body=js_ch, encode = "json")
     ##  ATTENTION: USE THE RESPONSE
     aJsonFile<-tempfile()
     writeBin(content(test_post, "raw"), aJsonFile)
     status_list[[i]]<-fromJSON(aJsonFile)
   }

   
   # Now do Survey of Public Officials
   
   ##  API parameters
   url=paste0(url_base, "/api/v1/assignments")
   QUID<-"25534a374fa8434bb7d6f5133cdebab2"
   version<-"2"
   quid=paste0(QUID,"$", version)
   ##  the post
   resp<-district_office_sample$supervisor
   #resp<-rep("bstacy1_inter",length(resp))
   status_list<-list()
   
   #drop some stray variables
   district_office_sample_clean <- district_office_sample %>%
     select(-c( supervisor ))
   
   for(i in 1:nrow(district_office_sample_clean)){
     print(resp[i])
     js_ch<-list(Responsible=unbox(resp[i]),
                 Quantity=unbox(1),
                 QuestionnaireId=unbox(quid),
                 IdentifyingData=data.frame(Variable=names(district_office_sample_clean),
                                            Answer=unlist(district_office_sample_clean[i,],use.names = F)))
     test_post<-httr::POST(url = url,
                           accept_json(),add_headers(charset="utf-8"),
                           authenticate(user, password, type = "basic"),
                           body=js_ch, encode = "json")
     ##  ATTENTION: USE THE RESPONSE
     aJsonFile<-tempfile()
     writeBin(content(test_post, "raw"), aJsonFile)
     status_list[[i]]<-fromJSON(aJsonFile)
   }
   
