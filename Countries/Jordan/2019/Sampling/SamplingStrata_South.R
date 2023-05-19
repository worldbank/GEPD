#Written by Brian Stacy

#NOTES: This file is difficult to make generic, because sampling is always different in each country.  
#So much customization may be needed.  However, the basic outline may be useful for several countries.
# I have left many bits of code that may or may not be useful that are commented
# I assume in much of this file that we want a national number with a breakdown by urban/rural, but this can be modified
# Most of this file was based on the Peru sampling file


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
library(tidyverse)
library(readxl)

set.seed(54351324)

#################################
#Specify Directories, etc
#################################

#Specify directory where sampling frame located
dir_frame<-"C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Jordan/2019/Data/Sampling"

#specify name of sampling frame file.  This needs to be a csv file
file_frame<-"schools_Jordan.xlsx"

#specify date that sample was created.  This may be useful if you want to access stored results
#date_frame<-c("2019-07-22")

date_frame<-c("2019-09-16")
  

#################################
#read in sampling frame from [XXX]
#################################


df=read_excel(paste(dir_frame, file_frame, sep="/"))


#make column names lower case 
colnames(df)<-tolower(colnames(df))

#remove spaces in column names
colnames(df)<-gsub(" ","_",colnames(df))


#rename one variable
df <- df %>%
  rename(district=major_general)

#Trim down data frame:
frame_vars<-c('directorate',
              'organization_code',
              'school_name',
              'sex_of_the_founder',
              'supervisory_authority',
              'territory',
              'governorate',
              'district',
              'elimination',
              'address',
              'longitude',
              'latitude',
              'education_type',
              'property_type',
              'classification_area',
              'foundation_period',
              'top_row',
              'the_lowest_row',
              'total_number_of_divisions',
              'total_male_students',
              'total_female_students',
              'total_students_grade_1',
              'total_students_grade_4',
              'total_teachers',
              'male_teachers',
              'female_teachers',
              'total_grade_1_teachers',
              'total_primary_teachers'
)

df_short<-df %>%
  dplyr::select(frame_vars)

#take a look at data
#number of supervisory types
ggplot(df_short, aes(x=supervisory_authority)) +
   geom_bar()

#town/village
ggplot(df_short, aes(x=classification_area)) +
   geom_bar()

#territory
ggplot(df_short, aes(x=territory)) +
  geom_bar()

#Governorate
ggplot(df_short, aes(x=governorate)) +
  geom_bar()

#district
ggplot(df_short, aes(x=district)) +
  geom_bar() +
  facet_wrap(vars(governorate), scales='free')

ggplot(df_short, aes(x=district, y=governorate)) +
  geom_raster() 

# #create variable ID for strata and keep schools with more than 3 1st and 4th grade students.  
# Also keep only private schools and schools supervised by Ministry of Education
df_short<-df_short %>%
  mutate(rural=(classification_area=="Village")) %>%
  filter(total_students_grade_4>=3 & total_students_grade_1>=3 & total_teachers>=3) %>%
  filter(supervisory_authority=="The Ministry of Education" | supervisory_authority=="Private")

domains<-df_short %>% group_by(governorate, supervisory_authority ) %>% summarise(n=n())





#########################################
#Prep database for Optimal Stratification
#########################################

# #turn string variables into factor variables
 df_short$governorate_factor=as.numeric(factor(df_short$governorate))
 df_short$supervisory_authority_factor=as.numeric(factor(df_short$supervisory_authority))
 df_short$rural_factor<-as.numeric(factor(df_short$rural))
 df_short$region_factor<-as.numeric(factor(df_short$territory))
 
 df_short <- df_short %>%
   mutate(rural_f=if_else(rural==TRUE, "Rural", "Urban")) %>%
   mutate(domain=paste(rural_f, territory, sep=" + ")) %>%
   mutate(domain_factor=as.numeric(factor(domain)))

#produce max coefficients of variation we will tolerate
cv <- as.data.frame(list(DOM=rep("DOM1",),
                         CV1=rep(0.05,2),
                         CV2=rep(0.05,2),
                         domainvalue=c(1:6)
))
cv

#See https://cran.r-project.org/web/packages/SamplingStrata/vignettes/SamplingStrata.html


df_short$id <- c(1:nrow(df_short))


frame <- buildFrameDF(df = df_short,
                      id = "organization_code",
                      X = c("governorate_factor", "supervisory_authority_factor"),
                      Y = c("total_students_grade_4", "total_students_grade_1"),
                      domainvalue = "domain_factor" )
str(frame)

#need to convert to discrete for sampling strata
#frame$X1 <- var.bin(data_set$inspection, bins=16)
#frame$X2 <- var.bin(data_set$narond, bins=10)

#need to convert to discrete for sampling strata



strata <- buildStrataDF(frame, progress = FALSE)
# #adjust values of strata for our X variables, d_score std deviations need to be adjusted for estimation
# strata <- strata %>%
#   left_join(df_stats) %>%
#   mutate(S1=M1_SD) %>%
#   mutate(CENS=1) %>%
#   select(-c("governorate", "rural", "M1_Mean", "M1_SD"))
# #strata$CENS<-1

  
  
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

    adjustedStrata <- adjustSize(size=230,strata=solution1$aggr_strata,cens=NULL)

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
   df_short<- df_short %>%
      mutate(ID=organization_code) %>%
      left_join(framenew, by="ID" )
    
    #create variable so that sorting is done reversed
  df_short$rev_total_4th=-df_short$total_students_grade_4
    
    #Note: This reverse order sampling approach is identical to TIMSS.  See https://timssandpirls.bc.edu/methods/pdf/Sampling_Schools.pdf
  df_short<-as.data.frame(df_short)
    
   sample_optimal <- selectSampleSystematic(frame=df_short, outstrata = adjustedStrata, sortvariable = 'rev_total_4th', writeFiles=FALSE) %>%
     sample_n(230)
  
  colnames(sample_optimal)<-tolower(colnames(sample_optimal))

  region_selected<-sample_optimal %>% group_by(governorate,supervisory_authority ) %>% summarise(n=n())

  
  
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
  adjustedStrata_actual<- df_short %>%
    group_by(governorate, rural) %>%
    summarise(N=n()) 
  
  
  adjustedStrata_actual$SOLUZ<-round(adjustedStrata_actual$N*(200/3300))
  adjustedStrata_actual %>%
    summarise(tot=sum(SOLUZ))
  
  df_short<- df_short %>%
    left_join( adjustedStrata_actual, c("governorate", "rural") )
  
  #produce table with randomly selected schools
  sample_random<- df_short %>%
    group_by(governorate, rural) %>%
    sample_n(SOLUZ, weight=total_students_grade_4 )

#summary stats to compare optimally stratified schools with random schools
  sumstats_rand<-sample_random %>%
    group_by(governorate, rural) %>%
    summarise(count=n(), frac_rural=mean(rural), mean_4th_grade=mean(total_students_grade_4)) %>%
    arrange(governorate, rural)

  sumstats_opt<-sample_optimal %>%
    group_by(governorate, rural) %>%
    summarise(count=n(), frac_rural=mean(rural), mean_4th_grade=mean(total_students_grade_4)) %>%
    arrange(governorate, rural) 
  
##################################
  #Select Replacement Schools
###############################

  #create new datatable with new name based on sample.  This is done to just be 100% our raw sampled schools are never touched again by code.
  sample_final<-sample_optimal %>%
    mutate(sample_dashboard=1)
  
  #update orginal dataframe with sampled schools.
data_set_updated <- df_short %>%
  left_join(sample_final) %>%
  mutate(sample_dashboard=ifelse(is.na(sample_dashboard),0,sample_dashboard))
  
#This will be done so that 2 replacements in every province are chosen  
  #get a list of districtl districts sampled

provinces <- sample_final %>%
  group_by(governorate) %>%
  summarise(n_prov=n())
  
province_list<-as.vector(provinces$governorate)

#produce table with randomly replacement selected schools
sample_replacement1<- data_set_updated %>%
  left_join(provinces) %>%
  filter(governorate %in% province_list) %>%
  filter(sample_dashboard==0) %>%
  group_by(governorate, rural) %>%
  sample_n(1, weight=total_students_grade_4 ) %>%
  mutate(sample_replacement1=1) %>%
  select(-n_prov)

data_set_updated <- data_set_updated %>%
  left_join(sample_replacement1) %>%
  mutate(sample_replacement1=ifelse(is.na(sample_replacement1),0,sample_replacement1))


sample_replacement2<- data_set_updated %>%
  left_join(provinces) %>%
  filter(governorate %in% province_list) %>%
  filter(sample_dashboard==0 & sample_replacement1==0) %>%
  group_by(governorate, rural) %>%
  sample_n(1, weight=total_students_grade_4 ) %>%
  mutate(sample_replacement2=1)

data_set_updated <- data_set_updated %>%
  left_join(sample_replacement2) %>%
  mutate(sample_replacement2=ifelse(is.na(sample_replacement2),0,sample_replacement2)) 


sample_amman<- data_set_updated %>%
  filter(governorate == "The Capital Amman") %>%
  filter(sample_dashboard==0 & sample_replacement1==0 & sample_replacement2==0) %>%
  ungroup() %>% 
  sample_n(5, weight=total_students_grade_4 ) %>%
  mutate(sample_amman=1)

data_set_updated <- data_set_updated %>%
  left_join(sample_amman) %>%
  mutate(sample_maputo=ifelse(is.na(sample_amman),0,sample_amman)) %>%
  mutate(sample=case_when(
    sample_dashboard==1  ~ 1,
    sample_replacement1==1 ~ 2,
    sample_replacement2==1 ~ 3,
    sample_amman==1 ~ 4)) %>%
  mutate(sample=factor(sample, levels=c(1,2,3,4), labels=c('Sampled School', "Replacement 1", "Replacement 2", 'Amman Pilot Schools')))


#get nearest neighbor replacement for each sampled school
# sample_optimal_match<-sample_optimal[,c("latitude", "longitude")]
# rownames(sample_optimal_match) <- sample_optimal$organization_code
# sample_replacement1_match<-sample_replacement1[,c("latitude", "longitude")]
# rownames(sample_replacement1_match) <- sample_replacement1$organization_code
# 
# neighbor1<-nn2(sample_optimal_match, sample_replacement1_match, k=1)



#replace one geocode that has lat and long switched
data_set_updated <- data_set_updated %>%
  mutate(latitude_temp=latitude) %>%
  mutate(latitude=if_else(organization_code==113128, longitude, latitude),
         longitude=if_else(organization_code==113128, latitude_temp, longitude)) %>%
  select(-latitude_temp)
  
sample_updated<-data_set_updated  %>%
  filter(!is.na(sample)) %>%
  select(-c("governorate_factor",
            "supervisory_authority_factor",
            "rural_factor",
            "id",
            "ID",
            "DOMAINVALUE",
            "STRATUM",
            "X1",
            "X2",
            "Y1",
            "Y2",
            "LABEL",
            "rev_total_4th",
            "domainvalue",
            "strato",
            "id.1",
            "stratum",
            "x1",
            "x2",
            "y1",
            "y2"))

sample_updated <- sample_updated %>% 
  arrange(district, sample) %>%
  group_by(district, sample) %>% 
  mutate(replace_group_id=row_number()) %>%
  group_by(district, replace_group_id) %>%
  mutate( replace_governorate=if_else((sample==1 | sample==4),as.character(NA), governorate)) %>%
  arrange(district, replace_group_id) 

#Save sample file
   currentDate<-Sys.Date()
   sample_file_name <- paste(dir_frame,"/school_sample_",currentDate,".xlsx", sep="")
   writexl::write_xlsx(sample_updated, path=sample_file_name)

sample_frame_name <- paste(dir_frame,"/school_sample_",currentDate,".RData", sep="")
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
    
  

  
  #################################################
  ###### Survey of Public Officials Sampling #####
  #################################################
  
  #This could also be split into a separate file.
  sample_file_name <- paste(dir_frame,"school_sample_",date_frame,".csv", sep="")
  load(file=sample_frame_name) 
  
  #Choose district offices to visit randomly among the list of districts we are already visiting for the schools
  #We will choose 20 of these district offices from the Regional level
  dist_list<- sample_updated %>%
    group_by(governorate) %>%
    summarise( )
  
  district_office_sample <- dist_list %>%
    sample_n(20)
  
  #save as csv
  dist_frame_name <- paste(dir_frame,"district_sample_",date_frame,".csv", sep="")
  
  write.csv(district_office_sample,dist_frame_name) 
  

  
  #Run an iteration where we choose 10 provinces (UGEL) linked to schools and link up to regions
  #columns to keep
  cols<-c( "district", "dsc.ugel")
  
  district_office_sample <- dist_list %>%
    sample_n(10)
  
  dist_list_alt2<- sample_updated %>%
    filter(governorate %in% as.vector(district_office_sample$governorate)) %>%
    group_by(governorate) %>%
    sample_n(1) %>%
    summarise_at(cols, first)
  
  #save as csv
  dist_frame_name <- paste(dir_frame,"district_sample_",date_frame,".csv", sep="")
  
  write.csv(district_office_sample,dist_frame_name) 
  
  
  ###save our sample  
  dist_frame_name <- paste(dir_frame,"district_sample_",date_frame,".RData", sep="")
  
  save(district_office_sample, dist_list_alt1, dist_list_alt2,
       file=dist_frame_name) 

  
 