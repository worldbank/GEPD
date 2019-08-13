library(SamplingStrata)
library(haven)
library(dplyr)
library(wbstats)
library(ggmap)
library(leaflet)
library(maps)
library(maptools)
library(rgeos)
library(sp)
library(rgdal)
library(tidyr)
library(stringr)

data_set=read_dta("C:/Users/WB469649/OneDrive - WBG/Education Policy Dashboard/Sampling/Sampling Data/Senegal/finalsample.dta")
data_set_orig=read_dta("C:/Users/WB469649/OneDrive - WBG/Education Policy Dashboard/Sampling/Sampling Data/Senegal/finalsample.dta")

#keep only sampled observations for choosing which schools to revisit
data_set<-data_set %>%
  filter(finalsample==1)

#create dataframe with the number selected from each stratum in the 


#create variable ID for strata
data_set<-data_set %>%
    mutate(domains=1)

data_set$strat<-factor(data_set$inspection)

#produce max coefficients of variation we will tolerate
cv <- as.data.frame(list(DOM=rep("DOM1",1),
                         CV1=rep(0.1,1),
                         CV2=rep(0.1,1),
                         CV3=rep(0.1,1),
                         domainvalue=c(1:1)
))
cv

#See https://cran.r-project.org/web/packages/SamplingStrata/vignettes/SamplingStrata.html


data_set$id <- c(1:nrow(data_set))


senegalframe <- buildFrameDF(df = data_set,
                             id = "codeetablissement",
                             X = c("inspection"),
                             Y = c("total_ce2", "garcons_ce2", "filles_ce2"),
                             domainvalue = "domains")
str(senegalframe)

#need to convert to discrete for sampling strata
#senegalframe$X1 <- var.bin(data_set$inspection, bins=16)
#senegalframe$X2 <- var.bin(data_set$narond, bins=10)

#need to convert to discrete for sampling strata



senegalstrata <- buildStrataDF(senegalframe, progress = FALSE)
senegalstrata$CENS<-1

  
  
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
                                 strata = senegalstrata, 
                                 sampframe = senegalframe))
  
  
  solution1 <- optimizeStrata(
    errors = cv, 
    strata = senegalstrata,
    parallel = FALSE,
    writeFiles = FALSE,
    showPlot = TRUE,
    minnumstr = 2)
  
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

    adjustedStrata <- adjustSize(size=204,strata=solution1$aggr_strata,cens=NULL)

  sum(adjustedStrata$SOLUZ)
  # The difference between the desired sample size and the actual adjusted size depends on the number of strata in the optimized solution. Consider that the adjustment is performed in each stratum by taking into account the relative difference between the current sample size and the desired one: this produces an allocation that is expressed by a real number, that must be rounded, while taking into account the requirement of the minimum number of units in the strata. The higher the number of strata, the higher the impact on the final adjusted sample size.
  

  #   
    newstrata <- updateStrata(senegalstrata, 
                              solution1, 
                              writeFiles = TRUE)
  # Now, the atomic strata are associated to the aggregate strata defined in the optimal solution, by means of the variable LABEL. If we want to analyse in detail the new structure of the stratification, we can look at the strata_aggregation.txt file:
    

  
  
  
  # Once the optimal stratification has been obtained, to be operational we need to accomplish the following two steps:
  #   
  #   to update the frame units with new stratum labels (combination of the new values of the auxiliary variables X's);
  # to select the sample from the frame.
  # As for the first, we execute the following command:
  #   
  

  
    framenew <- updateFrame(senegalframe, newstrata, writeFiles=FALSE)
    

     #update original database with stratum labels
    dataset<- data_set %>%
      mutate(ID=codeetablissement) %>%
      left_join(framenew, by="ID" )
    
    
  # The function updateFrame receives as arguments the indication of the dataframe in which the frame information is memorised, and of the dataframe produced by the execution of the updateStrata function. The execution of this function produces a dataframe framenew, and also a file (named framenew.txt) with the labels of the new strata produced by the optimisation step. The allocation of units is contained in the SOLUZ column of the dataset outstrata.txt. At this point it is possible to select the sample from the new version of the frame:
    #create variable so that sorting is done reversed
    dataset$rev_total_ce2=-dataset$total_ce2
    
    #Note: This reverse order sampling approach is identical to TIMSS.  See https://timssandpirls.bc.edu/methods/pdf/Sampling_Schools.pdf
    
    sample_optimal <- selectSampleSystematic(dataset, adjustedStrata, sortvariable = "rev_total_ce2", writeFiles=FALSE)

    #Tabulate by INSPECTION variable
   samp_stats<- sample_optimal %>%
      group_by(INSPECTION) %>%
      summarise(label=first(LABEL), n=n()) 
  
  samp_stats   
  samp_stats %>%
      summarise(tot=sum(n))
  
  #check <- read.delim("sampling_check") 
    #Adjust number selected per stata based on what is chosen in Waly's do file.  Just scale down number chosen to 25%.
  
  #adjustedStrata_actual<-adjustedStrata
  #adjustedStrata_actual$SOLUZ=round(adjustedStrata_actual$N*.25)
  #sample_actual <- selectSampleSystematic(dataset, adjustedStrata_actual, sortvariable = "rev_total_ce2", writeFiles=FALSE)

  adjustedStrata_actual<- dataset %>%
    group_by(inspection) %>%
    summarise(N=n()) 

  
  adjustedStrata_actual$SOLUZ<-round(adjustedStrata_actual$N*.24)
  adjustedStrata_actual %>%
    summarise(tot=sum(SOLUZ))
  
  dataset<- dataset %>%
    left_join( adjustedStrata_actual, by="inspection" )
  
  sample_actual <- dataset %>%
    group_by(inspection) %>%
    sample_n(SOLUZ, weight=total_ce2 )
  
  summarise(sample_actual, mean=mean(total_ce2))  
  
  

    
  #Tabulate by INSPECTION variable
  samp_stats2<- sample_actual %>%
    group_by(inspection) %>%
    summarise(label=first(LABEL), n=n()) 
  
  #compare optimal stratification with actual stratification
  samp_stats<-cbind(samp_stats, samp_stats2)   
  samp_stats
  samp_stats2 %>%
    summarise(tot=sum(n))
  
  #Examine how many urban/rural we have
  sample_actual %>%
    group_by(urbainrural) %>%
    summarise(n=n())
  
sample_final <- sample_actual %>%
  select(1:50) %>%
  mutate(sample_dashboard=1)

data_set_updated <- data_set_orig %>%
  left_join(sample_final) %>%
  mutate(sample_dashboard=ifelse(is.na(sample_dashboard),0,sample_dashboard))

data_set_updated %>%
  group_by(sample_dashboard) %>%
  summarise(n=n())

write_dta(data_set_updated, "C:/Users/WB469649/OneDrive - WBG/Education Policy Dashboard/Sampling/Sampling Data/Senegal/finalsample_dashboard.dta")
write.csv(data_set_updated, "C:/Users/WB469649/OneDrive - WBG/Education Policy Dashboard/Sampling/Sampling Data/Senegal/finalsample_dashboard.csv")

    