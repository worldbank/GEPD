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

#https://github.com/GIST-ORNL/wbstats

#Might add this subnational data as target variables
wbsearch("Malnutrition")
stunting<-wb(indicator = "SN.SH.STA.STNT.ZS", startdate=2013, enddate=2017)

#rename variable
stunting <- stunting %>%
  mutate(stunting=value) %>%
  select(-value)

#malnutrition  
malnutrition<-wb(indicator = "SN.SH.STA.MALN.ZS", startdate=2013, enddate=2017 )
#rename variable
malnutrition <- malnutrition %>%
  mutate(malnutrition=value) %>%
  select(-value)

#merge stunting and malnutrition
stunting<- stunting %>%
  left_join(malnutrition, by="country")

#break country variable into country name and region
stunting<- stunting %>%
  separate(country, into=c("country", "REGION"), sep=", ")

#keep on Ethiopia
stunting<- stunting %>%
  filter(country=="Ethiopia")

#keep just important columns
stunting<- stunting %>%
  select(c("REGION", "stunting", "malnutrition")) %>%
  mutate(region_name=REGION)

#Make some changes by hand to have regions with same exact title
stunting$REGION[stunting$REGION=='Beneshangul Gumu']='Benishangul-Gumuz'
stunting$REGION[stunting$REGION=='Gambela']='Gambella'
stunting$REGION[stunting$REGION=='Hareri']='Harari'
stunting$REGION[stunting$REGION=='Oromia']='Oromiya'
stunting$REGION[stunting$REGION=='SNNPR']='SNNP'


df=read_dta("C:/Users/wb469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Ethiopia/2019/Data/Sampling/ETH_sampling_data.dta")




#Add stunting and malnutrition variables
df<- df %>%
  left_join(stunting, by="REGION")

df %>%
  filter(is.na(stunting)) %>%
  summarise(n=n())

df %>%
  filter(is.na(stunting)) %>%
  summarise(n=n())

#create variable ID for strata
df<-df %>%
  mutate(private=(OWNERSHIP!="Goverment")) %>%
  mutate(domains=as.numeric(factor(REGION)))

factor(df$domains)

#produce max coefficients of variation we will tolerate
cv <- as.data.frame(list(DOM=rep("DOM1",11),
                         CV1=rep(0.1,11),
                         CV2=rep(0.1,11),
                         domainvalue=c(1:11)
))
cv

#See https://cran.r-project.org/web/packages/SamplingStrata/vignettes/SamplingStrata.html


df$id <- c(1:nrow(df))

#turn string variables into factor variables
df$woreda_factor=as.numeric(factor(df$WOREDA))
df$ownership_factor<-as.numeric(factor(df$OWNERSHIP))

frame <- buildFrameDF(df = df,
                      id = "ADMINCODE",
                      X = c("woreda_factor","ownership_factor"),
                      Y = c("stunting", "malnutrition"),
                      domainvalue = "domains")
str(frame)

#need to convert to discrete for sampling strata
#frame$X1 <- var.bin(data_set$inspection, bins=16)
#frame$X2 <- var.bin(data_set$narond, bins=10)

#need to convert to discrete for sampling strata



strata <- buildStrataDF(frame, progress = FALSE)
#adjust values of strata for our X variables, stunting and malnutrition to fix std deviations, which are off
strata <- strata %>%
  mutate(S1=sqrt(M1*(100-M1)), S2=sqrt(M2*(100-M2)))
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

    adjustedStrata <- adjustSize(size=300,strata=solution1$aggr_strata,cens=NULL)

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
    


    sample_optimal <- selectSample(framenew, adjustedStrata, writeFiles=FALSE)

    #update original database with stratum labels
    sample_optimal<- sample_optimal %>%
      mutate(ADMINCODE=ID) %>%
      left_join(df, by="ADMINCODE" )
    
    
    #Tabulate by OWNERSHIP variable
   samp_stats<- sample_optimal %>%
      group_by(REGION) %>%
      summarise(label=first(LABEL), n=n()) 
  
  samp_stats   
  samp_stats %>%
      summarise(tot=sum(n))
  
  


  sample_final<-sample_optimal

######################################################
#### Map the selected schools #####
######################################################
  #import world bank subnational data.  Find here:  https://datacatalog.worldbank.org/dataset/world-subnational-boundaries
  #need to point R to correct directory. Enter here
  fgdb <- "C:/Users/WB469649/OneDrive - WBG/Education Policy Dashboard/Dashboard Mockup/maps/2015_GAUL_Dataset_Mod.gdb"
  
  # List all feature classes in a file geodatabase
  subset(ogrDrivers(), grepl("GDB", name))
  fc_list <- ogrListLayers(fgdb)
  print(fc_list)
  
  # Read the feature class
  fc <- readOGR(dsn=fgdb,layer="g2015_2014_1")
  
  
  #select data just for peru
  fc_ethiopia <- fc[fc@data$ADM0_NAME=="Ethiopia",]
  
  #first collapse assignments to province level
  assign_prov<- sample_optimal %>%
    rename(ADM1_NAME=region_name) %>%
    group_by(ADM1_NAME) %>%
    summarise( n=n(), admin_code=first(ADMINCODE))
  
  assign_prov2<- df %>%
    group_by(REGION) %>%
    rename(ADM1_NAME=REGION) %>%
    summarise( n=n(), stunting=mean(stunting), malnutrition=mean(malnutrition))
  
  #Add assignments per province data
  fc_ethiopia@data <- fc_ethiopia@data %>%
    left_join(assign_prov, by='ADM1_NAME')

  #create color pallete
  bins <- c(0, 5, 10, 15, 20, 25, 30, 50, Inf)
  pal <- colorBin("YlOrRd", domain = fc_ethiopia$n, bins = bins)

  #add lables
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Schools Chosen",
    fc_ethiopia$ADM1_NAME, fc_ethiopia$n
  ) %>% lapply(htmltools::HTML)  
m<-leaflet() %>%
  addTiles() %>%
  addPolygons(data=fc_ethiopia, popup = ~ADM1_NAME, fillColor = ~pal(n),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))  %>%
  addLegend(data=fc_ethiopia, pal = pal, values = ~n, opacity = 0.7, title = "# Schools Sampled",
            position = "bottomright")


m
  
#####################################################
  #May want to split code off into new file here
#####################################################
  
  
  #prepare data for upload to API
  
    vars<-c("SCHOOL", "REGION", "WOREDA", "ADMINCODE")
  
  assignments<-select(sample_final, school_name_preload=SCHOOL, school_province_preload=REGION, school_district_preload=WOREDA, school_emis_preload=ADMINCODE)
  assignments <- assignments %>%
    mutate(school_code_preload=9000+1:n())
  
  
 
  
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
  
  #Choose district offices to visit randomly among the list of districts we are already visiting for the schools
  #We will choose 20 of these district offices
  
  district_office_sample <- assign_dist %>%
    sample_n(20)
  
 
  
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
   
