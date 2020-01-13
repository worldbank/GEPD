
indicator_means <- function(variable, dataset, tag,  unit) {
  
  
  if (dataset=='school') {
    
    
    if (unit=="All") {
      
      stat_df<-get(paste("final_indicator_data_",tag, sep=""))
      
      stat_df<- df_weights_function(stat_df,codigo.modular, total_4th, departamento)
      
    } else if (unit=="Female") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_F", sep=""))
      stat_df<- df_weights_function(stat_df,codigo.modular, total_4th, departamento)
      
    } else if (unit=="Male") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_M", sep=""))
      stat_df<- df_weights_function(stat_df,codigo.modular, total_4th, departamento)
      
    } else if (unit=="Rural") {
      
      stat_df<-get(paste("final_indicator_data_",tag, sep="")) 
      stat_df<- df_weights_function(stat_df,codigo.modular, total_4th, departamento) %>%
        filter(rural==TRUE)
      
    } else if (unit=="Urban") {
      
      stat_df<-get(paste("final_indicator_data_",tag, sep="")) 
      stat_df<- df_weights_function(stat_df,codigo.modular, total_4th, departamento) %>%
        filter(rural==FALSE)
      
    }
    r <- eval(substitute(variable), stat_df, parent.frame())
    
    weights <- stat_df$school_ipw
    
    wtd.mean(r, weights=weights, na.rm=T)
    
  } else if (dataset== 'public_officials') {
    
  } 
  
}



#function to create indicator data for a specified country and year
api_data <- function(data_dir1, data_dir2, data_dir3, cntry, yr) {
  
  

  #transpose the indicator values dataframe to make it easier to add values
  api_final <- api_final %>%
    mutate(value = as.numeric(NA))
  
  indicator_values_transpose <- as.data.frame(t(as.matrix(api_final))) 
  
  colnames(indicator_values_transpose) <- api_final$Series
 %>%  
  indicator_values_transpose <- indicator_values_transpose %>%
    filter(rownames(indicator_values_transpose)=="value")
 
   # In the remainder of this code, we will go through each indicator and align the indicators with the data
  
  ######################################
  #Proficiency by End of Primary (PRIM) TENR PRIM PROE
  ######################################

  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.LPV.PRIM	= wbopendat$SE.LPV.PRIM,
      SE.LPV.PRIM.1	= wbopendat$SE.LPV.PRIM,
      SE.LPV.PRIM.BMP	= wbopendat$SE.LPV.PRIM.BMP,
      SE.LPV.PRIM.BMP.1	= wbopendat$SE.LPV.PRIM.BMP
    )
  
  
  #######################################
  # Proficiency on GEPD Assessment	(LERN)
  #######################################
  
  #api_final[grep('LERN', api_final$Series),1]
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.LERN     = indicator_means(student_proficient, "school", "LERN",  "All"),
      SE.PRM.LERN.1   = indicator_means(student_proficient, "school", "LERN",  "All"),
      SE.PRM.LERN.1.F = indicator_means(student_proficient, "school", "LERN",  "Female"),
      SE.PRM.LERN.1.M = indicator_means(student_proficient, "school", "LERN",  "Male"),
      SE.PRM.LERN.1.R = indicator_means(student_proficient, "school", "LERN",  "Rural"),
      SE.PRM.LERN.1.U = indicator_means(student_proficient, "school", "LERN",  "Urban"),
      SE.PRM.LERN.2   = indicator_means(literacy_student_proficient, "school", "LERN",  "All"),  
      SE.PRM.LERN.2.F = indicator_means(literacy_student_proficient, "school", "LERN",  "Female"),
      SE.PRM.LERN.2.M = indicator_means(literacy_student_proficient, "school", "LERN",  "Male"),
      SE.PRM.LERN.2.R = indicator_means(literacy_student_proficient, "school", "LERN",  "Rural"),
      SE.PRM.LERN.2.U = indicator_means(literacy_student_proficient, "school", "LERN",  "Urban"),
      SE.PRM.LERN.3   = indicator_means(math_student_proficient, "school", "LERN",  "All"),
      SE.PRM.LERN.3.F = indicator_means(math_student_proficient, "school", "LERN",  "Female"),
      SE.PRM.LERN.3.M = indicator_means(math_student_proficient, "school", "LERN",  "Male"),
      SE.PRM.LERN.3.R = indicator_means(math_student_proficient, "school", "LERN",  "Rural"),
      SE.PRM.LERN.3.U = indicator_means(math_student_proficient, "school", "LERN",  "Urban")
    )
  
  
  #######################################
  # Teacher Effort		(EFFT)
  #######################################
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.EFFT     = 100-indicator_means(absence_rate, "school", "EFFT",  "All"),
      SE.PRM.EFFT.1   = 100-indicator_means(absence_rate, "school", "EFFT",  "All"),
      SE.PRM.EFFT.1.F = 100-indicator_means(absence_rate, "school", "EFFT",  "Female"),
      SE.PRM.EFFT.1.M = 100-indicator_means(absence_rate, "school", "EFFT",  "Male"),
      SE.PRM.EFFT.1.R = 100-indicator_means(absence_rate, "school", "EFFT",  "Rural"),
      SE.PRM.EFFT.1.U = 100-indicator_means(absence_rate, "school", "EFFT",  "Urban"),
      SE.PRM.EFFT.2   = 100-indicator_means(school_absence_rate, "school", "EFFT",  "All"),  
      SE.PRM.EFFT.2.F = 100-indicator_means(school_absence_rate, "school", "EFFT",  "Female"),
      SE.PRM.EFFT.2.M = 100-indicator_means(school_absence_rate, "school", "EFFT",  "Male"),
      SE.PRM.EFFT.2.R = 100-indicator_means(school_absence_rate, "school", "EFFT",  "Rural"),
      SE.PRM.EFFT.2.U = 100-indicator_means(school_absence_rate, "school", "EFFT",  "Urban")

    )
  
  
  #######################################
  # 	Teacher Content Knowledge	(CONT)
  #######################################
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.CONT     = indicator_means(content_proficiency, "school", "CONT",  "All"),
      SE.PRM.CONT.1   = indicator_means(content_proficiency, "school", "CONT",  "All"),
      SE.PRM.CONT.1.F = indicator_means(content_proficiency, "school", "CONT",  "Female"),
      SE.PRM.CONT.1.M = indicator_means(content_proficiency, "school", "CONT",  "Male"),
      SE.PRM.CONT.1.R = indicator_means(content_proficiency, "school", "CONT",  "Rural"),
      SE.PRM.CONT.1.U = indicator_means(content_proficiency, "school", "CONT",  "Urban"),
      SE.PRM.CONT.2   = indicator_means(literacy_content_proficiency, "school", "CONT",  "All"),  
      SE.PRM.CONT.2.F = indicator_means(literacy_content_proficiency, "school", "CONT",  "Female"),
      SE.PRM.CONT.2.M = indicator_means(literacy_content_proficiency, "school", "CONT",  "Male"),
      SE.PRM.CONT.2.R = indicator_means(literacy_content_proficiency, "school", "CONT",  "Rural"),
      SE.PRM.CONT.2.U = indicator_means(literacy_content_proficiency, "school", "CONT",  "Urban"),
      SE.PRM.CONT.3   = indicator_means(math_content_proficiency, "school", "CONT",  "All"),
      SE.PRM.CONT.3.F = indicator_means(math_content_proficiency, "school", "CONT",  "Female"),
      SE.PRM.CONT.3.M = indicator_means(math_content_proficiency, "school", "CONT",  "Male"),
      SE.PRM.CONT.3.R = indicator_means(math_content_proficiency, "school", "CONT",  "Rural"),
      SE.PRM.CONT.3.U = indicator_means(math_content_proficiency, "school", "CONT",  "Urban")
    )
  
  #######################################
  # Teacher Pedagogical Skills	(PEDG)
  #######################################
  
  #######################################
  # 	Basic Inputs	(INPT)
  #######################################
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.INPT    
      SE.PRM.INPT.1  
      SE.PRM.INPT.1.R
      SE.PRM.INPT.1.U
      SE.PRM.INPT.2  
      SE.PRM.INPT.2.R
      SE.PRM.INPT.2.U
      SE.PRM.INPT.3  
      SE.PRM.INPT.3.R
      SE.PRM.INPT.3.U
      SE.PRM.INPT.4  
      SE.PRM.INPT.4.R
      SE.PRM.INPT.4.U
      SE.PRM.INPT.5  
      SE.PRM.INPT.5.R
      SE.PRM.INPT.5.U
    )
  
  
  #######################################
  # 	Basic Infrastructure	(INFR)
  #######################################
  
  #######################################
  # Learning Capacity	(LCAP)
  #######################################
  
  #######################################
  # Student Attendance	(ATTD)
  #######################################
  
  #######################################
  # Operactional Management (OPMN)
  #######################################
  
  #######################################
  # Instructional Leadership	(ILDR)
  #######################################
  
  #######################################
  # Principal School Knowledge	(PKNW)
  #######################################
  
  #######################################
  # Principal Management Skills	(PMAN)
  #######################################
  
  #######################################
  # Policy Lever (Teaching) - Attraction	(TATT)
  #######################################
  
  #######################################
  # Policy Lever (Teaching) - Selection & Deployment	(TSDP)
  #######################################
  
  #######################################
  # Policy Lever (Teaching) - Support	(TSUP)
  #######################################
  
  #######################################
  # Policy Lever (Teaching) - Evaluation	(TEVL)
  #######################################
  
  #######################################
  # Policy Lever (Teaching) - Monitoring & Accountability 	(TMNA)
  #######################################
  
  #######################################
  # Policy Lever (Teaching) - Intrinsic Motivation 	(TINM)
  #######################################
  
  #######################################
  # Policy Lever (Inputs & Infrastructure) - Standards 	(ISTD)
  #######################################
  
  #######################################
  # olicy Lever (Inputs & Infrastructure) - Monitoring 	(IMON)
  #######################################
  
  #######################################
  # Policy Lever (Learners) - Nutrition Programs 	(LNTN)
  #######################################
  
  #######################################
  # Policy Lever (Learners) - Health 	(LHTH)
  #######################################
  
  #######################################
  # Policy Lever (Learners) - Center-Based Care 	(LCBC)
  #######################################
  
  #######################################
  # Policy Lever (Learners) - Caregiver Capacity - Financial Capacity 	(LFCP)
  #######################################
  
  #######################################
  # Policy Lever (Learners) - Caregiver Capacity - Skills Capacity 	(LSXC)
  #######################################
  
  #######################################
  # Policy Lever (School Management) - Clarity of Functions 	(SCFN)
  #######################################
  
  #######################################
  # Policy Lever (School Management) - Attraction 	(SATT)
  #######################################
  
  #######################################
  # Policy Lever (School Management) - Selection & Deployment 	(SSLD)
  #######################################
  
  #######################################
  # Policy Lever (School Management) - Support 	(SSUP)
  #######################################
  
  #######################################
  # Policy Lever (School Management) - Evaluation 	(SEVL)
  #######################################
  
  #######################################
  # Politics & Bureaucratic Capacity - Quality of Bureaucracy 	(BQBR)
  #######################################
  
  #######################################
  # Politics & Bureaucratic Capacity - Impartial Decision-Making 	(BIMP)
  #######################################
  
  #######################################
  # Politics & Bureaucratic Capacity - Mandates & Accountability 	(BMAC)
  #######################################
  
  #######################################
  # Politics & Bureaucratic Capacity - National Learning Goals 	(BNLG)
  #######################################
  
  #######################################
  # Politics & Bureaucratic Capacity - Financing 	(BFIN)
  #######################################
  
  
  #reshape dataframe back
  indicator_values_back <- as.data.frame(t(as.matrix(indicator_values_transpose))) %>%
    rownames_to_column(var='Series') %>%
    rename(value = V1  )
  
  
  
  
  #Tags
  practice_tags <- "SE.PRM.PROE|SE.LPV.PRIM|SE.PRM.LERN|SE.PRM.TENR|SE.PRM.EFFT|SE.PRM.CONT|SE.PRM.ATTD"
  
  
  
  
  #read in practice data
  api_p <- api_final %>%
    rename(Indicator.Name='Indicator Name') %>%
    filter(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name)) %>%
    rename(  'Indicator Name'=Indicator.Name) %>%
    select(Series, 'Indicator Name') %>%
    mutate(value=rbinom(n(), 100, 0.7)) %>%
    mutate(
      value_metadata=case_when(
        value <=50 ~ "Needs Improvement",
        value >50 & value<=80 ~ "Caution",
        value >80 ~ "On Target"
      ))
  
  #read in non-practice data
  api_c <- api_final %>%
    rename(Indicator.Name='Indicator Name') %>%
    filter(!(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name))) %>%
    rename(  'Indicator Name'=Indicator.Name) %>%
    select(Series, 'Indicator Name') %>%
    mutate(value=rbinom(n(), 5, 0.7)) %>%
    mutate(
      value_metadata=case_when(
        value <=2 ~ "Needs Improvement",
        value >2 & value<4 ~ "Caution",
        value >=4 ~ "On Target"
      ))
  
  api_dummy_p %>%
    bind_rows(api_dummy_c) %>%
    arrange(Series) %>%
    mutate(year=yr,
           cty_or_agg="cty",
           countrycode=cntry)
}
