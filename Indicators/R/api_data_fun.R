
indicator_means <- function(variable, dataset, tag,  unit) {
  
  
  if (dataset=='school') {
    
    
    if (unit=="All") {
      
      stat_df<-get(paste("final_indicator_data_",tag, sep=""))
      

    } else if (unit=="Female") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_F", sep=""))

    } else if (unit=="Male") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_M", sep=""))

    } else if (unit=="Rural") {
      
      stat_df<-get(paste("final_indicator_data_",tag, sep="")) 
        filter(rural==TRUE)
      
    } else if (unit=="Urban") {
      
      stat_df<-get(paste("final_indicator_data_",tag, sep="")) 
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
      #(De Facto) Average number of classroom inputs in classrooms	
      SE.PRM.INPT     =indicator_means(inputs, "school", "INPT",  "All"),
      SE.PRM.INPT.1   =indicator_means(inputs, "school", "INPT",  "All"),
      SE.PRM.INPT.1.R =indicator_means(inputs, "school", "INPT",  "Rural"),
      SE.PRM.INPT.1.U =indicator_means(inputs, "school", "INPT",  "Urban"),
      #(De facto) Percent of classrooms equipped with pens/pencils, textbooks, and exercise books	
      SE.PRM.INPT.2   =0.33*indicator_means(textbooks, "school", "INPT",  "All") + 0.67*indicator_means(pens_etc, "school", "INPT",  "All"),
      SE.PRM.INPT.2.R =0.33*indicator_means(textbooks, "school", "INPT",  "Rural") + 0.67*indicator_means(pens_etc, "school", "INPT",  "Rural"),
      SE.PRM.INPT.2.U =0.33*indicator_means(textbooks, "school", "INPT",  "Rural") + 0.67*indicator_means(pens_etc, "school", "INPT",  "Urban"),
      #(De Facto) Percent of classrooms with a functional blackboard and chalk	
      SE.PRM.INPT.3   =indicator_means(blackboard_functional, "school", "INPT",  "All"),
      SE.PRM.INPT.3.R =indicator_means(blackboard_functional, "school", "INPT",  "Rural"),
      SE.PRM.INPT.3.U =indicator_means(blackboard_functional, "school", "INPT",  "Urban"),
      #(De Facto) Percent of classrooms with basic classroom furniture	
      SE.PRM.INPT.4   =indicator_means(share_desk, "school", "INPT",  "All"),
      SE.PRM.INPT.4.R =indicator_means(share_desk, "school", "INPT",  "Rural"),
      SE.PRM.INPT.4.U =indicator_means(share_desk, "school", "INPT",  "Urban"),
      #(De Facto) Percent of schools with access to EdTech	
      SE.PRM.INPT.5   =indicator_means(access_ict, "school", "INPT",  "All"),
      SE.PRM.INPT.5.R =indicator_means(access_ict, "school", "INPT",  "Rural"),
      SE.PRM.INPT.5.U =indicator_means(access_ict, "school", "INPT",  "Urban"),
    )
  
  
  #######################################
  # 	Basic Infrastructure	(INFR)
  #######################################
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      #(De Facto) Average number of infrastructure aspects present in schools	
      SE.PRM.INFR     =indicator_means(infrastructure	, "school", "INFR",  "All"),
      SE.PRM.INFR.1   =indicator_means(infrastructure	, "school", "INFR",  "All"),
      SE.PRM.INFR.1.R =indicator_means(infrastructure	, "school", "INFR",  "Rural"),
      SE.PRM.INFR.1.U =indicator_means(infrastructure	, "school", "INFR",  "Urban"),
      #(De Facto) Percent of schools with drinking water	
      SE.PRM.INFR.2   =indicator_means(drinking_water	, "school", "INFR",  "All"),
      SE.PRM.INFR.2.R =indicator_means(drinking_water	, "school", "INFR",  "Rural"),
      SE.PRM.INFR.2.U =indicator_means(drinking_water	, "school", "INFR",  "Urban"),
      #(De Facto) Percent of schools with functioning toilets
      SE.PRM.INFR.3   =indicator_means(functioning_toilet	, "school", "INFR",  "All"),
      SE.PRM.INFR.3.R =indicator_means(functioning_toilet	, "school", "INFR",  "Rural"),
      SE.PRM.INFR.3.U =indicator_means(functioning_toilet	, "school", "INFR",  "Urban"),
      #(De Facto) Percent of schools with access to electricity	
      SE.PRM.INFR.4   =indicator_means(class_electricity, "school", "INFR",  "All"),
      SE.PRM.INFR.4.R =indicator_means(class_electricity, "school", "INFR",  "Rural"),
      SE.PRM.INFR.4.U =indicator_means(class_electricity, "school", "INFR",  "Urban"),
      #(De Facto) Percent of schools with access to internet	
      SE.PRM.INFR.5   =indicator_means(internet, "school", "INFR",  "All"),
      SE.PRM.INFR.5.R =indicator_means(internet, "school", "INFR",  "Rural"),
      SE.PRM.INFR.5.U =indicator_means(internet, "school", "INFR",  "Urban"),
      #	(De Facto) Percent of schools accessible to children with special needs	
      SE.PRM.INFR.6   =indicator_means(disability_accessibility, "school", "INFR",  "All"),
      SE.PRM.INFR.6.R =indicator_means(disability_accessibility, "school", "INFR",  "Rural"),
      SE.PRM.INFR.6.U =indicator_means(disability_accessibility, "school", "INFR",  "Urban"),
    )
  
  #######################################
  # Learning Capacity	(LCAP)
  #######################################
  
  #api_final[grep('LERN', api_final$Series),1]
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.LCAP     = indicator_means(ecd_student_proficiency	, "school", "LCAP",  "All"),
      SE.PRM.LCAP.1   = indicator_means(ecd_student_knowledge	, "school", "LCAP",  "All"),
      SE.PRM.LCAP.1.F = indicator_means(ecd_student_knowledge	, "school", "LCAP",  "Female"),
      SE.PRM.LCAP.1.M = indicator_means(ecd_student_knowledge	, "school", "LCAP",  "Male"),
      SE.PRM.LCAP.1.R = indicator_means(ecd_student_knowledge	, "school", "LCAP",  "Rural"),
      SE.PRM.LCAP.1.U = indicator_means(ecd_student_knowledge	, "school", "LCAP",  "Urban"),
      SE.PRM.LCAP.2   = indicator_means(ecd_math_student_knowledge, "school", "LCAP",  "All"),  
      SE.PRM.LCAP.2.F = indicator_means(ecd_math_student_knowledge, "school", "LCAP",  "Female"),
      SE.PRM.LCAP.2.M = indicator_means(ecd_math_student_knowledge, "school", "LCAP",  "Male"),
      SE.PRM.LCAP.2.R = indicator_means(ecd_math_student_knowledge, "school", "LCAP",  "Rural"),
      SE.PRM.LCAP.2.U = indicator_means(ecd_math_student_knowledge, "school", "LCAP",  "Urban"),
      SE.PRM.LCAP.3   = indicator_means(ecd_literacy_student_knowledge, "school", "LCAP",  "All"),
      SE.PRM.LCAP.3.F = indicator_means(ecd_literacy_student_knowledge, "school", "LCAP",  "Female"),
      SE.PRM.LCAP.3.M = indicator_means(ecd_literacy_student_knowledge, "school", "LCAP",  "Male"),
      SE.PRM.LCAP.3.R = indicator_means(ecd_literacy_student_knowledge, "school", "LCAP",  "Rural"),
      SE.PRM.LCAP.3.U = indicator_means(ecd_literacy_student_knowledge, "school", "LCAP",  "Urban"),
      SE.PRM.LCAP.4   = indicator_means(ecd_exec_student_knowledge, "school", "LCAP",  "All"),
      SE.PRM.LCAP.4.F = indicator_means(ecd_exec_student_knowledge, "school", "LCAP",  "Female"),
      SE.PRM.LCAP.4.M = indicator_means(ecd_exec_student_knowledge, "school", "LCAP",  "Male"),
      SE.PRM.LCAP.4.R = indicator_means(ecd_exec_student_knowledge, "school", "LCAP",  "Rural"),
      SE.PRM.LCAP.4.U = indicator_means(ecd_exec_student_knowledge, "school", "LCAP",  "Urban"),
      SE.PRM.LCAP.5   = indicator_means(ecd_soc_student_knowledge, "school", "LCAP",  "All"),
      SE.PRM.LCAP.5.F = indicator_means(ecd_soc_student_knowledge, "school", "LCAP",  "Female"),
      SE.PRM.LCAP.5.M = indicator_means(ecd_soc_student_knowledge, "school", "LCAP",  "Male"),
      SE.PRM.LCAP.5.R = indicator_means(ecd_soc_student_knowledge, "school", "LCAP",  "Rural"),
      SE.PRM.LCAP.5.U = indicator_means(ecd_soc_student_knowledge, "school", "LCAP",  "Urban")
    )
  
  #######################################
  # Student Attendance	(ATTD)
  #######################################
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.ATTD     = indicator_means(student_attendance	, "school", "ATTD",  "All"),
      SE.PRM.ATTD.1   = indicator_means(student_attendance	, "school", "ATTD",  "All"),
      SE.PRM.ATTD.1.F = indicator_means(student_attendance	, "school", "ATTD",  "Female"),
      SE.PRM.ATTD.1.M = indicator_means(student_attendance	, "school", "ATTD",  "Male"),
      SE.PRM.ATTD.1.R = indicator_means(student_attendance	, "school", "ATTD",  "Rural"),
      SE.PRM.ATTD.1.U = indicator_means(student_attendance	, "school", "ATTD",  "Urban"),

    )
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
