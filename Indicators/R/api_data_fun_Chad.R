
indicator_means <- function(variable, dataset, tag,  unit) {
  
  
  if (dataset=='school') {
    
    
    if (unit=="All") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep=""))
      

    } else if (unit=="Female") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_F", "_anon", sep=""))

    } else if (unit=="Male") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_M", "_anon", sep=""))

    } else if (unit=="Rural") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(urban_rural=="Rural")
      
    } else if (unit=="Urban") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(urban_rural=="Urban")
      
    } else if (unit=="Custom") {
      
      stat_df<-get(paste(tag)) 
      
    }
    r <- eval(substitute(variable), stat_df, parent.frame())
    
    weights <- stat_df$ipw
    
    wtd.mean(r, weights=weights, na.rm=T)
    
  } else if (dataset== 'public_officials') {
    
    if (unit=="All") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep=""))
      

    } else if (unit=="central") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(govt_tier=="Ministry of Education (or equivalent)")
      
    } else if (unit=="regional") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(rural=='Regional office (or equivalent)')
      
    }
    else if (unit=="district") {
      
      stat_df<-get(paste("final_indicator_data_",tag, "_anon", sep="")) %>%
        filter(rural=='District office (or equivalent)')
      
    }
    r <- eval(substitute(variable), stat_df, parent.frame())
    
    #produce weights of 1 for all observations
    stat_df<-stat_df %>%
      mutate(ipw=1)
    
    weights <- stat_df$ipw
    
    wtd.mean(r, weights=weights, na.rm=T)
    
  } 
  
}


#Create a function which will generate new binary variable using case_when, but 
#if value is misisng it will generate binary variable to be missing
#This is done a lot so will create function for it.
#e.g. school_absent=case_when(
#         m2sbq6_efft==6  ~ 1,
#         m2sbq6_efft!=6   ~ 0,
#         is.na(m2sbq6_efft) ~ as.numeric(NA))


bin_var <- function(var, val) {
  case_when(
    var==val  ~ 1,
    var!=val   ~ 0,
    is.na(var) ~ as.numeric(NA))
}


bin_var_NA0 <- function(var, val) {
  case_when(
    var==val  ~ 1,
    var!=val   ~ 0,
    is.na(var) ~ 0)
}

#function to create indicator data for a specified country and year

  #transpose the indicator values dataframe to make it easier to add values
api_template <- api_template %>%
    mutate(value = as.numeric(NA))
  
  indicator_values_transpose <- as.data.frame(t(as.matrix(api_template))) 
  
  colnames(indicator_values_transpose) <- api_template$Series 
  
  indicator_values_transpose <- indicator_values_transpose %>%
    filter(rownames(indicator_values_transpose)=="value")
 
   # In the remainder of this code, we will go through each indicator and align the indicators with the data
  
  ######################################
  #Proficiency by End of Primary (PRIM) TENR PRIM PROE
  ######################################

  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.LPV.PRIM	= g4_prof$SE.LPV.PRIM,
      SE.LPV.PRIM.1	= g4_prof$SE.LPV.PRIM,
      SE.LPV.PRIM.BMP	= 100-g4_prof$SE.LPV.PRIM.LD,
      SE.LPV.PRIM.BMP.1	= 100-g4_prof$SE.LPV.PRIM.LD,
      SE.PRM.PROE =if_else(is.na(uis_df$SDG4.1.1.a.r),uis_df$SDG4.1.1.a.m,uis_df$SDG4.1.1.a.r),
      SE.PRM.PROE.1 =if_else(is.na(uis_df$SDG4.1.1.a.r),uis_df$SDG4.1.1.a.m,uis_df$SDG4.1.1.a.r),
      SE.PRM.TENR	 =100-g4_prof$SE.LPV.PRIM.SD,
      SE.PRM.TENR.1	 =100-g4_prof$SE.LPV.PRIM.SD
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
      SE.PRM.EFFT     = 100-indicator_means(sch_absence_rate, "school", "EFFT",  "All"),
      SE.PRM.EFFT.1   = 100-indicator_means(absence_rate, "school", "EFFT",  "All"),
      SE.PRM.EFFT.1.F = 100-indicator_means(absence_rate, "school", "EFFT",  "Female"),
      SE.PRM.EFFT.1.M = 100-indicator_means(absence_rate, "school", "EFFT",  "Male"),
      SE.PRM.EFFT.1.R = 100-indicator_means(absence_rate, "school", "EFFT",  "Rural"),
      SE.PRM.EFFT.1.U = 100-indicator_means(absence_rate, "school", "EFFT",  "Urban"),
      SE.PRM.EFFT.2   = 100-indicator_means(sch_absence_rate, "school", "EFFT",  "All"),  
      SE.PRM.EFFT.2.F = 100-indicator_means(sch_absence_rate, "school", "EFFT",  "Female"),
      SE.PRM.EFFT.2.M = 100-indicator_means(sch_absence_rate, "school", "EFFT",  "Male"),
      SE.PRM.EFFT.2.R = 100-indicator_means(sch_absence_rate, "school", "EFFT",  "Rural"),
      SE.PRM.EFFT.2.U = 100-indicator_means(sch_absence_rate, "school", "EFFT",  "Urban")

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
  
  # #######################################
  # # Teacher Pedagogical Skills	(PEDG)
  # #######################################
  # 
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      # SE.PRM.PEDG     = indicator_means(teach_prof, "school", "PEDG",  "All"),
      # SE.PRM.PEDG.1   = indicator_means(teach_prof, "school", "PEDG",  "All"),
      #SE.PRM.PEDG.1.F = 100*indicator_means(teach_score>=3, "school", "PEDG",  "Female"),
      #SE.PRM.PEDG.1.M = 100*indicator_means(teach_score>=3, "school", "PEDG",  "Male"),
      # SE.PRM.PEDG.1.R = indicator_means(teach_prof, "school", "PEDG",  "Rural"),
      # SE.PRM.PEDG.1.U = indicator_means(teach_prof, "school", "PEDG",  "Urban"),
      # SE.PRM.PEDG.2   = indicator_means(classroom_culture_prof, "school", "PEDG",  "All"),
      #SE.PRM.PEDG.2.F = 100*indicator_means(classroom_culture>=3, "school", "PEDG",  "Female"),
      #SE.PRM.PEDG.2.M = 100*indicator_means(classroom_culture>=3, "school", "PEDG",  "Male"),
      # SE.PRM.PEDG.2.R = indicator_means(classroom_culture_prof, "school", "PEDG",  "Rural"),
      # SE.PRM.PEDG.2.U = indicator_means(classroom_culture_prof, "school", "PEDG",  "Urban"),
      # SE.PRM.PEDG.3   = indicator_means(instruction_prof, "school", "PEDG",  "All"),
      #SE.PRM.PEDG.3.F = 100*indicator_means(instruction>=3, "school", "PEDG",  "Female"),
      #SE.PRM.PEDG.3.M = 100*indicator_means(instruction>=3, "school", "PEDG",  "Male"),
      # SE.PRM.PEDG.3.R = indicator_means(instruction_prof, "school", "PEDG",  "Rural"),
      # SE.PRM.PEDG.3.U = indicator_means(instruction_prof, "school", "PEDG",  "Urban"),
      # SE.PRM.PEDG.4   = indicator_means(socio_emotional_skills_prof, "school", "PEDG",  "All"),
      #SE.PRM.PEDG.4.F = 100*indicator_means(socio_emotional_skills>=3, "school", "PEDG",  "Female"),
      #SE.PRM.PEDG.4.M = 100*indicator_means(socio_emotional_skills>=3, "school", "PEDG",  "Male"),
      # SE.PRM.PEDG.4.R = indicator_means(socio_emotional_skills_prof, "school", "PEDG",  "Rural"),
      # SE.PRM.PEDG.4.U = indicator_means(socio_emotional_skills_prof, "school", "PEDG",  "Urban")
    )

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
      #(De Facto) Percent of classrooms with a functional blackboard and chalk	
      SE.PRM.INPT.2   =100*indicator_means(blackboard_functional, "school", "INPT",  "All"),
      SE.PRM.INPT.2.R =100*indicator_means(blackboard_functional, "school", "INPT",  "Rural"),
      SE.PRM.INPT.3.U =100*indicator_means(blackboard_functional, "school", "INPT",  "Urban"),
      #(De facto) Percent of classrooms equipped with pens/pencils, textbooks, and exercise books	
      SE.PRM.INPT.3   =33*indicator_means(textbooks, "school", "INPT",  "All") + 67*indicator_means(pens_etc, "school", "INPT",  "All"),
      SE.PRM.INPT.3.R =33*indicator_means(textbooks, "school", "INPT",  "Rural") + 67*indicator_means(pens_etc, "school", "INPT",  "Rural"),
      SE.PRM.INPT.3.U =33*indicator_means(textbooks, "school", "INPT",  "Urban") + 67*indicator_means(pens_etc, "school", "INPT",  "Urban"),

      #(De Facto) Percent of classrooms with basic classroom furniture	
      SE.PRM.INPT.4   =100*indicator_means(share_desk, "school", "INPT",  "All"),
      SE.PRM.INPT.4.R =100*indicator_means(share_desk, "school", "INPT",  "Rural"),
      SE.PRM.INPT.4.U =100*indicator_means(share_desk, "school", "INPT",  "Urban"),
      #(De Facto) Percent of schools with access to EdTech	
      SE.PRM.INPT.5   =100*indicator_means(access_ict, "school", "INPT",  "All"),
      SE.PRM.INPT.5.R =100*indicator_means(access_ict, "school", "INPT",  "Rural"),
      SE.PRM.INPT.5.U =100*indicator_means(access_ict, "school", "INPT",  "Urban"),
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
      SE.PRM.INFR.2   =100*indicator_means(drinking_water	, "school", "INFR",  "All"),
      SE.PRM.INFR.2.R =100*indicator_means(drinking_water	, "school", "INFR",  "Rural"),
      SE.PRM.INFR.2.U =100*indicator_means(drinking_water	, "school", "INFR",  "Urban"),
      #(De Facto) Percent of schools with functioning toilets
      SE.PRM.INFR.3   =100*indicator_means(functioning_toilet	, "school", "INFR",  "All"),
      SE.PRM.INFR.3.R =100*indicator_means(functioning_toilet	, "school", "INFR",  "Rural"),
      SE.PRM.INFR.3.U =100*indicator_means(functioning_toilet	, "school", "INFR",  "Urban"),
      #(De Facto) Percent of schools with access to electricity	
      SE.PRM.INFR.4   =100*indicator_means(class_electricity, "school", "INFR",  "All"),
      SE.PRM.INFR.4.R =100*indicator_means(class_electricity, "school", "INFR",  "Rural"),
      SE.PRM.INFR.4.U =100*indicator_means(class_electricity, "school", "INFR",  "Urban"),
      #(De Facto) Percent of schools with access to internet	
      SE.PRM.INFR.5   =100*indicator_means(internet, "school", "INFR",  "All"),
      SE.PRM.INFR.5.R =100*indicator_means(internet, "school", "INFR",  "Rural"),
      SE.PRM.INFR.5.U =100*indicator_means(internet, "school", "INFR",  "Urban"),
      #	(De Facto) Percent of schools accessible to children with special needs	
      SE.PRM.INFR.6   =100*indicator_means(disability_accessibility, "school", "INFR",  "All"),
      SE.PRM.INFR.6.R =100*indicator_means(disability_accessibility, "school", "INFR",  "Rural"),
      SE.PRM.INFR.6.U =100*indicator_means(disability_accessibility, "school", "INFR",  "Urban"),
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
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.OPMN     = indicator_means(operational_management	, "school", "OPMN",  "All"),
      #(De Facto) Average score for the presence and quality of core operational management functions	
      SE.PRM.OPMN.1   = indicator_means(operational_management	, "school", "OPMN",  "All"),
      SE.PRM.OPMN.1.F = indicator_means(operational_management	, "school", "OPMN",  "Female"),
      SE.PRM.OPMN.1.M = indicator_means(operational_management	, "school", "OPMN",  "Male"),
      SE.PRM.OPMN.1.R = indicator_means(operational_management	, "school", "OPMN",  "Rural"),
      SE.PRM.OPMN.1.U = indicator_means(operational_management	, "school", "OPMN",  "Urban"),
      #(De Facto) Average score for infrastructure repair/maintenance	
      SE.PRM.OPMN.2   = 1+2*indicator_means(vignette_1, "school", "OPMN",  "All"),  
      SE.PRM.OPMN.2.F = 1+2*indicator_means(vignette_1, "school", "OPMN",  "Female"),
      SE.PRM.OPMN.2.M = 1+2*indicator_means(vignette_1, "school", "OPMN",  "Male"),
      SE.PRM.OPMN.2.R = 1+2*indicator_means(vignette_1, "school", "OPMN",  "Rural"),
      SE.PRM.OPMN.2.U = 1+2*indicator_means(vignette_1, "school", "OPMN",  "Urban"),
      #(De Facto) Average score for ensuring  availability of school inputs	
      SE.PRM.OPMN.3   = 1+2*indicator_means(vignette_2, "school", "OPMN",  "All"),
      SE.PRM.OPMN.3.F = 1+2*indicator_means(vignette_2, "school", "OPMN",  "Female"),
      SE.PRM.OPMN.3.M = 1+2*indicator_means(vignette_2, "school", "OPMN",  "Male"),
      SE.PRM.OPMN.3.R = 1+2*indicator_means(vignette_2, "school", "OPMN",  "Rural"),
      SE.PRM.OPMN.3.U = 1+2*indicator_means(vignette_2, "school", "OPMN",  "Urban")

    )
  
  
  #######################################
  # Instructional Leadership	(ILDR)
  #######################################
  
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.ILDR     = indicator_means(instructional_leadership		, "school", "ILDR",  "All"),
      #(De Facto) Average score for the presence and quality of instructional leadership	
      SE.PRM.ILDR.1   = indicator_means(instructional_leadership		, "school", "ILDR",  "All"),
      SE.PRM.ILDR.1.F = indicator_means(instructional_leadership		, "school", "ILDR",  "Female"),
      SE.PRM.ILDR.1.M = indicator_means(instructional_leadership		, "school", "ILDR",  "Male"),
      SE.PRM.ILDR.1.R = indicator_means(instructional_leadership		, "school", "ILDR",  "Rural"),
      SE.PRM.ILDR.1.U = indicator_means(instructional_leadership		, "school", "ILDR",  "Urban"),
      #(De Facto) Percent of teachers reporting having had their class observed	
      SE.PRM.ILDR.2   = 100*indicator_means(classroom_observed, "school", "ILDR",  "All"),  
      SE.PRM.ILDR.2.F = 100*indicator_means(classroom_observed, "school", "ILDR",  "Female"),
      SE.PRM.ILDR.2.M = 100*indicator_means(classroom_observed, "school", "ILDR",  "Male"),
      SE.PRM.ILDR.2.R = 100*indicator_means(classroom_observed, "school", "ILDR",  "Rural"),
      SE.PRM.ILDR.2.U = 100*indicator_means(classroom_observed, "school", "ILDR",  "Urban"),
      #(De Facto) Percent of teachers reporting that the classroom observation happened recently
      SE.PRM.ILDR.3   = 100*indicator_means(classroom_observed_recent, "school", "ILDR",  "All"),
      SE.PRM.ILDR.3.F = 100*indicator_means(classroom_observed_recent, "school", "ILDR",  "Female"),
      SE.PRM.ILDR.3.M = 100*indicator_means(classroom_observed_recent, "school", "ILDR",  "Male"),
      SE.PRM.ILDR.3.R = 100*indicator_means(classroom_observed_recent, "school", "ILDR",  "Rural"),
      SE.PRM.ILDR.3.U = 100*indicator_means(classroom_observed_recent, "school", "ILDR",  "Urban"),
      #(De Facto) Percent of teachers reporting having discussed the results of the classroom observation	
      SE.PRM.ILDR.4   = 100*indicator_means(discussed_observation, "school", "ILDR",  "All"),  
      SE.PRM.ILDR.4.F = 100*indicator_means(discussed_observation, "school", "ILDR",  "Female"),
      SE.PRM.ILDR.4.M = 100*indicator_means(discussed_observation, "school", "ILDR",  "Male"),
      SE.PRM.ILDR.4.R = 100*indicator_means(discussed_observation, "school", "ILDR",  "Rural"),
      SE.PRM.ILDR.4.U = 100*indicator_means(discussed_observation, "school", "ILDR",  "Urban"),
      #(De Facto) Percent of teachers reporting that the discussion was over 30 minutes	
      SE.PRM.ILDR.5   = 100*indicator_means(discussion_30_min, "school", "ILDR",  "All"),  
      SE.PRM.ILDR.5.F = 100*indicator_means(discussion_30_min, "school", "ILDR",  "Female"),
      SE.PRM.ILDR.5.M = 100*indicator_means(discussion_30_min, "school", "ILDR",  "Male"),
      SE.PRM.ILDR.5.R = 100*indicator_means(discussion_30_min, "school", "ILDR",  "Rural"),
      SE.PRM.ILDR.5.U = 100*indicator_means(discussion_30_min, "school", "ILDR",  "Urban"),
      #(De Facto) Percent of teachers reporting that they were provided with feedback in that discussion	
      SE.PRM.ILDR.6   = 100*indicator_means(feedback_observation, "school", "ILDR",  "All"),  
      SE.PRM.ILDR.6.F = 100*indicator_means(feedback_observation, "school", "ILDR",  "Female"),
      SE.PRM.ILDR.6.M = 100*indicator_means(feedback_observation, "school", "ILDR",  "Male"),
      SE.PRM.ILDR.6.R = 100*indicator_means(feedback_observation, "school", "ILDR",  "Rural"),
      SE.PRM.ILDR.6.U = 100*indicator_means(feedback_observation, "school", "ILDR",  "Urban"),
      #(De Facto) Percent of teachers reporting having lesson plans	
      SE.PRM.ILDR.7   = 100-100*indicator_means(lesson_plan, "school", "ILDR",  "All"),  
      SE.PRM.ILDR.7.F = 100-100*indicator_means(lesson_plan, "school", "ILDR",  "Female"),
      SE.PRM.ILDR.7.M = 100-100*indicator_means(lesson_plan, "school", "ILDR",  "Male"),
      SE.PRM.ILDR.7.R = 100-100*indicator_means(lesson_plan, "school", "ILDR",  "Rural"),
      SE.PRM.ILDR.7.U = 100-100*indicator_means(lesson_plan, "school", "ILDR",  "Urban"),
      #(De Facto) Percent of teachers reporting that they had discussed their lesson plans with someone else (pricinpal, pedagogical coordinator, another teacher)	
      SE.PRM.ILDR.8   = 100*indicator_means(m3sdq24_ildr, "school", "ILDR",  "All"),  
      SE.PRM.ILDR.8.F = 100*indicator_means(m3sdq24_ildr, "school", "ILDR",  "Female"),
      SE.PRM.ILDR.8.M = 100*indicator_means(m3sdq24_ildr, "school", "ILDR",  "Male"),
      SE.PRM.ILDR.8.R = 100*indicator_means(m3sdq24_ildr, "school", "ILDR",  "Rural"),
      SE.PRM.ILDR.8.U = 100*indicator_means(m3sdq24_ildr, "school", "ILDR",  "Urban")
      
    )
  #######################################
  # Principal School Knowledge	(PKNW)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.PKNW     = indicator_means(principal_knowledge_score		, "school", "PKNW",  "All"),
      #(De Facto) Average score for the extent to which principals are familiar with certain key aspects of the day-to-day workings of the school		
      SE.PRM.PKNW.1   = indicator_means(principal_knowledge_score		, "school", "PKNW",  "All"),
      SE.PRM.PKNW.1.F = indicator_means(principal_knowledge_score		, "school", "PKNW",  "Female"),
      SE.PRM.PKNW.1.M = indicator_means(principal_knowledge_score		, "school", "PKNW",  "Male"),
      SE.PRM.PKNW.1.R = indicator_means(principal_knowledge_score		, "school", "PKNW",  "Rural"),
      SE.PRM.PKNW.1.U = indicator_means(principal_knowledge_score		, "school", "PKNW",  "Urban"),
      #(De Facto) Percent of principals familiar with teachers' content knowledge	
      SE.PRM.PKNW.2   = 100*indicator_means((add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3, "school", "PKNW",  "All"),  
      SE.PRM.PKNW.2.F = 100*indicator_means((add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3, "school", "PKNW",  "Female"),
      SE.PRM.PKNW.2.M = 100*indicator_means((add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3, "school", "PKNW",  "Male"),
      SE.PRM.PKNW.2.R = 100*indicator_means((add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3, "school", "PKNW",  "Rural"),
      SE.PRM.PKNW.2.U = 100*indicator_means((add_triple_digit_pknw+complete_sentence_pknw+multiply_double_digit_pknw)/3, "school", "PKNW",  "Urban"),
      #(De Facto) Percent of principals familiar with teachers' experience	
      SE.PRM.PKNW.3   = 100*indicator_means(experience_pknw, "school", "PKNW",  "All"),
      SE.PRM.PKNW.3.F = 100*indicator_means(experience_pknw, "school", "PKNW",  "Female"),
      SE.PRM.PKNW.3.M = 100*indicator_means(experience_pknw, "school", "PKNW",  "Male"),
      SE.PRM.PKNW.3.R = 100*indicator_means(experience_pknw, "school", "PKNW",  "Rural"),
      SE.PRM.PKNW.3.U = 100*indicator_means(experience_pknw, "school", "PKNW",  "Urban"),
      #(De Facto) Percent of principals familiar with availability of classroom inputs	
      SE.PRM.PKNW.4   = 100*indicator_means((textbooks_pknw+blackboard_pknw)/2, "school", "PKNW",  "All"),  
      SE.PRM.PKNW.4.F = 100*indicator_means((textbooks_pknw+blackboard_pknw)/2, "school", "PKNW",  "Female"),
      SE.PRM.PKNW.4.M = 100*indicator_means((textbooks_pknw+blackboard_pknw)/2, "school", "PKNW",  "Male"),
      SE.PRM.PKNW.4.R = 100*indicator_means((textbooks_pknw+blackboard_pknw)/2, "school", "PKNW",  "Rural"),
      SE.PRM.PKNW.4.U = 100*indicator_means((textbooks_pknw+blackboard_pknw)/2, "school", "PKNW",  "Urban")
    )
  #######################################
  # Principal Management Skills	(PMAN)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.PMAN     = indicator_means(principal_management		, "school", "PMAN",  "All"),
      #(De Facto) Average score for the extent to which principals master two key managerial skills - problem-solving in the short-term, and goal-setting in the long term	
      SE.PRM.PMAN.1   = indicator_means(principal_management		, "school", "PMAN",  "All"),
      SE.PRM.PMAN.1.F = indicator_means(principal_management		, "school", "PMAN",  "Female"),
      SE.PRM.PMAN.1.M = indicator_means(principal_management		, "school", "PMAN",  "Male"),
      SE.PRM.PMAN.1.R = indicator_means(principal_management		, "school", "PMAN",  "Rural"),
      SE.PRM.PMAN.1.U = indicator_means(principal_management		, "school", "PMAN",  "Urban"),
      #(De Facto) Average score for the extent to which principals master problem-solving in the short-term	
      SE.PRM.PMAN.2   = indicator_means(goal_setting, "school", "PMAN",  "All"),  
      SE.PRM.PMAN.2.F = indicator_means(goal_setting, "school", "PMAN",  "Female"),
      SE.PRM.PMAN.2.M = indicator_means(goal_setting, "school", "PMAN",  "Male"),
      SE.PRM.PMAN.2.R = indicator_means(goal_setting, "school", "PMAN",  "Rural"),
      SE.PRM.PMAN.2.U = indicator_means(goal_setting, "school", "PMAN",  "Urban"),
      #(De Facto) Average score for the extent to which principals master goal-setting in the long term	
      SE.PRM.PMAN.3   = indicator_means(problem_solving, "school", "PMAN",  "All"),
      SE.PRM.PMAN.3.F = indicator_means(problem_solving, "school", "PMAN",  "Female"),
      SE.PRM.PMAN.3.M = indicator_means(problem_solving, "school", "PMAN",  "Male"),
      SE.PRM.PMAN.3.R = indicator_means(problem_solving, "school", "PMAN",  "Rural"),
      SE.PRM.PMAN.3.U = indicator_means(problem_solving, "school", "PMAN",  "Urban"),
    )
  #######################################
  # Policy Lever (Teaching) - Attraction	(TATT)
  #######################################
  #api_final[grep('TATT', api_final$Series),1:2]
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.TATT = indicator_means(teacher_attraction		, "school", "TATT",  "All"),        
      #(De Jure) Average starting public-school teacher salary as percent of GDP per capita	
      SE.PRM.TATT.1 = 100*expert_df$teacher_salary,
      #(De Facto) Percent of teachers reporting being satisfied or very satisfied with their social status in the community	
      SE.PRM.TATT.2 = 100*indicator_means(teacher_satisfied_status		, "school", "TATT",  "All"),   
      #(De Facto) Percent of teachers reporting being satisfied or very satisfied with their job as teacher	
      SE.PRM.TATT.3 = 100*indicator_means(teacher_satisfied_job		, "school", "TATT",  "All"),  
      #(De Facto) Percent of teachers reporting having received financial bonuses in addition to their salaries	
      SE.PRM.TATT.4 = 100*indicator_means(teacher_bonus		, "school", "TATT",  "All"),    
      #(De Facto) Percent of teachers reporting that there are incentives (financial or otherwise) for teachers to teach certain subjects/grades and/or in certain areas	
      SE.PRM.TATT.5 = 100*indicator_means(if_else((teacher_bonus_hard_staff==1 ),1,0	)	, "school", "TATT",  "All"),
      #(De Facto) Percent of teachers that performance matters for promotions	
      SE.PRM.TATT.6  = 100*indicator_means(better_teachers_promoted		, "school", "TATT",  "All"),  
      #(De Jure) Is there a well-established career path for teachers?	
      SE.PRM.TATT.7  = -999     ,
      #(De Facto) Percent of teachers that report salary delays in the past 12 months	
      SE.PRM.TATT.8  = 100*indicator_means(salary_delays		, "school", "TATT",  "All"),  
      #(De Facto) Policy Lever (Teaching) - Attraction	
      SE.PRM.TATT.DF =indicator_means(teacher_attraction		, "school", "TATT",  "All") ,
      #(De Jure) Policy Lever (Teaching) - Attraction	
      SE.PRM.TATT.DJ = expert_df$teacher_attraction 

    )
  #######################################
  # Policy Lever (Teaching) - Selection & Deployment	(TSDP)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
    SE.PRM.TSDP  =  indicator_means(teacher_selection_deployment		, "school", "TSDP",  "All"),
    #Policy Lever (Teaching) - Selection & Deployment                                 
    SE.PRM.TSDP.1  = expert_df$criteria_admittance,
    #(De Jure) Requirements to enter into initial education programs                  
    SE.PRM.TSDP.2  =  -999,
    #(De Facto) Average quality of applicants accepted into initial education programs
    SE.PRM.TSDP.3  = expert_df$criteria_become,
    #(De Jure) Requirements to become a primary school teacher                        
    SE.PRM.TSDP.4  = 1+2*indicator_means(teacher_selection		, "school", "TSDP",  "All"),
    #(De Facto) Requirements to become a primary school teacher                       
    SE.PRM.TSDP.5 =expert_df$criteria_transfer,
    #(De Jure) Requirements to fulfill a transfer request                             
    SE.PRM.TSDP.6  = 1+2*indicator_means(teacher_deployment		, "school", "TSDP",  "All"),
    #(De Facto) Requirements to fulfill a transfer request                            
    SE.PRM.TSDP.7  = -999,
    #(De Jure) Selectivity of teacher hiring process                                  
    SE.PRM.TSDP.DF =indicator_means(teacher_selection_deployment		, "school", "TSDP",  "All"),
    #(De Facto) Policy Lever (Teaching) - Selection & Deployment                      
    SE.PRM.TSDP.DJ =expert_df$teacher_selection_deployment
    #(De Jure) Policy Lever (Teaching) - Selection & Deployment   
    )
  #######################################
  # Policy Lever (Teaching) - Support	(TSUP)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
  
  SE.PRM.TSUP =indicator_means(teacher_support		, "school", "TSUP",  "All"),   
  #Policy Lever (Teaching) - Support                                                                                        
  SE.PRM.TSUP.1  =expert_df$practicum ,
  #(De Jure) Practicum required as part of pre-service training                                                             
  SE.PRM.TSUP.2 = 100-100*indicator_means(m3sdq6_tsup-1			, "school", "TSUP",  "All"),
  #(De Facto) Percent reporting they completed a practicum as part of pre-service training                                  
  SE.PRM.TSUP.3  = 100*indicator_means(m3sdq3_tsup		, "school", "TSUP",  "All"),
  #(De Facto) Percent of teachers reporting that they participated in an induction and/or mentorship program                
  SE.PRM.TSUP.4  =expert_df$prof_development,
  #(De Jure) Participation in professional development has professional implications for teachers                           
  SE.PRM.TSUP.5  = 100*indicator_means(m3sdq9_tsup			, "school", "TSUP",  "All"),
  #(De Facto) Percent of teachers reporting having attended in-service trainings in the past 12 months                      
  SE.PRM.TSUP.6  =indicator_means(m3sdq10_tsup			, "school", "TSUP",  "All"),
  #(De Facto) Average length of the trainings attended                                                                      
  SE.PRM.TSUP.7  =indicator_means(m3sdq11_tsup			, "school", "TSUP",  "All"),
  #(De Facto) Average span of time (in weeks) of those trainings                                                            
  SE.PRM.TSUP.8  =100*indicator_means((m3sdq13_tsup-1)/4			, "school", "TSUP",  "All"),
  #(De Facto) Average percent of time spent inside the classrooms during the trainings                                      
  SE.PRM.TSUP.9 = 100*indicator_means(opportunities_teachers_share			, "school", "TSUP",  "All"), 
  #(De Facto) Percent of teachers that report having opportunities to come together with other teachers to discuss ways of ~
  SE.PRM.TSUP.DF =indicator_means(teacher_support		, "school", "TSUP",  "All"),  
  #(De Facto) Policy Lever (Teaching) - Support                                                                             
  SE.PRM.TSUP.DJ =expert_df$teacher_support
  #(De Jure) Policy Lever (Teaching) - Support  
)
  #######################################
  # Policy Lever (Teaching) - Evaluation	(TEVL)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.TEVL  = indicator_means(teaching_evaluation		, "school", "TEVL",  "All"),     #Policy Lever (Teaching) - Evaluation                                                                                     
      SE.PRM.TEVL.1 =expert_df$evaluation_law, #(De Jure) Legislation assigns responsibility of evaluating the performance of teachers to a public authority (national)
      SE.PRM.TEVL.2 =expert_df$evaluation_law_school, #(De Jure) Legislation assigns responsibility of evaluating the performance of teachers to the schools                    
      SE.PRM.TEVL.3 = 100*indicator_means(formally_evaluated		, "school", "TEVL",  "All"),   #(De Facto) Percent of teachers that report being evaluated in the past 12 months                                         
      SE.PRM.TEVL.4 =expert_df$evaluation_criteria, #(De Jure) The criteria to evaluate teachers is clear                                                                     
      SE.PRM.TEVL.5 = indicator_means(m3sbq8_tmna__1	+m3sbq8_tmna__2 + m3sbq8_tmna__3 + m3sbq8_tmna__4 + m3sbq8_tmna__5 + m3sbq8_tmna__6 + m3sbq8_tmna__7 + m3sbq8_tmna__8 + m3sbq8_tmna__97		, "school", "TEVL",  "All"),  #(De Facto) Number of criteria used to evaluate teachers                                                                  
      SE.PRM.TEVL.6 = 100*indicator_means(negative_consequences		, "school", "TEVL",  "All"),  #(De Facto) Percent of teachers that report there would be consequences after two negative evaluations                    
      SE.PRM.TEVL.7 = 100*indicator_means(positive_consequences		, "school", "TEVL",  "All"),  #(De Facto) Percent of teachers that report there would be consequences after two positive evaluations                    
      SE.PRM.TEVL.8 =expert_df$negative_evaluations, #(De Jure) There are clear consequences for teachers who receive two or more negative evaluations                         
      SE.PRM.TEVL.9 =expert_df$positive_evaluations, #(De Jure) There are clear consequences for teachers who receive two or more positive evaluations                         
      SE.PRM.TEVL.DF = indicator_means(teaching_evaluation		, "school", "TEVL",  "All"),  #(De Facto) Policy Lever (Teaching) - Evaluation                                                                          
      SE.PRM.TEVL.DJ =expert_df$teaching_evaluation#(De Jure) Policy Lever (Teaching) - Evaluation 
    )
  #######################################
  # Policy Lever (Teaching) - Monitoring & Accountability 	(TMNA)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.TMNA = indicator_means(teacher_monitoring		, "school", "TMNA",  "All"),    #Policy Lever (Teaching) - Monitoring & Accountability                                                       
      SE.PRM.TMNA.1  = expert_df$absence_collected, #(De Jure) Information on teacher presence/absenteeism is being collected on a regular basis                 
      SE.PRM.TMNA.2  = expert_df$attendance_rewarded, #(De Jure) Teachers receive monetary compensation for being present                                          
      SE.PRM.TMNA.3 = 100*indicator_means(attendance_rewarded		, "school", "TMNA",  "All"),   #(De Facto) Teacher report receiving monetary compensation (aside from salary) for being present             
      SE.PRM.TMNA.4 = 100*indicator_means(miss_class_admin		, "school", "TMNA",  "All"),   #(De Facto) Percent of teachers that report having been absent because of administrative processes           
      SE.PRM.TMNA.5 = 100*indicator_means(attendence_sanctions		, "school", "TMNA",  "All"),  #(De Facto) Percent of teachers that report that there would be consequences for being absent 40% of the time
      SE.PRM.TMNA.DF = indicator_means(teacher_monitoring		, "school", "TMNA",  "All"),  #(De Facto) Policy Lever (Teaching) - Monitoring & Accountability                                            
      SE.PRM.TMNA.DJ =expert_df$teacher_monitoring #(De Jure) Policy Lever (Teaching) - Monitoring & Accountability
    )
  #######################################
  # Policy Lever (Teaching) - Intrinsic Motivation 	(TINM)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.TINM = indicator_means(intrinsic_motivation		, "school", "TINM",  "All"),    #Policy Lever (Teaching) - Intrinsic Motivation                                                                           
      SE.PRM.TINM.1 = indicator_means(SE_PRM_TINM_1		, "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if the ~
      SE.PRM.TINM.10 =indicator_means(SE_PRM_TINM_10		, "school", "TINM",  "All"), #(De Facto) Percent of teachers that agree or strongly agrees with \"Students can change even their basic intelligence l~
      SE.PRM.TINM.11 = indicator_means(motivation_teaching		, "school", "TINM",  "All"), #(De Facto) Percent of teachers who state that intrinsic motivation was the main reason to become teachers                
      SE.PRM.TINM.12 = indicator_means(m3sdq2_tmna		, "school", "TMNA",  "All"), #(De Facto) New teachers are required to undergo a probationary period                                                    
      SE.PRM.TINM.13 = expert_df$probationary_period, #(De Jure) New teachers are required to undergo a probationary period                                                     
      SE.PRM.TINM.2 = indicator_means(SE_PRM_TINM_2		, "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if stud~
      SE.PRM.TINM.3 = indicator_means(SE_PRM_TINM_3		, "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if the ~
      SE.PRM.TINM.4 = indicator_means(SE_PRM_TINM_4		, "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they attend scho~
      SE.PRM.TINM.5 = indicator_means(SE_PRM_TINM_5		, "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they come to sch~
      SE.PRM.TINM.6 = indicator_means(SE_PRM_TINM_6		, "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they are motivat~
      SE.PRM.TINM.7 = indicator_means(SE_PRM_TINM_7		, "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students have a certain amount of intelligence and ~
      SE.PRM.TINM.8 = indicator_means(SE_PRM_TINM_8		, "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with To be honest, students can't really change how inte~
      SE.PRM.TINM.9 = indicator_means(SE_PRM_TINM_9		, "school", "TINM",  "All"),  #(De Facto) Percent of teachers that agree or strongly agrees with Students can always substantially change how intell~
      SE.PRM.TINM.DF = indicator_means(intrinsic_motivation		, "school", "TINM",  "All"), #(De Facto) Policy Lever (Teaching) - Intrinsic Motivation                                                                
      SE.PRM.TINM.DJ = expert_df$intrinsic_motivation #(De Jure) Policy Lever (Teaching) - Intrinsic Motivation   
    )
  #######################################
  # Policy Lever (Inputs & Infrastructure) - Standards 	(ISTD)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.ISTD  =  indicator_means(standards_monitoring		, "school", "ISTD",  "All"), #Policy Lever (Inputs & Infrastructure) - Standards                                                                       
      SE.PRM.ISTD.1  =expert_df$textbook_policy, #(De Jure) Is there a policy in place to require that students have access to the prescribed textbooks?                   
      SE.PRM.ISTD.10 =  100*indicator_means(m1scq14_imon__4		, "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that schools have access to drinking water?              
      SE.PRM.ISTD.11 =expert_df$toilet_policy, #(De Jure) Is there a policy in place to require that schools have functioning toilets?                                   
      SE.PRM.ISTD.12 =  100*indicator_means(m1scq14_imon__1		, "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that schools have functioning toilets?                   
      SE.PRM.ISTD.13 =expert_df$disability_policy, #(De Jure) Is there a policy in place to require that schools are accessible to children with special needs?              
      SE.PRM.ISTD.14 =  100*indicator_means(m1scq14_imon__3		, "school", "ISTD",  "All"),#(De Facto) Do you know if there is there a policy in place to require that schools are accessible to children with speci~
      SE.PRM.ISTD.2  =  100*indicator_means(m1scq13_imon__2		, "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that students have access to the prescribed textbooks?   
      SE.PRM.ISTD.3  =expert_df$connectivity_program, #(De Jure) Is there a national connectivity program?                                                                      
      SE.PRM.ISTD.4  =  -999,#(De Facto) Do you know if there is a national connectivity program?                                                      
      SE.PRM.ISTD.5  =expert_df$materials_policy, #(De Jure) Is there a policy in place to require that students have access to PCs, laptops, tablets, and/or other computi~
      SE.PRM.ISTD.6  =  100*indicator_means(m1scq13_imon__5		, "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that students have access to PCs, laptops, tablets, and/~
      SE.PRM.ISTD.7  =expert_df$electricity_policy, #(De Jure) Is there a policy in place to require that schools have access to electricity?                                 
      SE.PRM.ISTD.8  =  100*indicator_means(m1scq14_imon__2		, "school", "ISTD",  "All"),#(De Facto) Do you know if there is a policy in place to require that schools have access to electricity?                 
      SE.PRM.ISTD.9  =expert_df$water_policy, #(De Jure) Is there a policy in place to require that schools have access to drinking water?                              
      SE.PRM.ISTD.DF =  indicator_means(standards_monitoring		, "school", "ISTD",  "All"),#(De Facto) Policy Lever (Inputs & Infrastructure) - Standards                                                            
      SE.PRM.ISTD.DJ =expert_df$inputs_standards #(De Jure) Policy Lever (Inputs & Infrastructure) - Standards    
    )
  #######################################
  # Policy Lever (Inputs & Infrastructure) - Monitoring 	(IMON)
  #######################################
  
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.IMON  =  indicator_means(sch_monitoring		, "school", "IMON",  "All"),    #Policy Lever (Inputs & Infrastructure) - Monitoring                                                                      
      SE.PRM.IMON.1  =  indicator_means(m1scq1_imon		, "school", "IMON",  "All"),  #(De Facto) Percent of schools that report there is someone monitoring that basic inputs are available to students        
      SE.PRM.IMON.10 =-999, #(De Jure) Number of basic infrastructure features clearly articulated as needing to be monitored                         
      SE.PRM.IMON.2  =  indicator_means(parents_involved		, "school", "IMON",  "All"),  #(De Facto) Percent of schools that report that parents or community members are involved in the monitoring of availabili~
      SE.PRM.IMON.3  =  indicator_means(m1scq5_imon		, "school", "IMON",  "All"),  #(De Facto) Percent of schools that report that there is an inventory to monitor availability of basic inputs             
      SE.PRM.IMON.4  =  indicator_means(m1scq7_imon		, "school", "IMON",  "All"),  #(De Facto) Percent of schools that report there is someone monitoring that basic infrastructure is available             
      SE.PRM.IMON.5  =  indicator_means(bin_var(m1scq10_imon,1)		, "school", "IMON",  "All"),  #(De Facto) Percent of schools that report that parents or community members are involved in the monitoring of availabili~
      SE.PRM.IMON.6  =  indicator_means(m1scq11_imon		, "school", "IMON",  "All"),  #(De Facto) Percent of schools that report that there is an inventory to monitor availability of basic infrastructure     
      SE.PRM.IMON.7  =-999, #(De Jure) Is the responsibility of monitoring basic inputs clearly articulated in the policies?                          
      SE.PRM.IMON.8  =-999, #(De Jure) Number of basic inputs clearly articulated as needing to be monitored                                          
      SE.PRM.IMON.9  =-999, #(De Jure) Is the responsibility of monitoring basic infrastructure clearly articulated in the policies?                  
      SE.PRM.IMON.DF  =  indicator_means(sch_monitoring		, "school", "IMON",  "All"), #(De Facto) Policy Lever (Inputs & Infrastructure) - Monitoring                                                           
      SE.PRM.IMON.DJ =-999, #(De Jure) Policy Lever (Inputs & Infrastructure) - Monitoring  
    )
  #######################################
  # Policy Lever (Learners) - Nutrition Programs 	(LNTN)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.LNTN.1 =expert_df$iodization,   #(De Jure) Does a national policy to encourage salt iodization exist?
      SE.PRM.LNTN.2 =100*as.numeric(defacto_dta_learners_final$`Percentage of households with salt testing positive for any iodide among households`), #(De Facto) Percent of households with salt testing positive for any iodide among households
      SE.PRM.LNTN.3 =expert_df$iron_fortification, #(De Jure) Does a national policy exist to encourage iron fortification of staples like wheat, maize, or rice?
      SE.PRM.LNTN.4  =100*as.numeric(defacto_dta_learners_final[,2]), #(De Facto) Percent of children age 6-23 months who had at least the minimum dietary diversity and the minimum meal frequ~
      SE.PRM.LNTN.5  =expert_df$breastfeeding, #(De Jure) Does a national policy exist to encourage breastfeeding?
      SE.PRM.LNTN.6  =100*as.numeric(defacto_dta_learners_final$`Percentage of children born in the five (three) years preceding the survey who were ever breastfed`), #(De Facto) Percent of children born in the five (three) years preceding the survey who were ever breastfed
      SE.PRM.LNTN.7  =expert_df$school_feeding, #(De Jure) Is there a publicly funded school feeding program?
      SE.PRM.LNTN.8  =100*indicator_means(m1saq9_lnut, "school", "school_dta_anon",  "Custom") #(De Facto) Percent of schools reporting having publicly funded school feeding program
    ) %>%
    mutate(
      SE.PRM.LNTN.DF =4*(SE.PRM.LNTN.2+SE.PRM.LNTN.4+SE.PRM.LNTN.6 +SE.PRM.LNTN.8)/400+1,#(De Facto) Policy Lever (Learners) - Nutrition Programs
      SE.PRM.LNTN.DJ =expert_df$nutrition_programs#(De Jure) Policy Lever (Learners) - Nutrition Programs
    ) %>%
    mutate(
      SE.PRM.LNTN =SE.PRM.LNTN.DF   #Policy Lever (Learners) - Nutrition Programs
      
    )
  #######################################
  # Policy Lever (Learners) - Health 	(LHTH)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.LHTH.1 =expert_df$immunization, #(De Jure) Are young children required to receive a complete course of childhood immunizations?
      SE.PRM.LHTH.2 = 100*as.numeric(defacto_dta_learners_final[,5]), #(De Facto) Percent of children who at age 24-35 months had received all vaccinations recommended in the national immuniz~
      SE.PRM.LHTH.3 =expert_df$healthcare_young_children, #(De Jure) Is there a policy that assures access to healthcare for young children? Either by offering these services free~
      SE.PRM.LHTH.4 =100*as.numeric(defacto_dta_learners_final$`MICS/Other - Percentage of children under 5 covered by health insurance`), #(De Facto) Percent of  children under 5 covered by health insurance
      SE.PRM.LHTH.5 =expert_df$deworming, #(De Jure) Are deworming pills funded and distributed by the government?
      SE.PRM.LHTH.6 =100*as.numeric(defacto_dta_learners_final$`MICS/Other - Percentage of children age 6-59 months who received deworming medication.`), #(De Facto) Percent of children age 6-59 months who received deworming medication
      SE.PRM.LHTH.7 =expert_df$antenatal_skilled_delivery, #(De Jure) Is there a policy that guarantees pregnant women free antenatal visits and skilled delivery?
      SE.PRM.LHTH.8 =100*as.numeric(defacto_dta_learners_final$`MICS/DHS - Percentage of women age 15-49 years with a live birth in the last 2 years whose most recent live birth was delivered in a health facility`) #(De Facto) Percent of women age 15-49 years with a live birth in the last 2 years whose most recent live birth was deliv~
    ) %>%
    mutate(
      SE.PRM.LHTH.DF =4*(SE.PRM.LHTH.2+SE.PRM.LHTH.3+ SE.PRM.LHTH.8)/300+1,#(De Facto) Policy Lever (Learners) - Health
      SE.PRM.LHTH.DJ =expert_df$health_programs#(De Jure) Policy Lever (Learners) - Health
    ) %>%
    mutate(
      SE.PRM.LHTH =SE.PRM.LHTH.DF   #Policy Lever (Learners) - Health
      
    )
  #######################################
  # Policy Lever (Learners) - Center-Based Care 	(LCBC)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.LCBC.1  =expert_df$pre_primary_free_some,#(De Jure) Is there a policy that guarantees free education for some or all grades and ages included in pre-primary educat~
      SE.PRM.LCBC.2  =100*(as.numeric(defacto_dta_learners_final$`Percentage of children age 36-59 months who are attending ECE`)),#(De Facto) Percent of children age 36-59 months who are attending an early childhood education programme
      SE.PRM.LCBC.3  =expert_df$developmental_standards,#(De Jure) Are there developmental standards established for early childhood care and education?
      SE.PRM.LCBC.4  =expert_df$ece_qualifications,#(De Jure) According to laws and regulations, are there requirement to become an early childhood educator, pre-primary tea~
      SE.PRM.LCBC.5  =expert_df$ece_in_service#(De Jure) According to policy, are ECCE professionals working at public or private centers required to complete in-servic~
    ) %>%
    mutate(
      SE.PRM.LCBC.DF =4*(SE.PRM.LCBC.2)/100+1,#(De Facto) Policy Lever (Learners) - Center-Based Care
      SE.PRM.LCBC.DJ =expert_df$ece_programs#(De Jure) Policy Lever (Learners) - Center-Based Care
    ) %>%
    mutate(
      SE.PRM.LCBC = SE.PRM.LCBC.DF  #Policy Lever (Learners) - Center-Based Care
    )
  #######################################
  # Policy Lever (Learners) - Caregiver Capacity - Financial Capacity 	(LFCP)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.LFCP.1 = expert_df$anti_poverty,  #(De Jure) Are anti poverty interventions that focus on ECD publicly supported?
      SE.PRM.LFCP.2 =-999, #(De Jure) Are cash transfers conditional on ECD services/enrollment publicly supported?
      SE.PRM.LFCP.3 =-999, #(De Jure) Are cash transfers focused partially on ECD publicly supported?
      SE.PRM.LFCP.4 =100*as.numeric(defacto_dta_learners_final$`Coverage of social protection programs (Best data source to be identified)`) #(De Facto) Coverage of social protection programs
    ) %>%
    mutate(
      SE.PRM.LFCP.DF=4*SE.PRM.LFCP.4/100 +1, #(De Facto) Policy Lever (Learners) - Caregiver Capacity - Financial Capacity
      SE.PRM.LFCP.DJ=expert_df$financial_capacity #(De Jure) Policy Lever (Learners) - Caregiver Capacity - Financial Capacity
    ) %>%
    mutate(
      SE.PRM.LFCP=SE.PRM.LFCP.DF    #Policy Lever (Learners) - Caregiver Capacity - Financial Capacity
      
    )
  #######################################
  # Policy Lever (Learners) - Caregiver Capacity - Skills Capacity 	(LSKC)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.LSKC.1 =expert_df$good_parent_sharing, #(De Jure) Does the government offer programs that aim to share good parenting practices with caregivers?
      SE.PRM.LSKC.2 =expert_df$promote_ece_stimulation, #(De Jure) Are any of the following publicly-supported delivery channels used to reach families in order to promote early ~
      SE.PRM.LSKC.3 =-999, #(De Facto) Percent of children under age 5 who have three or more children's books
      SE.PRM.LSKC.4 =100*as.numeric(defacto_dta_learners_final$`Percentage of children age 24-59 months engaged in four or more activities to provide early stimulation and responsive care in the last 3 days with any adult in the household`) #(De Facto) Percent of children age 24-59 months engaged in four or more activities to provide early stimulation and respo~
    ) %>%
    mutate(
      SE.PRM.LSKC.DF = 4*(SE.PRM.LSKC.4)/100+1,#(De Facto) Policy Lever (Learners) - Caregiver Capacity - Skills Capacity
      SE.PRM.LSKC.DJ =expert_df$caregiver_skills
    ) %>%
    mutate(#(De Jure) Policy Lever (Learners) - Caregiver Capacity - Skills Capacity
      SE.PRM.LSKC =  SE.PRM.LSKC.DF  #Policy Lever (Learners) - Caregiver Capacity - Skills Capacity
    )
  #######################################
  # Policy Lever (School Management) - Clarity of Functions 	(SCFN)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.SCFN   = indicator_means(sch_management_clarity		, "school", "SCFN",  "All"),  #Policy Lever (School Management) - Clarity of Functions                                                                  
      SE.PRM.SCFN.1 = 100*indicator_means(infrastructure_scfn		, "school", "SCFN",  "All"),  #(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of the maintenance~
      SE.PRM.SCFN.10 =expert_df$student_scfn, #(De Jure) Do the policies governing schools assign the responsibility of student learning assessments?                   
      SE.PRM.SCFN.11 = 100*indicator_means(principal_hiring_scfn		, "school", "SCFN",  "All"), #(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of principal hirin~
      SE.PRM.SCFN.12 =expert_df$principal_hiring_scfn,#(De Jure) Do the policies governing schools assign the responsibility of principal hiring and assignment?                
      SE.PRM.SCFN.13 = 100*indicator_means(principal_supervision_scfn		, "school", "SCFN",  "All"),#(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of principal super~
      SE.PRM.SCFN.14 =expert_df$principal_supervision_scfn, #(De Jure) Do the policies governing schools assign the responsibility of principal supervision and training?             
      SE.PRM.SCFN.2  =expert_df$infrastructure_scfn, #(De Jure) Do the policies governing schools assign the responsibility of maintenance and expansion of school infrastruct~
      SE.PRM.SCFN.3  = 100*indicator_means(materials_scfn		, "school", "SCFN",  "All"), #(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of the procurement~
      SE.PRM.SCFN.4  =expert_df$materials_scfn,#(De Jure) Do the policies governing schools assign the responsibility of procurement of materials?                       
      SE.PRM.SCFN.5  = 100*indicator_means(hiring_scfn		, "school", "SCFN",  "All"),#(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of teacher hiring ~
      SE.PRM.SCFN.6  =expert_df$hiring_scfn, #(De Jure) Do the policies governing schools assign the responsibility of teacher hiring and assignment?                  
      SE.PRM.SCFN.7  = 100*indicator_means(supervision_scfn		, "school", "SCFN",  "All"),#(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of teacher supervi~
      SE.PRM.SCFN.8  =expert_df$supervision_scfn, #(De Jure) Do the policies governing schools assign the responsibility of teacher supervision, training, and coaching?    
      SE.PRM.SCFN.9  = 100*indicator_means(student_scfn		, "school", "SCFN",  "All"),#(De Facto) Do you know if the policies governing schools assign responsibility for the implementation of student learnin~
      SE.PRM.SCFN.DF = indicator_means(sch_management_clarity		, "school", "SCFN",  "All"),#(De Facto) Policy Lever (School Management) - Clarity of Functions                                                       
      SE.PRM.SCFN.DJ =expert_df$sch_management_clarity#(De Jure) Policy Lever (School Management) - Clarity of Functions    
    )
  #######################################
  # Policy Lever (School Management) - Attraction 	(SATT)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.SATT  = indicator_means(sch_management_attraction		, "school", "SATT",  "All"),  #Policy Lever (School Management) - Attraction                                                                             
      SE.PRM.SATT.1  =expert_df$professionalized, #(De Jure) Do the national policies governing the education system portray the position of principal or head teacher as pr~
      SE.PRM.SATT.2 = indicator_means(principal_salary		, "school", "SATT",  "All"),  #(De Facto) Average principal salary as percent of GDP per capita                                                          
      SE.PRM.SATT.3  = indicator_means(principal_satisfaction		, "school", "SATT",  "All"),#(De Facto) Percent of principals reporting being satisfied or very satisfied with their social status in the community    
      SE.PRM.SATT.DF = indicator_means(sch_management_attraction		, "school", "SATT",  "All"), #(De Facto) Policy Lever (School Management) - Attraction                                                                  
      SE.PRM.SATT.DJ =expert_df$sch_management_attraction#(De Jure) Policy Lever (School Management) - Attraction  
    )
  #######################################
  # Policy Lever (School Management) - Selection & Deployment 	(SSLD)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.SSLD  =  indicator_means(sch_selection_deployment		, "school", "SSLD",  "All"),#Policy Lever (School Management) - Selection & Deployment                                                                
      SE.PRM.SSLD.1  =expert_df$principal_rubric,#(De Jure) Is there a systematic approach/rubric for the selection of principals?                                         
      SE.PRM.SSLD.10  =  100*indicator_means(m7sgq2_ssld==1		, "school", "SSLD",  "All"), #(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is yea~
      SE.PRM.SSLD.11 =  100*indicator_means(m7sgq2_ssld==2		, "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is qua~
      SE.PRM.SSLD.12 =  100*indicator_means(m7sgq2_ssld==3		, "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is dem~
      SE.PRM.SSLD.13 =  100*indicator_means(m7sgq2_ssld==4		, "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is hav~
      SE.PRM.SSLD.14 =  100*indicator_means(m7sgq2_ssld==6		, "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is pol~
      SE.PRM.SSLD.15 =  100*indicator_means(m7sgq2_ssld==7		, "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is eth~
      SE.PRM.SSLD.16 =  100*indicator_means(m7sgq2_ssld==8		, "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the most important factor considered when selecting a principal is kno~
      SE.PRM.SSLD.2  =expert_df$principal_factors,#(De Jure) How are the principals selected? Based on the requirements, is the selection system meritocratic?              
      SE.PRM.SSLD.3  =  100*indicator_means(m7sgq1_ssld__1		, "school", "SSLD",  "All"), #(De Facto) Percent of principals that report that the factors considered when selecting a principal include years of exp~
      SE.PRM.SSLD.4  =  100*indicator_means(m7sgq1_ssld__2		, "school", "SSLD",  "All"),#(De Facto)  Percent of principals that report that the factors considered when selecting a principal include quality of ~
      SE.PRM.SSLD.5  =  100*indicator_means(m7sgq1_ssld__3		, "school", "SSLD",  "All"), #(De Facto) Percent of principals that report that the factors considered when selecting a principal include demonstrated~
      SE.PRM.SSLD.6  =  100*indicator_means(m7sgq1_ssld__4		, "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the factors considered when selecting a principal include good relatio~
      SE.PRM.SSLD.7  =  100*indicator_means(m7sgq1_ssld__6		, "school", "SSLD",  "All"),#(De Facto) Percent of principals that report that the factors considered when selecting a principal include political af~
      SE.PRM.SSLD.8  =  -999,#(De Facto) Percent of principals that report that the factors considered when selecting a principal include ethnic group 
      SE.PRM.SSLD.9  =  100*indicator_means(m7sgq1_ssld__8		, "school", "SSLD",  "All"), #(De Facto) Percent of principals that report that the factors considered when selecting a principal include knowledge of~
      SE.PRM.SSLD.DF =  indicator_means(sch_selection_deployment		, "school", "SSLD",  "All"), #(De Facto) Policy Lever (School Management) - Selection & Deployment                                                     
      SE.PRM.SSLD.DJ =expert_df$sch_selection_deployment #(De Jure) Policy Lever (School Management) - Selection & Deployment  
    )
  #######################################
  # Policy Lever (School Management) - Support 	(SSUP)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.SSUP    =  indicator_means(sch_support		, "school", "SSUP",  "All"), #Policy Lever (School Management) - Support                                                                        
      SE.PRM.SSUP.1  =expert_df$principal_training_required,#(De Jure) Are principals required to have training on how to manage a school?                                     
      SE.PRM.SSUP.10 =  100*indicator_means(m7sgq5_ssup		, "school", "SSUP",  "All"),#(De Facto) Percent of principals that report having used the skills they gained at the last training they attended
      SE.PRM.SSUP.11 =  indicator_means(if_else(m7sgq7_ssup!=98,m7sgq7_ssup, as.numeric(NA)) 		, "school", "SSUP",  "All"),#(De Facto) Average number of trainings that principals report having been offered to them in the past year        
      SE.PRM.SSUP.2  =expert_df$principal_training_type1,#(De Jure) Are principals required to have management training for new principals?                                 
      SE.PRM.SSUP.3  =expert_df$principal_training_type2,#(De Jure) Are principals required to have in-service training?                                                    
      SE.PRM.SSUP.4  =expert_df$principal_training_type3,#(De Jure) Are principals required to have mentoring/coaching by experienced principals?                           
      SE.PRM.SSUP.5  =expert_df$principal_training_frequency,#(De Jure) How many times per year do principals have trainings?                                                   
      SE.PRM.SSUP.6  =  100*indicator_means(m7sgq3_ssup		, "school", "SSUP",  "All"), #(De Facto) Percent of principals that report ever having received formal training                                 
      SE.PRM.SSUP.7  =  100*indicator_means(m7sgq4_ssup__1		, "school", "SSUP",  "All"),#(De Facto) Percent of principals that report having received management training for new principals               
      SE.PRM.SSUP.8  =  100*indicator_means(m7sgq4_ssup__2		, "school", "SSUP",  "All"), #(De Facto) Percent of principals that report having received in-service training                                  
      SE.PRM.SSUP.9  =  100*indicator_means(m7sgq4_ssup__3		, "school", "SSUP",  "All"),#(De Facto) Percent of principals that report having received mentoring/coaching by experienced principals         
      SE.PRM.SSUP.DF   =  indicator_means(sch_support		, "school", "SSUP",  "All"),  #(De Facto) Policy Lever (School Management) - Support                                                             
      SE.PRM.SSUP.DJ =expert_df$sch_support#(De Jure) Policy Lever (School Management) - Support 
    )
  #######################################
  # Policy Lever (School Management) - Evaluation 	(SEVL)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.SEVL   =  indicator_means(principal_evaluation		, "school", "SEVL",  "All"), #Policy Lever (School Management) - Evaluation                                                          
      SE.PRM.SEVL.1  =expert_df$principal_monitor_law,#(De Jure) Is there a policy that specifies the need to monitor principal or head teacher performance?  
      SE.PRM.SEVL.2  =expert_df$principal_monitor_criteria,#(De Jure) Is the criteria to evaluate principals clear and includes multiple factors?                  
      SE.PRM.SEVL.3 =  100*indicator_means(m7sgq8_sevl		, "school", "SEVL",  "All"), #(De Facto) Percent of principals that report having been evaluated  during the last school year        
      SE.PRM.SEVL.4 =  100*indicator_means(principal_eval_tot>1		, "school", "SEVL",  "All"), #(De Facto) Percent of principals that report having been evaluated on multiple factors                 
      SE.PRM.SEVL.5 =  100*indicator_means(principal_negative_consequences	, "school", "SEVL",  "All"), #(De Facto) Percent of principals that report there would be consequences after two negative evaluations
      SE.PRM.SEVL.6  =100*indicator_means(principal_positive_consequences	, "school", "SEVL",  "All"),#(De Facto) Percent of principals that report there would be consequences after two positive evaluations
      SE.PRM.SEVL.DF =  indicator_means(principal_evaluation		, "school", "SEVL",  "All"), #(De Facto) Policy Lever (School Management) - Evaluation                                               
      SE.PRM.SEVL.DJ =expert_df$principal_evaluation #(De Jure) Policy Lever (School Management) - Evaluation     
    )
  #######################################
  # Politics & Bureaucratic Capacity - Quality of Bureaucracy 	(BQBR)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.BQBR   =  indicator_means(quality_bureaucracy		, "public_officials", "BQBR",  "All"),#Politics & Bureaucratic Capacity - Quality of Bureaucracy
      SE.PRM.BQBR.1 =  indicator_means(quality_bureaucracy		, "public_officials", "BQBR",  "All"),#Average score for Quality of Bureaucracy; where a score of 1 indicates low effectiveness and 5 indicates high effectiveness
      SE.PRM.BQBR.2 =  indicator_means(knowledge_skills		, "public_officials", "BQBR",  "All"),#(Quality of Bureaucracy) average score for knowledge and skills
      SE.PRM.BQBR.3 =  indicator_means(work_environment		, "public_officials", "BQBR",  "All"),#(Quality of Bureaucracy) average score for work environment
      SE.PRM.BQBR.4 =  indicator_means(merit		, "public_officials", "BQBR",  "All"),#(Quality of Bureaucracy) average score for merit
      SE.PRM.BQBR.5=  indicator_means(motivation_attitudes		, "public_officials", "BQBR",  "All")#Quality of Bureaucracy) average score for motivation and attitudes
    )
  #######################################
  # Politics & Bureaucratic Capacity - Impartial Decision-Making 	(BIMP)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.BIMP  =  indicator_means(impartial_decision_making		, "public_officials", "BIMP",  "All"), #Politics & Bureaucratic Capacity - Impartial Decision-Making
      SE.PRM.BIMP.1=  indicator_means(impartial_decision_making		, "public_officials", "BIMP",  "All"), #Average score for Impartial Decision-Making; where a score of 1 indicates low effectiveness and 5 indicates high effective~
      SE.PRM.BIMP.2=  indicator_means(politicized_personnel_management		, "public_officials", "BIMP",  "All"), #(Impartial Decision-Making) average score for politicized personnel management
      SE.PRM.BIMP.3=  indicator_means(politicized_policy_making		, "public_officials", "BIMP",  "All"), #(Impartial Decision-Making) average score for politicized policy-making
      SE.PRM.BIMP.4=  indicator_means(politicized_policy_implementation		, "public_officials", "BIMP",  "All"), #(Impartial Decision-Making) average score for politicized policy implementation
      SE.PRM.BIMP.5=  indicator_means(employee_unions_as_facilitators		, "public_officials", "BIMP",  "All") #(Impartial Decision-Making) average score for employee unions as facilitators
    )
  #######################################
  # Politics & Bureaucratic Capacity - Mandates & Accountability 	(BMAC)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.BMAC   =  indicator_means(mandates_accountability		, "public_officials", "BMAC",  "All"),#Politics & Bureaucratic Capacity - Mandates & Accountability
      SE.PRM.BMAC.1 =  indicator_means(mandates_accountability		, "public_officials", "BMAC",  "All"),#Average score for Mandates & Accountability; where a score of 1 indicates low effectiveness and 5 indicates high effective~
      SE.PRM.BMAC.2 =  indicator_means(coherence		, "public_officials", "BMAC",  "All"),#(Mandates & Accountability) Average score for coherence
      SE.PRM.BMAC.3 =  indicator_means(transparency		, "public_officials", "BMAC",  "All"),#(Mandates & Accountability) Average score for transparency
      SE.PRM.BMAC.4 =  indicator_means(accountability		, "public_officials", "BMAC",  "All"),#(Mandates & Accountability) Average score for accountability of public officials
    )
  #######################################
  # Politics & Bureaucratic Capacity - National Learning Goals 	(BNLG)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.BNLG   =  indicator_means(national_learning_goals		, "public_officials", "BNLG",  "All"),#Politics & Bureaucratic Capacity - National Learning Goals
      SE.PRM.BNLG.1 =  indicator_means(national_learning_goals		, "public_officials", "BNLG",  "All"),#Average score for National Learning Goals; where a score of 1 indicates low effectiveness and 5 indicates high effectivene~
      SE.PRM.BNLG.2 =  indicator_means(targeting		, "public_officials", "BNLG",  "All"),#(National Learning Goals) Average score for targeting
      SE.PRM.BNLG.3 =  indicator_means(monitoring		, "public_officials", "BNLG",  "All"),#(National Learning Goals) Average score for monitoring
      SE.PRM.BNLG.4 =  indicator_means(incentives		, "public_officials", "BNLG",  "All"),#(National Learning Goals) Average score for incentives
      SE.PRM.BNLG.5 =  indicator_means(community_engagement		, "public_officials", "BNLG",  "All"),#(National Learning Goals) Average score for community engagement
    )
  #######################################
  # Politics & Bureaucratic Capacity - Financing 	(BFIN)
  #######################################
  indicator_values_transpose <- indicator_values_transpose %>%
    mutate(
      SE.PRM.BFIN.6 = 4*as.numeric(finance_df_final$`Does the country spend 4-5%  of GDP or 15-20% of public expenditures on education spending?`)+1, #(Financing) - Adequacy expressed by the per child spending
      SE.PRM.BFIN.3 =4*as.numeric(finance_df_final$`Efficiency by the relationship between financing and outcomes; where 0 is the lowest possible efficiency and 1 is the highest`)+1,#(Financing) Efficiency - Expressed by the score from the Public Expenditure and Financial Accountability (PEFA) assessment~
      SE.PRM.BFIN.4 =4*as.numeric(finance_df_final$`Efficiency by the score from the Public Expenditure and Financial Accountability (PEFA) assessment; where 0 is the lowest possible efficiency and 1 is the highest`)+1,#(Financing) Efficiency - Expressed by the relationship between financing and outcomes; where 0 is the lowest possible effi~
      SE.PRM.BFIN.5 =-999,#(Financing) - Equity
      SE.PRM.BFIN.2 = as.numeric(finance_df_final$`Government expenditure per school age person, primary (% of GDP per capita)`), #(Financing) - Adequacy expressed by the per child spending
    ) %>%
    mutate(
      SE.PRM.BFIN   = as.numeric(0.5*SE.PRM.BFIN.2+0.5*(SE.PRM.BFIN.3+SE.PRM.BFIN.4)/2), #Politics & Bureaucratic Capacity - Financing
      SE.PRM.BFIN.1 = as.numeric(0.5*SE.PRM.BFIN.2+0.5*(SE.PRM.BFIN.3+SE.PRM.BFIN.4)/2),#Financing score; where a score of 1 indicates low effectiveness and 5 indicates high effectiveness in terms of adequacy, e~
      
    )


  #reshape dataframe back
  indicator_values_back <- as.data.frame(t(as.matrix(indicator_values_transpose))) %>%
    rownames_to_column(var='Series')
  # %>%
  #   rename(value = V1  )
  
api_final<-api_template %>%
    dplyr::select(-value) %>%
    left_join(indicator_values_back)
  
