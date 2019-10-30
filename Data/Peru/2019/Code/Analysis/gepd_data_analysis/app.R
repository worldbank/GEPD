#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)

library(shinyjs)
library(tidyverse)
library(plotly)
library(glue)
library(DT)
library(rvg)
#library(officer)
library(kableExtra)
library(ggcorrplot)
library(stargazer)
library(sandwich)
library(Cairo)
library(scales)
library(ggpmisc)
library(skimr)
library(Hmisc)

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Define UI for application that examines GEPD data
ui <- navbarPage("Global Education Policy Dashboard",
  tabPanel("Dashboard",
           
           # Output: infobox ----
           box(h2("Learning Outcomes"), width=10    ),
           #Outcome - Learning
           box(infoBoxOutput("achievement", width=10),
               width=10),      
           box(h2("Service Delivery Indicators"), width=10    ),
           #Practices - Teacher Effort
           box(  infoBoxOutput("absence", width=10),
                 width=10),      
           #Practices - Teacher Content Knowledge
           box(infoBoxOutput("knowledge", width=10),
               width=10),     
           
           #Practices - Teacher Pedagogical Skills
           box(
             infoBoxOutput("pedagogy", width=10),
             checkboxInput("pedagogy_checkbox", "Check for drilldown"),
             conditionalPanel(condition="input.pedagogy_checkbox==1",infoBoxOutput("pedagogy_culture", width=10)),
             conditionalPanel(condition="input.pedagogy_checkbox==1",infoBoxOutput("pedagogy_instruction", width=10)),
             conditionalPanel(condition="input.pedagogy_checkbox==1",infoBoxOutput("pedagogy_socioemotional", width=10))
             , width=10),
           
           #Practices - Basic Inputs
           box(infoBoxOutput("basic_inputs", width=10),
               width=10), 
           #Practices - Basic Infrastructure
           box(
             infoBoxOutput("basic_infrastructure", width=10),
             checkboxInput("basic_infrastructure_checkbox", "Check for drilldown"),
             conditionalPanel(condition="input.basic_infrastructure_checkbox==1",infoBoxOutput("basic_infrastructure_water", width=10)),
             conditionalPanel(condition="input.basic_infrastructure_checkbox==1",infoBoxOutput("basic_infrastructure_toilets", width=10)),
             conditionalPanel(condition="input.basic_infrastructure_checkbox==1",infoBoxOutput("basic_infrastructure_visibility", width=10)),
             conditionalPanel(condition="input.basic_infrastructure_checkbox==1",infoBoxOutput("basic_infrastructure_electricity", width=10)),
             conditionalPanel(condition="input.basic_infrastructure_checkbox==1",infoBoxOutput("basic_infrastructure_disabilities", width=10))
             , width=10),  
           
           #Practices - Capacity for Learning  
           box(infoBoxOutput("prepared_learners", width=10),
               width=10), 
           
           #Practices - Operational Functions
           box(infoBoxOutput("operational_functions", width=10),
               width=10), 
           
           #Practices - Instructional Leadership
           box(infoBoxOutput("instructional_leadership", width=10),
               width=10), 
           
           #Practices - Principal School Knowledge
           box(infoBoxOutput("principal_school_knowledge", width=10),
               width=10), 
           
           #Practices - Principal Management Skills
           box(infoBoxOutput("principal_management_skills", width=10),
               width=10), 
           
           box(h2("Policy Levers"), width=10    ),
           #Policy Lever (Teaching) - Attraction
           box(
             infoBoxOutput("attraction", width=10),
             checkboxInput("attraction_checkbox", "Check for drilldown"),
             conditionalPanel(condition="input.attraction_checkbox==1",infoBoxOutput("attraction_dejure1", width=10)),
             conditionalPanel(condition="input.attraction_checkbox==1",infoBoxOutput("attraction_dejure2", width=10)),
             conditionalPanel(condition="input.attraction_checkbox==1",infoBoxOutput("attraction_dejure3", width=10)),
             conditionalPanel(condition="input.attraction_checkbox==1",infoBoxOutput("attraction_defacto1", width=10)),
             conditionalPanel(condition="input.attraction_checkbox==1",infoBoxOutput("attraction_defacto2", width=10)),
             conditionalPanel(condition="input.attraction_checkbox==1",infoBoxOutput("attraction_defacto3", width=10))
             , width=10),  
           #Policy Lever (Teaching) - Selection & Deployment
           box(
             infoBoxOutput("teaching_selection_deployment", width=10),
             checkboxInput("teaching_selection_deployment_checkbox", "Check for drilldown"),
             conditionalPanel(condition="input.teaching_selection_deployment_checkbox==1",infoBoxOutput("teaching_selection_deployment_dejure1", width=10)),
             conditionalPanel(condition="input.teaching_selection_deployment_checkbox==1",infoBoxOutput("teaching_selection_deployment_dejure2", width=10)),
             conditionalPanel(condition="input.teaching_selection_deployment_checkbox==1",infoBoxOutput("teaching_selection_deployment_dejure3", width=10)),
             conditionalPanel(condition="input.teaching_selection_deployment_checkbox==1",infoBoxOutput("teaching_selection_deployment_defacto1", width=10)),
             conditionalPanel(condition="input.teaching_selection_deployment_checkbox==1",infoBoxOutput("teaching_selection_deployment_defacto2", width=10))
             , width=10),     
           #Policy Lever (Teaching) - Support
           box(infoBoxOutput("support", width=10),
               width=10), 
           
           #Policy Lever (Teaching) - Evaluation
           box(infoBoxOutput("evaluation", width=10),
               width=10), 
           
           #Policy Lever (Teaching) - Monitoring & Accountability
           box(
             infoBoxOutput("teaching_monitoring_accountability", width=10),
             checkboxInput("teaching_monitoring_accountability_checkbox", "Check for drilldown"),
             conditionalPanel(condition="input.teaching_monitoring_accountability_checkbox==1",infoBoxOutput("teaching_monitoring_accountability_dejure1", width=10)),
             conditionalPanel(condition="input.teaching_monitoring_accountability_checkbox==1",infoBoxOutput("teaching_monitoring_accountability_dejure2", width=10)),
             conditionalPanel(condition="input.teaching_monitoring_accountability_checkbox==1",infoBoxOutput("teaching_monitoring_accountability_defacto1", width=10)),
             conditionalPanel(condition="input.teaching_monitoring_accountability_checkbox==1",infoBoxOutput("teaching_monitoring_accountability_defacto2", width=10))
             , width=10),  
           
           #Policy Lever (Teaching) - Intrinsic Motivation
           box(infoBoxOutput("intrinsic_motivation", width=10),
               width=10), 
           
           #Policy Lever (Inputs & Infrastructure) - Standards
           box(infoBoxOutput("standards", width=10),
               width=10), 
           
           #Policy Lever (Inputs & Infrastructure) - Monitoring
           box(infoBoxOutput("monitoring", width=10),
               width=10), 
           
           #Policy Lever (Learners) - Nutrition Programs
           box(infoBoxOutput("nutrition_programs", width=10),
               width=10), 
           
           #Policy Lever (Learners) - Health Programs
           box(infoBoxOutput("health_programs", width=10),
               width=10), 
           
           #Policy Lever (Learners) - Center-Based Care
           box(infoBoxOutput("center_based_care", width=10),
               width=10), 
           
           #Policy Lever (Learners) - Caregiver Capacity
           box(infoBoxOutput("caregiver_capacity", width=10),
               width=10), 
           
           #Policy Lever (School Management) - Clarity of Functions
           box(infoBoxOutput("clarity_functions", width=10),
               width=10), 
           
           #Policy Lever (School Management) - Supervision
           box(infoBoxOutput("supervision", width=10),
               width=10), 
           
           #Policy Lever (School Management) - Attraction
           box(infoBoxOutput("management_attraction", width=10),
               width=10), 
           
           #Policy Lever (School Management) - Selection & Deployment
           box(infoBoxOutput("management_selection_deployment", width=10),
               width=10), 
           
           #Policy Lever (School Management) - Support
           box(infoBoxOutput("management_support", width=10),
               width=10), 
           
           #Policy Lever (School Management) - Evaluation
           box(infoBoxOutput("management_evaluation", width=10),
               width=10), 
           
           box(h2("Politics & Bureaucratic Capacity"), width=10    ),    
           #Politics & Bureaucratic Capacity - Quality of Bureaucracy
           box(infoBoxOutput("quality_bureaucracy", width=10),
               width=10),     
           #Politics & Bureaucratic Capacity - Impartial Decision-Making
           box(infoBoxOutput("impartial_decision", width=10),
               width=10), 
           
           #Politics & Bureaucratic Capacity - Mandates & Accountability
           box(
             infoBoxOutput("mandates_accountability", width=10),
             checkboxInput("mandates_checkbox", "Check for drilldown"),
             conditionalPanel(condition="input.mandates_checkbox==1",infoBoxOutput("mandates_accountability_clarity", width=5)),
             conditionalPanel(condition="input.mandates_checkbox==1",infoBoxOutput("mandates_accountability_coherence", width=5)),
             conditionalPanel(condition="input.mandates_checkbox==1",infoBoxOutput("mandates_accountability_transparency", width=5)),
             conditionalPanel(condition="input.mandates_checkbox==1",infoBoxOutput("mandates_accountability_accountability", width=5))
             , width=10),
           
           #Politics & Bureaucratic Capacity - National Learning Goals    
           box(infoBoxOutput("national_learning_goals", width=10),
               width=10),     
           #Politics & Bureaucratic Capacity - Financing    
           box(infoBoxOutput("financing", width=10),
               width=10)      

           ),
  tabPanel("GEPD Data Explorer",
    fluidPage(

    # Application title
    titlePanel("GEPD Data Explorer"),

    # Sidebar with an input for the specific indicator 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("indicators",
                        "Indicator Name:",
                        choices=NULL),
            selectizeInput("subgroup", "Subgroup:",
                        choices=NULL),
            selectizeInput("gender", "Gender:",
                           choices=NULL),
            h2("Scoring Metadata on Indicator"),
            htmlOutput('metadata' )

        ),

        # Show a plot of the generated distribution

        mainPanel(
          h2(textOutput('var_name')),
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        id='tabset',
                        tabPanel("Histogram Plot", value=1, 
                                 downloadButton("downloadhist", "Download"),
                                 plotOutput("distPlot", height=600)),
                        tabPanel("BoxPlot", value=2, 
                                 downloadButton("downloadboxplot", "Download"),
                                 plotOutput("boxPlot", height=600)),
                        tabPanel("Summary", value=3, DT::dataTableOutput("tableset") ),
                        tabPanel("Correlations", value=4, 
                                 downloadButton("downloadcorr", "Download"),
                                 plotlyOutput("corrPlot", height=1000)),
                        tabPanel("Regression Analysis", value=5, 
                                 selectizeInput("reg_choices", "Choose Outcome Variable for Regressions: (Default: 4th Grade Learning)", 
                                                choices=NULL)   ,
                                 downloadButton("downloadregplot", "Download"),
                                 plotOutput("regplot", height=1400)
                                 ),
                        tabPanel("Sub-Indicator Regression Analysis", value=6,
                                 selectizeInput("sub_reg_choices", "Choose Outcome Variable for Regressions: (Default: 4th Grade Learning)", 
                                                choices=NULL)   ,
                                 downloadButton("downloadscoring", "Download"),
                                 htmlOutput('scoring')
                                 )
            )        )
    )
)
)
)
# Define server logic required to produce output
server <- function(input, output, session) {

    #Load the GEPD indicator data
    load(paste("school_indicators_data.RData", sep="/"))
    load(paste("public_officials_indicators_data.RData", sep="/"))
    
    
    
    
    load(paste("school_sample_2019_07_22.RData"))
    
    indicators <- indicators %>%
      filter(!(indicator_tag=="PROE" | indicator_tag=="PRIM" | indicator_tag=="TENR"))
    
    names(indicators)<-make.names(names(indicators), unique=TRUE)
  
    
    #Create list of key indicators
    #Create list of key indicators
    ind_list<-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge',
                'student_attendance', 'absence_rate', 'school_absence_rate', 
                'content_knowledge', 'math_content_knowledge', 'literacy_content_knowledge', 'grammar', 'cloze', 'read_passage', 'arithmetic_number_relations', 'geometry', 'interpret_data',
                'ecd_student_knowledge', 'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
                'inputs', 'blackboard_functional', 'pens_etc', 'textbooks', 'share_desk', 'used_ict', 'access_ict',
                'infrastructure','drinking_water', 'functioning_toilet', 'visibility', 'class_electricity','disability_accessibility','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
                'operational_management', 'vignette_1', 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', 'vignette_2', 'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
                'intrinsic_motivation', 'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
                'instructional_leadership', 'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
                'principal_management', 'school_goals_exist','school_goals_clear','school_goals_relevant','school_goals_measured',
                'teacher_attraction', 'teacher_satisfied_job', 'teacher_satisfied_status', 'better_teachers_promoted' ,'teacher_bonus', 'salary_delays',
                'teacher_selection_deployment', 'teacher_selection','teacher_deployment',
                'teacher_support', 'pre_service','practicum','in_service','opportunities_teachers_share',
                'teaching_evaluation', 'formally_evaluated', 'evaluation_content', 'negative_consequences','positive_consequences',
                'teacher_monitoring','attendance_evaluated' , 'attendance_rewarded' , 'attendence_sanctions', 'miss_class_admin',
                'school_monitoring', 'standards_monitoring','monitoring_inputs','monitoring_infrastructure','parents_involved',
                'school_management_attraction', 'principal_satisfaction',
                'school_selection_deployment', 
                'school_support', 'prinicipal_trained','principal_training','principal_used_skills','principal_offered',
                'principal_evaluation', 'principal_formally_evaluated','principal_evaluation_multiple','principal_negative_consequences','principal_positive_consequences'
    )
    
    ind_list<-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge',
                'student_attendance', 'absence_rate', 'school_absence_rate', 
                'content_knowledge', 'math_content_knowledge', 'literacy_content_knowledge', 'grammar', 'cloze',  'read_passage', 'arithmetic_number_relations', 'geometry', 'interpret_data',
                'ecd_student_knowledge', 'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
                'inputs', 'blackboard_functional', 'pens_etc','textbooks', 'share_desk', 'used_ict', 'access_ict',
                'infrastructure','drinking_water', 'functioning_toilet', 'visibility', 'class_electricity','disability_accessibility','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
                'operational_management', 'vignette_1', 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', 'vignette_2', 'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
                'intrinsic_motivation', 'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
                'instructional_leadership', 'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
                'principal_management', 'school_goals_exist','school_goals_clear','school_goals_relevant','school_goals_measured',
                'teacher_attraction', 'teacher_satisfied_job', 'teacher_satisfied_status', 'better_teachers_promoted' ,'teacher_bonus', 'salary_delays',
                'teacher_selection_deployment', 'teacher_selection','teacher_deployment',
                'teacher_support', 'pre_service','practicum','in_service','opportunities_teachers_share',
                'teaching_evaluation', 'formally_evaluated', 'evaluation_content', 'negative_consequences','positive_consequences',
                'teacher_monitoring','attendance_evaluated' , 'attendance_rewarded' , 'attendence_sanctions', 'miss_class_admin',
                'school_monitoring', 'standards_monitoring','monitoring_inputs','monitoring_infrastructure','parents_involved',
                'school_management_attraction', 'principal_satisfaction',
                'school_selection_deployment', 
                'school_support', 'prinicipal_trained','principal_training','principal_used_skills','principal_offered',
                'principal_evaluation', 'principal_formally_evaluated','principal_evaluation_multiple','principal_negative_consequences','principal_positive_consequences',
                "national_learning_goals",
                "mandates_accountability",
                "quality_bureaucracy",
                "impartial_decision_making"
    )
    
    indicator_labels<-c("4th Grade Student Knowledge", "4th Grade Math Knowledge", "4th Grade Literacy Knowledge",
                        "Student Attendance Rate",
                        "Teacher Classroom Absence Rate", "Teacher School Absence Rate", 
                        "Teacher Content Knowledge", "Teacher Math Content Knowledge", "Teacher Literacy Content Knowledge", 'Grammer', 'Cloze Task',  'Read Passage', 'Arithmetic & Number Relations', 'Geometry', 'Interpret Data',
                        "1st Grade Assessment Score", "1st Grade Numeracy Score", "1st Grade Literacy Score", "1st Grade Executive Functioning Score", "1st Grade Socio-Emotional Score",
                        "Inputs", "Functioning Blackboard", "Classroom Materials", "Textbooks", "Desks", "ICT Usage", "ICT Access",
                        "Infrastructure", "Clean Drinking Water", "Functioning/Accessible Toilets", "Classroom Visibility", "Electricity", "Disability Accessibility", "Disability Road Access", "School Ramps", "Disability School Entrance", "Classroom Ramps", "Disability Classroom Entrance", "Disability Screening",
                        "Operational Management", "Operational Management - Vignette 1", 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', "Operational Management - Vignette 2",'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
                        "Teacher Intrinsic Motivation", 'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
                        "Instructional Leadership", 'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
                        'Principal Management Skills', 'school_goals_exist','school_goals_clear','school_goals_relevant','school_goals_measured',
                        'Teacher Attraction (De Facto)', 'teacher_satisfied_job', 'teacher_satisfied_status', 'better_teachers_promoted' ,'teacher_bonus', 'salary_delays',
                        'Teacher Selection & Deployment (De Facto)', 'teacher_selection','teacher_deployment',
                        'Teacher Support (De Facto)', 'pre_service','practicum','in_service','opportunities_teachers_share',
                        'Teacher Evaluation (De Facto)', 'formally_evaluated', 'evaluation_content', 'negative_consequences','positive_consequences',
                        'Teacher Monitoring & Accountability (De Facto)', 'attendance_evaluated' , 'attendance_rewarded' , 'attendence_sanctions', 'miss_class_admin',
                        "Inputs and Infrastructure Monitoring", 'standards_monitoring','monitoring_inputs','monitoring_infrastructure','parents_involved',
                        "School Management Attraction", 'principal_satisfaction',
                        "School Management Selection & Deployment",
                        "School Management Support", 'prinicipal_trained','principal_training','principal_used_skills','principal_offered',
                        "School Management Evaluation", 'principal_formally_evaluated','principal_evaluation_multiple','principal_negative_consequences','principal_positive_consequences',
                        "National Learning Goals",
                        "Mandates and Accountability",
                        "Quality of Bureaucracy",
                        "Impartial Decision Making"
                        )
  
    #create subset with just main indicators
    main_indicator_labels<-c("4th Grade Student Knowledge", 
                        "Student Attendance Rate",
                        "Teacher Classroom Absence Rate", 
                        "Teacher Content Knowledge", 
                        "1st Grade Assessment Score", 
                        "Inputs", 
                        "Infrastructure", 
                        "Operational Management", 
                        "Teacher Intrinsic Motivation", 
                        "Instructional Leadership", 
                        'Principal Management Skills', 
                        'Teacher Attraction (De Facto)',
                        'Teacher Selection & Deployment (De Facto)',
                        'Teacher Support (De Facto)', 
                        'Teacher Evaluation (De Facto)', 
                        'Teacher Monitoring & Accountability (De Facto)', 
                        "Inputs and Infrastructure Monitoring", 
                        "School Management Attraction", 
                        "School Management Selection & Deployment",
                        "School Management Support", 
                        "School Management Evaluation", 
                        "National Learning Goals",
                        "Mandates and Accountability",
                        "Quality of Bureaucracy",
                        "Impartial Decision Making"
    )  
      
    labels_df<-data.frame(indicators=as.character(ind_list),
                          indicator_labels=as.character(indicator_labels))
    
    indicators<- indicators %>%
        filter(indicator_tag!="PROE" & indicator_tag!="PROP" & indicator_tag!="TENR" ) 

        
       #fix a few issues with Survey of Public Officials naming
    
    #update indicator choices for session and for regression tables
    updateSelectizeInput(session, 'indicators', choices = indicators$Indicator.Name, server = TRUE)
    
    updateSelectizeInput(session, 'reg_choices', choices = indicator_labels, server = TRUE)
    
    updateSelectizeInput(session, 'sub_reg_choices', choices = main_indicator_labels, server = TRUE)
    

    get_tag <- reactive({
    
    get_tag_df<-indicators %>%
        filter(Indicator.Name==input$indicators)
    
    
    get_tag_df[,'indicator_tag']
    
    })
    

    #update subgroup selector
    observe({ 
      if (!(str_sub(get_tag()[1],1,1) %in% c('B'))) {
        
        updateSelectizeInput(session, 'subgroup', choices = c('All', 'Urban', 'Rural'))
        
      } else if ((str_sub(get_tag()[1],1,1) %in% c('B'))) {
        
        choice<-unique(as.character(public_officials_dta_clean$govt_tier))
        choice<-append('All',choice)
        
        updateSelectizeInput(session, 'subgroup', choices = choice)
        
        
      }
      
    })  
    #update gender selector
    gender_available <- c("ATTD",  "CONT", "EFFT", "LCAP", "LERN")
    
    observe({ 
      
      
      if (!(get_tag()[1] %in% gender_available)) {
        
        updateSelectizeInput(session, 'gender', choices = c('All'))
        
      } else if (get_tag()[1] %in% gender_available) {
        
        updateSelectizeInput(session, 'gender', choices = c('All', 'Female', 'Male'))
        
        
      }
      
    })
    
    ##########################
    #Produce dataset based on indicator selected
    ##########################
    dat <- reactive({
      if (!(str_sub(get_tag()[1],1,1) %in% c('B'))) {
        
        
        if (input$gender=="All") {
          
          df<-get(paste("final_indicator_data_",get_tag()[1], sep=""))
          
        } else if (input$gender=="Female") {
          
          df<-get(paste("final_indicator_data_",get_tag()[1], "_F", sep=""))
          
        } else if (input$gender=="Male") {
          
          df<-get(paste("final_indicator_data_",get_tag()[1], "_M", sep=""))
          
        }

        df<- df %>%
          mutate(codigo.modular=as.numeric(school_code)) %>%
          left_join(data_set_updated)

        
       if (input$subgroup=="Rural") {
          df<- df %>%
            filter(rural==TRUE)
        } else if (input$subgroup=="Urban") {
          df<- df %>%
            filter(rural==FALSE)  
        }
        } else if ((str_sub(get_tag()[1],1,1) %in% c('B'))) {
          
          df<-get(paste("final_indicator_data_",get_tag()[1], sep=""))
          
          
          if (input$subgroup!="All") {
            df<- df %>%
              filter(govt_tier==input$subgroup)
          }
          
        }
        df
        
        

    })
    
    
    
    # #Update list of subindicators
    # 
    # subind_dat <- reactive({
    #   if (!(str_sub(get_tag()[1],1,1) %in% c('B'))) {
    #     df_subindicators<-get(paste("final_indicator_data_",get_tag()[1], sep=""))
    #     } else if ((str_sub(get_tag()[1],1,1) %in% c('B'))) {
    #     
    #     df_subindicators<-public_officials_dta_clean 
    # 
    #     }
    #   
    #   df_subindicators<- df_subindicators %>%
    #     select(one_of(ind_list))  
    #   
    #   df_subindicators
    #   
    #   
    #   
    # })
    #   
    # 
    # updateVarSelectInput(session, 'subindicators', data = subind_dat() , server = TRUE)
    # 
    #   

    
    

    
    ##########################
    #Diplay name of variable
    ##########################
    output$var_name<-renderText({paste(input$indicators, input$subgroup, sep=" - ")})

    
    ##########################
    #Diplay metadata of variable
    ##########################
    
    #Get metadata from github
    indicator_choices<-read_delim('indicators_choices.md', delim="|", trim_ws=TRUE)
    
    #Display metadata for indicator
    indicator_choices <- indicator_choices %>%
      dplyr::select(-X1, -X6) %>%
      dplyr::filter(Series!="---") 
    
    names(indicator_choices)<-make.names(names(indicator_choices), unique=TRUE)
    
    
    get_meta <- reactive({
      
      get_meta_df<-indicator_choices %>%
        dplyr::filter(Indicator.Name==input$indicators) 
      
      
      get_meta_df[,'How.is.the.indicator.scored.']
      
    })
    
    output$metadata<-renderUI({
      
      HTML(paste(get_meta()[1]))
      
    })
    
    
    

        
    ###################################
    #Output histogram of key indicators
    ##################################
    
    histo <- reactive ({
      
      df_plot <- dat() %>%
        select(one_of(ind_list)) %>%
        rowid_to_column("ID") %>%
        pivot_longer(cols=one_of(ind_list),
                     names_to='indicators', values_to='values') %>%
        left_join(labels_df)
      
      
      
      p<- ggplot(data=na.omit(df_plot), aes(x=values, group=indicator_labels, colour=indicator_labels)) +
        geom_histogram() +
        facet_wrap(indicator_labels ~ ., scales='free_x' , labeller=labeller(indicator_labels=label_wrap_gen(10)), nrow=3) +
        theme_classic() + 
        theme(
          text = element_text(size = 16),
          
        ) +
        expand_limits(x = 0, y = 0) +
        ggtitle("Histograms of Dashboard Indicators") +
        labs(colour = "Indicator")
      
      p
      
    })
    
    output$distPlot<-renderPlot({
        
      histo()
      
   
    })
    
    # Downloadable png of selected dataset ----
    output$downloadhist <- downloadHandler(
      filename = function() {
        paste('histogram - ',input$indicators," - ",input$subgroup, ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = histo(), device = "png", width=16, height=12)
      }
    )
    
    
    ###################################
    #Output boxplot of key indicators
    ##################################
    
    boxp <- reactive({
      
      df_plot <- dat() %>%
        select(one_of(ind_list)) %>%
        rowid_to_column("ID") %>%
        pivot_longer(cols=one_of(ind_list),
                     names_to='indicators', values_to='values') %>%
        left_join(labels_df)
      
      
      q<- ggplot(data=na.omit(df_plot), aes(y=values, x=indicator_labels)) +
        geom_boxplot(fill="gold1", line='goldernrod2') +
        theme_classic() + 
        theme(
          text = element_text(size = 16),
        ) +
        expand_limits(x = 0, y = 0) +
        ggtitle("Boxplot of Dashboard Indicators")+
        xlab("Indicator") +
        coord_flip()
      
      q
      
    })
    
    output$boxPlot<-renderPlot({
      
      boxp()

    })
    
    # Downloadable png of selected dataset ----
    output$downloadboxplot <- downloadHandler(
      filename = function() {
        paste('boxplot - ',input$indicators," - ",input$subgroup, ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = boxp(), device = "png", width=16, height=12)
      }
    )
    
    

    
    

    ##########################
    #summary statistics table
    #########################
    output$tableset <- DT::renderDataTable({

      if (!(str_sub(get_tag()[1],1,1) %in% c('B'))) {
        sum_items<-colnames(dat()[,grep(x=colnames(dat()), pattern="m1s?q?|m2s?q?|m3s?q?|m4s?q?|m5s?q?|m6s?q?|m7s?q?|m8s?q?")])
        metadata<-metadta
        
        weights <- dat() %>%
          mutate(codigo.modular=as.numeric(school_code)) %>%
          left_join(data_set_updated) %>%
          mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th)
        
        sch_ipw<-weights$school_ipw 
        
        #add function to produce weighted summary stats
        skim_with( numeric = list( mean = ~ wtd.mean(.,  w=sch_ipw, na.rm=TRUE),
                                   sd = ~ sqrt(wtd.var(.,  weights=sch_ipw, na.rm=TRUE)),
                                   p25 = ~ (wtd.quantile(., probs=c(0.25),  weights=sch_ipw, na.rm=TRUE)),
                                   p50 = ~ (wtd.quantile(., probs=c(0.5), weights=sch_ipw, na.rm=TRUE)),
                                   p75 = ~ (wtd.quantile(., probs=c(0.75), weights=sch_ipw, na.rm=TRUE)),
                                   complete_count= ~ sum(!is.na(.))))
        
      } else if ((str_sub(get_tag()[1],1,1) %in% c('B'))) {
        sum_items<-colnames(dat()[,grep(x=colnames(dat()), pattern="gender|DEM|NLG|ACM|QB|ORG")])
        metadata<-public_officials_metadata
        
        #add function to produce weighted summary stats
        skim_with_defaults()
      
      }
      
        sumstats <- dat() %>%
          select(one_of(ind_list), one_of(sum_items) ) 
        
        
        
         sumstats_df<-skim(sumstats) %>%
           select(-level, -type, -value) %>%
           spread(stat, formatted) %>%
           select(variable, mean, sd, p0, p25, p50, p75, p100, complete, missing, hist) 
        

        #add variable label
        sumstats_df <- sumstats_df %>%
            mutate(name=variable,
                   indicators=variable) %>%
            left_join(metadata) %>%
            left_join(labels_df) %>%
            mutate(varlabel=if_else(is.na(varlabel),as.character(indicator_labels),as.character(varlabel))) %>%
            select(variable, varlabel, mean, sd, p0, p25, p50, p75, p100, complete, missing, hist)

        DT::datatable(sumstats_df, caption="Summary Statistics of Key Indicator Variables and Components of Indicator",
                      colnames=c("Indicator", "Label", "Mean", "Std Dev","Min", "25th Percentile", "Median", "75th Percentile", "Max", "# Complete Cases", "# Missing Cases", "Histogram"),
                      extensions = 'Buttons', options=list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                          pageLength = 60)) %>%
          formatRound(columns = c('mean', 'sd', 'p0', 
                                  'p25', 'p50', 'p75', 'p100'),
                      digits=2)

        
    })
  
    #####################################
    #Correlation Plot of key indicators
    ####################################
    
    corrp<- reactive({
      
      #drop non-numeric elements
      
      corr_items<-colnames(dat()[,grep(x=colnames(dat()), pattern="m2s?q?|m3s?q?|m4s?q?|m5s?q?|m6s?q?|m7s?q?|m8s?q?")])
      
      df_corr_plot <- dat() %>%
        select(one_of(ind_list), one_of(corr_items) ) %>%
        select_if(is.numeric)
      
      df_corr_plot <-    round(cor(df_corr_plot, use="complete.obs"), 2)
      
      
      pcorr<- ggcorrplot(df_corr_plot,
                         outline.color = "white",
                         ggtheme = theme_bw(),
                         colors = c("#F8696B", "#FFEB84", "#63BE7B"),
                         legend.title = "Correlation",
                         title = "Correlation Between Questionnaire Items") + 
        theme(
          text = element_text(size = 16),
        )
      
      pcorr
      
    })
    
    output$corrPlot<-renderPlotly({
        
      ggplotly(corrp())

    })
    
    # Downloadable png of selected dataset ----
    output$downloadcorr <- downloadHandler(
      filename = function() {
        paste('Correlation Plot - ',input$indicators," - ",input$subgroup, ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = corrp(), device = "png", width=16, height=12)
      }
    )
    
    
    #############################
    # Create database with just learning outcomes for regressions
    ##############################
    
    
    get_tag_reg <- reactive({
      
      get_tag_reg_df<-labels_df %>%
        filter(indicator_labels==input$reg_choices)
      
      
      get_tag_reg_df[,'indicators']
      
    })
    
    
    dat_reg <- reactive({
      #create database with just learning outcomes
      
      if (!(str_sub(get_tag()[1],1,1) %in% c('B'))) {
        
      df_reg<-school_dta_short
      
      df_reg<- df_reg %>%
        mutate(codigo.modular=as.numeric(school_code)) %>%
        left_join(data_set_updated)
      
      
      if (input$subgroup=="Rural") {
        df_reg<- df_reg %>%
          filter(rural==TRUE)
      } else if (input$subgroup=="Urban") {
        df_reg<- df_reg %>%
          filter(rural==FALSE)  
      }
      
      #keep just school code and learning outcome
      df_reg %>%
        select(school_code, as.character(get_tag_reg()[1]), weights, total_4th ) %>%
        rename(y=2) %>%
        mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th)
      
      } else if ((str_sub(get_tag()[1],1,1) %in% c('B'))) {
      
        df_reg<-public_officials_dta_clean 
        
        if (input$subgroup!="All") {
          df_reg <- df_reg %>%
            filter(govt_tier==input$subgroup)
        }
        #keep just school code and learning outcome
        df_reg %>%
          select(interview__id, as.character(get_tag_reg()[1])) %>%
          rename(y=2) %>%
          mutate(school_ipw=1)

        }
      })
    
    ########################################
    #Regression Analysis of key indicators
    #########################################
    
    
    regp<- reactive({
      
      if (!(str_sub(get_tag()[1],1,1) %in% c('B'))) {
        df_reg_plot <- dat() %>%
          select(one_of(ind_list), school_code) %>%
          rowid_to_column("ID") %>%
          pivot_longer(cols=one_of(ind_list),
                     names_to='indicators', values_to='values') %>%
          left_join(dat_reg()) %>%
          left_join(labels_df) 
        
      } else if ((str_sub(get_tag()[1],1,1) %in% c('B'))) {
        df_reg_plot <- dat() %>%
          select(one_of(ind_list), interview__id) %>%
          rowid_to_column("ID") %>%
          pivot_longer(cols=one_of(ind_list),
                       names_to='indicators', values_to='values') %>%
          left_join(dat_reg()) %>%
          left_join(labels_df) 
      }

      
      regplots<- ggplot(data=na.omit(df_reg_plot), aes(x=values, y=y, group=indicator_labels, colour=indicator_labels)) +
        geom_point() +
        geom_smooth(method='lm', mapping = aes(weight = school_ipw)) +
        facet_wrap(indicator_labels ~ ., scales='free_x' , labeller=labeller(indicator_labels=label_wrap_gen(10)), nrow = 5) +
        theme_classic() + 
        theme(
          text = element_text(size = 16),
          
        ) +
        expand_limits(x = 0, y = 0) +
        ggtitle(paste0("Linear Regression of Dashboard Indicators on Subindicators for ", input$reg_choices)) +
        labs(colour = "Indicator") +
        ylab(input$reg_choices) +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     label.x.npc = "right", label.y.npc = 0.2,
                     formula = 'y~x', parse = TRUE, size = 5)
      
      
      regplots
      
    })
    output$regplot<-renderPlot({
      
      regp()


    })  
    
    # Downloadable png of selected dataset ----
    output$downloadregplot <- downloadHandler(
      filename = function() {
        paste('Regression Plot - ',input$indicators," - ",input$subgroup, ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = regp(), device = "png", width=16, height=12)
      }
    )
    

    #Create list of key sub-indicators

    

    sub_ind_list<-c(      'math_student_knowledge', 'literacy_student_knowledge',
                          'school_absence_rate', 'student_attendance',
                          'math_content_knowledge', 'literacy_content_knowledge',
                          'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
                          'blackboard_functional', 'pens_etc', 'share_desk', 'used_ict', 'access_ict',
                          'drinking_water', 'functioning_toilet', 'visibility', 'class_electricity','disability_accessibility','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
                          'vignette_1', 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', 'vignette_2', 'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
                          'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
                          'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
                          'school_goals_exist','school_goals_clear','school_goals_relevant','school_goals_measured',
                          'teacher_satisfied_job', 'teacher_satisfied_status', 'better_teachers_promoted' ,'teacher_bonus', 'salary_delays',
                          'teacher_selection','teacher_deployment',
                          'pre_service','practicum','in_service','opportunities_teachers_share',
                          'formally_evaluated', 'evaluation_content', 'negative_consequences','positive_consequences', 
                          'attendance_evaluated' , 'attendance_rewarded' , 'attendence_sanctions', 'miss_class_admin',
                          'standards_monitoring','monitoring_inputs','monitoring_infrastructure','parents_involved',
                          'principal_satisfaction',
                          'prinicipal_trained','principal_training','principal_used_skills','principal_offered',
                          'principal_formally_evaluated','principal_evaluation_multiple','principal_negative_consequences','principal_positive_consequences'
    )

########################################
#Sub-indicator Regressions
#########################################
    
    get_tag_sub_reg <- reactive({
      
      get_tag_sub_reg_df<-labels_df %>%
        filter(indicator_labels==input$sub_reg_choices)
      
      
      get_tag_sub_reg_df[,'indicators']
      
    })
    
    
    dat_sub_reg <- reactive({
      #create database with just learning outcomes
      df_sub_reg<-school_dta_short
      
      df_sub_reg<- df_sub_reg %>%
        mutate(codigo.modular=as.numeric(school_code)) %>%
        left_join(data_set_updated)
      
      
      if (input$subgroup=="Rural") {
        df_sub_reg<- df_sub_reg %>%
          filter(rural==TRUE)
      } else if (input$subgroup=="Urban") {
        df_sub_reg<- df_sub_reg %>%
          filter(rural==FALSE)  
      }
      
      #keep just school code and learning outcome
      df_sub_reg %>%
        select(school_code, as.character(get_tag_sub_reg()[1]), weights, total_4th ) %>%
        rename(y=2) %>%
        mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th)
    })
    

score<-reactive({
  
  df_scoring_reg <- dat() %>%
    select(one_of(sub_ind_list), school_code) %>%
    left_join(dat_sub_reg()) %>%
    select(-school_code,-weights, -school_ipw)
  
  scoring_reg<-lm(y~., df_scoring_reg, weights = dat()$school_ipw)
  
  # Adjust standard errors
  cov1         <- vcovHC(scoring_reg, type = "HC1")
  robust_se    <- sqrt(diag(cov1))
  
  stargazer( scoring_reg, type = "html",
             se        = list(robust_se),
             title = "Regressions of Indicator variables on Set of Sub-Indicators",
             column.labels = input$sub_reg_choices
  )
  
})        
    
output$scoring<-renderUI({
  
  
  




HTML(paste(score()

))      
  

})  


# Downloadable html of selected regressions ----
output$downloadscoring <- downloadHandler(
  
  
  filename = function() {
    paste('Subindicator Regressions - ',input$indicators," - ",input$subgroup, ".html", sep = "")
  },
  content = function(file) {
    
    writeLines(paste(score()), file)
    
      }
)


}





# Run the application 
shinyApp(ui = ui, server = server)
