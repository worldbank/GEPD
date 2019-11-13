#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyBS)
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

#library(wbgcharts)

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# define stle for ggplot based on BBC plotting styles
bbc_style <- function() {
  font <- "Helvetica"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=28,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=22,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=18,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=18,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}


# Define UI for application that examines GEPD data
ui <- navbarPage("Global Education Policy Dashboard",
  #####################################################
  # Dashboard Section
  ####################################################
  tabPanel("Dashboard",
           fluidPage(theme = shinytheme("cerulean"),
             includeMarkdown("header.md"),          
             h2("What are the results?"),
             selectInput('table_weights', "Use Survey Weights?", choices=c("Yes", "No"), selected="Yes"),
             DT::dataTableOutput("indicators_table"),
             h2("How are the indicators scored?"),
             DT::dataTableOutput("indicators_choices"),
             includeMarkdown("footer.md")
           )
      ),
  
  #############################################
  # Data Explorer Section
  #############################################
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
            selectInput('explorer_weights', "Use Survey Weights?", choices=c("Yes", "No"), selected="Yes"),
            
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

    
    ind_list<-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge', 'student_proficient',
                'student_attendance', 'absence_rate', 'school_absence_rate', 
                'content_knowledge', 'math_content_knowledge', 'literacy_content_knowledge', 'grammar', 'cloze',  'read_passage', 'arithmetic_number_relations', 'geometry', 'interpret_data',
                'ecd_student_knowledge', 'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
                'inputs', 'blackboard_functional', 'pens_etc','textbooks', 'share_desk', 'used_ict', 'access_ict',
                'infrastructure','drinking_water', 'functioning_toilet', 'internet', 'class_electricity','disability_accessibility','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
                'operational_management', 'vignette_1', 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', 'vignette_2', 'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
                'intrinsic_motivation', 'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
                'instructional_leadership', 'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
                'principal_knowledge_score', 'add_triple_digit_pknw', 'multiply_double_digit_pknw', 'complete_sentence_pknw', 'experience_pknw', 'textbooks_pknw', 'blackboard_pknw',
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
                'national_learning_goals', 'targeting', 'monitoring', 'incentives', 'community_engagement',
                'mandates_accountability' , 'coherence', 'transparency', 'accountability', 
                'quality_bureaucracy', 'knowledge_skills', 'work_environment', 'merit', 'motivation_attitudes',
                'impartial_decision_making','politicized_personnel_management', 'politicized_policy_making', 'politicized_policy_implementation', 'employee_unions_as_facilitators'
    )
    
    indicator_labels<-c("4th Grade Student Knowledge", "4th Grade Math Knowledge", "4th Grade Literacy Knowledge", "4th Grade Student Proficiency",
                        "Student Attendance Rate",
                        "Teacher Classroom Absence Rate", "Teacher School Absence Rate", 
                        "Teacher Content Knowledge", "Teacher Math Content Knowledge", "Teacher Literacy Content Knowledge", 'Grammar', 'Cloze Task',  'Read Passage', 'Arithmetic & Number Relations', 'Geometry', 'Interpret Data',
                        "1st Grade Assessment Score", "1st Grade Numeracy Score", "1st Grade Literacy Score", "1st Grade Executive Functioning Score", "1st Grade Socio-Emotional Score",
                        "Inputs", "Functioning Blackboard", "Classroom Materials", "Textbooks", "Desks", "ICT Usage", "ICT Access",
                        "Infrastructure", "Clean Drinking Water", "Functioning Toilets", "Internet", "Electricity", "Disability Accessibility", "Disability Road Access", "School Ramps", "Disability School Entrance", "Classroom Ramps", "Disability Classroom Entrance", "Disability Screening",
                        "Operational Management", "Operational Management - Vignette 1", 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', "Operational Management - Vignette 2",'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
                        "Teacher Intrinsic Motivation", 'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
                        "Instructional Leadership", 'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
                        'Principal Knowledge of School', 'Correct on # of Teachers correct on Triple Digit Addition', 'Correct on # of Teachers correct on Double Digit Multiplication', 'Correct on # of Teachers correct on Completing Sentence Question', 'Correct on # of Teachers Under 3 Years Experience', 'Correct on # of Students with Textbooks ', 'Correct on Functional Blackboard',
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
                        "National Learning Goals", 'Targeting', 'Monitorinig', 'Incentives', 'Community Engagement',
                        "Mandates and Accountability", 'Coherence', 'Transparency', 'Accountability of Public Officials',
                        "Quality of Bureaucracy", 'Knowledge and Skills', 'Work Environment', 'Merit', 'Motivation and Attitudes',
                        "Impartial Decision Making", 'Politicized personnel management', 'Politicized policy-making', 'Politicized policy-implementation', 'Employee unions as facilitators'
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
                        'Principal Knowledge of School',
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

    ################################################################################################################
    # Data Explorer Section
    ################################################################################################################     
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
          left_join(data_set_updated) %>%
          mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th)
        
        
        if (input$explorer_weights=="No") {
          #add function to produce weighted summary stats
          df <- df %>%
            mutate(school_ipw=1)
          
        }
        
        

        
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
    output$var_name<-renderText({paste(input$indicators, input$subgroup, input$gender, sep=" - ")})

    
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
        select(one_of(ind_list), school_ipw) %>%
        rowid_to_column("ID") %>%
        pivot_longer(cols=one_of(ind_list),
                     names_to='indicators', values_to='values') %>%
        left_join(labels_df)
      
      
      
      p<- ggplot(data=na.omit(df_plot), aes(x=values, y=..density.., 
                                            weight=school_ipw,
                                            group=indicator_labels, 
                                            fill=if_else((indicator_labels %in% main_indicator_labels), 
                                                         '#ff0000', '#d4d4d4'   ))) +
        geom_histogram() +
        facet_wrap(indicator_labels ~ ., scales='free_x' , labeller=labeller(indicator_labels=label_wrap_gen(10))) +
        scale_fill_manual(labels = c("Sub-Indicator", "Primary Indicator"),  values= c("#d4d4d4", "#ff0000")) +
        bbc_style() +
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
        select(one_of(ind_list), school_ipw) %>%
        rowid_to_column("ID") %>%
        pivot_longer(cols=one_of(ind_list),
                     names_to='indicators', values_to='values') %>%
        left_join(labels_df)
      
      
      q<- ggplot(data=na.omit(df_plot), aes(y=values, x=indicator_labels, 
                                            weight=school_ipw,
                                            fill=if_else((indicator_labels %in% main_indicator_labels), 
                                                                                       '#ff0000', '#d4d4d4'   ))) +
        geom_boxplot(line='goldernrod2') +
        scale_fill_manual(labels = c("Sub-Indicator", "Primary Indicator"),  values= c("#d4d4d4", "#ff0000")) +
        bbc_style() + 
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
        
        if (input$explorer_weights=="Yes") {
          #add function to produce weighted summary stats
          skim_with( numeric = list( mean = ~ wtd.mean(.,  w=sch_ipw, na.rm=TRUE),
                                     sd = ~ sqrt(wtd.var(.,  weights=sch_ipw, na.rm=TRUE)),
                                     p25 = ~ (wtd.quantile(., probs=c(0.25),  weights=sch_ipw, na.rm=TRUE)),
                                     p50 = ~ (wtd.quantile(., probs=c(0.5), weights=sch_ipw, na.rm=TRUE)),
                                     p75 = ~ (wtd.quantile(., probs=c(0.75), weights=sch_ipw, na.rm=TRUE)),
                                     complete_count= ~ sum(!is.na(.))))
        } else {
          skim_with_defaults()
        }
        

      } else if ((str_sub(get_tag()[1],1,1) %in% c('B'))) {
        sum_items<-colnames(dat()[,grep(x=colnames(dat()), pattern="gender|DEM|IDM|NLG|ACM|QB|ORG")])
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
        bbc_style() +
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
      df_reg <- df_reg %>%
        select(school_code, as.character(get_tag_reg()[1]), weights, total_4th ) %>%
        rename(y=2) %>%
        mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th)
      
      
        if (input$explorer_weights=="No") {
          #add function to produce weighted summary stats
          df_reg <- df_reg %>%
            mutate(school_ipw=1)
          
            }
        
      df_reg
      
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
        facet_wrap(indicator_labels ~ ., scales='free_x' , labeller=labeller(indicator_labels=label_wrap_gen(10))) +
        bbc_style() +
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
                          'drinking_water', 'functioning_toilet', 'internet', 'class_electricity','disability_accessibility','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
                          'vignette_1', 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', 'vignette_2', 'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
                          'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
                          'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
                          'add_triple_digit_pknw', 'multiply_double_digit_pknw', 'complete_sentence_pknw', 'experience_pknw', 'textbooks_pknw', 'blackboard_pknw',
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
      df_sub_reg <- df_sub_reg %>%
        select(school_code, as.character(get_tag_sub_reg()[1]), weights, total_4th ) %>%
        rename(y=2) %>%
        mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th)
      
      if (input$explorer_weights=="No") {
        #add function to produce weighted summary stats
        df_sub_reg <- df_sub_reg %>%
          mutate(school_ipw=1)
        
      }
      
      df_sub_reg
      
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



################################################################################################################     
# Dashboard Section
################################################################################################################     

school_dta_collapsed <- school_dta_short %>%
  summarise_all(~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))

public_officials_dta_collapsed <- public_officials_dta_clean %>%
  summarise_all(~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))

indicators_list<-c('student_knowledge',
            'student_attendance', 
            'absence_rate',
            'content_knowledge', 
            'ecd_student_knowledge', 
            'inputs', 
            'infrastructure',
            'operational_management', 
            'instructional_leadership',
            'principal_knowledge_score',
            'principal_management', 
            'teacher_attraction', 
            'teacher_selection_deployment', 
            'teacher_support', 
            'teaching_evaluation', 
            'teacher_monitoring',
            'intrinsic_motivation', 
            'school_monitoring', 
            'school_management_attraction', 
            'school_selection_deployment', 
            'school_support', 
            'principal_evaluation', 
            'national_learning_goals',
            'mandates_accountability',
            'quality_bureaucracy',
            'impartial_decision_making'
)

drilldown_list<-c(      'math_student_knowledge', 'literacy_student_knowledge',
                        'school_absence_rate', 'student_attendance',
                        'math_content_knowledge', 'literacy_content_knowledge',
                        'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
                        'blackboard_functional', 'pens_etc', 'share_desk', 'used_ict', 'access_ict',
                        'drinking_water', 'functioning_toilet', 'internet', 'class_electricity','disability_accessibility','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
                        'vignette_1', 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', 'vignette_2', 'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
                        'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
                        'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
                        'add_triple_digit_pknw', 'multiply_double_digit_pknw', 'complete_sentence_pknw', 'experience_pknw', 'textbooks_pknw', 'blackboard_pknw',
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

indicator_labels<-c('4th Grade Student Knowledge', '4th Grade Math Knowledge', '4th Grade Literacy Knowledge',
                    'Student Attendance Rate',
                    'Teacher Classroom Absence Rate', 'Teacher School Absence Rate', 
                    'Teacher Content Knowledge', 'Teacher Math Content Knowledge', 'Teacher Literacy Content Knowledge', 'Grammer', 'Cloze Task',  'Read Passage', 'Arithmetic & Number Relations', 'Geometry', 'Interpret Data',
                    '1st Grade Assessment Score', '1st Grade Numeracy Score', '1st Grade Literacy Score', '1st Grade Executive Functioning Score', '1st Grade Socio-Emotional Score',
                    'Inputs', 'Functioning Blackboard', 'Classroom Materials', 'Textbooks', 'Desks', 'ICT Usage', 'ICT Access',
                    'Infrastructure', 'Clean Drinking Water', 'Functioning Toilets', 'Internet', 'Electricity', 'Disability Accessibility', 'Disability Road Access', 'School Ramps', 'Disability School Entrance', 'Classroom Ramps', 'Disability Classroom Entrance', 'Disability Screening',
                    'Operational Management', 'Operational Management - Vignette 1', 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', 'Operational Management - Vignette 2','vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
                    'Teacher Intrinsic Motivation', 'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
                    'Instructional Leadership', 'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
                    'Principal Knowledge of School', 'Correct on # of Teachers correct on Triple Digit Addition', 'Correct on # of Teachers correct on Double Digit Multiplication', 'Correct on # of Teachers correct on Completing Sentence Question', 'Correct on # of Teachers Under 3 Years Experience', 'Correct on # of Students with Textbooks ', 'Correct on Functional Blackboard',
                    'Principal Management Skills', 'school_goals_exist','school_goals_clear','school_goals_relevant','school_goals_measured',
                    'Teacher Attraction (De Facto)', 'teacher_satisfied_job', 'teacher_satisfied_status', 'better_teachers_promoted' ,'teacher_bonus', 'salary_delays',
                    'Teacher Selection & Deployment (De Facto)', 'teacher_selection','teacher_deployment',
                    'Teacher Support (De Facto)', 'pre_service','practicum','in_service','opportunities_teachers_share',
                    'Teacher Evaluation (De Facto)', 'formally_evaluated', 'evaluation_content', 'negative_consequences','positive_consequences',
                    'Teacher Monitoring & Accountability (De Facto)', 'attendance_evaluated' , 'attendance_rewarded' , 'attendence_sanctions', 'miss_class_admin',
                    'Inputs and Infrastructure Monitoring', 'standards_monitoring','monitoring_inputs','monitoring_infrastructure','parents_involved',
                    'School Management Attraction', 'principal_satisfaction',
                    'School Management Selection & Deployment',
                    'School Management Support', 'prinicipal_trained','principal_training','principal_used_skills','principal_offered',
                    'School Management Evaluation', 'principal_formally_evaluated','principal_evaluation_multiple','principal_negative_consequences','principal_positive_consequences',
                    'National Learning Goals',
                    'Mandates and Accountability',
                    'Quality of Bureaucracy',
                    'Impartial Decision Making'
)

#create subset with just main indicators

 
main_indicator_labels2<-c('Proficiency on GEPD Assessment', 
                         'Student Attendance',
                         'Teacher Effort', 
                         'Teacher Content Knowledge', 
                         'Capacity for Learning', 
                         'Basic Inputs', 
                         'Basic Infrastructure', 
                         'Operational Management', 
                         'Instructional Leadership', 
                         'Principal Knowledge of School',
                         'Principal Management Skills', 
                         'Policy Lever (Teaching) - Attraction',
                         'Policy Lever (Teaching) - Selection & Deployment',
                         'Policy Lever (Teaching) - Support', 
                         'Policy Lever (Teaching) - Evaluation', 
                         'Policy Lever (Teaching) - Monitoring & Accountability', 
                         'Policy Lever (Teaching) - Intrinsic Motivation', 
                         'Policy Lever (Inputs & Infrastructure) - Monitoring',
                         'Policy Lever (School Management) - Attraction' ,                   
                         'Policy Lever (School Management) - Selection & Deployment'  ,      
                         'Policy Lever (School Management) - Support' ,                      
                         'Policy Lever (School Management) - Evaluation'    , 
                         'Politics & Bureaucratic Capacity - National Learning Goals' ,
                         'Politics & Bureaucratic Capacity - Mandates & Accountability'   ,  
                         'Politics & Bureaucratic Capacity - Quality of Bureaucracy'    ,    
                         'Politics & Bureaucratic Capacity - Impartial Decision-Making'    
                         
                         
) 


labels_df_2<-data.frame(indicators=as.character(indicators_list),
                      indicator_labels=as.character(main_indicator_labels2))
##########################
#summary statistics table
#########################



output$indicators_table <- DT::renderDataTable({
  
# School Survey
    metadata<-metadta
    
    weights <- school_dta_short %>%
      mutate(codigo.modular=as.numeric(school_code)) %>%
      left_join(data_set_updated) %>%
      mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th)
    
    sch_ipw<-weights$school_ipw 
    
    if (input$table_weights=="Yes") {
    #add function to produce weighted summary stats
    skim_with( numeric = list( mean = ~ wtd.mean(.,  w=sch_ipw, na.rm=TRUE),
                               sd = ~ sqrt(wtd.var(.,  weights=sch_ipw, na.rm=TRUE)),
                               p25 = ~ (wtd.quantile(., probs=c(0.25),  weights=sch_ipw, na.rm=TRUE)),
                               p50 = ~ (wtd.quantile(., probs=c(0.5), weights=sch_ipw, na.rm=TRUE)),
                               p75 = ~ (wtd.quantile(., probs=c(0.75), weights=sch_ipw, na.rm=TRUE)),
                               complete_count= ~ sum(!is.na(.))))
    } else {
      skim_with_defaults()
    }
 
  
    sumstats_school <- school_dta_short %>%
      select(one_of(indicators_list) ) 
    
    
    
    sumstats_school_df<-skim(sumstats_school) %>%
      select(-level, -type, -value) %>%
      spread(stat, formatted) %>%
      select(variable, mean, sd, p0, p25, p50, p75, p100, complete, missing, hist) 
    
    
    #add variable label
    sumstats_school_df <- sumstats_school_df %>%
      mutate(name=variable,
             indicators=variable) %>%
      left_join(labels_df_2) %>%
      mutate(varlabel=indicator_labels) %>%
      mutate(ci_low=as.numeric(mean)-1.96*(as.numeric(sd)/sqrt(as.numeric(complete))),
             ci_high=as.numeric(mean)+1.96*(as.numeric(sd)/sqrt(as.numeric(complete)))) %>%
      mutate(ci=paste("[",round(ci_low,2),", ", round(ci_high,2),"]", sep="")) %>%
      select(varlabel, mean, ci)
    
    #Now do breakdown by Urban/Rural
    #urban
    sumstats_school_urban <- school_dta_short %>%
      mutate(codigo.modular=as.numeric(school_code)) %>%
      left_join(data_set_updated) %>%
      filter(rural==FALSE) %>%
      mutate(school_ipw=if_else(is.na(weights), median(weights, na.rm=T), weights)*total_4th)
    
    sch_ipw<-sumstats_school_urban$school_ipw 
    
    sumstats_school_urban <- sumstats_school_urban %>%
      select(one_of(indicators_list)) 
    
    
    
    sumstats_school_urban_df<-skim(sumstats_school_urban) %>%
      select(-level, -type, -value) %>%
      spread(stat, formatted) %>%
      select(variable, mean, sd, p0, p25, p50, p75, p100, complete, missing, hist) 
    
    
    #add variable label
    sumstats_school_urban_df <- sumstats_school_urban_df %>%
      mutate(name=variable,
             indicators=variable) %>%
      left_join(labels_df_2) %>%
      mutate(varlabel=indicator_labels) %>%
      mutate(ci_low=as.numeric(mean)-1.96*(as.numeric(sd)/sqrt(as.numeric(complete))),
             ci_high=as.numeric(mean)+1.96*(as.numeric(sd)/sqrt(as.numeric(complete)))) %>%
      mutate(ci=paste("[",round(ci_low,2),", ", round(ci_high,2),"]", sep="")) %>%
      mutate(mean_urban=mean,
             ci_urban=ci) %>%
      select(varlabel, mean_urban, ci_urban)
    
    #rural
      sumstats_school_rural <- school_dta_short %>%
        mutate(codigo.modular=as.numeric(school_code)) %>%
        left_join(data_set_updated) %>%
        filter(rural==TRUE) 
        
        sch_ipw<-sumstats_school_rural$school_ipw 
      
        sumstats_school_rural <- sumstats_school_rural %>%
        select(one_of(indicators_list)) 
      

    sumstats_school_rural_df<-skim(sumstats_school_rural) %>%
      select(-level, -type, -value) %>%
      spread(stat, formatted) %>%
      select(variable, mean, sd, p0, p25, p50, p75, p100, complete, missing, hist) 
    
    
    #add variable label
    sumstats_school_rural_df <- sumstats_school_rural_df %>%
      mutate(name=variable,
             indicators=variable) %>%
      left_join(labels_df_2) %>%
      mutate(varlabel=indicator_labels) %>%
      mutate(ci_low=as.numeric(mean)-1.96*(as.numeric(sd)/sqrt(as.numeric(complete))),
             ci_high=as.numeric(mean)+1.96*(as.numeric(sd)/sqrt(as.numeric(complete)))) %>%
      mutate(ci=paste("[",round(ci_low,2),", ", round(ci_high,2),"]", sep="")) %>%
      mutate(mean_rural=mean,
             ci_rural=ci) %>%
      select(varlabel, mean_rural, ci_rural)
    
    #now bind urban/rural with the main results
    sumstats_school_df <- sumstats_school_df %>%
      left_join(sumstats_school_urban_df) %>%
      left_join(sumstats_school_rural_df)
    
      
    
  #Survey of Public Officials
    metadata<-public_officials_metadata
    
    #add function to produce weighted summary stats
    skim_with_defaults()
    
  
  
  sumstats_public_officials <- public_officials_dta_clean %>%
    select(one_of(indicators_list) ) 
  
  
  
  sumstats_public_officials_df<-skim(sumstats_public_officials) %>%
    select(-level, -type, -value) %>%
    spread(stat, formatted) %>%
    select(variable, mean, sd, p0, p25, p50, p75, p100, complete, missing, hist) 
  
  
  #add variable label
  sumstats_public_officials_df <- sumstats_public_officials_df %>%
    mutate(name=variable,
           indicators=variable) %>%
    left_join(labels_df_2) %>%
    mutate(varlabel=indicator_labels) %>%
    mutate(ci_low=as.numeric(mean)-1.96*(as.numeric(sd)/sqrt(as.numeric(complete))),
           ci_high=as.numeric(mean)+1.96*(as.numeric(sd)/sqrt(as.numeric(complete)))) %>%
    mutate(ci=paste("[",round(ci_low,2),", ", round(ci_high,2),"]", sep="")) %>%
    mutate(mean_urban=as.numeric(NA),
           ci_urban=as.numeric(NA),
           mean_rural=as.numeric(NA),
           ci_rural=as.numeric(NA)) %>%
    select(varlabel, mean, ci, mean_urban, ci_urban, mean_rural, ci_rural)
  
  
  sumstats_df <- sumstats_school_df %>%
    bind_rows(sumstats_public_officials_df) %>%
    arrange(factor(varlabel, levels=main_indicator_labels2))
  
   sumstats_df <- sumstats_df %>%
     inner_join(indicator_choices, by=c('varlabel'='Indicator.Name')) 
   
   sumstats_df <- sumstats_df %>%
     select(varlabel, Value, mean, ci, mean_urban, ci_urban, mean_rural, ci_rural)
   
  #add in custom column sub-headers
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th( rowspan = 2, 'Indicator'),
        th( rowspan = 2, 'Value Range'),
        th(colspan = 2, 'Overall'),
        th(colspan = 2, 'Urban'),
        th(colspan = 2, 'Rural')
        
      ),
      tr(
        lapply(rep(c('Mean', '95% Confident Interval'), 3), th)
      )
    )
  ))
  
  DT::datatable(sumstats_df, caption="Summary Statistics of Dashboard Indicators - Peru 2019",
                container = sketch, rownames=FALSE,
                class='cell-border stripe',
                escape = FALSE,
                extensions = c ('Buttons', 'FixedHeader'), options=list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 60,
                  scrollX = TRUE, 
                  paging=FALSE,
                  ordering=F)) %>%
    formatRound(columns = c('mean', 'ci', 'mean_urban', 'ci_urban', 'mean_rural', 'ci_rural' ),
                digits=2)
  
  
})




output$indicators_choices <- DT::renderDataTable({
  
  indicator_choices_table<-read_delim('indicators_choices.md', delim="|", trim_ws=TRUE)

  

  #Display metadata for indicator
  indicator_choices_table <- indicator_choices_table %>%
    dplyr::select(-X1, -X6) %>%
    dplyr::filter(Series!="---") %>%
    dplyr::select(-Series) %>%
    rename(name='Indicator Name' ) %>%
    dplyr::filter(name %in% main_indicator_labels2) %>%
    arrange(factor(name, levels=main_indicator_labels2)) %>%
    rename('Indicator Name'=name ) 
  
    
  
  s <- input$indicators_table_rows_selected
  if (length(s)) {
    indicator_choices_table <- indicator_choices_table[s,]
  }
  DT::datatable(indicator_choices_table, caption="Indicator Scoring",
                rownames=FALSE,
                class='cell-border stripe',
                escape = FALSE,
                extensions = c ('Buttons', 'FixedHeader'), options=list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 60,
                  scrollX = TRUE, 
                  paging=FALSE,
                  ordering=F)) 
  
})



}





# Run the application 
shinyApp(ui = ui, server = server)
