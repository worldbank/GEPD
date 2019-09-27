#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(glue)
library(DT)
library(rvg)
library(officer)
library(kableExtra)
library(skimr)
library(ggcorrplot)
library(Cairo)
library(scales)
library(ggpmisc)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Define UI for application that examines GEPD data
ui <- fluidPage(

    # Application title
    titlePanel("GEPD Data Explorer"),

    # Sidebar with an input for the specific indicator 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("indicators",
                        "Indicator Name:",
                        choices=NULL),
            selectizeInput("subgroup", "Subgroup:",
                        choices=NULL)
        ),

        # Show a plot of the generated distribution

        mainPanel(
          h2(textOutput('var_name')),
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Histogram Plot", plotOutput("distPlot", height=800)),
                        tabPanel("BoxPlot", plotOutput("boxPlot", height=800)),
                        tabPanel("Summary", DT::dataTableOutput("tableset") ),
                        tabPanel("Correlations", plotlyOutput("corrPlot", height=1200)),
                        tabPanel("Regression Analysis", plotOutput("regplot", height=800))
            )        )
    )
)

# Define server logic required to produce output
server <- function(input, output, session) {

    #Load the GEPD indicator data
    load(paste("school_indicators_data.RData", sep="/"))
    load(paste("public_officials_indicators_data.RData", sep="/"))
    
    
    
    
    load(paste("school_sample_2019_07_22.RData"))
    
    names(indicators)<-make.names(names(indicators), unique=TRUE)
    
    #Create list of key indicators
    ind_list<-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge',
                'student_attendance',
                'absence_rate', 'school_absence_rate',
                'content_knowledge', 'math_content_knowledge', 'literacy_content_knowledge',
                'ecd_student_knowledge', 'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
                'inputs', 'blackboard_functional', 'pens_etc', 'share_desk', 'used_ict', 'access_ict',
                'infrastructure','drinking_water', 'functioning_toilet', 'visibility', 'class_electricity', 'disability_accessibility', 'disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
                'operational_management', 'vignette_1', 'vignette_2', 
                'instructional_leadership',
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
                "national_learning_goals",
                "mandates_accountability",
                "quality_bureaucracy",
                "impartial_decision_making"
    )
    
    
    indicator_labels<-c("4th Grade Student Knowledge", "4th Grade Math Knowledge", "4th Grade Literacy Knowledge",
                        "Student Attendance Rate",
                        "Teacher Classroom Absence Rate", "Teacher School Absence Rate", 
                        "Teacher Content Knowledge", "Teacher Math Content Knowledge", "Teacher Literacy Content Knowledge",
                        "1st Grade Assessment Score", "1st Grade Numeracy Score", "1st Grade Literacy Score", "1st Grade Executive Functioning Score", "1st Grade Socio-Emotional Score",
                        "Inputs", "Functioning Blackboard", "Classroom Materials", "Desks", "ICT Usage", "ICT Access",
                        "Infrastructure", "Clean Drinking Water", "Functioning/Accessible Toilets", "Classroom Visibility", "Electricity", "Disability Accessibility", "Disability Road Access", "School Ramps", "Disability School Entrance", "Classroom Ramps", "Disability Classroom Entrance", "Disability Screening",
                        "Operational Management", "Operational Management - Vignette 1", "Operational Management - Vignette 2",
                        "Instructional Leadership",
                        'Principal Management Skills',
                        'Teacher Attraction (De Facto)',
                        'Teacher Selection & Deployment (De Facto)',
                        'Teacher Support (De Facto)',
                        'Teacher Evaluation (De Facto)',
                        'Teacher Monitoring & Accountability (De Facto)',
                        "Teacher Intrinsic Motivation",
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
        filter(indicator_tag!="PROE" & indicator_tag!="PROP" & indicator_tag!="TENR" & 
                 indicator_tag!="BIMP" & indicator_tag!="BMAC" & indicator_tag!="BNLG" & indicator_tag!="BFIN") %>%
#keep just one Bureaucracy indicator, because all with show in one
      mutate(indicator_tag=case_when(
        indicator_tag=='BQBR' ~ "QB",
        TRUE ~ indicator_tag)) %>%
      mutate(Indicator.Name=if_else(indicator_tag=="QB", 'Politics and Bureaucratic Capacity', Indicator.Name))
        
       #fix a few issues with Survey of Public Officials naming
    
    updateSelectizeInput(session, 'indicators', choices = indicators$Indicator.Name, server = TRUE)
    
    
    
    get_tag <- reactive({
    
    get_tag_df<-indicators %>%
        filter(Indicator.Name==input$indicators)
    
    
    get_tag_df[,'indicator_tag']
    
    })
    

    
    observe({ 
      if (!(str_sub(get_tag()[1],1,2) %in% c('QB', 'ID', 'AC', 'NL'))) {
        
        updateSelectizeInput(session, 'subgroup', choices = c('All', 'Urban', 'Rural'))
        
      } else if ((str_sub(get_tag()[1],1,2) %in% c('QB', 'ID', 'AC', 'NL'))) {
        
        choice<-unique(as.character(public_officials_dta_clean$govt_tier))
        choice<-append('All',choice)
        
        updateSelectizeInput(session, 'subgroup', choices = choice)
        
        
      }
      
    })  
      
    ##########################
    #Produce dataset based on indicator selected
    ##########################
    dat <- reactive({
      if (!(str_sub(get_tag()[1],1,2) %in% c('QB', 'ID', 'AC', 'NL'))) {
        df<-get(paste("final_indicator_data_",get_tag()[1], sep=""))
        
        

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
        } else if ((str_sub(get_tag()[1],1,2) %in% c('QB', 'ID', 'AC', 'NL'))) {
          
          df<-public_officials_dta_clean 
          
          
          if (input$subgroup!="All") {
            df<- df %>%
              filter(govt_tier==input$subgroup)
          }
          
        }
        df
        
        

    })
    
    

    
    
    ##########################
    #Diplay name of variable
    ##########################
    output$var_name<-renderText({paste(input$indicators, input$subgroup, sep=" - ")})
    
    ###################################
    #Output histogram of key indicators
    ##################################
    output$distPlot<-renderPlot({
        
        df_plot <- dat() %>%
            select(one_of(ind_list)) %>%
            rowid_to_column("ID") %>%
            pivot_longer(cols=one_of(ind_list),
                         names_to='indicators', values_to='values') %>%
            left_join(labels_df)
        
        

        p<- ggplot(data=na.omit(df_plot), aes(x=values, group=indicator_labels, colour=indicator_labels)) +
          geom_histogram() +
          facet_wrap(indicator_labels ~ ., scales='free_x' , labeller=labeller(indicator_labels=label_wrap_gen(10))) +
          theme_classic() + 
          theme(
            text = element_text(size = 16),
            
          ) +
          ggtitle("Histograms of Dashboard Indicators") +
          labs(colour = "Indicator")
        
        
        
        p
   
    })
    ###################################
    #Output boxplot of key indicators
    ##################################
    output$boxPlot<-renderPlot({
      
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
          ggtitle("Boxplot of Dashboard Indicators")+
          xlab("Indicator")
        
        q
    })
    
    ##########################
    #summary statistics table
    #########################
    output$tableset <- DT::renderDataTable({

      if (!(str_sub(get_tag()[1],1,2) %in% c('QB', 'ID', 'AC', 'NL'))) {
        sum_items<-colnames(dat()[,grep(x=colnames(dat()), pattern="m1s?q?|m2s?q?|m3s?q?|m4s?q?|m5s?q?|m6s?q?|m7s?q?|m8s?q?")])
        metadata<-metadta
      } else if ((str_sub(get_tag()[1],1,2) %in% c('QB', 'ID', 'AC', 'NL'))) {
        sum_items<-colnames(dat()[,grep(x=colnames(dat()), pattern="gender|DEM|NLG|ACM|QB|ORG")])
        metadata<-public_officials_metadata
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
                          pageLength = 60))

        
    })
  
    #####################################
    #Correlation Plot of key indicators
    ####################################
    output$corrPlot<-renderPlotly({
        
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
        
        
        ggplotly(pcorr)
    })
    
    
    
    #############################
    # Create database with just learning outcomes for regressions
    ##############################
    
    dat_learn <- reactive({
      #create database with just learning outcomes
      df_learn<-get(paste("final_indicator_data_",'LERN', sep=""))
      
      df_learn<- df_learn %>%
        mutate(codigo.modular=as.numeric(school_code)) %>%
        left_join(data_set_updated)
      
      
      if (input$subgroup=="Rural") {
        df_learn<- df_learn %>%
          filter(rural==TRUE)
      } else if (input$subgroup=="Urban") {
        df_learn<- df_learn %>%
          filter(rural==FALSE)  
      }
      
      #keep just school code and learning outcome
      df_learn %>%
        select(school_code, student_knowledge)
    })
    
    ########################################
    #Regression Analysis of key indicators
    #########################################
    output$regplot<-renderPlot({
      
      


      df_reg_plot <- dat() %>%
        select(one_of(ind_list), school_code) %>%
        rowid_to_column("ID") %>%
        pivot_longer(cols=one_of(ind_list),
                     names_to='indicators', values_to='values') %>%
        left_join(labels_df) %>%
        left_join(dat_learn())

      regplots<- ggplot(data=na.omit(df_reg_plot), aes(x=values, y=student_knowledge, group=indicator_labels, colour=indicator_labels)) +
        geom_point() +
        geom_smooth(method='lm') +
        facet_wrap(indicator_labels ~ ., scales='free_x' , labeller=labeller(indicator_labels=label_wrap_gen(10)), nrow = 3) +
        theme_classic() + 
        theme(
          text = element_text(size = 16),
          
        ) +
        ggtitle("Linear Regression of Dashboard Indicators on 4th Grade Student Knowledge") +
        labs(colour = "Indicator") +
        ylab('4th Grade Student Knowledge') +
        stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                     label.x.npc = "right", label.y.npc = 0.15,
                     formula = 'y~x', parse = TRUE, size = 5)
      
      
      regplots
    })  
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
