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
                        choices=NULL)
        ),

        # Show a plot of the generated distribution

        mainPanel(
          h2(textOutput('var_name')),
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Histogram Plot", plotlyOutput("distPlot", height=800)),
                        tabPanel("Summary", DT::dataTableOutput("tableset") ),
                        tabPanel("Correlations", plotlyOutput("corrPlot", height=1200))
            )        )
    )
)

# Define server logic required to produce output
server <- function(input, output, session) {

    #Load the GEPD indicator data
    load(paste("school_indicators_data.RData", sep="/"))
    load(paste("public_officials_indicators_data.RData", sep="/"))
    
    names(indicators)<-make.names(names(indicators), unique=TRUE)
    
    #Create list of key indicators
    ind_list<-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge',
                'absence_rate', 'school_absence_rate', 'student_attendance',
                'content_knowledge', 'math_content_knowledge', 'literacy_content_knowledge',
                'ecd_student_knowledge', 'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
                'inputs', 'blackboard_functional', 'pens_etc', 'share_desk', 'used_ict', 'access_ict',
                'infrastructure','drinking_water', 'functioning_toilet', 'visibility', 'class_electricity', 'disability_accessibility', 'disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
                'operational_management', 'vignette_1', 'vignette_2', 'intrinsic_motivation', 'instructional_leadership','principal_management','teacher_attraction',
                'teacher_selection_deployment', 'teacher_support', 'teaching_evaluation', 'teacher_monitoring','school_monitoring',
                'school_management_attraction', 'school_selection_deployment', 'school_support', 'principal_evaluation'
    )
    
    indicators<- indicators %>%
        filter(indicator_tag!="PROE" & indicator_tag!="PROP" & indicator_tag!="TENR")
    
    updateSelectizeInput(session, 'indicators', choices = indicators$Indicator.Name, server = TRUE)
    
    
    get_tag <- reactive({
    
    get_tag_df<-indicators %>%
        filter(Indicator.Name==input$indicators)
    
    get_tag_df[,'indicator_tag']
    })
    
    dat <- reactive({

        df<-get(paste("final_indicator_data_",get_tag()[1], sep=""))
        df
    })
    
    #Diplay name of variable
    output$var_name<-renderText({input$indicators})
    
    #Output histogram of key indicators
    output$distPlot<-renderPlotly({
        
        df_plot <- dat() %>%
            select(one_of(ind_list)) %>%
            rowid_to_column("ID") %>%
            pivot_longer(cols=one_of(ind_list),
                         names_to='indicator', values_to='values')
        

        
        p<- ggplot(data=df_plot, aes(x=values)) +
            geom_histogram() +
            facet_wrap(indicator ~ ., scales='free_x' ) +
            theme_classic() + 
            theme(
                text = element_text(size = 16),
                
            )
        
        
        ggplotly(p)
    })

    #summary statistics table
    output$tableset <- DT::renderDataTable({

        sum_items<-colnames(dat()[,grep(x=colnames(dat()), pattern="m1s?q?|m2s?q?|m3s?q?|m4s?q?|m5s?q?|m6s?q?|m7s?q?|m8s?q?")])
        
        sumstats <- dat() %>%
          select(one_of(ind_list), one_of(sum_items) ) 
        
        sumstats_df<-skim(sumstats) %>%
            select(-level, -type, -value) %>%
            spread(stat, formatted) %>%
            select(variable, mean, sd, p0, p25, p50, p75, p100, complete, missing, hist) 
        
        #add variable label
        sumstats_df <- sumstats_df %>%
            mutate(name=variable) %>%
            left_join(metadta) %>%
            select(variable, varlabel, mean, sd, p0, p25, p50, p75, p100, complete, missing, hist)

        DT::datatable(sumstats_df, caption="Summary Statistics of Key Indicator Variables and Components of Indicator",
                      colnames=c("Indicator", "Label", "Mean", "Std Dev","Min", "25th Percentile", "Median", "75th Percentile", "Max", "# Complete Cases", "# Missing Cases", "Histogram"),
                      extensions = 'Buttons', options=list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                          pageLength = 60))

        
    })
  
    
    #Correlation Plot of key indicators
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
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
