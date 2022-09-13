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
library(skimr)
library(DT)
library(Hmisc)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ECD Cutscores"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("domain", 
                        "Select either 4th Grade Overall Score, Math Score, or Lanugage Score for Cutpoints",
                        choices=c("Overall", "Math", "Literacy"),
                        selected="student_knowledge"
            ),
                        
            sliderInput("pctl",
                        "Percentile of 4th Grade Overall Score:",
                        min = 1,
                        max = 100,
                        value = 75)
            # sliderInput("pctl_math",
            #             "Percentile of 4th Grade Math Score:",
            #             min = 1,
            #             max = 100,
            #             value = 50),
            # sliderInput("pctl_lit",
            #             "Percentile of 4th Grade Literacy Score:",
            #             min = 1,
            #             max = 100,
            #             value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput("tableset")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
    load(paste("school_indicators_data_anon.RData", sep="/"))
    

    
    
    
    
    ##########################
    #Produce dataset based on indicator selected
    ##########################
    dat <- reactive({
        
        #need to modify this depending on country
        
        
        
        df<-school_dta_short_anon  %>%
            select(contains("ecd_student_proficiency"), contains("student_knowledge"),contains("student_proficient"),contains("content"), ipw ) 
        
        if (input$domain=="Overall") {
        df<- df %>%
            filter(student_knowledge>=quantile(school_dta_short_anon$student_knowledge, probs=c(as.numeric(input$pctl)/100), na.rm=T))
                
        } else if (input$domain=="Math") {
            df<- df %>%
                filter(math_student_knowledge>=quantile(school_dta_short_anon$math_student_knowledge, probs=c(as.numeric(input$pctl)/100), na.rm=T))
        } else if (input$domain=="Literacy") {
            df<- df %>%
                filter(literacy_student_knowledge>=quantile(school_dta_short_anon$literacy_student_knowledge, probs=c(as.numeric(input$pctl)/100), na.rm=T))
        }
        
        df
        
        
        
    })
    
    
    output$tableset <- DT::renderDataTable({
        
        sumstats <- dat() 
        
        
        
        my_skim<-    skim_with( numeric = sfl( mean = ~ wtd.mean(.,  w=ipw, na.rm=TRUE),
                                               sd = ~ sqrt(wtd.var(.,  weights=ipw, na.rm=TRUE)),
                                               p25 = ~ (wtd.quantile(., probs=c(0.25),  weights=ipw, na.rm=TRUE)),
                                               p50 = ~ (wtd.quantile(., probs=c(0.5), weights=ipw, na.rm=TRUE)),
                                               p75 = ~ (wtd.quantile(., probs=c(0.75), weights=ipw, na.rm=TRUE)),
                                               complete = ~ sum(!is.na(.))))
        
        
        ipw <- sumstats$ipw
        
        sumstats_df<-my_skim(sumstats) %>%
            yank("numeric") %>%
            mutate(variable=skim_variable) %>%
            select(variable, mean, sd, p0, p25, p50, p75, p100, complete,  hist) %>%
            filter(grepl('ecd|proficien|student_knowledge|content', variable))
        
        
        DT::datatable(sumstats_df, caption=paste("Summary Statistics of ECD Student Scores for Schools above", input$pctl, "Percentile in Overall 4th Grade Score", sep=" "),
                      colnames=c("Indicator",  "Mean", "Std Dev","Min", "25th Percentile", "Median", "75th Percentile", "Max", "# Complete Cases",  "Histogram"),
                      extensions = 'Buttons', options=list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel'),
                          pageLength = 60)) %>%
            formatRound(columns = c('mean', 'sd', 'p0', 
                                    'p25', 'p50', 'p75', 'p100'),
                        digits=2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
