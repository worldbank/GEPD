#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(skimr)
library(flextable)
library(haven)

#set max upload size to 30MB
options(shiny.maxRequestSize = 30*1024^2)

#create customizable skim function
my_skim<-    skim_with( numeric = sfl( mean = ~ mean(.,   na.rm=TRUE),
                                       sd = ~ sqrt(var(.,   na.rm=TRUE)),
                                       p25 = ~ (quantile(., probs=c(0.25),   na.rm=TRUE)),
                                       p50 = ~ (quantile(., probs=c(0.5),  na.rm=TRUE)),
                                       p75 = ~ (quantile(., probs=c(0.75),  na.rm=TRUE)),
                                       complete = ~ sum(!is.na(.))))

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Upload Files for Quick Inspection"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".dta")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Stata = "dta",
                                     Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = "dta"),

            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     `Summary Stats` = "sumstats",
                                     All = "all"),
                         selected = "sumstats")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tableOutput("contents")
            
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        if (input$sep!="dta") {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        }
        else {
            df <- read_stata(input$file1$datapath)  
        }
        
        if(input$disp == "head") {
            return(head(df))
        } 
        else if (input$disp == "sumstats") {
            
            skim_df <- df %>%
                my_skim() %>%
                yank("numeric") %>%
                mutate(variable=skim_variable) %>%
                select(variable, mean, sd, p0, p25, p50, p75, p100, complete) 
                
            return(skim_df)
            
        }    
        else {
            return(df)
        }
        
    })
    
}
# Run the app ----
shinyApp(ui, server)