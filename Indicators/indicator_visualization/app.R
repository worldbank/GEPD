library(shiny)
library(skimr)
library(flextable)
library(haven)
library(tidyverse)
library(highcharter)

# set max upload size to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# read in details
indicator_details <- read_csv('indicators.csv')
practice_tags <- "SE.PRM.PROE|SE.LPV.PRIM|SE.LPV.PRIM.BMP|SE.PRM.LERN|SE.PRM.TENR|SE.PRM.EFFT|SE.PRM.CONT|SE.PRM.ATTD|SE.PRM.LCAP|SE.PRM.PEDG|SE.LPV"
colors <- c("#20a39e", "#ffba49", "#ef5b5b")

indicators <- indicator_details$Indicator.Name

# create customizable skim function
my_skim <- skim_with(
  numeric = sfl(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sqrt(var(., na.rm = TRUE)),
    p25 = ~ (quantile(., probs = c(0.25), na.rm = TRUE)),
    p50 = ~ (quantile(., probs = c(0.5), na.rm = TRUE)),
    p75 = ~ (quantile(., probs = c(0.75), na.rm = TRUE)),
    complete = ~ sum(!is.na(.))
  )
)

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
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(
                               Table = "table",
                               Figure = "fig"),
                   selected = "fig"),
      
      # Allow the user to choose a column for histogram
      selectizeInput("col",
                     "Choose Indicators:",
                     choices = NULL,
                     selected = NULL),
      # Horizontal line ----
      tags$hr(),
      # Input: Select number of rows to display ----
      radioButtons("gender", "Gender",
                   choices = c(
                     All = "All",
                     Male = "Male",
                     Female = "Female",
                     Combined = "Combined"),
                   selected = "All"),
      
      # Horizontal line ----
      tags$hr(),
      
      radioButtons("location", "Location",
                   choices = c(
                     All = "All",
                     Rural = "Rural",
                     Urban = "Urban",
                     Combined = "Combined"),
                   selected = "All")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      #uiOutput("contents"), but make give the output more height
      uiOutput("contents")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  df_read <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    
    df <- read_csv(input$file1$datapath) %>%
        fuzzyjoin::regex_left_join(indicator_details, by = c("Series" = "Series")) %>%
        mutate(Type=if_else(
          grepl(practice_tags, Series.y) | grepl("Percent", `Indicator Name`),
          "0-100",
          "0-5"
        )) %>%
        group_by(`Indicator Name`)
    
  })
  

  
  # Filter data on the basis of Indicator.Name in df
  # Update selectizeInput choices
  observeEvent(df_read(), {
    updateSelectizeInput(session, "col", choices = c("All", df_read()$`Related.Indicators`))
  })
  
  df <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$col)
    
    if (input$col == "All" | is.na(input$col)) {
      df <- df_read()
    }
    else {
      df <- df_read() %>%
        filter(`Related.Indicators`==input$col)
    }
    
    #gender
    if (input$gender=="All") {
      df
    } 
    else if (input$gender=="Male") {
      df <- df %>%
        filter(grepl("Male", `Indicator Name`))
    }
    else if (input$gender=="Female") {
      df <- df %>%
        filter(grepl("Female", `Indicator Name`))
    }
    else if (input$gender=="Combined") {
      df <- df %>%
        filter(!grepl("Female", `Indicator Name`) & !grepl("Male", `Indicator Name`))
    }
    
    #location
    if (input$location=="All") {
      df
    } 
    else if (input$location=="Rural") {
      df <- df %>%
        filter(grepl("Rural", `Indicator Name`))
    }
    else if (input$location=="Urban") {
      df <- df %>%
        filter(grepl("Urban", `Indicator Name`))
    }
    else if (input$location=="Combined") {
      df <- df %>%
        filter(!grepl("Urban", `Indicator Name`) & !grepl("Rural", `Indicator Name`))
    }
    
    
  })
  
    output$heatmap <- renderHighchart({
            
            
          #create a highcharter column chart, plotting the value column with x as the indicator name
    #and y as the value
      # highcharter
      
      chart_df <- df() %>%
        group_by(Series.y) %>%
        mutate(subindicator= row_number(),
               Indicator.Name=factor(Indicator.Name, levels=rev(indicators)),
               value_metadata=factor(value_metadata, levels=c("Needs Improvement", "Caution", "On Target")),
               value_metadata_num=as.numeric(value_metadata),
               Name=`Indicator Name`,
               score=value) 
        
    

      #highcharter chart showing heatmap of values by indicator and subindicator
     hchart(
        chart_df, 
        "heatmap", 
        hcaes(
          x = subindicator,
          y = Indicator.Name, 
          value = value_metadata_num
        ),
       
      ) %>%
      #remove x axis labels
      hc_xAxis(
        labels = list(
          enabled = FALSE
        )
      ) %>%
      # use color scale based on #20a39e", "#ffba49", "#ef5b5b" in descending order
      hc_colorAxis(
        stops = color_stops(
          n = 3,
          colors = rev(colors)
        )
      ) %>%
      # Add the custom tooltip formatter
      hc_tooltip(
      formatter = JS("function() {
    return 'Indicator: ' + this.point.Name + '</b><br/>' + 
    'Score: ' + 
    Highcharts.numberFormat(this.point.score, 1);
    }")
      ) %>%
      #remove legend
      hc_legend(enabled = FALSE) %>%
      # add title
      hc_title(text = "GEPD Indicator Scores") 

                        
    })



  output$contents <- renderUI({
    req(input$file1)
    
    if (input$disp == "table") {
      
      value_col <- df() %>%
        mutate(
          value_color=case_when(
            value_metadata=="Needs Improvement" ~ "#ef5b5b",
            value_metadata=="Caution" ~ "#ffba49",
            value_metadata=="On Target" ~ "#20a39e",
            TRUE ~ "#808080"
          ))
      
      value_col_vec <- value_col$value_color
      
      df() %>%
        select(Related.Indicators, `Indicator Name`, value ) %>%
        flextable() %>%
        bg(j = c('value'),
           bg = value_col_vec) %>%
        theme_booktabs() %>%
        autofit() %>%
        width(j=2, 4) %>%
        htmltools_value()
        
      
    }
    else if (input$disp =="fig") {
      

    highchartOutput("heatmap", height='900px')
      
    }

    

    
  })
}

# Run the app ----
shinyApp(ui, server)
