#Create by Brian Stacy 6/14/2019
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(ggplot2)
library(maps)
library(tmap)
library(htmltools)
library(leaflet)
library(rgdal)
library(sp)
library(dplyr)
library(mapview)
library(leafem)
library(shiny)
library(shinydashboard)
library(DT)
library(Hmisc)


#Read in school level data
school_dta<-read.csv(file = "./school_dta.csv")
school_dta <- school_dta %>%
  filter(!is.na(lat) & !is.na(lon))

#Create indicator of schools with missing values in our indicators

school_dta <- school_dta %>%
  mutate(missings=(is.na(student_knowledge) | is.na(absence_rate) | is.na(content_knowledge) | 
           is.na(pedagogical_knowledge) | is.na(inputs) | is.na(infrastructure)))


#color coded markers for missing values
getColor <- function(df_input) {
  sapply(df_input$missings, function(missings) {
    if(missings == FALSE) {
      "green"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'graduation-cap',
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(school_dta)
)



#create datatable with school level indicators
output_table_keeplist <- c("school", "province", "district", "sch_id", "student_knowledge", "absence_rate", 
                           "content_knowledge", "pedagogical_knowledge","inputs", 
                           "infrastructure", "ecd", "operational_management", "instructional_leadership",
                           "school_knowledge", "management_skills")

school_indicator_data <- school_dta %>%
  select(output_table_keeplist) %>%
  mutate(school=structure(school, label="School Name")) %>%
  mutate(province=structure(province, label="Province")) %>%
  mutate(district=structure(district, label="District")) %>%
  mutate(sch_id=structure(sch_id, label="EMIS")) %>%
  mutate(student_knowledge=structure(100*round(student_knowledge, digits=2), label="Avg 4th Grade Learning Score")) %>%
  mutate(absence_rate=structure(100*round(absence_rate, digits=2), label="Teacher Absence Rate")) %>%
  mutate(content_knowledge=structure(100*round(content_knowledge, digits=2), label="Teacher Content Knowledge Score")) %>%
  mutate(pedagogical_knowledge=structure(round(pedagogical_knowledge, digits=2), label="Teacher Pedagogical Skill")) %>%
  mutate(inputs=structure(round(inputs, digits=2), label="Basic Inputs")) %>%
  mutate(infrastructure=structure(round(infrastructure, digits=2), label="Basic Infrastructure")) %>%
  mutate(ecd=structure(100*round(ecd, digits=2), label="Capacity for Learning")) %>%
  mutate(operational_management=structure(round(operational_management, digits=2), label="Operational Management")) %>%
  mutate(instructional_leadership=structure(round(instructional_leadership, digits=2), label="Instructional Leadership")) %>%
  mutate(school_knowledge=structure(round(school_knowledge, digits=2), label="School Knowledge")) %>%
  mutate(management_skills=structure(round(management_skills, digits=2), label="Management Skills"))

ui <- dashboardPage(
  # Application title
  dashboardHeader(title="Mozambique SDI 2018 School Level Data - Mapped"),
  # Sidebar with user input elements
  dashboardSidebar(
    
      passwordInput("pwIn", "Passcode")
    ),
  dashboardBody(
    fluidRow(
    leafletOutput("mymap", height="95vh", width="95vh")
    #DT::dataTableOutput("school_indicator_data")
    )
  )
  )




server <- function(input, output, session) {
  #Create map
  map <-     leaflet() %>% 
    addTiles()  %>%
    addAwesomeMarkers(lng=school_dta$lon, lat= school_dta$lat, icon=icons,
               popup = paste("Name: ", school_dta$school, " <br>",
                                                                      "Province: ", school_dta$province, " <br>",
                                                                      "District: ", school_dta$district, " <br>",
                                                                      "EMIS Code: ", school_dta$sch_id, " <br>",
                                                                      "<font color='red' > Outcomes ", " <br> <font color='black' >",
                                                                      "Avg 4th Grade Learning Score:", 100*round(school_dta$student_knowledge, digits=2), "% <br>",
                                                                      "<font color='red' > Practice Indicators ", " <br> <font color='black' >",
                                                                      "Teacher Absence Rate (1st visit): ", 100*round(school_dta$absence_rate_1st, digits=2), "% <br>",
                                                                      "Teacher Absence Rate: ", 100*round(school_dta$absence_rate, digits=2), "% <br>",
                                                                      "Teacher Content Knowledge Score: ", 100*round(school_dta$content_knowledge, digits=2),"% <br>",
                                                                      "Teacher Pedagogical Skill: ", round(school_dta$pedagogical_knowledge, digits=2), " <br>",
                                                                      "Basic Inputs: ", round(school_dta$inputs, digits=2), " <br>",
                                                                      "Basic Infrastructure: ", round(school_dta$infrastructure, digits=2), " <br>")  ) 
                                                                      #"Capacity for Learning: ", 100*round(school_dta$ecd, digits=2), "% <br>",
                                                                      #"Operational Management: ", round(school_dta$operational_management, digits=2), " <br>",
                                                                      #"Instructional Leadership: ", round(school_dta$instructional_leadership, digits=2), " <br>",
                                                                      #"School Knowledge: ", round(school_dta$school_knowledge, digits=2), " <br>",
                                                                      #"Management Skills: ", round(school_dta$management_skills, digits=2), " <br>" , sep ="")  ) 
  
  
  #use leaflet package.  See here for examples.  
  
  output$mymap <- renderLeaflet({
    validate(
      need(input$pwIn=="sdi_school_map", "Please enter the passcode"
      ))
    
    
    map
    })
  
 
    
  output$school_indicator_data = DT::renderDataTable({
    as.data.frame(as.list(school_indicator_data), col.names=label(school_indicator_data))
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

