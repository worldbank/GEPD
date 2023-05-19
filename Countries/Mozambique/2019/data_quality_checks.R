#Quality checks on cleaned data
#Written by Brian Stacy 7/9/2019

#load relevant libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)
library(plotly)
library(naniar)
library(crosstalk)
library(leaflet)
library(mice)

#Load the data
#read in school level file
data_folder <- file.path("C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/clean")

load(file.path(data_folder, "all_modules_clean.RData"))


####################################################################
#Basic Summary Tables
####################################################################

#plot missing values by indicator
gg_miss_var(school_indicator_data)


#table of summary stats
#merge on teacher content knowledge
school_indicator_data <- school_indicator_data %>%
  right_join(region, by="sch_id")

school_count_table<- school_indicator_data %>%
  group_by(region) %>%
  summarise(n=n()) 

sumstats<-school_indicator_data %>%
  rename_all(.funs=funs(sub("_","", names(school_indicator_data)))) %>%
  select(c("studentknowledge", "absencerate", 
           "contentknowledge", "pedagogicalknowledge", "inputs", "infrastructure")) %>%  
  summarise_all(funs(min = min(.,na.rm=TRUE), 
                      q25 = quantile(., 0.25, na.rm=TRUE), 
                      median = median(.,na.rm=TRUE), 
                      q75 = quantile(., 0.75, na.rm=TRUE),
                      max = max(.,na.rm=TRUE),
                      mean = mean(.,na.rm=TRUE), 
                      sd = sd(.,na.rm=TRUE),
                      n=n(),
                      nmissing=sum(is.na(.))))

#reshape using tidyr function
df.stats.tidy<-sumstats %>%
  gather(stat, val) %>%
  separate(stat, into=c("var", "stat"), sep="_") %>%
  spread(stat,val) %>%
  select(var, min, q25, median, q75, max, mean,sd,n, nmissing)


####################################################################
#Create dataset of schools with missing values in our indicators
####################################################################

missings <- school_indicator_data %>%
  filter(is.na(student_knowledge) | is.na(absence_rate) | is.na(content_knowledge) | 
           is.na(pedagogical_knowledge) | is.na(inputs) | is.na(infrastructure)) %>%
  mutate(total_missing=is.na(student_knowledge) + is.na(absence_rate) + is.na(content_knowledge) + 
           is.na(pedagogical_knowledge) + is.na(inputs) + is.na(infrastructure))

missings <- school_indicator_data %>%
  mutate(total_missing=is.na(student_knowledge) + is.na(absence_rate) + is.na(content_knowledge) + 
           is.na(pedagogical_knowledge) + is.na(inputs) + is.na(infrastructure)) %>%
  mutate(total_missing_teach_grd4_assess=is.na(student_knowledge) + is.na(pedagogical_knowledge) )

missings_table<- missings %>%
  group_by(region, total_missing) %>%
  summarise(n=n()) %>%
  spread(region, n, fill=0)

write_dta( missings, "C:/Users/WB469649/WBG/Ezequiel Molina - Dashboard (Team Folder)/Country_Work/Mozambique/2019/Data/school_inicators_data.dta")


####################################################################
#create simple model of indicators related to learning to check for strange outliers
####################################################################
linmod<- lm(student_knowledge ~ absence_rate + content_knowledge + pedagogical_knowledge + inputs + infrastructure + factor(province), data=school_indicator_data)
summary(linmod)

school_indicator_data$student_knowledge_01<-school_indicator_data$student_knowledge/100
logitmod<- glm(student_knowledge_01 ~ absence_rate + content_knowledge + pedagogical_knowledge + inputs + infrastructure + factor(province), family=binomial(), data=school_indicator_data)
summary(logitmod)

school_map <- school_indicator_data %>%
  filter(!is.na(lat) & !is.na(lon))

#impute missing values for model using mice
school_map_imp<-mice(school_map, m=5, maxit=40)

logitmod_imp<- with(school_map_imp, glm(student_knowledge_01 ~ absence_rate + content_knowledge + pedagogical_knowledge + inputs + infrastructure , family=binomial()))
summary(logitmod_imp)

#impute missing values for model using mice
school_indicator_data_imp=mice(school_indicator_data, m=5, maxit=40)

#add fitted values from model
school_indicator_data$fitted_ols<- predict(linmod, school_indicator_data)
school_indicator_data$fitted_logit<- 100*predict(logitmod, school_indicator_data, type="response")

#predicted values from logit and ols model seem to be pretty similar

####################################################################
#######create plotly plot to see outliers based on fitted model#####
####################################################################
outlier_plot <- function(df_input) { df_input %>%
  plot_ly(x= ~df_input$fitted_logit, y=~df_input$student_knowledge, type="scatter", name="Fitted Values",
          text=paste("Name: ", df_input$school, " <br>",
                     "Province: ", df_input$province, " <br>",
                     "District: ", df_input$district, " <br>",
                     "EMIS Code: ", df_input$sch_id, " <br>",
                     "Avg 4th Grade Learning Score:", 100*round(df_input$student_knowledge, digits=2), "% <br>",
                     "Teacher Absence Rate: ", 100*round(df_input$absence_rate, digits=2), "% <br>",
                     "Teacher Content Knowledge Score: ", 100*round(df_input$content_knowledge, digits=2),"% <br>",
                     "Teacher Pedagogical Skill: ", round(df_input$pedagogical_knowledge, digits=2), " <br>",
                     "Basic Inputs: ", round(df_input$inputs, digits=2), " <br>",
                     "Basic Infrastructure: ", round(df_input$infrastructure, digits=2), " <br>",
                     "Capacity for Learning: ", 100*round(df_input$ecd, digits=2), "% <br>",
                     "Operational Management: ", round(df_input$operational_management, digits=2), " <br>",
                     "Instructional Leadership: ", round(df_input$instructional_leadership, digits=2), " <br>",
                     "School Knowledge: ", round(df_input$school_knowledge, digits=2), " <br>",
                     "Management Skills: ", round(df_input$management_skills, digits=2), " <br>" , sep ="")  ) %>%
  add_lines(x=~df_input$fitted_logit, y=~df_input$fitted_logit, name='45 degree line') %>%
  layout(xaxis=list(title="Logit Fitted Values"), yaxis=list(title="4th Grade Student Achievement"))
}

outlier_plot(school_indicator_data)

####################################################################
#######Map of schools with missing school info#####
####################################################################
school_map <- school_indicator_data %>%
  filter(!is.na(lat) & !is.na(lon))

#Create indicator of schools with missing values in our indicators

school_map <- school_map %>%
  mutate(missings=(is.na(student_knowledge) | is.na(absence_rate) | is.na(content_knowledge) | 
                     is.na(pedagogical_knowledge) | is.na(inputs) | is.na(infrastructure)))%>%
  mutate(total_missing=is.na(student_knowledge) + is.na(absence_rate) + is.na(content_knowledge) + 
           is.na(pedagogical_knowledge) + is.na(inputs) + is.na(infrastructure)) %>%
  mutate(total_missing=is.na(student_knowledge) + is.na(absence_rate) + is.na(content_knowledge) + 
           is.na(pedagogical_knowledge) + is.na(inputs) + is.na(infrastructure))

#color coded markers for missing values
getColor <- function(df_input) {
  sapply(df_input$total_missing, function(total_missing) {
    if (total_missing == 0) {
      "green"
    } 
    else if (total_missing == 1) {
      "blue"
    } 
    else if (total_missing == 2) {
      "yellow"
    } 
    else if (total_missing == 3) {
      "orange"
    }     
    else if (total_missing == 4) {
      "purple"
    } 
    else if (total_missing == 5) {
      "red"
    }
    else {
      "black"
    } 
    })
}
#add legend for icons
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = school_map$total_missing
)
icons <- awesomeIcons(
  icon = 'graduation-cap',
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(school_map)
)

map <- function(df_input) {    leaflet() %>% 
  addTiles()  %>%
  addAwesomeMarkers(df_input, lng=df_input$lon, lat= df_input$lat, icon=icons,
                    popup = paste("Name: ", df_input$school, " <br>",
                                  "Province: ", df_input$province, " <br>",
                                  "District: ", df_input$district, " <br>",
                                  "EMIS Code: ", df_input$sch_id, " <br>",
                                  "<font color='red' > Outcomes ", " <br> <font color='black' >",
                                  "Avg 4th Grade Learning Score:", 100*round(df_input$student_knowledge, digits=2), "% <br>",
                                  "<font color='red' > Practice Indicators ", " <br> <font color='black' >",
                                  "Teacher Absence Rate: ", 100*round(df_input$absence_rate, digits=2), "% <br>",
                                  "Teacher Content Knowledge Score: ", 100*round(df_input$content_knowledge, digits=2),"% <br>",
                                  "Teacher Pedagogical Skill: ", round(df_input$pedagogical_knowledge, digits=2), " <br>",
                                  "Basic Inputs: ", round(df_input$inputs, digits=2), " <br>",
                                  "Basic Infrastructure: ", round(df_input$infrastructure, digits=2), " <br>",
                                  "Capacity for Learning: ", 100*round(df_input$ecd, digits=2), "% <br>",
                                  "Operational Management: ", round(df_input$operational_management, digits=2), " <br>",
                                  "Instructional Leadership: ", round(df_input$instructional_leadership, digits=2), " <br>",
                                  "School Knowledge: ", round(df_input$school_knowledge, digits=2), " <br>",
                                  "Management Skills: ", round(df_input$management_skills, digits=2), " <br>" , sep ="")  ) %>%
    addLegend("bottomright", pal=pal, values=df_input$total_missing, title="Missing Indicators"    )

}

map(school_map)
##################################################
#Link outlier plot with map
##################################################

#based on crosstalk package

linked_df<-SharedData$new(school_map)



bscols(
  leaflet(linked_df) %>% 
    addTiles()  %>%
    addAwesomeMarkers( lng=~lon, lat= ~lat, icon=icons )  ,
    plot_ly(linked_df,x= ~fitted_logit, y=~student_knowledge, type="scatter", name="Fitted Values") %>%
      layout(xaxis=list(title="Logit Fitted Values"), yaxis=list(title="4th Grade Student Achievement"))
)


linked_df<-SharedData$new(school_map)

province_df <- function(df_input) {
  df_prov <- df_input %>%
    distinct(province)
  
  kable(df_prov, caption="Provinces in Mozambique")
}



bscols(
  leaflet(linked_df) %>% 
    addTiles()  %>%
    addAwesomeMarkers( lng=~lon, lat= ~lat, icon=icons                        )  ,
  province_df(linked_df)
)
