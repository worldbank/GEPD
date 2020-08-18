library(tidyverse)
library(haven)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(stringr)
library(scales)
library(readxl)
library(glue)
library(httr)
library(jsonlite)
library(wbstats)
library(ggrepel)
library(here)

<<<<<<< HEAD
#get region and income group of the country
region_label <- hci_lp_final[which(hci_lp_final$iso3c==country_file_name),]$admin
income_label <- hci_lp_final[which(hci_lp_final$iso3c==country_file_name),]$income
country_label <- country

hci_lp_final <- hci_lp_final %>%
  mutate(ID = case_when(country_name %in% c(country_label, region_label, income_label) ~ country_name,
                        TRUE ~ as.character(NA)),
         )

#Create figure for learning poverty
lp_figure <-   hci_lp_final %>%
  ggplot(aes(x=LPV_decimal, y= tab)) +
  geom_point(pch = 21, aes(fill= LPV_decimal), size = 2) +
  scale_fill_gradient(low = "grey21", high = "grey100") +
  geom_point(data = hci_lp_final[hci_lp_final$ID == country_label,], aes(x = LPV_decimal, y=tab), 
             pch=21, color = "black", fill = "#ffffb3", size = 3.5) +
  geom_point(data = hci_lp_final[hci_lp_final$ID == region_label,], aes(x = LPV_decimal, y=tab), 
             pch=21, color = "black", fill = "#0066ff", size = 3.5) +
  geom_point(data = hci_lp_final[hci_lp_final$ID == income_label,], aes(x = LPV_decimal, y=tab), 
             pch=21, color = "black", fill = "#ff9933", size = 3.5) +
  #scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = percent) +
  geom_text_repel(data = subset(hci_lp_final, !is.na(ID)), aes(x = LPV_decimal, y = tab, label = iso3c, color = iso3c), 
=======
#########################
# Launch Code
########################

#########################
# Learning poverty
########################

##########
# Pull Ed Stats Data using World Bank API
##########
# make request to World Bank API and get a list of all EdStats indicators (including learning poverty)
EdStatsRequest <- GET(url = "http://api.worldbank.org/v2/indicator?per_page=20000&format=json&source=12")
EdStatsResponse <- content(EdStatsRequest, as = "text", encoding = "UTF-8")

# Parse the JSON content and convert it to a data frame.
EdStatsJSON <- fromJSON(EdStatsResponse, flatten = TRUE) %>%
  data.frame()

# Create a list of indicators to pull from edstats. More can be added as needed
learning_pov_indicators<- 'SE.LPV.PRIM' # Pupils below minimum reading proficiency at end of primary (%). Low GAML threshold

#get WDI metadata infor
cache_list<-wbstats::wbcache()
country_list <- wbstats::wbcountries()

#get region and income group of the country
region_label <- country_list[which(country_list$iso3c==country_file_name),]$admin
income_label <- country_list[which(country_list$iso3c==country_file_name),]$income
country_label <- country
#set reference year to 2019 (Year of survey)
reference_year<-as.numeric(year)

# Now use wbstats package in R to pull from World Bank API

learning_poverty_df <-wbstats::wb(country="all", #by specifying all, we can get a list of all aggregates (such as region or income level)
                                  indicator=learning_pov_indicators,
                                  startdate=reference_year-10,
                                  enddate=reference_year,
                                  return_wide = T,
                                  removeNA=TRUE,
                                  cache = cache_list
) %>%
  filter(((reference_year-as.numeric(date))<=10) & (reference_year>=as.numeric(date))) %>% #filter out years outside reference window of 10 years     
  group_by(iso3c, country) %>%
  filter(as.numeric(date)==max(as.numeric(date))) %>% #group by country to create one observation per country which is the latest value
  mutate(tab = 0 , 
         SE.LPV.PRIM = as.numeric(SE.LPV.PRIM),
         ID = case_when(country %in% c(country_label,region_label,income_label) ~ country,
                        TRUE ~ as.character(NA)),
         grx = SE.LPV.PRIM/100)

#Create figure for learning poverty
lp_figure <-   learning_poverty_df %>%
  ggplot(aes(x=grx, y= tab)) +
  geom_point(pch = 21, aes(fill= grx), size = 2) +
  scale_fill_gradient(low = "grey21", high = "grey100") +
  geom_point(data = learning_poverty_df[learning_poverty_df$ID == country_label,], aes(x = grx, y=tab), 
             pch=21, color = "black", fill = "#ffffb3", size = 3.5) +
  geom_point(data = learning_poverty_df[learning_poverty_df$ID == region_label,], aes(x = grx, y=tab), 
             pch=21, color = "black", fill = "#0066ff", size = 3.5) +
  geom_point(data = learning_poverty_df[learning_poverty_df$ID == income_label,], aes(x = grx, y=tab), 
             pch=21, color = "black", fill = "#ff9933", size = 3.5) +
  #scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = percent) +
  geom_text_repel(data = subset(learning_poverty_df, !is.na(ID)), aes(x = grx, y = tab, label = iso3c, color = iso3c), 
>>>>>>> d19ef3fd968d6ca00fe4446c60f3c37f7e09ef76
                  fontface = 'bold', size = 3.5, box.padding = 2, segment.color = 'black', ylim = c(0,0.1), force = 1) +
  scale_colour_manual(values = c("#282828", "#0066ff", "#ff9933")) +
  ylim(-0.1,0.1)+
  annotate("text", x = c(0,0.25,0.5,0.75,1), y = -0.06, label = c("0%", "25%", "50%", "75%", "100%")) +
<<<<<<< HEAD
=======
  labs(x = NULL, y = NULL) +
>>>>>>> d19ef3fd968d6ca00fe4446c60f3c37f7e09ef76
  theme_void() +
  theme(axis.ticks.length = unit(0, "pt"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "mm"))
<<<<<<< HEAD
ggsave(here("LP_LAYS_figures", paste("lp_figure_", country_file_name, ".png", sep = '')), height = 1.5, width = 5)

#Set objects for use in learning poverty text
lp_num_country <- hci_lp_final$`SE.LPV.PRIM`[hci_lp_final$country == country_label]
lp_region <- hci_lp_final$`SE.LPV.PRIM`[hci_lp_final$country == region_label]
lp_income <- hci_lp_final$SE.LPV.PRIM[hci_lp_final$country == income_label]
diff_lp_regionval <- round(if_else(lp_num_country>lp_region, lp_num_country-lp_region, lp_region-lp_num_country), digits =0)
diff_lp_regionval_vf <- diff_lp_regionval[1]
diff_lp_regiontext <- if_else(lp_num_country>lp_region, glue("{diff_lp_regionval_vf}% points worse"), glue("{diff_lp_regionval_vf}% points better"))
diff_lp_incomeval <- round(if_else(lp_num_country>lp_income, lp_num_country-lp_income, lp_region-lp_income), digits =0)
diff_lp_incomeval_vf <- diff_lp_incomeval[1]
diff_lp_incometext <- if_else(lp_num_country>lp_income, glue("{diff_lp_incomeval_vf}% points worse"), glue("{diff_lp_incomeval_vf}% points better"))
=======
ggsave(here("LP figures", paste("lp_figure_", country_file_name, ".png", sep = '')), height = 1.5, width = 5)

#Set objects for use in learning poverty text
lp_num_country <- round(learning_poverty_df$`SE.LPV.PRIM`[learning_poverty_df$country == country_label], digits = 0)
lp_region <- round(learning_poverty_df$`SE.LPV.PRIM`[learning_poverty_df$country == region_label], digits = 0)
lp_category <- round(learning_poverty_df$SE.LPV.PRIM[learning_poverty_df$country == income_label], digits = 0)
diff_lp_regionval <- if_else(lp_num_country>lp_region, lp_num_country-lp_region, lp_region-lp_num_country)
diff_lp_regionval_vf <- diff_lp_regionval[1]
diff_lp_regiontext <- if_else(lp_num_country>lp_region, glue("{diff_lp_regionval_vf}% points worse"), glue("{diff_lp_regionval_vf}% points better"))
diff_lp_catval <- if_else(lp_num_country>lp_category, lp_num_country-lp_category, lp_region-lp_category)
diff_lp_catval_vf <- diff_lp_catval[1]
diff_lp_cattext <- if_else(lp_num_country>lp_category, glue("{diff_lp_catval_vf}% points worse"), glue("{diff_lp_catval_vf}% points better"))
>>>>>>> d19ef3fd968d6ca00fe4446c60f3c37f7e09ef76
learning_poverty_str <- str_c(lp_num_country, "%", sep = "")

##########
# LAYS figure
##########

<<<<<<< HEAD
#Create LAYS figure

segment_data <- data.frame(
  x = c(2,5,7.5,10,12.5),
  xend = c(2,5,7.5,10,12.5),
  y = c(-0.05, -0.05, -0.05, -0.05, -0.05),
  yend = c(-0.04, -0.04, -0.04, -0.04,-0.04)
)

lays_figure <-   hci_lp_final %>%
  ggplot(aes(x=HD.HCI.LAYS, y=tab)) +
  geom_point(pch = 21, aes(fill= HD.HCI.LAYS), size = 2) +
  scale_fill_gradient(low = "grey21", high = "grey100") +
  geom_point(data = hci_lp_final[hci_lp_final$ID == country_label,], aes(x = HD.HCI.LAYS, y=tab), 
             pch=21, color = "black", fill = "#ffffb3", size = 3.5) +
  geom_point(data = hci_lp_final[hci_lp_final$ID == region_label,], aes(x = HD.HCI.LAYS, y=tab), 
             pch=21, color = "black", fill = "#0066ff", size = 3.5) +
  geom_point(data = hci_lp_final[hci_lp_final$ID == income_label,], aes(x = HD.HCI.LAYS, y=tab), 
             pch=21, color = "black", fill = "#ff9933", size = 3.5) +
  #scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = percent) +
  geom_text_repel(data = subset(hci_lp_final, !is.na(ID)), aes(x = HD.HCI.LAYS, y = tab, label = iso3c, color = iso3c), 
                  fontface = 'bold', size = 3.5, box.padding = 2, segment.color = 'black', ylim = c(0,0.1), force = 1) +
  scale_colour_manual(values = c("#282828", "#0066ff", "#ff9933")) +
  ylim(-0.1,0.1)+
  annotate("text", x = c(2,5,7.5,10,12.5), y = -0.06, label = c("2 years", "5 years", "7.5 years", "10 years", "12.5 years"), size = 3) +
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend), colour = "black", size = 0.3) +
=======
##########
# Pull  Data using World Bank API
##########

#Pulll data for the Human Capital INdex - Learning Adjusted Years of Schooling:
hci_df <- GET(url = "http://api.worldbank.org/v2/country/all/indicator/HD.HCI.LAYS?per_page=500&format=json") %>%
  content( as = "text", encoding = "UTF-8") %>%
  fromJSON( flatten = TRUE) %>%
  data.frame()

country_temp <- country_list %>%
  select(iso3c, admin, income)

hci_df <- hci_df %>%
  rename(iso3c = countryiso3code) %>%
  left_join(country_temp, by = "iso3c")

#get region, income group name and HCI value of the country, add World HCI to table
region_label_short <- country_list[which(country_list$iso3c==country_file_name),]$adminID
income_label_short <- country_list[which(country_list$iso3c==country_file_name),]$incomeID
country_label <- country

#Country's Region and income group LAYS value
lays_region <- round(mean(hci_df$value[hci_df$admin == region_label], na.rm = TRUE), digits = 1)
lays_income <- round(mean(hci_df$value[hci_df$income == income_label], na.rm = TRUE), digits = 1)
lays_world <- round(mean(hci_df$value, na.rm = TRUE), digits = 1)

new_names <- c(region_label_short, income_label_short, "World average")
new_lays <- c(lays_region, lays_income, lays_world)

hci_df <- hci_df %>%
  mutate(ID = case_when(iso3c == country_file_name ~ iso3c)) %>%
  add_row(iso3c = new_names, value = new_lays, ID = new_names) %>%
  mutate(value = round(value, digits = 1),
         ID = case_when(!is.na(ID) ~ glue("{iso3c}({value} years)")),
         tab= 0)

#Create LAYS figure
lays_figure <-   hci_df %>%
  ggplot(aes(x=value, y=tab )) +
  geom_point(pch = 21, aes(fill= value), size = 2) +
  scale_fill_gradient(low = "grey21", high = "grey100") +
  geom_point(data = hci_df[hci_df$country.value == country_label,], aes(x = value, y=tab), 
             pch=21, color = "black", fill = "#ffffb3", size = 3.5) +
  geom_point(data = hci_df[hci_df$iso3c == region_label_short,], aes(x = value, y=tab), 
             pch=21, color = "black", fill = "#0066ff", size = 3.5) +
  geom_point(data = hci_df[hci_df$iso3c == income_label_short,], aes(x = value, y=tab), 
             pch=21, color = "black", fill = "#ff9933", size = 3.5) +
  geom_point(data = hci_df[hci_df$iso3c == 'World average',], aes(x = value, y=tab), 
             pch=21, color = "black", fill = "#ADFF2F", size = 3.5) +
  #scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = percent) +
  geom_text_repel(data = hci_df[hci_df$country.value == country_label,], aes(x = value, y = tab, label = ID), color = "#282828", 
                  fontface = 'bold', size = 3, box.padding = 2, direction = "y", segment.color = 'black', 
                  ylim = c(0,0.1), force =2) +
  geom_text_repel(data = subset(hci_df, is.na(country.value)), aes(x = value, y = tab, label = ID, color = iso3c), 
                  fontface = 'bold', size = 3, box.padding = 2, segment.color = 'black', 
                  ylim = c(0,-0.1), force =2) +
  scale_colour_manual(values = c("#ff9933","#0066ff","#006400")) +
  ylim(-0.1,0.1) +
  labs(x = NULL, y = NULL) +
>>>>>>> d19ef3fd968d6ca00fe4446c60f3c37f7e09ef76
  theme_void() +
  theme(axis.ticks.length = unit(0, "pt"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "mm"))
<<<<<<< HEAD
ggsave(here("LP_LAYS_figures", paste("lays_figure_", country_file_name, ".png", sep = '')), height = 1.5, width = 5)

#Setting objects for LAYS text
lays_num_country <- hci_lp_final$HD.HCI.LAYS[hci_lp_final$country_name == country_label][1]
lays_region <- hci_lp_final$`HD.HCI.LAYS`[hci_lp_final$country == region_label]
lays_income <- hci_lp_final$HD.HCI.LAYS[hci_lp_final$country == income_label]
diff_lays_regionval <- round(if_else(lays_num_country>lays_region, lays_num_country-lays_region, lays_region-lays_num_country), digits =1)
diff_lays_regiontext <- if_else(lays_num_country>lays_region, glue("{diff_lays_regionval} years higher"), glue("{diff_lays_regionval} years lower"))
diff_lays_income <- round(if_else(lays_num_country>lays_income, lays_num_country-lays_income, lays_income-lays_num_country), digits =1)
diff_lays_inctext <- if_else(lays_num_country>lays_income, glue("{diff_lays_income} years higher"), glue("{diff_lays_income} years lower"))
lays_80 <- round(quantile(hci_lp_final$HD.HCI.LAYS, probs = 0.8, na.rm = TRUE), digits =1)
lays_60 <- round(quantile(hci_lp_final$HD.HCI.LAYS, probs = 0.6, na.rm = TRUE), digits =1)

#Set marker for which indicator and figure will be displayed
#Learning poverty is displayed if present, if absent, LAYS will be shown instead
marker <- if_else(is.na(hci_lp_final[which(hci_lp_final$iso3c==country_file_name),]$SE.LPV.PRIM), 0, 1)
#Grade 4 proficiency
grade4_overall <- if_else(input$value[input$Series=="SE.PRM.LERN"]<1,
                          round(input$value[input$Series=="SE.PRM.LERN"], digits = 1),
                          round(input$value[input$Series=="SE.PRM.LERN"], digits = 0))

#Set text for section headline, main paragraph, figure headline and figure Notes.
lern_section_headline <- if_else(marker == 0,
                           glue("LEARNING OUTCOMES: {lays_num_country} LEARNING ADJUSTED YEARS IN SCHOOL, {grade4_overall}% GEPD PROFICIENCY IN GRADE 4"),
                           glue("LEARNING OUTCOMES: {learning_poverty_str} LEARNING POVERTY, {grade4_overall}% GEPD PROFICIENCY IN GRADE 4"))

LP_para <-   paste("Learning poverty is defined as the share of children at end of primary age below minimum reading proficiency, adjusted for out of school children. Learning poverty in ", country, " is ", diff_lp_regiontext, " than the average for ", region_label, " region and ", diff_lp_incometext, " than the average for ", income_label, " countries. ", sep = "")
LAYS_para <- paste("Learning adjusted years of school (LAYS) is calculated by adjusting expected years of schooling for schooling quality. Learning adjusted years of schooling in ",country," is ", diff_lays_regiontext, " than the average for ", region_label, " region and ", diff_lays_inctext, " than the average for " , income_label, " countries.", sep = "")

lern_para <- if_else(marker == 0, LAYS_para, LP_para)

figure_headline <- if_else(marker == 0,
                       glue("Figure 2. Learning adjusted years in school comparison"),
                       glue("Figure 2. Leaning poverty comparison"))

figure_note <- if_else(marker == 0,
                       glue("Grey circles represent other countries. Yellow circle represents {country}. Other circles represent average LAYS in {country}'s region and income group."),
                       glue("Grey circles represent other countries. Yellow circle represents {country}. Other circles represent average learning poverty in {country}'s region and income group."))
lays <- "Learning adjusted years in schooling (in years)"
lays_units <- ""
lp <- "Learning poverty"
other_units <- "\\%"

=======
ggsave(here("LP figures", paste("lays_figure_", country_file_name, ".png", sep = '')), height = 1.5, width = 5)

#Setting objects for LAYS text
lays_num_country <- round(hci_df$value[hci_df$country.value == country_label], digits = 1)[1]
diff_lays_regionval <- round(if_else(lays_num_country>lays_region, lays_num_country-lays_region, lays_region-lays_num_country), digits =1)
diff_lays_regiontext <- if_else(lays_num_country>lays_region, glue("{diff_lays_regionval} years higher"), glue("{diff_lays_regionval} years lower"))
diff_lays_income <- round(if_else(lays_num_country>lays_income, lays_num_country-lays_income, lays_income-lays_num_country), digits = 1)
diff_lays_inctext <- if_else(lays_num_country>lays_income, glue("{diff_lays_income} years higher"), glue("{diff_lays_income} years lower"))

#########################
# LP vs. LAYS in table display
########################

lays <- "Learning adjusted years of schooling (in years)"
lp <- "Learning poverty"

lays_units <- ""
lp_units <- "%"
>>>>>>> d19ef3fd968d6ca00fe4446c60f3c37f7e09ef76
