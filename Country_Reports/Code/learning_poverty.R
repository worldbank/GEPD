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

#########################
# Launch Code
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
                  fontface = 'bold', size = 3.5, box.padding = 2, segment.color = 'black', ylim = c(0,0.1), force = 1) +
  scale_colour_manual(values = c("#282828", "#0066ff", "#ff9933")) +
  ylim(-0.1,0.1)+
  annotate("text", x = c(0,0.25,0.5,0.75,1), y = -0.06, label = c("0%", "25%", "50%", "75%", "100%")) +
  theme_void() +
  theme(legend.position = "none")
ggsave(here("LP figures", paste("lp_figure_", country_file_name, ".png", sep = '')), height = 1.5, width = 5)


lp_num_country <- round(learning_poverty_df$`SE.LPV.PRIM`[learning_poverty_df$country == country_label], digits = 0)
lp_region <- round(learning_poverty_df$`SE.LPV.PRIM`[learning_poverty_df$country == region_label], digits = 0)
lp_category <- round(learning_poverty_df$SE.LPV.PRIM[learning_poverty_df$country == income_label], digits = 0)
diff_lp_regionval <- if_else(lp_num_country>lp_region, lp_num_country-lp_region, lp_region-lp_num_country)
diff_lp_regionval_vf <- diff_lp_regionval[1]
diff_lp_regiontext <- if_else(lp_num_country>lp_region, glue("{diff_lp_regionval_vf}% points worse"), glue("{diff_lp_regionval_vf}% points better"))
diff_lp_catval <- if_else(lp_num_country>lp_category, lp_num_country-lp_category, lp_region-lp_category)
diff_lp_catval_vf <- diff_lp_catval[1]
diff_lp_cattext <- if_else(lp_num_country>lp_category, glue("{diff_lp_catval_vf}% points worse"), glue("{diff_lp_catval_vf}% points better"))
learning_poverty_str <- str_c(lp_num_country, "%", sep = "")