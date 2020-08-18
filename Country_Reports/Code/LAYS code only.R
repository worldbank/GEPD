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
# Pull  Data using World Bank API
##########

#Pulll data for the Human Capital INdex - Learning Adjusted Years of Schooling:
hci_df <- GET(url = "http://api.worldbank.org/v2/country/all/indicator/HD.HCI.LAYS?per_page=500&format=json") %>%
  content( as = "text", encoding = "UTF-8") %>%
  fromJSON( flatten = TRUE) %>%
  data.frame()

#get WDI metadata infor
cache_list<-wbstats::wbcache()
country_list <- wbstats::wbcountries()

country_temp <- country_list %>%
  select(iso3c, admin, income)

hci_df <- hci_df %>%
  rename(iso3c = countryiso3code) %>%
  left_join(country_temp, by = "iso3c")

#get region, income group name and HCI value of the country, add World HCI to table
region_label <- country_list[which(country_list$iso3c==country_file_name),]$admin
region_label_short <- country_list[which(country_list$iso3c==country_file_name),]$adminID
income_label <- country_list[which(country_list$iso3c==country_file_name),]$income
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
                  fontface = 'bold', size = 3.5, box.padding = 2, direction = "y", segment.color = 'black', 
                  ylim = c(0,0.1), force =2) +
  geom_text_repel(data = subset(hci_df, is.na(country.value)), aes(x = value, y = tab, label = ID, color = iso3c), 
                  fontface = 'bold', size = 3.5, box.padding = 2, segment.color = 'black', 
                  ylim = c(0,-0.1), force =2) +
  scale_colour_manual(values = c("#ff9933","#0066ff","#006400")) +
  ylim(-0.1,0.1) +
  theme_void() +
  theme(legend.position = "none")
ggsave(here("LP figures", paste("lays_figure_", country_file_name, ".png", sep = '')), height = 1.5, width = 5)


lays_num_country <- round(hci_df$value[hci_df$country.value == country_label], digits = 1)[1]
diff_lays_regionval <- round(if_else(lays_num_country>lays_region, lays_num_country-lays_region, lays_region-lays_num_country), digits =1)
diff_lays_regiontext <- if_else(lays_num_country>lays_region, glue("{diff_lays_regionval} years higher"), glue("{diff_lays_regionval} years lower"))
diff_lays_income <- round(if_else(lays_num_country>lays_income, lays_num_country-lays_income, lays_income-lays_num_country), digits = 1)
diff_lays_inctext <- if_else(lays_num_country>lays_income, glue("{diff_lays_income} years higher"), glue("{diff_lays_income} years lower"))
lays_country_str <- str_c(lays_num_country, "years", sep = " ")
