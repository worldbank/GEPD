#File written by Brian Stacy on July 30, 2020
# File will produce automated country briefs of 2 pages and 4 pages
  
# Load libraries
library(rmarkdown)
library(tidyverse)
library(here)
library(tidyr)
library(stringr)
library(scales)
library(readxl)
library(glue)
library(httr)
library(jsonlite)
library(wbstats)
library(ggrepel)
library(haven)


#anchor directory to correct place
here()
setwd(here())
#Set year
year <- "2019"
#Load required file for creating figures for Learning poverty and LAYS
source('./Code/HCI_LP_fileload.R')
#############
# Rwanda
#############

#Set the country name here
country_file_name <- "RWA"
country <- "Rwanda"
country_year <- "2020"

takeaway <- "
- Substantially low learning outcomes observed for students in Grade 1 and 4.
- Weak teacher pedagogical skills and low teacher content knowledge attributed to poor teaching support and weak monitoring and accountability systems.
- Grade 1 proficiency of students is ~9%, with students scoring lower on executive functions and socio-emotional learning. Only 13% students are enrolled in early childhood programs which face gaps in caregiver skills and financial constraints.
- Basic inputs and infrastructure are weak in areas of functional toilets and electricity in schools.
- Major gaps are seen in implementation of teaching support policies, teaching monitoring and accountability systems and selection and deployment policies for school principals.
- Primary education funding amount and efficiency of spending is low and education policy implementation is politicized, lowering bureaucratic capacity.
"

# 2 Pager
rmarkdown::render('./Code/GEPD_Brief_2page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "2Pager.pdf", sep=''), 
                  output_dir = './Output/')

#4 Pager
rmarkdown::render('./Code/GEPD_Brief_4page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "4Pager.pdf", sep=''), 
                  output_dir = './Output/')

#############
# Peru
#############

#Set the country name here
country_file_name <- "PER"
country <- "Peru"
country_year <- "2020"

takeaway <- "
- Learning poverty is substantial, and rural children lag far beyond urban children in learning.
- Disparities are driven by differences in practices (less than ½ of variation explained by within-school differences).
- Of all practice indicators, teacher skills & children's capacity for learning at primary entry explain low learning the best.
- Infrastructure is advanced, but internet access and accessibility to kids with disabilities are still poor (just 8% & 54% in rural schools).
- Policy frameworks are comprehensive, but the de facto implementation varies across types of policies.  E.g. teaching support.
- Bureaucratic capacity scores are affected by the lack of recognition of good performance for individuals, units, and departments. 
"
# 2 Pager
rmarkdown::render('./Code/GEPD_Brief_2page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "2Pager.pdf", sep=''), 
                  output_dir = './Output/')

#4 Pager
rmarkdown::render('./Code/GEPD_Brief_4page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "4Pager.pdf", sep=''), 
                  output_dir = './Output/')



#############
# Jordan
#############

#Set the country name here
country_file_name <- "JOR"
country <- "Jordan"
country_year <- "2020"

takeaway <- "
- 52% learning poverty is observed in Grade 4. GEPD Grade 4 student proficiency (>80% student knowledge) is only 4%, attributed to low numeracy proficiency 2%.
- Low teacher content knowledge attributed to poor teacher support and lack of strong instructional leadership (especially principal feedback on classroom observation)
- Low capacity for learning in Grade 1 (particularly socio-emotional knowledge) is attributed to low enrolment in early childhood programs (13%)
- A ~0.8 point difference exists in the policy de-jure and de-facto indicators on a 5 point scale for teaching and school management. Major gaps are seen in teacher support, teacher motivation and evaluation of school management.
- Quality of teaching support emerges as a major area requiring intervention, particularly through in-classroom support/feedback and pre-service training.
- Bureaucracy scores high on mandates and accountability but low on impartial decision making
"

# 2 Pager
rmarkdown::render('./Code/GEPD_Brief_2page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "2Pager.pdf", sep=''), 
                  output_dir = './Output/')

#4 Pager
rmarkdown::render('./Code/GEPD_Brief_4page.Rmd',  
                  output_file =  paste(country,"_", year,"_", "4Pager.pdf", sep=''), 
                  output_dir = './Output/')
