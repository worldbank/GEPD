#load data
library(tidyverse)
library(haven)
library(stats)
library(ggcorrplot)


load('//wbgfscifs01/GEDEDU/datalib-edu/projects/GEPD-Confidential/CNT/PER/PER_2019_GEPD/PER_2019_GEPD_v01_RAW/Data/confidential/School/school_indicators_data.RData')

geoinfo<-read_dta('//wbgfscifs01/GEDEDU/datalib-edu/projects/GEPD-Confidential/CNT/PER/PER_2019_GEPD/PER_2019_GEPD_v01_RAW/Data/Maps/edu_per_w_geovars_2020_0224.dta')

geoinfo<- geoinfo %>%
  mutate(school_code=as.numeric(cod_mod))

school_gdp_merge <- school_dta_short %>%
  left_join(school_gdp) %>%
  left_join(geoinfo)

cor(school_gdp_merge$GDP, school_gdp_merge$mn_gdp2010)

cor(school_gdp_merge$GDP, school_gdp_merge$mn_aggdp2010)


nums <- unlist(lapply(school_gdp_merge, is.numeric))  

df_corr_plot <-    round(cor(school_gdp_merge[,nums], use="complete.obs"), 2)


pcorr<- ggcorrplot(df_corr_plot,
                   outline.color = "white",
                   ggtheme = theme_bw(),
                   colors = c("#F8696B", "#FFEB84", "#63BE7B"),
                   legend.title = "Correlation",
                   title = "Correlation Between Geo Items") + 
  theme(
    text = element_text(size = 16),
  )

pcorr