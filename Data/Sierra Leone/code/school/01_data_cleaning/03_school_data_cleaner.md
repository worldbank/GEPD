

```r
#Clean data files downloaded from API
#Written by Brian Stacy 6/14/2019

#load relevant libraries

library(skimr)
library(naniar)
library(vtable)
```

```
## Warning: package 'vtable' was built under R version 4.1.3
```

```
## Loading required package: kableExtra
```

```
## Warning: package 'kableExtra' was built under R version 4.1.3
```

```
## 
## Attaching package: 'kableExtra'
```

```
## The following objects are masked from 'package:flextable':
## 
##     as_image, footnote
```

```
## The following object is masked from 'package:dplyr':
## 
##     group_rows
```

```r
library(digest)
```

```
## Warning: package 'digest' was built under R version 4.1.3
```

```r
library(tidyverse)
library(haven)
library(stringr)
library(Hmisc)
#NOTE:  The R script to pull the data from the API should be run before this file



#Create function to save metadata for each question in each module
makeVlist <- function(dta) { 
  varlabels <- sapply(dta, function(x) attr(x,"label"))
  vallabels <- sapply(dta, function(x) attr(x,"labels"))
  tibble(name = names(varlabels),
         varlabel = varlabels, vallabel = vallabels) 
}







############################
#read in teacher roster file
############################

teacher_roster<-read_dta(file.path(download_folder, "TEACHERS.dta")) %>%
  mutate(teacher_name=m2saq2,
         teacher_number=TEACHERS__id)



###########################
#read in school level file
###########################
school_dta<-read_dta(file.path(download_folder, "EPDash_school_survey_v1.dta"))
```

```
## Error: 'C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT//SLE/SLE_2022_GEPD/SLE_2022_GEPD_v01_RAW/Data/raw/School/EPDash_school_survey_v1.dta' does not exist.
```

```r
vtable(school_dta)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>school_dta</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:left;"> Class </th>
   <th style="text-align:left;"> Label </th>
   <th style="text-align:left;"> Values </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> interview__key </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Interview key (identifier in XX-XX-XX-XX format) </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> interview__id </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Unique 32-character long identifier of the interview </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_name_preload </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> school_name_preload </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_province_preload </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> school_province_preload </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_district_preload </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> school_district_preload </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_chiefdom_preload </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> school_chiefdom_preload </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_section_preload </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> school_section_preload </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_town_preload </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> school_town_preload </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_emis_preload </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> school_emis_preload </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__0 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:0 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:2 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__3 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__4 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:4 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__5 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:5 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__6 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:6 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__7 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:7 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__8 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:8 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__9 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:9 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__10 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:10 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__11 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:11 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__12 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:12 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__13 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:13 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__14 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:14 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__15 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:15 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__16 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:16 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__17 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:17 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__18 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:18 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__19 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:19 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__20 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:20 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__21 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:21 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__22 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:22 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__23 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:23 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__24 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:24 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__25 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:25 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__26 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:26 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__27 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:27 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__28 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:28 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__29 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:29 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__30 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:30 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__31 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:31 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__32 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:32 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__33 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:33 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__34 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:34 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__35 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:35 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__36 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:36 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__37 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:37 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__38 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:38 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__39 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:39 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__40 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:40 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__41 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:41 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__42 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:42 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__43 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:43 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__44 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:44 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__45 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:45 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__46 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:46 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__47 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:47 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__48 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:48 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__49 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:49 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__50 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:50 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__51 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:51 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__52 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:52 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__53 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:53 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__54 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:54 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__55 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:55 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__56 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:56 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__57 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:57 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__58 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:58 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__59 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:59 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__60 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:60 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__61 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:61 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__62 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:62 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__63 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:63 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__64 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:64 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__65 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:65 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__66 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:66 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__67 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:67 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__68 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:68 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__69 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:69 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__70 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:70 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__71 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:71 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__72 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:72 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__73 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:73 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__74 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:74 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__75 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:75 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__76 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:76 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__77 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:77 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__78 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:78 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__79 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:79 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__80 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:80 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__81 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:81 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__82 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:82 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__83 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:83 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__84 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:84 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__85 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:85 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__86 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:86 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__87 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:87 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__88 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:88 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__89 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:89 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__90 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:90 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__91 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:91 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__92 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:92 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__93 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:93 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__94 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:94 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__95 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:95 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__96 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:96 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__97 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:97 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__98 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:98 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enumerators_preload__99 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Preloaded Enumerators:99 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q1_name </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Enumerator Name </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q1_name_other </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Enumerator Name </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q1_comments </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Enumerator Comments </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> school_info_correct </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is the school information displayed correct? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q2_name </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Please enter the School Name </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q2_code </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Please enter the Survey Code </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q2_emis </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Please enter the EMIS Code/School Number </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q1 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is School Open </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q8 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> current time </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q9__Latitude </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Current Location: Latitude </td>
   <td style="text-align:left;"> Num: 7.255 to 9.76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q9__Longitude </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Current Location: Longitude </td>
   <td style="text-align:left;"> Num: -13.282 to -10.308 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q9__Accuracy </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Current Location: Accuracy </td>
   <td style="text-align:left;"> Num: 0.609 to 49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q9__Altitude </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Current Location: Altitude </td>
   <td style="text-align:left;"> Num: -309.6 to 45055.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q9__Timestamp </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Current Location: Timestamp </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modules__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which modules will you be completing in this school?:Module 1 - Rosters </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modules__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which modules will you be completing in this school?:Module 2 - (Principal) Scho </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modules__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which modules will you be completing in this school?:Module 3 - (Principal) Scho </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modules__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which modules will you be completing in this school?:Module 4 - Teacher Question </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modules__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which modules will you be completing in this school?:Module 5 - Teacher Assessme </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modules__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which modules will you be completing in this school?:Module 6 - Early Childhood </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modules__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which modules will you be completing in this school?:Module 7 - 4th Grade Classr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modules__8 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which modules will you be completing in this school?:Module 8 - 4th Grade Studen </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> modules__9 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which modules will you be completing in this school?:Module 9 - 2nd Grade Classr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many teachers (permanent or privately/locally recruited) work in this school </td>
   <td style="text-align:left;"> Num: 1 to 25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__0 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:0 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:2 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__3 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__4 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:4 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__5 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:5 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__6 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:6 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__7 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:7 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__8 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:8 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__9 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:9 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__10 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:10 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__11 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:11 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__12 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:12 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__13 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:13 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__14 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:14 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__15 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:15 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__16 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:16 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__17 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:17 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__18 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:18 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__19 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:19 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__20 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:20 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__21 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:21 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__22 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:22 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__23 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:23 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__24 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:24 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__25 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:25 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__26 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:26 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__27 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:27 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__28 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:28 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m2saq2__29 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher list:29 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> numEligible </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Number of eligible teachers </td>
   <td style="text-align:left;"> Num: 0 to 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> i1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Index of the first selected person </td>
   <td style="text-align:left;"> Num: 1 to 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> i2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Index of the second selected person </td>
   <td style="text-align:left;"> Num: 1 to 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> i3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Index of the third selected person </td>
   <td style="text-align:left;"> Num: 1 to 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> i4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Index of the fourth selected person </td>
   <td style="text-align:left;"> Num: 1 to 19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> i5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Index of the fifth selected person </td>
   <td style="text-align:left;"> Num: 1 to 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> name1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Name of the first selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> name2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Name of the second selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> name3 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Name of the third selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> name4 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Name of the fourth selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> name5 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Name of the second selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grade1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Grade of the first selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grade2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Grade of the second selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grade3 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Grade of the third selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grade4 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Grade of the fourth selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grade5 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Grade of the fifth selected person </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> available1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Availability of the first selected person </td>
   <td style="text-align:left;"> Num: 1 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> available2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Availability of the second selected person </td>
   <td style="text-align:left;"> Num: 1 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> available3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Availability of the third selected person </td>
   <td style="text-align:left;"> Num: 1 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> available4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Availability of the fourth selected person </td>
   <td style="text-align:left;"> Num: 1 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> available5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Availability of the fifth selected person </td>
   <td style="text-align:left;"> Num: 1 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> teacher_phone_number1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> phone number for 1st teacher </td>
   <td style="text-align:left;"> Num: 0 to 999999999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> teacher_phone_number2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> phone number for 2nd teacher </td>
   <td style="text-align:left;"> Num: 999 to 999999999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> teacher_phone_number3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> phone number for third teacher </td>
   <td style="text-align:left;"> Num: 999 to 743131717 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> teacher_phone_number4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> phone number for 4th teacher </td>
   <td style="text-align:left;"> Num: 999 to 99999999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> teacher_phone_number5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> phone number for 5thteacher </td>
   <td style="text-align:left;"> Num: 999 to 999999999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q2_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is the road leading to the school accessible to a student in wheelchair? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q3_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are there steps leading up to the main entrance? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q4_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there a proper ramp in good condition usable by a person in a wheelchair? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q5_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is the main entrance to the school wide enough for a person in a wheelchair to e </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q6 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Did the respondent agree to be interviewed </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1s0q7 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> If refused, reason for refusal </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq1_first </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> What is your first name? </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq1_last </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> What is your last name? </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Mobile Phone Number </td>
   <td style="text-align:left;"> Num: 30027041 to 999999999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq2b </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please, can we have the school landline number? </td>
   <td style="text-align:left;"> Num: 0 to 999999999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq3 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Position at school </td>
   <td style="text-align:left;"> Num: 1 to 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq3_other </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq4 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> school type </td>
   <td style="text-align:left;"> Num: 1 to 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq5 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> What is the school category? </td>
   <td style="text-align:left;"> Num: 1 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq6a </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Language of instruction </td>
   <td style="text-align:left;"> Num: 1 to 99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq6a_other </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specified language if other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq6b </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Language of instruction </td>
   <td style="text-align:left;"> Num: 1 to 99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq6b_other </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specified language if other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Number of students </td>
   <td style="text-align:left;"> Num: 40 to 2006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq8 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> How many of them are boys? </td>
   <td style="text-align:left;"> Num: 0 to 550 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq8a_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> How many grade 5 students are currently enrolled in this school? </td>
   <td style="text-align:left;"> Num: 0 to 170 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1saq9_lnut </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there a public school feeding program at this school? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq1_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> What is the main pupil toilet facility used at the school? </td>
   <td style="text-align:left;"> Num: 1 to 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq2_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are the toilets/latrines separate for girls and boys? </td>
   <td style="text-align:left;"> Num: 0 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq3_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are the pupil's toilets clean? </td>
   <td style="text-align:left;"> Num: 1 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq4_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are the pupils' toilets private </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq5_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are the pupils' toilets useable </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq6_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are the toilets accessible to a student with physical disabilities? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq7_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are there handwashing facilities at the school? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq8_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are both, soap and water, currently available </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq9_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> What is the main source of drinking water </td>
   <td style="text-align:left;"> Num: 1 to 99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq9_other_infr </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq10_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is drinking water from the main source currently available </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq11_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Does the school have access to electricity? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq12_inpt </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many PCs, laptops, and/or tablets available </td>
   <td style="text-align:left;"> Num: 0 to 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq12a_inpt_etri </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many of the devices are in working condition at this school? </td>
   <td style="text-align:left;"> Num: 0 to 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq13_inpt </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are the PCs, laptops, and/or tablets functional </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq13a_inpt_etri </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> how many are available for students to use in learning activities? </td>
   <td style="text-align:left;"> Num: 0 to 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq13b_inpt_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> digital devices adapted for students with disabilities </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq15_inpt </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Does at least one computer(s)/tablet(s) have internet connectivity? </td>
   <td style="text-align:left;"> Num: 0 to 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq15a_inpt_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> how many digital devices are connected to the Internet? </td>
   <td style="text-align:left;"> Num: 0 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq16_infr__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Compared with children of the same age, do some children enrolled in your school </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq16_infr__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Compared with children of the same age, do some children enrolled in your school </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq16_infr__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Compared with children of the same age, do some children enrolled in your school </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq16_infr__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Compared with children of the same age, do some children enrolled in your school </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq16_infr__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Compared with children of the same age, do some children enrolled in your school </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq17_infr__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you have a process to screen students at your school for the following diffic </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq17_infr__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you have a process to screen students at your school for the following diffic </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq17_infr__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you have a process to screen students at your school for the following diffic </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq17_infr__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you have a process to screen students at your school for the following diffic </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq17_infr__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you have a process to screen students at your school for the following diffic </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq17_other_infr </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1sbq18_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Does your school have learning material accessible for all students (such as bra </td>
   <td style="text-align:left;"> Num: 1 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq1_imon </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there someone monitoring that all basic inputs are available to the students </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq2_imon </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Who has responsibility for monitoring basic inputs? </td>
   <td style="text-align:left;"> Num: 1 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq2_other_imon </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> specify other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq4_imon__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are the inputs that are being monitored?:functioning blackboard and chalk </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq4_imon__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are the inputs that are being monitored?:pens and pencils </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq4_imon__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are the inputs that are being monitored?:basic classroom furniture </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq4_imon__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are the inputs that are being monitored?:access to functioning digital devi </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq4_imon__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are the inputs that are being monitored?:textbooks </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq4_imon__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are the inputs that are being monitored?:exercise books </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq4_imon__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are the inputs that are being monitored?:use of digital devices and connect </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq5_imon </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Does the school have a school inventory to monitor availability of basic inputs </td>
   <td style="text-align:left;"> Num: 0 to 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq6_imon__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the inputs that are being monitored through school inventory::blac </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq6_imon__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the inputs that are being monitored through school inventory::pens </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq6_imon__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the inputs that are being monitored through school inventory::basi </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq6_imon__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the inputs that are being monitored through school inventory::digi </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq6_imon__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the inputs that are being monitored through school inventory::text </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq6_imon__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the inputs that are being monitored through school inventory::exer </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq3_imon </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are parents or community members involved in the monitoring of availability of b </td>
   <td style="text-align:left;"> Num: 1 to 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq7_imon </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there someone monitoring that all basic infrastructure is available </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq8_imon </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Who has responsibility for monitoring basic infrastructure? </td>
   <td style="text-align:left;"> Num: 1 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq8_other_imon </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq9_imon__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What infrastructure items are being monitored?:toilets </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq9_imon__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What infrastructure items are being monitored?:electricity </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq9_imon__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What infrastructure items are being monitored?:drinking water </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq9_imon__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What infrastructure items are being monitored?:accessibility for people with dis </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq9_imon__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What infrastructure items are being monitored?:internet connectivity </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq9a_imon_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there government legislation that assigns responsibility for maintaining ICT? </td>
   <td style="text-align:left;"> Num: 1 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq10_imon </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are parents or community members involved in the monitoring of availability of b </td>
   <td style="text-align:left;"> Num: 0 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq11_imon </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there a system to monitor availability of basic infrastructure in all public </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq12_imon__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the infrastructure that are being monitored through school invento </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq12_imon__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the infrastructure that are being monitored through school invento </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq12_imon__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the infrastructure that are being monitored through school invento </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq12_imon__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please select the infrastructure that are being monitored through school invento </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq13_imon__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require that students in all publ </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq13_imon__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require that students in all publ </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq13_imon__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require that students in all publ </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq13_imon__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require that students in all publ </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq13_imon__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require that students in all publ </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq13_imon__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require that students in all publ </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq14_imon__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require all schools to have…?:toi </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq14_imon__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require all schools to have…?:ele </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq14_imon__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require all schools to have…?:dri </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq14_imon__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require all schools to have…?:acc </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m1scq14_imon__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Do you know if there are standards in place to require all schools to have…?:Int </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq1 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> A1) What is your position in the school?  (most senior position) </td>
   <td style="text-align:left;"> Num: 1 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq1_other </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Have you ever taught in a school? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What year did you begin teaching? </td>
   <td style="text-align:left;"> Num: 1980 to 2019 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq4 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Do you presently teach at this school? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq5__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which grades do you teach this academic year?:Grade 1 </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq5__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which grades do you teach this academic year?:Grade 2 </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq5__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which grades do you teach this academic year?:Grade 3 </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq5__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which grades do you teach this academic year?:Grade 4 </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq5__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which grades do you teach this academic year?:Grade 5 </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq5__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which grades do you teach this academic year?:Grade 6 </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq5__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which grades do you teach this academic year?:Grade 7 </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq5__8 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which grades do you teach this academic year?:Pre-School </td>
   <td style="text-align:left;"> Num: 0 to 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq5__9 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which grades do you teach this academic year?:Special needs </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq6__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which subjects did you teach this academic year?:Language </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq6__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which subjects did you teach this academic year?:Mathematics </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq6__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which subjects did you teach this academic year?:All subjects </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq6__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Which subjects did you teach this academic year?:Other (Specify) </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq6_other </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq7 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> What is the highest level of education that you have completed? </td>
   <td style="text-align:left;"> Num: 4 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq7_other </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq8 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In what year did you achieve your present position in this school? </td>
   <td style="text-align:left;"> Num: 1997 to 2021 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq9 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What is your age? </td>
   <td style="text-align:left;"> Num: 21 to 67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7saq10 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> What is your gender </td>
   <td style="text-align:left;"> Num: 1 to 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sbq1_opmn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Would your school be responsible for fixing the problem? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sbq2_opmn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> How would you address the problem? </td>
   <td style="text-align:left;"> Num: 1 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sbq2_other_opmn </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sbq3_opmn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Do you feel that your school could address the problem within a one-year time fr </td>
   <td style="text-align:left;"> Num: 1 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sbq3_other_opmn </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sbq4_opmn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> If the problem would not be addressed by your school, who would be responsible f </td>
   <td style="text-align:left;"> Num: 1 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sbq4_other_opmn </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sbq5_opmn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Do you feel that the authorities who are responsible will address the problem wi </td>
   <td style="text-align:left;"> Num: 1 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sbq5_other_opmn </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq1_opmn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Who is responsible for providing students with textbooks in your school? </td>
   <td style="text-align:left;"> Num: 1 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq1_other_opmn </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq2_opmn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> If the school formally communicated a need for books to the authority responsibl </td>
   <td style="text-align:left;"> Num: 1 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq3_opmn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Please think back to the first month of this school year. How many students in y </td>
   <td style="text-align:left;"> Num: 1 to 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq4_opmn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Did the school do anything to provide some access to textbooks to the students w </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq5_opmn__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What steps did your school take to provide some textbook access to students who: </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq5_opmn__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What steps did your school take to provide some textbook access to students who: </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq5_opmn__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What steps did your school take to provide some textbook access to students who: </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq5_opmn__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What steps did your school take to provide some textbook access to students who: </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq5_opmn__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What steps did your school take to provide some textbook access to students who: </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq5_opmn__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What steps did your school take to provide some textbook access to students who: </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq5_opmn__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What steps did your school take to provide some textbook access to students who: </td>
   <td style="text-align:left;"> Num: 0 to 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7scq5_other_opmn </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq1_pman </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Does your school have any specific goals established for this academic year? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq2_pman </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Were these goals established by your school or determined by a higher authority? </td>
   <td style="text-align:left;"> Num: 1 to 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq3_pman__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Who in the school community has a clear idea on what the school goals are for th </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq3_pman__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Who in the school community has a clear idea on what the school goals are for th </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq3_pman__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Who in the school community has a clear idea on what the school goals are for th </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq3_pman__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Who in the school community has a clear idea on what the school goals are for th </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq3_pman__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Who in the school community has a clear idea on what the school goals are for th </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_pman__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Can you please let us know if your school goals for this academic year are?:impr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_pman__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Can you please let us know if your school goals for this academic year are?:impr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_pman__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Can you please let us know if your school goals for this academic year are?:redu </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_pman__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Can you please let us know if your school goals for this academic year are?:redu </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_pman__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Can you please let us know if your school goals for this academic year are?:redu </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_pman__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Can you please let us know if your school goals for this academic year are?:supp </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_pman__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Can you please let us know if your school goals for this academic year are?:gett </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_pman__8 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Can you please let us know if your school goals for this academic year are?:gett </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_pman__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Can you please let us know if your school goals for this academic year are?:Othe </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq4_other_pman </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq5_pman </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> How are you planning to measure if your school can achieve the goals that it has </td>
   <td style="text-align:left;"> Num: 1 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sdq5_other_pman </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq1_pman </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> What is the first thing you would do? </td>
   <td style="text-align:left;"> Num: 2 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq1_other_pman </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq2_pman__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How would you go about collecting the information that might help you understand </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq2_pman__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How would you go about collecting the information that might help you understand </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq2_pman__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How would you go about collecting the information that might help you understand </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq2_pman__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How would you go about collecting the information that might help you understand </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq2_pman__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How would you go about collecting the information that might help you understand </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq2_pman__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How would you go about collecting the information that might help you understand </td>
   <td style="text-align:left;"> Num: 0 to 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq2_other_pman </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq3_pman </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Which one of the following options do you think would probably be the most effec </td>
   <td style="text-align:left;"> Num: 1 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7seq3_other_pman </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq1_pknw </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many new teachers have been hired to work at this school in the past 2 years </td>
   <td style="text-align:left;"> Num: 0 to 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq2_pknw </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many of those teachers had completed a practicum prior to starting employmen </td>
   <td style="text-align:left;"> Num: 0 to 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq3_pknw </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are new teachers required to undergo a probationary period? </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq4_pknw </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Since you started working as principal, has there been any case of a teacher’s c </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sb_troster_pknw__0 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:0 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sb_troster_pknw__1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sb_troster_pknw__2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:2 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sb_troster_pknw__3 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sb_troster_pknw__4 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:4 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sb_troster_pknw__5 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:5 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sb_troster_pknw__6 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:6 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sb_troster_pknw__7 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:7 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7_teach_count </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Calculated variable of type Double </td>
   <td style="text-align:left;"> Num: 1 to 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq5_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 1 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq5_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 2 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq5_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 3 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq5_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 4 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq5_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 5 to 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq5_pknw__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 6 to 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq5_pknw__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 7 to 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq5_pknw__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 8 to 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq6_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 1 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq6_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 2 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq6_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 3 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq6_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 4 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq6_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 5 to 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq6_pknw__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 6 to 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq6_pknw__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 7 to 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq6_pknw__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 8 to 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq7_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq7_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 2 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq7_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 3 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq7_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 4 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq7_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 5 to 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq7_pknw__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 6 to 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq7_pknw__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 7 to 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq7_pknw__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 8 to 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq9_pknw_filter </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> correctly match opposite words. For instance, how many can match the word “under </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq9_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 1 to 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq9_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 2 to 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq9_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 3 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq9_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 4 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq9_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;"> Num: 5 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq9_pknw__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq9_pknw__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq9_pknw__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NULL </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq10_pknw </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In the selected 4th grade classroom, how many of the pupils have the relevant te </td>
   <td style="text-align:left;"> Num: 0 to 9999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq11_pknw </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> In the selected 4th grade classroom, is there a functioning blackboard? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq12_pknw </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Students deserve more attention if they attend school regularly </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq13_pknw </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Students deserve more attention if they come to school with materials </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq14_pknw </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Students deserve more attention if they are motivated to learn </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15a_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Maintenance and expansion of school infrastructure:No responsibility assigned </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15a_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Maintenance and expansion of school infrastructure:National </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15a_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Maintenance and expansion of school infrastructure:Provinces </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15a_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Maintenance and expansion of school infrastructure:Local </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15a_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Maintenance and expansion of school infrastructure:School </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15a_pknw__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Maintenance and expansion of school infrastructure:Don't Know </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15b_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Procurement of materials:No responsibility assigned </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15b_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Procurement of materials:National </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15b_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Procurement of materials:State </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15b_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Procurement of materials:Local </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15b_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Procurement of materials:School </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15b_pknw__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Procurement of materials:Don't Know </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15c_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher hiring and assignment:No responsibility assigned </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15c_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher hiring and assignment:National </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15c_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher hiring and assignment:State </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15c_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher hiring and assignment:Local </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15c_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher hiring and assignment:School </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15c_pknw__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher hiring and assignment:Don't Know </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15d_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher supervision, training, and coaching of teachers:No responsibility assign </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15d_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher supervision, training, and coaching of teachers:National </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15d_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher supervision, training, and coaching of teachers:State </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15d_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher supervision, training, and coaching of teachers:Local </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15d_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher supervision, training, and coaching of teachers:School </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15d_pknw__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Teacher supervision, training, and coaching of teachers:Don't Know </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15e_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Student learning assessments:No responsibility assigned </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15e_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Student learning assessments:National </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15e_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Student learning assessments:State </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15e_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Student learning assessments:Local </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15e_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Student learning assessments:School </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15e_pknw__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Student learning assessments:Don't Know </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15f_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal hiring and assignment:No responsibility assigned </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15f_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal hiring and assignment:National </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15f_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal hiring and assignment:State </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15f_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal hiring and assignment:Local </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15f_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal hiring and assignment:School </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15f_pknw__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal hiring and assignment:Don't Know </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15g_pknw__0 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal supervision and training:No responsibility assigned </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15g_pknw__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal supervision and training:National </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15g_pknw__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal supervision and training:State </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15g_pknw__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal supervision and training:Local </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15g_pknw__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal supervision and training:School </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sfq15g_pknw__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Principal supervision and training:Don't Know </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_ssld__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In this district, what factors are considered when selecting a Principal? Please </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_ssld__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In this district, what factors are considered when selecting a Principal? Please </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_ssld__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In this district, what factors are considered when selecting a Principal? Please </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_ssld__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In this district, what factors are considered when selecting a Principal? Please </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_ssld__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In this district, what factors are considered when selecting a Principal? Please </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_ssld__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In this district, what factors are considered when selecting a Principal? Please </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_ssld__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In this district, what factors are considered when selecting a Principal? Please </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_ssld__8 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In this district, what factors are considered when selecting a Principal? Please </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_ssld__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> In this district, what factors are considered when selecting a Principal? Please </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq1_other_ssld </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq2_ssld </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Which one of the previously mentioned do you think is the most important? </td>
   <td style="text-align:left;"> Num: 1 to 97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq3_ssup </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Have you ever received formal training on how to manage a school? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq4_ssup__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What type of training have you received?:Management training for new principals </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq4_ssup__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What type of training have you received?:In-service training for principals </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq4_ssup__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What type of training have you received?:Mentoring/Coaching by experienced princ </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq4_ssup__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What type of training have you received?:Other (specify) </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq4_other_ssup </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq5_ssup </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> have you used the skills you gained at that training? </td>
   <td style="text-align:left;"> Num: 1 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:How to prepare a budget </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:How to manage the financial resources </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:How to manage the relationship with th </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:How to provide feedback and mentoring </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:How to motivate teachers </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:How to develop a lesson plan </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:Pedagogical skills </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__8 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:How to report data on the school </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__9 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:How to ask for material needed for sch </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:Other (to specify) </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_ssup__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What are those skills that you have used?:Don't know </td>
   <td style="text-align:left;"> Num: 0 to 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq6_other_ssup </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq7_ssup </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Thinking of the past year, how many trainings and professional development cours </td>
   <td style="text-align:left;"> Num: 1 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq7a_ssup_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> did you attend any training on the use of ICT? </td>
   <td style="text-align:left;"> Num: 0 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq7b_ssup_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> How was this training delivered? </td>
   <td style="text-align:left;"> Num: 1 to 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq7c_ssup_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Did you find this training effective? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq8_sevl </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> During the last school year did any authority evaluate your work? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq9_sevl__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> During the last school year which authority evaluated your work?:Ministry of Edu </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq9_sevl__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> During the last school year which authority evaluated your work?:Ministry of Edu </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq9_sevl__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> During the last school year which authority evaluated your work?:District Educat </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq9_sevl__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> During the last school year which authority evaluated your work?:Heads of subjec </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq9_sevl__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> During the last school year which authority evaluated your work?:Parents’ associ </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Teaching material a </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Student discipline </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Teachers’ knowledge </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Teaching methods </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__5 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Teacher attendance </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__6 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Student attendance </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:School facilities a </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__8 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Student assessment </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__9 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Parent assessment </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__10 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Integration of ICT </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Other (specify) </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_sevl__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What specific aspects of your work did they evaluate you on?:Don’t know </td>
   <td style="text-align:left;"> Num: 0 to 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq10_other_sevl </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq11_sevl__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more negative evaluations?:The pr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq11_sevl__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more negative evaluations?:The pr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq11_sevl__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more negative evaluations?:The pr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq11_sevl__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more negative evaluations?:The pr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq11_sevl__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more negative evaluations?:Other </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq11_sevl__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more negative evaluations?:Don’t </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq11_sevl__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more negative evaluations?:No con </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq11_other_sevl </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq12_sevl__1 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more positive evaluations?:The pr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq12_sevl__2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more positive evaluations?:The pr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq12_sevl__3 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more positive evaluations?:The pr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq12_sevl__4 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more positive evaluations?:The pr </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq12_sevl__97 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more positive evaluations?:Other </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq12_sevl__98 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more positive evaluations?:Don’t </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq12_sevl__7 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What would happen if a principal received 2 or more positive evaluations?:No con </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sgq12_other_sevl </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Specify Other </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7shq1_satt </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> How satisfied or dissatisfied are you with your social status in the community a </td>
   <td style="text-align:left;"> Num: 1 to 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7shq2_satt </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What is your monthly salary as a public-school principal? </td>
   <td style="text-align:left;"> Num: 0 to 4e+06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7shq3_currency_satt </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Currency </td>
   <td style="text-align:left;"> Num: 1 to 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq1a_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> there is a plan/strategy to incorporate the use of technology (a) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq1b_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> there is a plan/strategy to incorporate the use of technology (b) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq1c_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> there is a plan/strategy to incorporate the use of technology (c) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq1d_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> there is a plan/strategy to incorporate the use of technology (d) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq2a_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> how important is it to ensure students have the skills to use ICT (a) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq2b_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> how important is it to ensure students have the skills to use ICT (b) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq2c_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> how important is it to ensure students have the skills to use ICT (c) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq2d_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> how important is it to ensure students have the skills to use ICT (d) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq3_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Who is responsible for ICT strategic plan? </td>
   <td style="text-align:left;"> Num: 1 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7siq4_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Do you have guidelines to integrate ICT into learning? </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sjq1a_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Number of digital devices supporting teaching (a) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sjq1b_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Number of digital devices supporting teaching (b) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sjq1c_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Number of digital devices supporting teaching (c) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sjq1d_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Number of digital devices supporting teaching (d) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sjq1e_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Number of digital devices supporting teaching (e) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7sjq2_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Do you have problem with internet? </td>
   <td style="text-align:left;"> Num: 1 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq1a_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Using digital education resources (a) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq1b_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Using digital education resources (b) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq1c_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Using digital education resources (c) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq1d_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Using digital education resources (d) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq1e_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Using digital education resources (e) </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq2a_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Policy about digital education (a) </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq2b_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Policy about digital education (b) </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq2c_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Policy about digital education (c) </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq2d_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Policy about digital education (d) </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m7skq2e_etri </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Policy about digital education (e) </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> numEligible4th </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Number of eligible 4th grade teachers </td>
   <td style="text-align:left;"> Num: 0 to 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m3sb_troster__0 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:0 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m3sb_troster__1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m3sb_troster__2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:2 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m3sb_troster__3 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m3sb_troster__4 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:4 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grade5_yesno </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Existence of at least one class of grade 5 in the school </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> randomization </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> random selection teacher grade 5 </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> public_officials_list_photo </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> picture of list of Grade 5 teachers </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> list_total </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many public officials are on the list? </td>
   <td style="text-align:left;"> Num: 1 to 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> needed_total </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many Grade 5 teachers need to be selected? </td>
   <td style="text-align:left;"> Num: 1 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> numEligible5th </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 5th grade teacher for ETRI questions </td>
   <td style="text-align:left;"> Num: 0 to 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m3sb_etri_roster__0 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> EdTech list:0 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m5sb_troster__0 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:0 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m5sb_troster__1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m5sb_troster__2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:2 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m5sb_troster__3 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m5sb_troster__4 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> teacher questionnaire list:4 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6_teacher_name </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> What is the name of the teacher instructing the students? </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6_teacher_code </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What is the teachers code </td>
   <td style="text-align:left;"> Num: 0 to 528002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6_class_count </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many students are in the class? </td>
   <td style="text-align:left;"> Num: 2 to 292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6_instruction_time </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How much time per day is dedicated to reading practice and/or instruction in rea </td>
   <td style="text-align:left;"> Num: 0 to 120 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6s1q1__0 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:0 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6s1q1__1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6s1q1__2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:2 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6s1q1__3 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6s1q1__4 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:4 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m6s1q1__5 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:5 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4saq1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Please enter the name of the teacher that is being recorded </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4saq1_number </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please enter the teacher's code that is being observed </td>
   <td style="text-align:left;"> Num: 0 to 410403466 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> class_start_sched </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> SCHEDULED: Class Start Time </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> class_end_sched </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> SCHEDULED: Class End Time </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Date_time </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> ACTUAL: Class Start Time </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> subject_test </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Domain under classroom observation </td>
   <td style="text-align:left;"> Num: 1 to 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq1_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are there steps leading up to the classroom? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq2_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there a proper ramp in good condition usable by a person in a wheelchair to a </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq3_infr </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is the main entrance to the classroom wide enough for a person in a wheelchair t </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq4_inpt </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many pupils are in the room? </td>
   <td style="text-align:left;"> Num: 1 to 93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq4n_girls </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many of them are boys? </td>
   <td style="text-align:left;"> Num: 0 to 56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq5_inpt </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many total pupils have the textbook for the class (English or mathematics)? </td>
   <td style="text-align:left;"> Num: 0 to 76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq6_inpt </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many total pupils in the class have a pencil or pen? </td>
   <td style="text-align:left;"> Num: 0 to 65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq7_inpt </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many total pupils in the class have an exercise book? </td>
   <td style="text-align:left;"> Num: 0 to 85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq8_inpt </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there a blackboard and/or whiteboard in the class? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq9_inpt </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there chalk or marker to write on the board available during the lesson? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq10_inpt </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Does the blackboard have sufficient light and contrast for reading what is writt </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq11_inpt </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many pupils were not sitting on desks? </td>
   <td style="text-align:left;"> Num: 0 to 57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq12_inpt </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many students are in the class, according to the class list? </td>
   <td style="text-align:left;"> Num: 1 to 135 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq13_girls </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> How many students on the class list are boys? </td>
   <td style="text-align:left;"> Num: 0 to 127 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_see </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> problems to see even if they wear glasses </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_sound </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> problems hearing sounds such as people's voices or music </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_walk </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Compared to children of the same age, how many children have problems with walki </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_comms </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> problems communicating, e.g., understanding or being understood by others </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_learn </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> learning disability. For example, dyslexia, dyscalculia, attention deficit disor </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_behav </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> behavioral problems. For example, hitting students repeatedly, disrespecting the </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq15_lang </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> kids language at home </td>
   <td style="text-align:left;"> Num: 1 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8_teacher_name </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> What is the name of the teacher instructing the students? </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8_teacher_code </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> What is the teacher's code? </td>
   <td style="text-align:left;"> Num: 0 to 2580963 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__0 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:0 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__1 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:1 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:2 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__3 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:3 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__4 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:4 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__5 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:5 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__6 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:6 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__7 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:7 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__8 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:8 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__9 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:9 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__10 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:10 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__11 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:11 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__12 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:12 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__13 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:13 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__14 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:14 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__15 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:15 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__16 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:16 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__17 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:17 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__18 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:18 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__19 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:19 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__20 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:20 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__21 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:21 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__22 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:22 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__23 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:23 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__24 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:24 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__25 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:25 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__26 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:26 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__27 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:27 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__28 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:28 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__29 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:29 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__30 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:30 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__31 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:31 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__32 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:32 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__33 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:33 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__34 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:34 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__35 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:35 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__36 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:36 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__37 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:37 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__38 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:38 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__39 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:39 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__40 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:40 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__41 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:41 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__42 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:42 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__43 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:43 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__44 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:44 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__45 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:45 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__46 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:46 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__47 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:47 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__48 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:48 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m8s1q1__49 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Students Taking Assessment:49 </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4saq1_g2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Please enter the name of the teacher that is being recorded </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4saq1_number_g2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Please enter the teacher's code that is being observed </td>
   <td style="text-align:left;"> Num: 0 to 395444 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> class_start_sched_g2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> SCHEDULED: Class Start Time </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> class_end_sched_g2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> SCHEDULED: Class End Time </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Date_time_g2 </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> ACTUAL: Class Start Time </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> subject_test_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Domain under classroom observation </td>
   <td style="text-align:left;"> Num: 1 to 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq1_infr_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Are there steps leading up to the classroom? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq2_infr_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there a proper ramp in good condition usable by a person in a wheelchair to a </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq3_infr_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is the main entrance to the classroom wide enough for a person in a wheelchair t </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq4_inpt_g2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many pupils are in the room? </td>
   <td style="text-align:left;"> Num: 3 to 145 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq4n_girls_g2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many of them are boys? </td>
   <td style="text-align:left;"> Num: 0 to 58 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq5_inpt_g2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many total pupils have the textbook for the class (English or mathematics)? </td>
   <td style="text-align:left;"> Num: 0 to 80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq6_inpt_g2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many total pupils in the class have a pencil or pen? </td>
   <td style="text-align:left;"> Num: 0 to 96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq7_inpt_g2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many total pupils in the class have an exercise book? </td>
   <td style="text-align:left;"> Num: 0 to 138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq8_inpt_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there a blackboard and/or whiteboard in the class? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq9_inpt_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Is there chalk or marker to write on the board available during the lesson? </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq10_inpt_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Does the blackboard have sufficient light and contrast for reading what is writt </td>
   <td style="text-align:left;"> Num: 0 to 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq11_inpt_g2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many pupils were not sitting on desks? </td>
   <td style="text-align:left;"> Num: 0 to 75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq12_inpt_g2 </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> How many students are in the class, according to the class list? </td>
   <td style="text-align:left;"> Num: 4 to 190 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq13_girls_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> How many students on the class list are boys? </td>
   <td style="text-align:left;"> Num: 0 to 90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_see_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> problems to see even if they wear glasses </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_sound_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> problems hearing sounds such as people's voices or music </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_walk_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Compared to children of the same age, how many children have problems with walki </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_comms_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> problems communicating, e.g., understanding or being understood by others </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_learn_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> learning disability. For example, dyslexia, dyscalculia, attention deficit disor </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq14_behav_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> behavioral problems. For example, hitting students repeatedly, disrespecting the </td>
   <td style="text-align:left;"> Num: 0 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> m4scq15_lang_g2 </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> kids language at home </td>
   <td style="text-align:left;"> Num: 1 to 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> comments </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Comments: </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sssys_irnd </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Random number in the range 0..1 associated with interview </td>
   <td style="text-align:left;"> Num: 0.001 to 0.997 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> has__errors </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Errors count in the interview </td>
   <td style="text-align:left;"> Num: 0 to 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> interview__status </td>
   <td style="text-align:left;"> haven_labelled </td>
   <td style="text-align:left;"> Status of the interview </td>
   <td style="text-align:left;"> Num: 100 to 100 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> assignment__id </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Assignment id (identifier in numeric format) </td>
   <td style="text-align:left;"> Num: 3730 to 4683 </td>
  </tr>
</tbody>
</table>

```r
#rename a few key variables up front
school_dta<- school_dta %>%
  mutate(enumerator_name_other= m1s0q1_name_other  ,
         enumerator_number=m1s0q1_name ,
         survey_time=m1s0q8,
         lat=m1s0q9__Latitude,
         lon=m1s0q9__Longitude,
         school_code=if_else(!is.na(school_code_preload),as.double(school_code_preload), as.double(m1s0q2_code)),
         m7_teach_count_pknw=m7_teach_count, #this variable was mistakenly not tagged as pknw
         total_enrolled=m1saq7
  ) %>%
  mutate(school_code=if_else(school_code==0, 328328, school_code)) %>%
  mutate(school_code=if_else(school_code==62181, 558163, school_code))  #fix an error where the school code was loaded incorrectly
```

```
## Error in `mutate()`:
## ! Problem while computing `school_code = if_else(...)`.
## Caused by error in `if_else()`:
## ! object 'school_code_preload' not found
```

```r
#create school metadata frame
school_metadta<-makeVlist(school_dta)

#Read in list of indicators
indicators <- read_delim(here::here('Indicators','indicators.md'), delim="|", trim_ws=TRUE)
```

```
## New names:
## * `` -> `...1`
## * `` -> `...8`
```

```
## Rows: 40 Columns: 8
## -- Column specification -------------------------------------------------------------------------------------------------------------------------------------------------
## Delimiter: "|"
## chr (6): Series, Indicator Name, Short Definition, Topic, Related Indicators, Value
## lgl (2): ...1, ...8
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
indicators <- indicators %>%
  filter(Series!="---") %>%
  separate(Series, c(NA, NA, "indicator_tag"), remove=FALSE)
```

```
## Warning: Expected 3 pieces. Additional pieces discarded in 1 rows [2].
```

```r
#Get list of indicator tags, so that we are able to select columns from our dataframe using these indicator tags that were also programmed into Survey Solutions
indicator_names <- indicators$indicator_tag

#list additional info that will be useful to keep in each indicator dataframe
preamble_info <- c( 'interview__key', 'school_code',
                   'school_name_preload', 'school_address_preload', 
                   'school_province_preload', 'school_district_preload', 'school_code_preload', 'school_emis_preload',
                   'school_info_correct', 'm1s0q2_name', 'm1s0q2_code', 'm1s0q2_emis',
                   'survey_time', 'lat', 'lon' , 'total_enrolled' , 'm7saq10'
                   )

drop_school_info <- c(

)

#Create a list of info to drop from final teacher files aggregated to school level. This will be necessary for merging these databases later

drop_teacher_info <- c( "questionnaire_roster__id", "teacher_name", "teacher_number", "available", 
                       "teacher_position", "teacher_grd1", "teacher_grd2", "teacher_grd3", "teacher_grd4", 
                       "teacher_language", "teacher_math", "teacher_both_subj", "teacher_other_subj", 
                       "teacher_education", "teacher_year_began", "teacher_age" )


#create school database with just preamble info.  This will be useful for merging on school level info to some databases
school_data_preamble_temp <- school_dta %>%
  group_by(school_code) %>%
  select( preamble_info) %>%
  select(-interview__key) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 
```

```
## Error in `group_by()`:
## ! Must group by variables found in `.data`.
## x Column `school_code` is not found.
```

```r
school_data_preamble <- school_dta %>%
  group_by(interview__key) %>%
  select(interview__key, school_code) %>%
  left_join(school_data_preamble_temp)
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
#use dplyr select(contains()) to search for variables with select tags and create separate databases by indicator
#This will make the information for each indicator contained in an independent database
#Will need to join the school level information with teacher level questionnaire information for some indicators.  This will be done later.
for (i in indicator_names ) {
  temp_df<-school_dta %>%
    select( contains(i))
  if (ncol(temp_df) > 0) {
    temp_df<-school_dta %>%
      select(preamble_info, contains(i))
    assign(paste("school_data_",i, sep=""), temp_df )
  }
}
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(preamble_info)` instead of `preamble_info` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Columns `school_code`, `school_address_preload`, `school_code_preload`, `survey_time`, `lat`, etc. don't exist.
```

```r
#####################
#create one copy of each dataframe that never gets touched and is carried forward to public folder
#####################
school_dta_raw <- school_dta

#merge school data to teacher roster
teacher_roster <- teacher_roster %>%
  left_join(school_data_preamble)
```

```
## Error in is.data.frame(y): object 'school_data_preamble' not found
```

```r
#########################################
#read in teacher questionnaire level file
#########################################
teacher_questionnaire<-read_dta(file.path(download_folder, "questionnaire_roster.dta"))
teacher_questionnaire_metadta<-makeVlist(teacher_questionnaire)



#Add school preamble info
teacher_questionnaire <- teacher_questionnaire %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything())
```

```
## Error in is.data.frame(y): object 'school_data_preamble' not found
```

```r
teacher_questionnaire_school <- teacher_questionnaire %>%
  group_by(interview__id) %>%
  summarise_all(~first(na.omit(.))) %>%
  write_excel_csv(path=file.path(confidential_folder, "teacher_questionnaire_school_info.csv"))
```

```
## Warning: The `path` argument of `write_excel_csv()` is deprecated as of readr 1.4.0.
## Please use the `file` argument instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

```r
#filter out teachers who did not consent to interview

teacher_questionnaire <- teacher_questionnaire %>%
  filter(m3s0q1==1)

#Create a function which will generate new binary variable using case_when, but 
#if value is misisng it will generate binary variable to be missing
#This is done a lot so will create function for it.
#e.g. school_absent=case_when(
#         m2sbq6_efft==6  ~ 1,
#         m2sbq6_efft!=6   ~ 0,
#         is.na(m2sbq6_efft) ~ as.numeric(NA))


bin_var <- function(var, val) {
  case_when(
    var==val  ~ 1,
    var!=val   ~ 0,
    is.na(var) ~ as.numeric(NA))
}



#rename a few key variables up front
teacher_questionnaire<- teacher_questionnaire %>%
  mutate(teacher_name=m3sb_troster  ,
         teacher_number=m3sb_tnumber ,
         available=m3s0q1,
         teacher_position=m3saq1,
         teacher_grd1=bin_var(m3saq2__1,1),
         teacher_grd2=bin_var(m3saq2__2,1),
         teacher_grd3=bin_var(m3saq2__3,1),
         teacher_grd4=bin_var(m3saq2__4,1),
         #teacher_grd5=bin_var(m3saq2__5,1),
         teacher_language=bin_var(m3saq3__1,1),
         teacher_math=bin_var(m3saq3__2,1),
         teacher_both_subj=bin_var(m3saq3__3,1),
         teacher_other_subj=bin_var(m3saq3__97,1),
         teacher_education=m3saq4,
         teacher_year_began=m3saq5,
         teacher_age=m3saq6,
  )

teacher_questionnaire<- teacher_questionnaire %>%
  mutate(temp=rowSums(select(.,teacher_grd1, teacher_grd2, teacher_grd3, teacher_grd4))) %>% # here we count the number of grades present in the classroom
  mutate(multigrade=case_when(temp > 1 ~ 1,
                              TRUE ~ 0)) %>% #this variable is equal to 1 if there are several grades and zero otherwise
  select(-temp)


#  We create a unique grade variable
teacher_questionnaire <- teacher_questionnaire %>%
  mutate(grade=case_when(teacher_grd1==1 & multigrade!=1 ~ 1,
                         teacher_grd2==1 & multigrade!=1 ~ 2,
                         teacher_grd3==1 & multigrade!=1 ~ 3,
                         teacher_grd4==1 & multigrade!=1 ~ 4,
                         #teacher_grd5==1 & multigrade!=1 ~ 5,
                         multigrade==1 ~ 6,
                         TRUE ~ NA_real_)) %>%
  mutate(grade=factor(grade, levels=c(1,2,3,4,6), labels=c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Multigrade")))

label(teacher_questionnaire$grade) <- "Grade"



#list additional info that will be useful to keep in each indicator dataframe
preamble_info_teacher <- c('interview__key', 'questionnaire_roster__id', 'teacher_name', 'teacher_number', 'm7saq10',
                          'available', 'teacher_position', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 
                          'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'teacher_education', 'teacher_year_began',
                          'teacher_age')



#use dplyr select(contains()) to search for variables with select tags and create separate databases
for (i in indicator_names ) {
  temp_df<-teacher_questionnaire %>%
    select( contains(i))
  if (ncol(temp_df) > 0) {
    temp_df<-teacher_questionnaire %>%
      select(school_code, preamble_info_teacher, contains(i))
    assign(paste("teacher_questionnaire_",i, sep=""), temp_df )
  }
}
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
#####################
#create one copy of each dataframe that never gets touched and is carried forward to public folder
#####################
teacher_questionnaire_raw <- teacher_questionnaire

#############################################
##### Teacher Absence ###########
#############################################

# School survey. Percent of teachers absent. Teacher is coded absent if they are: 
#   - not in school 
# - in school but absent from the class 
# - in the class but not teaching.


bin_var <- function(var, val) {
  case_when(
    var==val  ~ 1,
    var!=val   ~ 0,
    is.na(var) ~ as.numeric(NA))
}



bin_var_2 <- function(var, val) {
  if_else(var==val, 
          as.numeric(1),
          as.numeric(var))
}

#read in teacher absence file
teacher_absence_dta<-read_dta(file.path(download_folder, "TEACHERS.dta"))
teacher_absence_metadta<-makeVlist(teacher_absence_dta)

#Add school preamble info
teacher_absence_dta <- teacher_absence_dta %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything()) 
```

```
## Error in is.data.frame(y): object 'school_data_preamble' not found
```

```r
#number missing
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(n_mssing_EFFT=n_miss_row(.))

#rename a few key variables up front
teacher_absence_dta<- teacher_absence_dta %>%
  mutate(teacher_name=m2saq2  ,
         teacher_number=TEACHERS__id ,
         teacher_position=m2saq4,
         teacher_permanent=bin_var(m2saq5,1),
         teacher_contract=bin_var(m2saq5,2),
         teacher_temporary=bin_var(m2saq5,3),
         teacher_volunteer=bin_var(m2saq5,4),
         teacher_ngo=bin_var(m2saq5,5)==5,
         teacher_other=case_when(
           m2saq5==97 ~ m2saq5_other,
           m2saq5!=97 ~ "NA"
         ),
         teacher_fulltime=bin_var(m2saq6,1),
         teacher_male=bin_var(m2saq3,1),
         teacher_grd1=bin_var(m2saq7__1,1),
         teacher_grd2=bin_var(m2saq7__2,1),
         teacher_grd3=bin_var(m2saq7__3,1),
         teacher_grd4=bin_var(m2saq7__4,1),
         #teacher_grd5=bin_var(m2saq7__5,1),
         teacher_language=bin_var(m2saq8__1,1),
         teacher_math=bin_var(m2saq8__2,1),
         teacher_both_subj=bin_var(m2saq8__3,1),
         teacher_other_subj=bin_var(m2saq8__97,1),
  )

teacher_absence_dta<- teacher_absence_dta %>%
  mutate(temp=rowSums(select(.,teacher_grd1, teacher_grd2, teacher_grd3, teacher_grd4))) %>% # here we count the number of grades present in the classroom
  mutate(multigrade=case_when(temp > 1 ~ 1,
                              TRUE ~ 0)) %>% #this variable is equal to 1 if there are several grades and zero otherwise
  select(-temp)

#  We create a unique grade variable

teacher_absence_dta <- teacher_absence_dta %>%
  mutate(grade=case_when(teacher_grd1==1 & multigrade!=1 ~ 1,
                         teacher_grd2==1 & multigrade!=1 ~ 2,
                         teacher_grd3==1 & multigrade!=1 ~ 3,
                         teacher_grd4==1 & multigrade!=1 ~ 4,
                         #teacher_grd5==1 & multigrade!=1 ~ 5,
                         multigrade==1 ~ 6,
                         TRUE ~ NA_real_)) %>%
  mutate(grade=factor(grade, levels=c(1,2,3,4,6), labels=c("Grade 1", "Grade 2", "Grade 3", "Grade 4",  "Multigrade")))

label(teacher_absence_dta$grade) <- "Grade"

#list additional info that will be useful to keep in each indicator dataframe
preamble_info_absence <- c('interview__key', 'TEACHERS__id', 'teacher_name', 'teacher_number', 
                   'teacher_position', 'teacher_permanent', 'teacher_contract', 'teacher_temporary', 'teacher_volunteer', 'teacher_ngo', 'teacher_other',
                   'teacher_fulltime', 'teacher_male', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 'grade',
                   'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'subject_joined', 'm2sbq6_efft')

#create indicator for whether each teacher was absent from school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(sch_absence_rate=100*case_when(
    m2sbq6_efft==6 | teacher_available==2 ~ 1,
    m2sbq6_efft!=6   ~ 0,
    is.na(m2sbq6_efft) ~ as.numeric(NA)))

#create indicator for whether each teacher was absent from classroom or school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(absence_rate=100*case_when(
    m2sbq6_efft==6 | m2sbq6_efft==5 |  teacher_available==2 ~ 1,
    m2sbq6_efft==1 | m2sbq6_efft==3 | m2sbq6_efft==2 | m2sbq6_efft==4  ~ 0,
    is.na(m2sbq6_efft) ~ as.numeric(NA)) )

#create indicator for whether each principal was absent from school
teacher_absence_dta <- teacher_absence_dta %>%
  mutate(principal_absence=100*case_when(
    m2sbq3_efft==8  ~ 1,
    m2sbq3_efft!=8   ~ 0,
    is.na(m2sbq3_efft) ~ as.numeric(NA))) %>%
  mutate(absence_rate=if_else(is.na(absence_rate), principal_absence, absence_rate ),
         sch_absence_rate=if_else(is.na(sch_absence_rate), principal_absence, sch_absence_rate ),
         presence_rate=100-absence_rate)


teacher_absence_final<- teacher_absence_dta %>%
  select(preamble_info, preamble_info_absence, contains('absent')) %>%
  group_by(school_code, teacher_number) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Columns `school_code`, `school_name_preload`, `school_address_preload`, `school_province_preload`, `school_district_preload`, etc. don't exist.
```

```r
#Build teacher absence practice indicator
final_indicator_data_EFFT <- teacher_absence_dta %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select( -starts_with('interview'), -starts_with('enumerator'), -c('teacher_name', 'm2saq2'))
```

```
## Error in `group_by()`:
## ! Must group by variables found in `.data`.
## x Column `school_code` is not found.
```

```r
#Breakdowns by Male/Female
final_indicator_data_EFFT_M <- teacher_absence_dta %>%
  filter(teacher_male==1) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select( -starts_with('interview'), -starts_with('enumerator'), -c('teacher_name', 'm2saq2'))
```

```
## Error in `group_by()`:
## ! Must group by variables found in `.data`.
## x Column `school_code` is not found.
```

```r
final_indicator_data_EFFT_F <- teacher_absence_dta %>%
  filter(teacher_male==0) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select( -starts_with('interview'), -starts_with('enumerator'), -c('teacher_name', 'm2saq2'))
```

```
## Error in `group_by()`:
## ! Must group by variables found in `.data`.
## x Column `school_code` is not found.
```

```r
#############################################
##### Student Attendance ###########
#############################################

#Percent of 4th grade students who are present during an unannounced visit.

final_indicator_data_ATTD<- school_data_INPT %>%
  select(preamble_info, m4scq4_inpt, m4scq4_inpt, m4scq12_inpt )  %>%
  mutate(student_attendance=m4scq4_inpt/m4scq12_inpt) %>%
  mutate(student_attendance=if_else(m4scq4_inpt>m4scq12_inpt, m4scq12_inpt/m4scq4_inpt,as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=if_else(student_attendance>1, 1, as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=100*student_attendance) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.)) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in select(., preamble_info, m4scq4_inpt, m4scq4_inpt, m4scq12_inpt): object 'school_data_INPT' not found
```

```r
#Breakdowns by Male/Female

num_boys <- school_dta %>%
  select(interview__key, m4scq4n_girls, m4scq13_girls )

final_indicator_data_ATTD_M<- school_data_INPT %>%
  left_join(num_boys) %>%
  select(preamble_info, m4scq4_inpt, m4scq4_inpt, m4scq4n_girls, m4scq12_inpt, m4scq13_girls )  %>%
  mutate(student_attendance=m4scq4n_girls/m4scq13_girls) %>%
  mutate(student_attendance=if_else(student_attendance>1, 1,as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=100*student_attendance) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.)) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in left_join(., num_boys): object 'school_data_INPT' not found
```

```r
final_indicator_data_ATTD_F<- school_data_INPT %>%
  left_join(num_boys) %>%
  select(preamble_info, m4scq4_inpt, m4scq4_inpt, m4scq4n_girls, m4scq12_inpt, m4scq13_girls )  %>%
  mutate(student_attendance=(m4scq4_inpt-m4scq4n_girls)/(m4scq12_inpt-m4scq13_girls)) %>%
  mutate(student_attendance=if_else(student_attendance>1, 1,as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=if_else(student_attendance<0, 0,as.numeric(student_attendance)))  %>% #fix an issue where sometimes enumerators will get these two questions mixed up.
  mutate(student_attendance=100*student_attendance) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.)) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in left_join(., num_boys): object 'school_data_INPT' not found
```

```r
#############################################
##### Teacher Knowledge ###########
#############################################


teacher_assessment_dta <- read_dta(file.path(download_folder, "teacher_assessment_answers.dta")) %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything()) 
```

```
## Error in is.data.frame(y): object 'school_data_preamble' not found
```

```r
#number missing
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(n_mssing_CONT=n_miss_row(.))


#####################
#create one copy of each dataframe that never gets touched and is carried forward to public folder
#####################
teacher_assessment_dta_raw <- teacher_assessment_dta


#Drop columns that end in "mistake".  THis is not necessary for computing indicator
teacher_assessment_dta <- teacher_assessment_dta %>% 
  select(-ends_with("mistake"))

#drop the correct the letter
teacher_assessment_dta <- teacher_assessment_dta %>% 
  select(-starts_with("m5s1q3"))

#recode assessment variables to be 1 if student got it correct and zero otherwise
teacher_assessment_dta<- teacher_assessment_dta %>%
  mutate_at(vars(starts_with("m5s1q"), starts_with("m5s2q")), ~bin_var_2(.,2)  ) %>%
  mutate_at(vars(starts_with("m5s1q"), starts_with("m5s2q")), ~bin_var(.,1)  )


#create indicator for % correct on teacher assessment
#Note:  in the future we could incorporate irt program like mirt


####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m5s1q, which is the prefix of literacy items
teacher_assessment_dta$literacy_length<-length(grep(x=colnames(teacher_assessment_dta), pattern="m5s1q"))


lit_items<-colnames(teacher_assessment_dta[,grep(x=colnames(teacher_assessment_dta), pattern="m5s1q")])

#calculate teachers lit items correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(literacy_content_knowledge=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q")], na.rm=TRUE),
         correct_letter=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q3")], na.rm=TRUE),
         cloze=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q2")], na.rm=TRUE),
         grammar=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q1")], na.rm=TRUE),
         read_passage=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s1q4")], na.rm=TRUE))

####Math####
#calculate # of math items
teacher_assessment_dta$math_length<-length(grep(x=colnames(teacher_assessment_dta), pattern="m5s2q"))

math_items<-colnames(teacher_assessment_dta[,grep(x=colnames(teacher_assessment_dta), pattern="m5s2q")])

#calculate teachers math items correct
teacher_assessment_dta <- teacher_assessment_dta %>%
  mutate(math_content_knowledge=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="m5s2q")], na.rm=TRUE),
         arithmetic_number_relations=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="number")], na.rm=TRUE),
         geometry=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="geometric")], na.rm=TRUE),
         interpret_data=100*rowMeans(.[grep(x=colnames(teacher_assessment_dta), pattern="data")], na.rm=TRUE))

#rename a few key variables up front
teacher_assessment_dta<- teacher_assessment_dta %>%
  mutate(g4_teacher_name=m5sb_troster  ,
         g4_teacher_number=m5sb_tnum   )


#pull apart dataset with just domains
teacher_assessment_domains <- teacher_assessment_dta %>%
  dplyr::select(typetest, school_code, literacy_content_knowledge, correct_letter, cloze, grammar, read_passage,
                math_content_knowledge, arithmetic_number_relations, geometry, interpret_data
  )
```

```
## Error in `dplyr::select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
teacher_assessment_language <-teacher_assessment_dta %>%
  select(typetest, school_code, m5sb_tnum, lit_items) %>%
  filter(typetest==2) %>%
  select(-typetest)
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
teacher_assessment_math <- teacher_assessment_dta %>%
  select(typetest, school_code, m5sb_tnum, math_items) %>%
  filter(typetest==1) %>%
  select(-typetest)
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
#calculate % correct for literacy, math, and total
final_indicator_data_CONT <- teacher_assessment_dta %>%
  group_by(school_code) %>%
  add_count(school_code,name='m5_teach_count') %>%
  add_count(typetest,name='m5_teach_count_math') %>%
  mutate(m5_teach_count_math= if_else(typetest==1, as.numeric(m5_teach_count_math), as.numeric(NA))) %>%
  mutate(content_knowledge=case_when(
    (!is.na(math_content_knowledge) & !is.na(literacy_content_knowledge)) ~ (math_content_knowledge+literacy_content_knowledge)/2,
    is.na(math_content_knowledge)  ~ literacy_content_knowledge,
    is.na(literacy_content_knowledge)  ~ math_content_knowledge)) %>%
  mutate(content_proficiency=100*as.numeric(content_knowledge>=80),
         content_proficiency_70=100*as.numeric(content_knowledge>=70),
         content_proficiency_75=100*as.numeric(content_knowledge>=75),
         literacy_content_proficiency=100*as.numeric(literacy_content_knowledge>=80),
         literacy_content_proficiency_70=100*as.numeric(literacy_content_knowledge>=70),
         literacy_content_proficiency_75=100*as.numeric(literacy_content_knowledge>=75),
         math_content_proficiency=100*as.numeric(math_content_knowledge>=80),
         math_content_proficiency_70=100*as.numeric(math_content_knowledge>=70),
         math_content_proficiency_75=100*as.numeric(math_content_knowledge>=75)) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  
  select(-ends_with('length'), -ends_with('items'), -typetest, -starts_with('interview'), -starts_with('enumerator'),
         -starts_with('g4_teacher'), -c('teacher_assessment_answers__id', 'm5sb_troster', 'm5sb_tnum'))
```

```
## Error in `group_by()`:
## ! Must group by variables found in `.data`.
## x Column `school_code` is not found.
```

```r
#Breakdown by Male/Female
final_indicator_data_CONT_M <- teacher_assessment_dta %>%
  mutate(TEACHERS__id=g4_teacher_number) %>%
  left_join(teacher_absence_dta, by=c('school_code', 'TEACHERS__id')) %>%
  filter(m2saq3==1) %>%
  group_by(school_code) %>%
  add_count(school_code,name='m5_teach_count') %>%
  mutate(content_knowledge=case_when(
    (!is.na(math_content_knowledge) & !is.na(literacy_content_knowledge)) ~ (math_content_knowledge+literacy_content_knowledge)/2,
    is.na(math_content_knowledge)  ~ literacy_content_knowledge,
    is.na(literacy_content_knowledge)  ~ math_content_knowledge)) %>%
  mutate(content_proficiency=100*as.numeric(content_knowledge>=80),
         content_proficiency_70=100*as.numeric(content_knowledge>=70),
         content_proficiency_75=100*as.numeric(content_knowledge>=75),
         literacy_content_proficiency=100*as.numeric(literacy_content_knowledge>=80),
         literacy_content_proficiency_70=100*as.numeric(literacy_content_knowledge>=70),
         literacy_content_proficiency_75=100*as.numeric(literacy_content_knowledge>=75),
         math_content_proficiency=100*as.numeric(math_content_knowledge>=80),
         math_content_proficiency_70=100*as.numeric(math_content_knowledge>=70),
         math_content_proficiency_75=100*as.numeric(math_content_knowledge>=75)) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'),
         -starts_with('g4_teacher'), -c('teacher_assessment_answers__id', 'm5sb_troster', 'm5sb_tnum'))
```

```
## Error in `left_join()`:
## ! Join columns must be present in data.
## x Problem with `school_code`.
```

```r
final_indicator_data_CONT_F <- teacher_assessment_dta %>%
  mutate(TEACHERS__id=g4_teacher_number) %>%
  left_join(teacher_absence_dta, by=c('school_code', 'TEACHERS__id')) %>%
  filter(m2saq3==2) %>%
  group_by(school_code) %>%
  add_count(school_code,name='m5_teach_count') %>%
  mutate(content_knowledge=case_when(
    (!is.na(math_content_knowledge) & !is.na(literacy_content_knowledge)) ~ (math_content_knowledge+literacy_content_knowledge)/2,
    is.na(math_content_knowledge)  ~ literacy_content_knowledge,
    is.na(literacy_content_knowledge)  ~ math_content_knowledge)) %>%
  mutate(content_proficiency=100*as.numeric(content_knowledge>=80),
         content_proficiency_70=100*as.numeric(content_knowledge>=70),
         content_proficiency_75=100*as.numeric(content_knowledge>=75),
         literacy_content_proficiency=100*as.numeric(literacy_content_knowledge>=80),
         literacy_content_proficiency_70=100*as.numeric(literacy_content_knowledge>=70),
         literacy_content_proficiency_75=100*as.numeric(literacy_content_knowledge>=75),
         math_content_proficiency=100*as.numeric(math_content_knowledge>=80),
         math_content_proficiency_70=100*as.numeric(math_content_knowledge>=70),
         math_content_proficiency_75=100*as.numeric(math_content_knowledge>=75)) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'),
         -starts_with('g4_teacher'), -c('teacher_assessment_answers__id', 'm5sb_troster', 'm5sb_tnum'))
```

```
## Error in `left_join()`:
## ! Join columns must be present in data.
## x Problem with `school_code`.
```

```r
#############################################
### Teacher Pedagogy ###
#############################################
if (teach_avail==1) {
  
  teacher_pedagogy <- read.csv(paste0(confidential_folder,"/teach_raw_data_eth.csv"))
  
  score_var <- teacher_pedagogy%>% select(starts_with("s_"))%>% names()
  
  ## Wrangling
  
  teacher_pedagogy <- teacher_pedagogy  %>% 
    
    ## Cleaning the scores
    mutate_all(funs(str_replace(., "Y", "1"))) %>% 
    mutate_all(funs(str_replace(., "N", "0"))) %>% 
    mutate_all(funs(str_replace(., "L", "2"))) %>% 
    mutate_all(funs(str_replace(., "M", "3"))) %>% 
    mutate_all(funs(str_replace(., "H", "4"))) %>% 
    mutate_all(funs(str_replace(., "NA", "1")))%>% 
    rename(school_code = 1) %>% 
    select(-Enumerator, -starts_with("X"))%>%
    mutate(across(everything(), as.numeric)) %>% distinct(school_code, Segment, .keep_all=T)

  
  
  # Generate useful variables
  
  teacher_pedagogy_segments <- teacher_pedagogy
  
  #create sub-indicators from TEACH
  teacher_pedagogy_segments <- teacher_pedagogy_segments %>%
    mutate(classroom_culture=rowMeans(select(.,s_a1, s_a2)),
           instruction=rowMeans(select(.,s_b3, s_b4, s_b5, s_b6)),
           socio_emotional_skills=rowMeans(select(.,s_c7, s_c8, s_c9))
    ) %>%
    mutate(teach_score=rowMeans(select(.,classroom_culture, instruction, socio_emotional_skills)))
  
  # Time on task - First measure (Yes/No on "Teacher provides learning activites to most students")
  # Generate a variable computing the proportion of times each teacher for each segment is providing a learning activity to students
  # We are only taking into account teachers for which we have at least 2 snapshots observed
  
  teacher_pedagogy_segments <- teacher_pedagogy_segments %>%
    mutate(nb_tt1=3-(is.na(s_0_1_1) + is.na(s_0_2_1) + is.na(s_0_3_1))) %>%
    mutate(timeontask1=if_else(nb_tt1>=2, rowMeans(select(.,s_0_1_1, s_0_2_1, s_0_3_1), na.rm=TRUE), NA_real_)) 
  
  #een tt_yes=rowmean(s_0_1_1_yes s_0_2_1_yes s_0_3_1_yes) if nb_tt1>=2
  #replace tt_yes=tt_yes*100
  #egen tt_no=rowmean(s_0_1_1_no s_0_2_1_no s_0_3_1_no) if nb_tt1>=2
  #replace tt_no=tt_no*100
  
  # Time on task - Second measure
  # Proportion of classes where a low number of students are on task, a medium number of students are on task
  teacher_pedagogy_segments <- teacher_pedagogy_segments %>%
    mutate(tot_low=rowSums(select(.,s_0_1_2,s_0_2_2,s_0_3_2) == 2),
           tot_medium=rowSums(select(.,s_0_1_2,s_0_2_2,s_0_3_2) == 3),
           tot_high=rowSums(select(.,s_0_1_2,s_0_2_2,s_0_3_2) == 4))
  
  # We count the number of snapshots observed (in case the observation lasted less than 15 minutes) and for which the teacher was providing a learning activity
  
  # For each of the variables "Low", "Medium" and "High", we create our own mean (in case the observation lasted less than 15 minutes or teacher was not providing a learning activity)
  # We are only taking into account teachers for which we have at least 2 snapshots observed
  
  teacher_pedagogy_segments <- teacher_pedagogy_segments %>%
    mutate(nb_tt2=3-(is.na(s_0_1_2) + is.na(s_0_2_2) + is.na(s_0_3_2)),
           tt_low=if_else(nb_tt2 >= 2, 100*tot_low/nb_tt2, NA_real_),
           tt_medium=if_else(nb_tt2 >= 2, 100*tot_medium/nb_tt2, NA_real_),
           tt_high=if_else(nb_tt2 >= 2, 100*tot_high/nb_tt2, NA_real_))
  
  
  final_indicator_data_PEDG <- teacher_pedagogy_segments %>%
    mutate(teach_prof=100*as.numeric(teach_score>=3),                      #rate teacher as proficient in teach and the subcomponents if they score at least 3
           classroom_culture_prof=100*as.numeric(classroom_culture>=3),
           instruction_prof=100*as.numeric(instruction>=3),
           socio_emotional_skills_prof=100*as.numeric(socio_emotional_skills>=3)) %>%
    filter(!is.na(teach_score)) %>%
    #write_excel_csv(path = paste(confidential_folder, "teach_raw_data.csv", sep="/")) %>%
    group_by(school_code) %>%
    mutate(number_segments=  sum(!is.na(teach_score))) %>%
    summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
    select( -starts_with('interview'), -starts_with('enumerator'),
            -starts_with('m4saq1'))
  
  #Breakdowns by Male/Female
  
  
  write_excel_csv(final_indicator_data_PEDG, path = paste(confidential_folder, "teach_score_counts.csv", sep="/"))
  
}
```

```
## Warning in file(file, "rt"): cannot open file 'C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT//SLE/SLE_2022_GEPD/SLE_2022_GEPD_v01_RAW/Data/
## confidential/School/teach_raw_data_eth.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
  #############################################
#############################################
##### 4th Grade Assessment ###########
#############################################

#read in 4th grade assessment level file
assess_4th_grade_dta<-read_dta(file.path(download_folder, "fourth_grade_assessment.dta"))

#Add school preamble info
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything()) 
```

```
## Error in is.data.frame(y): object 'school_data_preamble' not found
```

```r
#number missing
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(n_mssing_LERN=n_miss_row(.))

#create indicator for % correct on student assessment
#Note:  in the future we could incorporate irt program like mirt


#rename a few key variables up front
assess_4th_grade_dta<- assess_4th_grade_dta %>%
  mutate(student_name=m8s1q1  ,
         student_number=fourth_grade_assessment__id,
         student_age=m8s1q2,
         student_male=bin_var(m8s1q3,1),
  )

#####################
#create one copy of each dataframe that never gets touched and is carried forward to public folder
#####################
assess_4th_grade_dta_raw <- assess_4th_grade_dta


# create a function to score questions m8saq2 and m8saq3, in which students identify letters/words that enumerator calls out.
# This question is tricky, because enumerators would not always follow instructions to say out loud the same letters/words
# In order to account for this, will assume if 80% of the class has a the exact same response, then this is the letter/word called out
# Score this so that if there is a deviation from what 80% of the class says, then it is wrong.
call_out_scorer <- function(var, pctl) {
  1-abs(var - quantile(var, pctl, na.rm=T))
}
# #old scoring code:
# mutate(m8saq2_id=rowMeans(select(.,m8saq2_id__3,m8saq2_id__4, m8saq2_id__6), na.rm=TRUE),
#        m8saq3_id=rowMeans(select(.,m8saq3_id__2,m8saq2_id__6, m8saq2_id__7), na.rm=TRUE),
#        m8saq4_id=if_else(m8saq4_id!=99, m8saq4_id/5,0),
#        m8saq7_word_choice=bin_var(m8saq7_word_choice,2),
#        m8sbq1_number_sense=rowMeans(select(.,m8sbq1_number_sense__3,m8sbq1_number_sense__4, m8sbq1_number_sense__1), na.rm=TRUE)) 


#create a dataset with observations with issues based on enumerator calling out items
assess_4th_grade_dta_issues <- assess_4th_grade_dta %>%
  mutate(
         m8saq2_id_tot=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq2_id")])), 
         m8saq3_id_tot=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq3_id")])),
         m8sbq1_number_sense_tot=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq1_number_sense")])))   %>%
  filter(m8saq2_id_tot>3 | m8saq3_id_tot>3 | m8sbq1_number_sense_tot>3) 

# assess_4th_grade_dta_issues %>%
#   write_excel_csv(path= file.path(save_folder_onedrive, "assess_4th_grade_dta_issues.csv"))

# assess_4th_grade_dta_issues %>%
#   group_by(school_code) %>%
#   summarise_all(~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
#   write_excel_csv(path= file.path(save_folder_onedrive, "assess_4th_grade_dta_issues_school_level.csv"))



#recode assessment variables to be 1 if student got it correct and zero otherwise
assess_4th_grade_dta<- assess_4th_grade_dta %>%
  mutate_at(vars(starts_with("m8saq5"), 
                 starts_with("m8saq6"),
                 starts_with("m8sbq2"),
                 starts_with("m8sbq3"),
                 starts_with("m8sbq4"),
                 starts_with("m8sbq5"),
                 starts_with("m8sbq6"),
  ), ~bin_var(.,1)  ) %>% #now handle the special cases
  mutate(m8saq4_id=if_else(m8saq4_id==5,4, as.numeric(m8saq4_id))) %>% #fix case where some enumerators recorded the pre-filled answer.
  mutate(m8saq7a_gir=bin_var(m8saq7a_gir, 3),
         m8saq7b_gir=bin_var(m8saq7b_gir, 3),
         m8saq7c_gir=bin_var(m8saq7c_gir, 2),
         m8saq7d_gir=bin_var(m8saq7d_gir, 3),
         m8saq7e_gir=bin_var(m8saq7e_gir, 4),
         m8saq7f_gir=bin_var(m8saq7f_gir, 1),
         m8saq7g_gir=bin_var(m8saq7g_gir, 2),
         m8saq7h_gir=bin_var(m8saq7h_gir, 2),
         m8saq7i_gir=bin_var(m8saq7i_gir, 4),
         m8saq7j_gir=bin_var(m8saq7j_gir, 1),
         m8saq7k_gir=bin_var(m8saq7k_gir, 3)) %>% #grade lonely giraffe question
  group_by(school_code) %>%
  mutate_at(vars(starts_with("m8saq2_id"),starts_with("m8saq3_id"), starts_with("m8sbq1_number_sense")),
            ~call_out_scorer(.,0.8)) %>%
  ungroup() %>%
  mutate(m8saq2_id=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq2_id")])-7)/3, #subtract some letters not assessed and make out of 3 points
         m8saq3_id=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq3_id")])-7)/3) %>%
  mutate(m8saq2_id=if_else(m8saq2_id<0,0,m8saq2_id), #subtract some letters not assessed and make out of 3 points
         m8saq3_id=if_else(m8saq3_id<0,0,m8saq3_id)) %>%
  mutate(m8saq2_id=if_else(m8saq2_id>1,1,m8saq2_id), #subtract some letters not assessed and make out of 3 points
         m8saq3_id=if_else(m8saq3_id>1,1,m8saq3_id)) %>%
  mutate(m8saq4_id=if_else(m8saq4_id!=99, m8saq4_id/4,0),
         m8saq7_word_choice=bin_var(m8saq7_word_choice,2),
         m8sbq1_number_sense=(rowSums(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq1_number_sense")])-7)/3)         %>%
    mutate( 
      m8sbq1_number_sense=if_else(m8sbq1_number_sense<0,0,m8sbq1_number_sense)) %>%
    mutate(
      m8sbq1_number_sense=if_else(m8sbq1_number_sense>1,1,m8sbq1_number_sense)) %>%
  select(-starts_with("m8saq2_id__"),-starts_with("m8saq3_id__"),-starts_with("m8sbq1_number_sense__"))
```

```
## Error in `group_by()`:
## ! Must group by variables found in `.data`.
## x Column `school_code` is not found.
```

```r
####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items
assess_4th_grade_dta$literacy_length<-length(grep(x=colnames(assess_4th_grade_dta), pattern="m8saq"))

lit_items<-colnames(assess_4th_grade_dta[,grep(x=colnames(assess_4th_grade_dta), pattern="m8saq")])


#calculate students lit items correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(literacy_student_knowledge=100*rowMeans(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8saq")], na.rm=TRUE),
         literacy_student_knowledge_nogiraffe=100*rowMeans(.[c('m8saq4_id', 'm8saq5_story', 'm8saq6_story', 'm8saq7_word_choice', 'm8saq2_id', 'm8saq3_id')], na.rm=TRUE))
```

```
## Error in `mutate()`:
## ! Problem while computing `literacy_student_knowledge_nogiraffe = 100 * ...`.
## Caused by error in `vectbl_as_col_location()`:
## ! Can't subset columns that don't exist.
## x Columns `m8saq2_id` and `m8saq3_id` don't exist.
```

```r
####Math####
#calculate # of math items
assess_4th_grade_dta$math_length<-length(grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq"))

math_items<-colnames(assess_4th_grade_dta[,grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq")])


#calculate students math items correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(math_student_knowledge=100*rowMeans(.[grep(x=colnames(assess_4th_grade_dta), pattern="m8sbq")], na.rm=TRUE))

####Total score####
#calculate students percent correct
assess_4th_grade_dta <- assess_4th_grade_dta %>%
  mutate(student_knowledge=(math_student_knowledge+literacy_student_knowledge)/2) %>%
  mutate(student_proficient=100*as.numeric(student_knowledge>=82.9), #34/41
         student_proficient_nogiraffe=100*as.numeric((literacy_student_knowledge_nogiraffe+math_student_knowledge)/2>=86.6), #12/13 points
         student_proficient_70=100*as.numeric(student_knowledge>=70),
         student_proficient_75=100*as.numeric(student_knowledge>=75),
         literacy_student_proficient_nogiraffe=100*as.numeric(literacy_student_knowledge_nogiraffe>=92), #12/13 points
         literacy_student_proficient=100*as.numeric(literacy_student_knowledge>=83.3), #20/24 points
         literacy_student_proficient_70=100*as.numeric(literacy_student_knowledge>=70),
         literacy_student_proficient_75=100*as.numeric(literacy_student_knowledge>=75),
         math_student_proficient=100*as.numeric(math_student_knowledge>=82), #14/17 points
         math_student_proficient_70=100*as.numeric(math_student_knowledge>=70),
         math_student_proficient_75=100*as.numeric(math_student_knowledge>=75))
```

```
## Error in `mutate()`:
## ! Problem while computing `student_knowledge = (math_student_knowledge + literacy_student_knowledge)/2`.
## Caused by error:
## ! object 'literacy_student_knowledge' not found
```

```r
#save  4th grade data at student level anonymized
assess_4th_grade_anon <- assess_4th_grade_dta %>%
  select(school_code, interview__key, student_number, student_age, student_male, 
         contains('student_proficient'),
         contains('student_knowledge'),
         contains('ses'),
         math_items, lit_items)
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
assess_4th_grade_metadata <- makeVlist(assess_4th_grade_dta)


save(assess_4th_grade_anon, assess_4th_grade_metadta, 
     file = file.path(confidential_folder, "dashboard_4th_grade_assessment_data.RData"))
```

```
## Error in save(assess_4th_grade_anon, assess_4th_grade_metadta, file = file.path(confidential_folder, : object 'assess_4th_grade_anon' not found
```

```r
#calculate % correct for literacy, math, and total
final_indicator_data_LERN <- assess_4th_grade_anon %>%
  left_join(school_dta[,c('interview__key', 'm8_teacher_name', 'm8_teacher_code')]) %>%
  group_by(school_code) %>%
  mutate(n_students=n()) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items') , -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in left_join(., school_dta[, c("interview__key", "m8_teacher_name", : object 'assess_4th_grade_anon' not found
```

```r
#Breakdowns by Male/Female
final_indicator_data_LERN_M <- assess_4th_grade_anon %>%
  left_join(school_dta[,c('interview__key', 'm8_teacher_name', 'm8_teacher_code')]) %>%
  filter(student_male==1) %>%
  group_by(school_code) %>%
  mutate(n_students=n()) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items') , -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in left_join(., school_dta[, c("interview__key", "m8_teacher_name", : object 'assess_4th_grade_anon' not found
```

```r
final_indicator_data_LERN_F <- assess_4th_grade_anon %>%
  left_join(school_dta[,c('interview__key', 'm8_teacher_name', 'm8_teacher_code')]) %>%
  filter(student_male==0) %>%
  group_by(school_code) %>%
  mutate(n_students=n()) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items') , -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in left_join(., school_dta[, c("interview__key", "m8_teacher_name", : object 'assess_4th_grade_anon' not found
```

```r
#############################################
##### ECD Assessment ###########
#############################################

# School survey. Fraction correct on the Early Childhoold Assessment given to students in school.


#read in ecd level file
ecd_dta<-read_dta(file.path(download_folder, "ecd_assessment.dta"))

ecd_dta_metadata <- makeVlist(ecd_dta)


#Add school preamble info
ecd_dta <- ecd_dta %>%
  left_join(school_data_preamble) %>%
  select(preamble_info, everything())  
```

```
## Error in is.data.frame(y): object 'school_data_preamble' not found
```

```r
#create indicator for % correct on student assessment
#Note:  in the future we could incorporate irt program like mirt
#number of missing values
ecd_dta <- ecd_dta %>%
  mutate(n_mssing_ECD=n_miss_row(.))

#rename a few key variables up front
ecd_dta<- ecd_dta %>%
  mutate(ecd_student_name=m6s1q1  ,
         ecd_student_number=ecd_assessment__id,
         ecd_student_age=m6s1q2,
         ecd_student_male=bin_var(m6s1q3,1),
         ecd_consent=bin_var(m6s1q4,1)
  ) %>% 
  rename( #rename this variable to avoid dropping when I run anonymization program later
    m6s2q6a_nm_writing=m6s2q6a_name_writing,
    m6s2q6b_nm_writing_response=m6s2q6b_name_writing
  )

list_topics<-c("vocabn", "comprehension","letters","words","sentence","nm_writing","print",
               "countingproduce_set","number_ident","number_compare","simple_add",
               "backward_digit","head_shoulders",
               "perspective","conflict_resol")

#####################
#create one copy of each dataframe that never gets touched and is carried forward to public folder
#####################
ecd_dta_raw <- ecd_dta

#recode ECD variables to be 1 if student got it correct and zero otherwise
ecd_dta<- ecd_dta %>%
  mutate_at(vars(ends_with("comprehension"),
                 ends_with("letters"),
                 ends_with("words"),
                 ends_with("sentence"),
                 ends_with("nm_writing"),
                 ends_with("print"),
                 ends_with("produce_set"),
                 ends_with( "number_ident"),
                 ends_with("number_compare"),
                 ends_with("simple_add"),
                 ends_with("backward_digit"),
                 ends_with("perspective"),
                 ends_with("conflict_resol")), ~bin_var(.,1)  ) %>%
  mutate_at(vars(ends_with("head_shoulders")), ~if_else(.x==2,1,0,missing=NULL)) %>%
  mutate_at(vars(ends_with("vocabn")), ~case_when(.x==98 ~ as.numeric(NA),
                                                  .x==99 ~ 0,
                                                  .x==77 ~ 0,
                                                  (.x!=98 & .x!=99 & .x>=10) ~ 1,
                                                  (.x!=98 & .x!=99 & .x<10) ~ as.numeric(.x)/10,
                                                  is.na(.x) ~ as.numeric(NA))) %>%
  mutate_at(vars(ends_with("counting")), ~case_when(.x==98 ~ as.numeric(NA),
                                                    .x==99 ~ 0,
                                                    .x==77 ~ 0,
                                                    (.x!=98 & .x!=99 & .x>=30) ~ 1,
                                                    (.x!=98 & .x!=99 & .x<30) ~ as.numeric(.x)/30,
                                                    is.na(.x) ~ as.numeric(NA)))



####Literacy####
#calculate # of literacy items
#note: grep is a tool for searching text with specified pattern.  In our case, we are looking for m8s1q, which is the prefix of literacy items


lit_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "vocabn|comprehension|letters|words|sentence|nm_writing$|print")])

ecd_dta$literacy_length<-length(lit_items)

#calculate students lit items correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_literacy_student_knowledge=100*rowMeans(.[grep(x=colnames(ecd_dta), 
                                                   pattern="vocabn|comprehension|letters|words|sentence|nm_writing$|print")], na.rm=TRUE))

####Math####
#calculate # of math items

math_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "counting|produce_set|number_ident|number_compare|simple_add")])

ecd_dta$math_length<-length(math_items)


#calculate students math items correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_math_student_knowledge=100*rowMeans(.[grep(x=colnames(ecd_dta), 
                                               pattern="counting|produce_set|number_ident|number_compare|simple_add")], na.rm=TRUE))

####Executive Functioning####
#calculate # of Exec Function items

exec_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "backward_digit|head_shoulders")])

ecd_dta$exec_length<-length(exec_items)


#calculate students excec items correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_exec_student_knowledge=100*rowMeans(.[grep(x=colnames(ecd_dta), 
                                               pattern="backward_digit|head_shoulders")], na.rm=TRUE))

####Socio-Emotional####
#calculate # of Exec Function items

#NOTE:  Ending persepectives and conflict resolution in $ is a grep trick to make sure columns with trailing characters aren't included.  
#This means specifically the perspetive_responses conflict_resol_responses columns, which are text and just for quality control.
soc_items<-colnames(ecd_dta[,str_detect(
  colnames(ecd_dta), "perspective$|conflict_resol$")])

ecd_dta$soc_length<-length(soc_items)


#calculate students excec items correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_soc_student_knowledge=100*rowMeans(.[grep(x=colnames(ecd_dta), 
                                              pattern="perspective$|conflict_resol$")], na.rm=TRUE))


####Total score####
#calculate students percent correct
ecd_dta <- ecd_dta %>%
  mutate(ecd_student_knowledge=(ecd_math_student_knowledge+ecd_literacy_student_knowledge+
                                  ecd_exec_student_knowledge + ecd_soc_student_knowledge)/4) %>%
  mutate(ecd_student_proficiency=100*as.numeric(ecd_student_knowledge>=80),
         ecd_math_student_proficiency=100*as.numeric(ecd_math_student_knowledge>=80),
         ecd_literacy_student_proficiency=100*as.numeric(ecd_literacy_student_knowledge>=80),
         ecd_exec_student_proficiency=100*as.numeric(ecd_exec_student_knowledge>=80),
         ecd_soc_student_proficiency=100*as.numeric(ecd_soc_student_knowledge>=80)
  ) 
#save ecd data at student level anonymized
ecd_dta_anon <- ecd_dta %>%
  select(school_code, interview__key, ecd_student_number, ecd_student_age, ecd_student_male, 
         ecd_student_knowledge, ecd_math_student_knowledge, ecd_literacy_student_knowledge, ecd_soc_student_knowledge, ecd_exec_student_knowledge,
         ecd_student_proficiency, ecd_math_student_proficiency, ecd_literacy_student_proficiency, ecd_soc_student_proficiency, ecd_exec_student_proficiency,
         math_items, lit_items, soc_items, exec_items)
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
save(ecd_dta_anon, ecd_dta_metadata, 
     file = file.path(confidential_folder, "dashboard_ecd_data.RData"))
```

```
## Error in save(ecd_dta_anon, ecd_dta_metadata, file = file.path(confidential_folder, : object 'ecd_dta_anon' not found
```

```r
#calculate % correct for literacy, math, and total
final_indicator_data_LCAP <- ecd_dta_anon %>%
  left_join(school_dta[,c('interview__key',  'm6_teacher_code', 'm6_class_count', 'm6_instruction_time')]) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in left_join(., school_dta[, c("interview__key", "m6_teacher_code", : object 'ecd_dta_anon' not found
```

```r
#Breakdowns of Male/Female
final_indicator_data_LCAP_M <- ecd_dta_anon %>%
  left_join(school_dta[,c('interview__key', 'm6_teacher_code', 'm6_class_count', 'm6_instruction_time')]) %>%
  filter(ecd_student_male==1) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in left_join(., school_dta[, c("interview__key", "m6_teacher_code", : object 'ecd_dta_anon' not found
```

```r
final_indicator_data_LCAP_F <- ecd_dta_anon %>%
  left_join(school_dta[,c('interview__key', 'm6_teacher_code', 'm6_class_count', 'm6_instruction_time')]) %>%
  filter(ecd_student_male==0) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-ends_with('length'), -ends_with('items'), -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in left_join(., school_dta[, c("interview__key", "m6_teacher_code", : object 'ecd_dta_anon' not found
```

```r
#############################################
##### School Inputs ###########
#############################################

# School survey. Total score starts at 1 and points added are the sum of whether a school has: 
#   - Functional blackboard 
# - Pens, pencils, textbooks, exercise books 
# - Fraction of students in class with a desk 
# - Used ICT in class and have access to ICT in the school.


#functioning blackboard and chalk
school_data_INPT <- school_data_INPT %>%
  mutate(blackboard_functional=case_when(
    m4scq10_inpt==1 & m4scq9_inpt==1 & m4scq8_inpt==1  ~ 1,
    m4scq10_inpt==0 | m4scq9_inpt==0 | m4scq8_inpt==0 ~ 0)) 
```

```
## Error in mutate(., blackboard_functional = case_when(m4scq10_inpt == 1 & : object 'school_data_INPT' not found
```

```r
#pens, pencils, textbooks, exercise books
school_data_INPT <- school_data_INPT %>%
  mutate(share_textbook=(m4scq5_inpt)/(m4scq4_inpt)) %>%
  mutate(share_pencil=(m4scq6_inpt)/(m4scq4_inpt)) %>%
  mutate(share_exbook=(m4scq7_inpt)/(m4scq4_inpt)) %>%
  mutate(pens_etc=case_when(
    share_pencil>=0.9 & share_exbook>=0.9  ~ 1,
    share_pencil<0.9 | share_exbook<0.9 ~ 0),
    textbooks=case_when(
      share_textbook>=0.9   ~ 1,
      share_textbook<0.9  ~ 0)) 
```

```
## Error in mutate(., share_textbook = (m4scq5_inpt)/(m4scq4_inpt)): object 'school_data_INPT' not found
```

```r
#basic classroom furniture
school_data_INPT <- school_data_INPT %>%
  mutate(share_desk=1-(m4scq11_inpt)/(m4scq4_inpt))
```

```
## Error in mutate(., share_desk = 1 - (m4scq11_inpt)/(m4scq4_inpt)): object 'school_data_INPT' not found
```

```r
#Used ICT 
school_teacher_questionnaire_INPT <- teacher_questionnaire_INPT %>%
  group_by(school_code) %>%
  summarise(used_ict_pct=mean(m3sbq4_inpt, na.rm=TRUE))
```

```
## Error in group_by(., school_code): object 'teacher_questionnaire_INPT' not found
```

```r
school_data_INPT <- school_data_INPT %>%
  mutate(used_ict_num=case_when(
    m1sbq12_inpt==0  ~ 0,
    (m1sbq12_inpt>=1 ) ~ m1sbq14_inpt,
    (is.na(m1sbq12_inpt==0) | is.na(m1sbq14_inpt)) ~ as.numeric(NA)
  ))
```

```
## Error in mutate(., used_ict_num = case_when(m1sbq12_inpt == 0 ~ 0, (m1sbq12_inpt >= : object 'school_data_INPT' not found
```

```r
#access to ICT
school_data_INPT <- school_data_INPT %>%
  mutate(access_ict=case_when(
    m1sbq12_inpt==0 | m1sbq13_inpt==0 ~ 0,
    (m1sbq12_inpt>=1 & m1sbq13_inpt==1 ) ~ 1,
    (m1sbq12_inpt>=1 & m1sbq13_inpt==1 ) ~ 0.5, #Internet didn't work when tested
    (is.na(m1sbq12_inpt==0) | is.na(m1sbq13_inpt) ) ~ as.numeric(NA)
  ))
```

```
## Error in mutate(., access_ict = case_when(m1sbq12_inpt == 0 | m1sbq13_inpt == : object 'school_data_INPT' not found
```

```r
inpt_list<-c('blackboard_functional', 'pens_etc', 'textbooks', 'share_desk',  'used_ict', 'access_ict')

final_indicator_data_INPT <- school_data_INPT %>%
  left_join(school_teacher_questionnaire_INPT) %>%
  mutate(used_ict=if_else((used_ict_pct>=0.5 & used_ict_num>=3), 1,0))     %>%  #Set percentage of teachers to use ICT over 50% and number over 3
  group_by(school_code) %>%
  select(preamble_info, inpt_list, contains('INPT')) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_INPT=n_miss_row(.)) %>%
  mutate(inputs=textbooks+blackboard_functional + pens_etc + share_desk +  0.5*used_ict + 0.5*access_ict) %>%
  select(preamble_info, inputs, everything()) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))
```

```
## Error in left_join(., school_teacher_questionnaire_INPT): object 'school_data_INPT' not found
```

```r
#############################################
##### School Infrastructure ###########
#############################################

# School survey. Total score starts at 1 and points added are the sum of whether a school has: 
#   - Access to adequate drinking water 
# -Functional toilets.  Extra points available if are separate for boys/girls, private, useable, and have hand washing facilities 
# - Electricity  in the classroom 
# - Internet
# - School is accessible for those with disabilities (road access, a school ramp for wheelchairs, an entrance wide enough for wheelchairs, ramps to classrooms where needed, accessible toilets, and disability screening for seeing, hearing, and learning disabilities with partial credit for having 1 or 2 or the 3).)

#drinking water
school_data_INFR <- school_data_INFR %>%
  #
  mutate(drinking_water=if_else((m1sbq9_infr==1 | m1sbq9_infr==2 | m1sbq9_infr==5 | m1sbq9_infr==6), 1,0, as.numeric(NA) ))
```

```
## Error in mutate(., drinking_water = if_else((m1sbq9_infr == 1 | m1sbq9_infr == : object 'school_data_INFR' not found
```

```r
#functioning toilets
school_data_INFR <- school_data_INFR %>%
  mutate(toilet_exists=if_else(m1sbq1_infr==7 ,0,1),
         toilet_separate=if_else((m1sbq2_infr==1 | m1sbq2_infr==3),1,0),
         toilet_private=as.numeric(m1sbq4_infr),
         toilet_usable=as.numeric(m1sbq5_infr),
         toilet_handwashing=as.numeric(m1sbq7_infr),
         toilet_soap=as.numeric(m1sbq8_infr)) %>%
  mutate(functioning_toilet=case_when(
    # exist, separate for boys/girls, clean, private, useable,  handwashing available
    toilet_exists==1 & toilet_usable==1 & toilet_separate==1  & toilet_private==1  & toilet_handwashing==1 ~ 1,
    toilet_exists==0 | toilet_usable==0 | toilet_separate==0  | toilet_private==0  | toilet_handwashing==0 ~ 0
  )) 
```

```
## Error in mutate(., toilet_exists = if_else(m1sbq1_infr == 7, 0, 1), toilet_separate = if_else((m1sbq2_infr == : object 'school_data_INFR' not found
```

```r
#visibility
school_data_INFR <- school_data_INFR %>%
  left_join(select(school_data_INPT, interview__key, m4scq8_inpt, m4scq9_inpt, m4scq10_inpt, m1sbq15_inpt )) %>%
  mutate(visibility=case_when(
    m4scq10_inpt==1 &  m4scq8_inpt==1  ~ 1,
    m4scq10_inpt==0 & m4scq8_inpt==1 ~ 0)) 
```

```
## Error in left_join(., select(school_data_INPT, interview__key, m4scq8_inpt, : object 'school_data_INFR' not found
```

```r
#electricity
school_data_INFR <- school_data_INFR %>%
  mutate(class_electricity=if_else(m1sbq11_infr==1,1,0)) 
```

```
## Error in mutate(., class_electricity = if_else(m1sbq11_infr == 1, 1, 0)): object 'school_data_INFR' not found
```

```r
#accessibility for people with disabilities
final_indicator_data_INFR <- school_data_INFR %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(
    disab_road_access=bin_var(m1s0q2_infr,1),
    disab_school_ramp=case_when(
      m1s0q3_infr==0 ~ 1,
      (m1s0q4_infr	==1 & m1s0q3_infr	==1) ~ 1,
      (m1s0q4_infr	==0 & m1s0q3_infr	==1) ~ 0,
      is.na(m4scq1_infr) ~ as.numeric(NA)),
    disab_school_entr=bin_var(m1s0q5_infr,1),
    disab_class_ramp=case_when(
      m4scq1_infr==0 ~ 1,
      (m4scq1_infr==1 & m4scq1_infr==1) ~ 1,
      (m4scq1_infr==0 & m4scq1_infr==1) ~ 0,
      is.na(m4scq1_infr) ~ as.numeric(NA)),
    disab_class_entr=bin_var(m4scq3_infr,1),
    disab_screening=rowMeans(select(.,m1sbq17_infr__1,m1sbq17_infr__2,m1sbq17_infr__3), na.rm = TRUE),
    #sum up all components for overall disability accessibility score
    disability_accessibility=(disab_road_access+disab_school_ramp+disab_school_entr+
                                disab_class_ramp+disab_class_entr+
                                if_else(m1sbq1_infr==7,0,as.numeric(m1sbq6_infr))+
                                disab_screening)/7
  ) %>%
  mutate(internet=case_when(
    m1sbq15_inpt==2  ~ 1,
    m1sbq15_inpt==1  ~ 0.5,
    m1sbq15_inpt==0   ~ 0,
    is.na(as.numeric(m1sbq15_inpt)) ~ 0,
    TRUE ~ 0) ) # 1 point if internet working, 0.5 if doesn't work well, 0 if not at all
```

```
## Error in group_by(., school_code): object 'school_data_INFR' not found
```

```r
infr_list<-c('drinking_water', 'functioning_toilet', 'internet',  'class_electricity', 'disability_accessibility')

final_indicator_data_INFR <- final_indicator_data_INFR %>%
  mutate(n_mssing_INFR=n_miss_row(.)) %>%
  mutate(infrastructure=(drinking_water+ functioning_toilet+ internet + class_electricity+ disability_accessibility)) %>%
  select(preamble_info, infrastructure, everything()) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in mutate(., n_mssing_INFR = n_miss_row(.)): object 'final_indicator_data_INFR' not found
```

```r
#############################################
##### School Operational Management ###########
#############################################

#Princials/head teachers are given two vignettes:
```


```r
#Each vignette is worth 2 points.  
#
#The indicator will measure two things: presence of functions and quality of functions. In each vignette: 
```



```r
final_indicator_data_OPMN <- school_data_OPMN %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(
    vignette_1_resp=if_else((m7sbq1_opmn==0 & (m7sbq4_opmn==4 | m7sbq4_opmn==98)), 0, 0.5),
    vignette_1_finance=case_when(
      m7sbq2_opmn==1 ~ 0.5,
      (m7sbq2_opmn==2 | m7sbq2_opmn==97) ~ 0.25,
      m7sbq2_opmn==3 ~ 0,
      m7sbq1_opmn==0 & !(m7sbq4_opmn==4 | m7sbq4_opmn==98) ~ 0.5),
    vignette_1_address=if_else(m7sbq1_opmn==1, case_when(
      m7sbq3_opmn==1 ~ 0,
      (m7sbq3_opmn==2 | m7sbq3_opmn==97) ~ .5,
      m7sbq3_opmn==3 ~ 1),
      case_when(
        m7sbq5_opmn==1 ~ 0,
        m7sbq5_opmn==2 ~ .5,
        m7sbq5_opmn==3 ~ 1))) %>% 
  #give total score for this vignette
  mutate(vignette_1=vignette_1_resp+vignette_1_finance+vignette_1_address) %>% 
  mutate(vignette_2_resp=if_else(m7scq1_opmn==98, 0, 0.5), # no one responsible that is known
         vignette_2_finance=if_else(m7scq1_opmn==1,0,0.5),      #parents are forced to buy textbooks          
         #give partial credit based on how quickly it will be solved <1 month, 1-3, 3-6, 6-12, >1 yr
         vignette_2_address=case_when(
           m7scq2_opmn==1 ~ 1,
           m7scq2_opmn==2 ~ .75,
           m7scq2_opmn==3 ~ .5,
           m7scq2_opmn==4 ~ .25,
           (m7scq2_opmn==5 |m7scq2_opmn==98) ~ 0)) %>%
  mutate(vignette_2=vignette_2_resp+vignette_2_finance+vignette_2_address) %>%          #sum all components for overall score
  mutate(operational_management=1+vignette_1+vignette_2 )
```

```
## Error in group_by(., school_code): object 'school_data_OPMN' not found
```

```r
final_indicator_data_OPMN <- final_indicator_data_OPMN %>%
  mutate(n_mssing_OPMN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in mutate(., n_mssing_OPMN = n_miss_row(.)): object 'final_indicator_data_OPMN' not found
```

```r
#Breakdowns by Male/Female
final_indicator_data_OPMN_M <- final_indicator_data_OPMN %>%
  filter(m7saq10==1) %>%
  mutate(n_mssing_OPMN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in filter(., m7saq10 == 1): object 'final_indicator_data_OPMN' not found
```

```r
final_indicator_data_OPMN_F <- final_indicator_data_OPMN %>%
  filter(m7saq10==2) %>%
  mutate(n_mssing_OPMN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in filter(., m7saq10 == 2): object 'final_indicator_data_OPMN' not found
```

```r
#############################################
##### School Instructional Leadership ###########
#############################################

# School survey. Total score starts at 1 and points added are the sum of whether a teacher has: 
#   - Had a classroom observation in past year 
# - Had a discussion based on that observation that lasted longer than 10 min 
# - Received actionable feedback from that observation 
# - Teacher had a lesson plan and discussed it with another person

#list additional info that will be useful to keep in each indicator dataframe
preamble_info_teacher_drop_ildr <- c('interview__key', 'questionnaire_roster__id', 'teacher_name', 'teacher_number', 
                                     'available', 'teacher_position', 'teacher_grd1', 'teacher_grd2', 'teacher_grd3', 'teacher_grd4', 
                                     'teacher_language', 'teacher_math', 'teacher_both_subj', 'teacher_other_subj', 'teacher_education', 'teacher_year_began',
                                     'teacher_age')

final_indicator_data_ILDR <- teacher_questionnaire_ILDR %>%
  mutate(n_mssing_ILDR=n_miss_row(.)) %>%
  mutate(classroom_observed=bin_var(m3sdq15_ildr,1),
         classroom_observed_recent=if_else((classroom_observed==1 & m3sdq16_ildr<=12),1,0), #set recent to mean under 12 months
         purpose_observation=case_when(
           m3sdq18_ildr__1==1 ~ "Evaluation",
           m3sdq18_ildr__2==1 ~ "Professional Development",
           m3sdq18_ildr__3==1 ~ "Monitoring",
           m3sdq18_ildr__97==1 ~ m3sdq18_other_ildr ),
         discussion_30_min=bin_var(m3sdq20_ildr,3),
         discussed_observation=if_else((classroom_observed==1 & m3sdq19_ildr==1 & m3sdq20_ildr>=2),1,0), #make sure there was discussion and lasted more than 10 min
         feedback_observation=if_else((m3sdq21_ildr==1 & (m3sdq22_ildr__1==1 | m3sdq22_ildr__2==1 | m3sdq22_ildr__3==1
                                                          | m3sdq22_ildr__4==1 | m3sdq22_ildr__5==1)),1,0), #got feedback and was specific
         lesson_plan=if_else(m3sdq23_ildr==1,1,0),
         lesson_plan_w_feedback=if_else((m3sdq23_ildr==1 & m3sdq24_ildr==1),1,0)) %>%
  mutate(feedback_observation=if_else(m3sdq15_ildr==1 & m3sdq19_ildr==1, feedback_observation, 0)) %>% #fix an issue where teachers that never had classroom observed arent asked this question.
  mutate(instructional_leadership=1+0.5*classroom_observed + 0.5*classroom_observed_recent + discussed_observation + feedback_observation + lesson_plan_w_feedback) %>%
  mutate(instructional_leadership=if_else(classroom_observed==1,instructional_leadership, 1.5 + lesson_plan_w_feedback )) %>%
  group_by(interview__key) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-preamble_info_teacher_drop_ildr  )  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in mutate(., n_mssing_ILDR = n_miss_row(.)): object 'teacher_questionnaire_ILDR' not found
```

```r
#Breakdowns by Male/Female
final_indicator_data_ILDR_M <- final_indicator_data_ILDR %>%
  filter(m7saq10==1) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in filter(., m7saq10 == 1): object 'final_indicator_data_ILDR' not found
```

```r
final_indicator_data_ILDR_F <- final_indicator_data_ILDR %>%
  filter(m7saq10==2) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in filter(., m7saq10 == 2): object 'final_indicator_data_ILDR' not found
```

```r
#############################################
##### School Principal School Knowledge ###########
#############################################
# The aim of this indicator is to measure the extent to which principals have the knowledge about their own schools that is necessary for them to be effective managers. A score from 1 to 5 capturing the extent to which the principal is familiar with certain key aspects of the day-to-day workings of the school (in schools that have principals). Principal receives points in the following way: 
#   - 5 points. Principal gets all 90-100% of questions within accuracy bounds (defined below). 
# - 4 points. Principal gets 80-90% of question within accuracy bounds. 
# - 3 points. Principal gets 70-80% of question within accuracy bounds. 
# - 2 points. Principal gets 60-70% of question within accuracy bounds. 
# - 1 points. Principal gets under 60% of question within accuracy bounds. 
# 
# Accuracy bounds for each question. 
# Within 1 teacher/student for each of the following: 
#   - Out of these XX teachers, how many do you think would be able to correctly add triple digit numbers (i.e. 343+215+127)? 
#   - Out of these XX teachers, how many do you think would be able to correctly to multiply double digit numbers (i.e. 37 x 13)? 
#   - Out of these XX teachers, how many do you think would be able to complete sentences with the correct world (i.e. The accident _____ (see, saw, had seen, was seen) by three people)? 
#   - Any of these XX teachers have less than 3 years of experience? 
#   - Out of these XX teachers, which ones have less than 3 years of experience as a teacher? 
# Within 3 teacher/student for each of the following: 
#   - In the selected 4th grade classroom, how many of the pupils have the relevant textbooks? 
#   Must identify whether or not blackboard was working in a selected 4th grade classroom.




#first create a database containing actual values for each question for the principal
pknw_actual_cont <- final_indicator_data_CONT %>%
  select(school_code, m5_teach_count, m5_teach_count_math, m5s2q1c_number, m5s2q1e_number, m5s1q1f_grammer ) 
```

```
## Error in select(., school_code, m5_teach_count, m5_teach_count_math, m5s2q1c_number, : object 'final_indicator_data_CONT' not found
```

```r
pknw_actual_exper <- teacher_questionnaire %>%
  select(school_code, m3sb_tnumber, m3sb_troster,m3saq5, m3saq6 ) %>%
  mutate(experience=(2021-m3saq5)) %>%
  filter(experience <3) %>% 
  group_by(school_code) %>%
  summarise(teacher_count_experience_less3=n())
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
pknw_actual_school_inpts <- final_indicator_data_INPT %>%
  select(school_code, blackboard_functional, m4scq5_inpt, m4scq4_inpt)
```

```
## Error in select(., school_code, blackboard_functional, m4scq5_inpt, m4scq4_inpt): object 'final_indicator_data_INPT' not found
```

```r
pknw_actual_combined <- pknw_actual_school_inpts %>%
  left_join(pknw_actual_cont) %>%
  left_join(pknw_actual_exper) %>%
  mutate(teacher_count_experience_less3=if_else(is.na(teacher_count_experience_less3), as.numeric(0), as.numeric(teacher_count_experience_less3)),
         m5s2q1c_number=m5s2q1c_number*m5_teach_count,
         m5s2q1e_number=m5s2q1e_number*m5_teach_count,
         m5s1q1f_grammer=m5s1q1f_grammer*m5_teach_count)
```

```
## Error in left_join(., pknw_actual_cont): object 'pknw_actual_school_inpts' not found
```

```r
#create function to compare principal responses to actual
# if principal is within 1 student/teacher, then score as 1, 0 otherwise
principal_scorer <- function(var_guess, var_actual, var_total, margin1, margin2) {
  if_else(
    ((1-abs(var_guess-var_actual)/var_total>= as.numeric(margin1)) | (var_guess-var_actual <= as.numeric(margin2))),
    1,
    0)
}



final_indicator_data_PKNW <- school_data_PKNW %>%
  group_by(school_code) %>%
  select(school_code, starts_with('m7sfq5_pknw'), starts_with('m7sfq6_pknw'), starts_with('m7sfq7_pknw'), m7sfq9_pknw_filter, m7sfq10_pknw, m7sfq11_pknw, m7_teach_count_pknw, m7saq10) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_PKNW=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  %>%
  left_join(pknw_actual_combined) %>%
  mutate_at(vars(starts_with('m7sfq5_pknw'), starts_with('m7sfq6_pknw'), starts_with('m7sfq7_pknw')), ~if_else(is.na(.),as.numeric(NA),1)) %>%
  mutate(add_triple_digit_pknw=principal_scorer(rowSums(select(.,starts_with('m7sfq5_pknw')), na.rm=T), m5s2q1c_number, m7_teach_count_pknw,0.8,1),
         multiply_double_digit_pknw=principal_scorer(rowSums(select(.,starts_with('m7sfq6_pknw')), na.rm=T), m5s2q1e_number, m7_teach_count_pknw,0.8,1),
         complete_sentence_pknw=principal_scorer(rowSums(select(.,starts_with('m7sfq7_pknw')), na.rm=T), m5s1q1f_grammer, m7_teach_count_pknw,0.8,1),
         experience_pknw=principal_scorer(m7sfq9_pknw_filter, teacher_count_experience_less3, m7_teach_count_pknw,0.8,1),
         textbooks_pknw=principal_scorer(m7sfq10_pknw, m4scq5_inpt, m4scq4_inpt,0.8,3),
         blackboard_pknw=if_else(m7sfq11_pknw==blackboard_functional,1,0)) %>%
  mutate(principal_knowledge_avg=rowMeans(select(.,add_triple_digit_pknw, multiply_double_digit_pknw, complete_sentence_pknw, experience_pknw, textbooks_pknw, blackboard_pknw), na.rm=TRUE)) %>%
  mutate(principal_knowledge_score=case_when(
    principal_knowledge_avg ==1 ~ 5,
    (principal_knowledge_avg >=5/6 & principal_knowledge_avg<1) ~ 4,
    (principal_knowledge_avg >=4/6 & principal_knowledge_avg<5/6) ~ 3,
    (principal_knowledge_avg >=3/6 & principal_knowledge_avg<4/6) ~ 2,
    (principal_knowledge_avg <3/6 ) ~ 1  )
  ) %>%
  select(school_code, starts_with('m7sfq5_pknw'),m5s2q1c_number, starts_with('m7sfq6_pknw'), m5s2q1e_number, starts_with('m7sfq7_pknw'), m5s1q1f_grammer, m7sfq9_pknw_filter, teacher_count_experience_less3,  m7sfq10_pknw,m4scq5_inpt,  m7sfq11_pknw, blackboard_functional, principal_knowledge_score, add_triple_digit_pknw, 
         multiply_double_digit_pknw, complete_sentence_pknw, experience_pknw, textbooks_pknw, blackboard_pknw, m7_teach_count_pknw,m7saq10) %>%
  select(school_code, starts_with('m7sfq5_pknw'), starts_with('m7sfq6_pknw'), starts_with('m7sfq7_pknw'), m7sfq9_pknw_filter, m7sfq10_pknw, m7sfq11_pknw, principal_knowledge_score, add_triple_digit_pknw, 
         multiply_double_digit_pknw, complete_sentence_pknw, experience_pknw, textbooks_pknw, blackboard_pknw, m7_teach_count_pknw, m7saq10)
```

```
## Error in group_by(., school_code): object 'school_data_PKNW' not found
```

```r
#Breakdowns by Male/Female
final_indicator_data_PKNW_M <- final_indicator_data_PKNW %>%
  filter(m7saq10==1) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in filter(., m7saq10 == 1): object 'final_indicator_data_PKNW' not found
```

```r
final_indicator_data_PKNW_F <- final_indicator_data_PKNW %>%
  filter(m7saq10==2) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in filter(., m7saq10 == 2): object 'final_indicator_data_PKNW' not found
```

```r
#############################################
##### School Principal Management Skills ###########
#############################################


# Score of 1-5 based on sum of following: 
#   - 1 Point. School Goals Exists 
# - 1 Point. School goals are clear to school director, teachers, students, parents, and other members of community (partial credit available) 
# - 1 Point. Specific goals related to improving student achievement ( improving test scores, improving pass rates, reducing drop out, reducing absenteeism, improving pedagogy, more resources for infrastructure, more resources for inputs) 
# - 1 Point. School has defined system to measure goals (partial credit available)

#Create variables for whether school goals exists, are clear, are relevant to learning, and are measured in an appropriate way.


final_indicator_data_PMAN <- school_data_PMAN %>%
  mutate(sch_goals_exist=bin_var(m7sdq1_pman,1),
         sch_goals_clear=if_else(m7sdq1_pman==1, 
                                    rowMeans(select(.,m7sdq3_pman__1, m7sdq3_pman__2, m7sdq3_pman__3, m7sdq3_pman__4, m7sdq3_pman__5), na.rm=TRUE),
                                    0) ,
         sch_goals_relevant_total=rowSums(select(.,m7sdq4_pman__1, m7sdq4_pman__2, m7sdq4_pman__3, m7sdq4_pman__4, m7sdq4_pman__5, m7sdq4_pman__6, m7sdq4_pman__7, m7sdq4_pman__8, m7sdq4_pman__97), na.rm=TRUE),
         sch_goals_relevant=if_else(m7sdq1_pman==1, 
                                       case_when(
                                         (sch_goals_relevant_total > 0) ~ 1,
                                         (sch_goals_relevant_total == 0) ~ 0),
                                       0),
         sch_goals_measured=if_else((m7sdq1_pman==1), 
                                       case_when(
                                         (m7sdq5_pman==1) ~ 0,
                                         (m7sdq5_pman==2 | m7sdq5_pman==97 ) ~ 0.5,
                                         (m7sdq5_pman==3) ~ 1),
                                       0)) %>%
  mutate(goal_setting=1+sch_goals_exist+sch_goals_clear+sch_goals_relevant+sch_goals_measured) %>%
  # Now for problem solving
  mutate(
    problem_solving_proactive=case_when(
      (m7seq1_pman==4 ) ~ 1,
      (m7seq1_pman==2 | m7seq1_pman==3 ) ~ 0.5,
      (m7seq1_pman==1 | m7seq1_pman==98 ) ~ 0,
      TRUE ~ 0),
    problem_solving_info_collect=(m7seq2_pman__1+m7seq2_pman__2 + m7seq2_pman__3 + m7seq2_pman__4)/4,
    problem_solving_stomach=case_when(
      (m7seq3_pman==4 ) ~ 1,
      (m7seq3_pman==3 ) ~ 0.5,
      (m7seq3_pman==1 | m7seq3_pman==2 | m7seq3_pman==98 ) ~ 0.25,
      TRUE ~ 0)
    
  ) %>%
  mutate(problem_solving=1+(4/3)*problem_solving_proactive+(4/3)*problem_solving_info_collect+(4/3)*problem_solving_stomach) %>%
  
  
  mutate(principal_management=(goal_setting+problem_solving)/2)
```

```
## Error in mutate(., sch_goals_exist = bin_var(m7sdq1_pman, 1), sch_goals_clear = if_else(m7sdq1_pman == : object 'school_data_PMAN' not found
```

```r
final_indicator_data_PMAN <- final_indicator_data_PMAN %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_PMAN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in group_by(., school_code): object 'final_indicator_data_PMAN' not found
```

```r
#Breakdowns by Male/Female
final_indicator_data_PMAN_M <- final_indicator_data_PMAN %>%
  filter(m7saq10==1) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in filter(., m7saq10 == 1): object 'final_indicator_data_PMAN' not found
```

```r
final_indicator_data_PMAN_F <- final_indicator_data_PMAN %>%
  filter(m7saq10==2) %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in filter(., m7saq10 == 2): object 'final_indicator_data_PMAN' not found
```

```r
#############################################
##### Teacher Teaching Attraction ###########
#############################################

# In the school survey, a number of De Facto questions on teacher attraction are asked. 0.8 points is awarded for each of the following: 
#   - 0.8 Points. Teacher satisfied with job 
# - 0.8 Points. Teacher satisfied with status in community 
# - 0.8 Points. Would better teachers be promoted faster? 
#   - 0.8 Points. Do teachers receive bonuses? 
#   - 0.8 Points. One minus the fraction of months in past year with a salary delay.


#create function to clean teacher attitudes questions.  Need to reverse the order for scoring for some questions.  
#Should have thought about this, when programming in Survey Solutions and scale 1-5.

attitude_fun  <- function(x) {
  case_when(
    x==99 ~ as.numeric(NA),
    x==4 ~ 5,
    x==3 ~ 3.67,
    x==2 ~ 2.33,
    x==1 ~ 1
  )
}

attitude_fun_rev  <- function(x) {
  case_when(
    x==99 ~ as.numeric(NA),
    x==1 ~ 5,
    x==2 ~ 3.67,
    x==3 ~ 2.33,
    x==4 ~ 1
  )
}


teacher_questionnaire_TATT <- teacher_questionnaire_TATT %>%
  mutate(teacher_satisfied_job=attitude_fun_rev(m3seq1_tatt)/5,
         teacher_satisfied_status=attitude_fun_rev(m3seq2_tatt)/5,
         better_teachers_promoted=bin_var(m3seq3_tatt,1),
         teacher_bonus=bin_var(m3seq4_tatt,1),
         teacher_bonus_attend=if_else(m3seq4_tatt==1,
                                      bin_var(m3seq5_tatt__1,1),
                                      0),
         teacher_bonus_student_perform=if_else(m3seq4_tatt==1,
                                               bin_var(m3seq5_tatt__2,1),
                                               0),
         teacher_bonus_extra_duty=if_else(m3seq4_tatt==1,
                                          bin_var(m3seq5_tatt__3,1),
                                          0),
         teacher_bonus_hard_staff=if_else(m3seq4_tatt==1,
                                          bin_var(m3seq5_tatt__4,1),
                                          0),
         teacher_bonus_subj_shortages=if_else(m3seq4_tatt==1,
                                              bin_var(m3seq5_tatt__5,1),
                                              0),
         teacher_bonus_add_qualif=if_else(m3seq4_tatt==1,
                                          bin_var(m3seq5_tatt__6,1),
                                          0),
         teacher_bonus_school_perform=if_else(m3seq4_tatt==1,
                                              bin_var(m3seq5_tatt__7,1),
                                              0),
         teacher_bonus_other=if_else(m3seq4_tatt==1,
                                     if_else(m3seq5_tatt__97==1,m3seq5_other_tatt,"NA"),
                                     "NA"),
         salary_delays=if_else(m3seq6_tatt==1, m3seq7_tatt,0)) %>%
  mutate(salary_delays=if_else(salary_delays>12,12,salary_delays)) %>%
  mutate(teacher_attraction=1+0.8*teacher_satisfied_job+.8*teacher_satisfied_status+.8*better_teachers_promoted+.8*teacher_bonus+.8*(1-salary_delays/12))
```

```
## Error in mutate(., teacher_satisfied_job = attitude_fun_rev(m3seq1_tatt)/5, : object 'teacher_questionnaire_TATT' not found
```

```r
final_indicator_data_TATT <- teacher_questionnaire_TATT %>%
  mutate(n_mssing_TATT=n_miss_row(.)) %>%
  group_by(interview__key) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in mutate(., n_mssing_TATT = n_miss_row(.)): object 'teacher_questionnaire_TATT' not found
```

```r
#############################################
##### Teacher Teaching Selection and Deployment ###########
#############################################

# School Survey. The De Facto portion of the Teacher Selection and Deployment Indicator considers two issues: how teachers are selected into the profession and how teachers are assigned to positions (transferred) once in the profession. Research shows that degrees and years of experience explanin little variation in teacher quality, so more points are assigned for systems that also base hiring on content knowledge or pedagogical skill. 2 points are available for the way teachers are selected and 2 points are available for deployment. 
# 
# Selection 
# - 0 Points. None of the below 
# - 1 point. Teachers selected based on completion of coursework, educational qualifications, graduating from tertiary program (including specialized programs), selected based on experience 
# - 2 points. Teacher recruited based on passing written content knowledge test, passed interview stage assessment, passed an assessment conducted by supervisor based on practical experience, conduct during mockup class. 
# 
# Deployment 
# - 0 Points. None of the below 
# - 1 point. Teachers deployed based on years of experience or job title hierarchy 
# - 2 points. Teacher deployed based on performance assessed by school authority, colleagues, or external evaluator, results of interview.


teacher_questionnaire_TSDP <- teacher_questionnaire_TSDP %>%
  mutate(
    teacher_selection=case_when(
      (m3sdq1_tsdp__5==1 | m3sdq1_tsdp__6==1 | m3sdq1_tsdp__8==1 | m3sdq1_tsdp__9==1 )  ~ 2,
      (m3sdq1_tsdp__1==1 | m3sdq1_tsdp__2==1 | m3sdq1_tsdp__3==1 | m3sdq1_tsdp__4==1 | m3sdq1_tsdp__7==1) ~ 1,
      (m3sdq1_tsdp__1==0 & m3sdq1_tsdp__2==0 & m3sdq1_tsdp__3==0 & m3sdq1_tsdp__4==0 & m3sdq1_tsdp__5==0 & m3sdq1_tsdp__6==0 & m3sdq1_tsdp__7==0 & m3sdq1_tsdp__8==0 & m3sdq1_tsdp__9==0) ~ 0
    ),
    teacher_deployment=case_when(
      (m3seq8_tsdp__3==1 | m3seq8_tsdp__4==1 | m3seq8_tsdp__5==1  )  ~ 2,
      (m3seq8_tsdp__1==1 | m3seq8_tsdp__2==1 | m3seq8_tsdp__97==1) ~ 1,
      ((m3seq8_tsdp__1==0 & m3seq8_tsdp__2==0 & m3seq8_tsdp__3==0 & m3seq8_tsdp__4==0 & m3seq8_tsdp__5==0) | ( m3seq8_tsdp__99==1)) ~ 0
      
    )
  ) %>%
  mutate(teacher_selection_deployment=1+teacher_selection+teacher_deployment)
```

```
## Error in mutate(., teacher_selection = case_when((m3sdq1_tsdp__5 == 1 | : object 'teacher_questionnaire_TSDP' not found
```

```r
final_indicator_data_TSDP <- teacher_questionnaire_TSDP %>%  
  mutate(n_mssing_TSDP=n_miss_row(.)) %>%
  group_by(interview__key) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in mutate(., n_mssing_TSDP = n_miss_row(.)): object 'teacher_questionnaire_TSDP' not found
```

```r
#############################################
##### Teacher Teaching Support ###########
#############################################


# School survey. Our teaching support indicator asks teachers about participation and the experience with several types of formal/informal training: 
#   
#   Pre-Service (Induction) Training: 
#   - 0.5 Points. Had a pre-service training 
# - 0.5 Points. Teacher reported receiving usable skills from training 
# 
# Teacher practicum (teach a class with supervision) 
# - 0.5 Points. Teacher participated in a practicum 
# - 0.5 Points. Practicum lasted more than 3 months and teacher spent more than one hour per day teaching to students. 
# 
# In-Service Training: 
#   - 0.5 Points. Had an in-service training 
# - 0.25 Points. In-service training lasted more than 2 total days 
# - 0.125 Points. More than 25% of the in-service training was done in the classroom. 
# - 0.125 Points. More than 50% of the in-service training was done in the classroom. 
# 
# Opportunities for teachers to come together to share ways of improving teaching: 
#   - 1 Point if such opportunities exist.

#Add in question on teach opportunities so share ways of teaching
opp_share<- teacher_questionnaire_ILDR %>%
  select(interview__key, m3sdq14_ildr) %>%
  mutate(opportunities_teachers_share=bin_var(m3sdq14_ildr,1))
```

```
## Error in select(., interview__key, m3sdq14_ildr): object 'teacher_questionnaire_ILDR' not found
```

```r
teacher_questionnaire_TSUP <- teacher_questionnaire_TSUP %>%
  mutate(pre_training_exists=bin_var(m3sdq3_tsup,1)/2,
         pre_training_useful=if_else(m3sdq3_tsup==1,
                                     bin_var(m3sdq4_tsup,1),
                                     0)/2,
         pre_training_practicum=bin_var(m3sdq6_tsup,1)/2,
         pre_training_practicum_lngth=case_when(
           (m3sdq6_tsup==1 & m3sdq7_tsup>=3 & m3sdq8_tsup>=1) ~  0.5,
           (m3sdq6_tsup==1 & (m3sdq7_tsup<3 | m3sdq8_tsup<1))  ~ 0,
           m3sdq6_tsup==2 ~ 0,
           TRUE ~ 0),
         in_service_exists=bin_var(m3sdq9_tsup,1),
         in_servce_lngth=case_when(
           (m3sdq9_tsup==1 & m3sdq10_tsup>2 ) ~ 1,
           (m3sdq9_tsup==1 & m3sdq10_tsup<=2) ~ 0,
           m3sdq9_tsup==0 ~ 0,
           TRUE ~ 0
         ),
         in_service_classroom=case_when(
           (m3sdq9_tsup==1 & m3sdq13_tsup>=3)  ~ 1,
           (m3sdq9_tsup==1 & m3sdq13_tsup==2)  ~ 0.5,
           (m3sdq9_tsup==1 & m3sdq13_tsup==1)  ~ 0,
           m3sdq9_tsup==0 ~ 0,
           TRUE ~ 0
         )
  ) %>%
  left_join(opp_share) %>%
  mutate(pre_service=pre_training_exists+pre_training_useful,
         practicum=pre_training_practicum+pre_training_practicum_lngth,
         in_service=0.5*in_service_exists+0.25*in_servce_lngth+0.25*in_service_classroom) %>%
  mutate(teacher_support=1+pre_service+practicum+in_service+opportunities_teachers_share) 
```

```
## Error in mutate(., pre_training_exists = bin_var(m3sdq3_tsup, 1)/2, pre_training_useful = if_else(m3sdq3_tsup == : object 'teacher_questionnaire_TSUP' not found
```

```r
# mutate(teacher_support=if_else(teacher_support>5,5,teacher_support)) #need to fix






final_indicator_data_TSUP <- teacher_questionnaire_TSUP %>%
  mutate(n_mssing_TSUP=n_miss_row(.)) %>%
  group_by(interview__key) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in mutate(., n_mssing_TSUP = n_miss_row(.)): object 'teacher_questionnaire_TSUP' not found
```

```r
#############################################
##### Teacher Teaching Evaluation ###########
#############################################

# School survey. This policy lever measures whether there is a teacher evaluation system in place, and if so, the types of decisions that are made based on the evaluation results. Score is the sum of the following: 
#   - 1 Point. Was teacher formally evaluated in past school year? 
#   - 1 Point total. 0.2 points for each of the following: Evaluation included evaluation of attendance, knowledge of subject matter, pedagogical skills in the classroom, students' academic achievement, students' socio-emotional development 
# - 1 Point. Consequences exist if teacher receives 2 or more negative evaluations 
# - 1 Point. Rewards exist if teacher receives 2 or more positive evaluations


#list of teacher evluation questions
tevl<-c(
  'm3sbq7_tmna__1', 'm3sbq7_tmna__2', 'm3sbq7_tmna__3', 'm3sbq7_tmna__4','m3sbq7_tmna__5', 'm3sbq7_tmna__6', 'm3sbq7_tmna__97',
  'm3sbq8_tmna__2', 'm3sbq8_tmna__3', 'm3sbq8_tmna__4','m3sbq8_tmna__5', 'm3sbq8_tmna__6', 'm3sbq8_tmna__7', 'm3sbq8_tmna__8', 'm3sbq8_tmna__97', 'm3sbq8_tmna__98',
  'm3sbq9_tmna__1', 'm3sbq9_tmna__2', 'm3sbq9_tmna__3', 'm3sbq9_tmna__4', 'm3sbq9_tmna__7', 'm3sbq9_tmna__97', 'm3sbq9_tmna__98',
  'm3bq10_tmna__1', 'm3bq10_tmna__2', 'm3bq10_tmna__3', 'm3bq10_tmna__4', 'm3bq10_tmna__7', 'm3bq10_tmna__97', 'm3bq10_tmna__98')

teacher_questionnaire_TEVL <- teacher_questionnaire_TMNA %>%
  dplyr::select(school_code, preamble_info_teacher, tevl, m3sbq6_tmna, m3sbq8_tmna__1)
```

```
## Error in dplyr::select(., school_code, preamble_info_teacher, tevl, m3sbq6_tmna, : object 'teacher_questionnaire_TMNA' not found
```

```r
teacher_questionnaire_TEVL<- teacher_questionnaire_TEVL %>%
  mutate(formally_evaluated=bin_var(m3sbq6_tmna,1),
         evaluation_content=if_else(m3sbq6_tmna==1,
                                    (m3sbq8_tmna__1+m3sbq8_tmna__2+ m3sbq8_tmna__3 + m3sbq8_tmna__5 + m3sbq8_tmna__6)/5,
                                    0),
         negative_consequences=case_when(
           (m3sbq9_tmna__1==1 | m3sbq9_tmna__2==1 | m3sbq9_tmna__3==1 | m3sbq9_tmna__4==1 | m3sbq9_tmna__97==1) ~ 1,
           (is.na(m3sbq9_tmna__1) & is.na(m3sbq9_tmna__2) & is.na(m3sbq9_tmna__3) & is.na(m3sbq9_tmna__4) & is.na(m3sbq9_tmna__97)) ~ as.numeric(NA),
           TRUE ~ 0),
         positive_consequences=case_when(
           (m3bq10_tmna__1==1 | m3bq10_tmna__2==1 | m3bq10_tmna__3==1 | m3bq10_tmna__4==1 | m3bq10_tmna__97==1) ~ 1,
           (is.na(m3bq10_tmna__1) & is.na(m3bq10_tmna__2) & is.na(m3bq10_tmna__3) & is.na(m3bq10_tmna__4) & is.na(m3bq10_tmna__97)) ~ as.numeric(NA),
           TRUE ~ 0)
  ) %>%
  mutate(teaching_evaluation=1+formally_evaluated+evaluation_content+negative_consequences+positive_consequences)
```

```
## Error in mutate(., formally_evaluated = bin_var(m3sbq6_tmna, 1), evaluation_content = if_else(m3sbq6_tmna == : object 'teacher_questionnaire_TEVL' not found
```

```r
final_indicator_data_TEVL <- teacher_questionnaire_TEVL %>%
  mutate(n_mssing_TEVL=n_miss_row(.)) %>%
  group_by(interview__key) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in mutate(., n_mssing_TEVL = n_miss_row(.)): object 'teacher_questionnaire_TEVL' not found
```

```r
#############################################
##### Teacher  Monitoring and Accountability ###########
#############################################

# School Survey. This policy lever measures the extent to which teacher presence is being monitored, whether attendance is rewarded, and whether there are consequences for chronic absence. Score is the sum of the following: 
#   - 1 Point. Teachers evaluated by some authority on basis of absence. 
# - 1 Point. Good attendance is rewarded. 
# - 1 Point. There are consequences for chronic absence (more than 30% absence). 
# - 1 Point. One minus the fraction of teachers that had to miss class because of any of the following: collect paycheck, school administrative procedure, errands or request of the school district office, other administrative tasks.

teacher_questionnaire_TMNA2 <- teacher_questionnaire_TATT %>%
  select(interview__key, school_code, teacher_name, teacher_number, m3seq4_tatt, m3seq5_tatt__1, m3sbq1_tatt__1, m3sbq1_tatt__2, m3sbq1_tatt__3, m3sbq1_tatt__97, m3sbq1_other_tatt )
```

```
## Error in select(., interview__key, school_code, teacher_name, teacher_number, : object 'teacher_questionnaire_TATT' not found
```

```r
teacher_questionnaire_TMNA <- teacher_questionnaire_TMNA %>%
  dplyr::select(-tevl)
```

```
## Error in dplyr::select(., -tevl): object 'teacher_questionnaire_TMNA' not found
```

```r
teacher_questionnaire_TMNA <- teacher_questionnaire_TMNA %>%
  left_join(teacher_questionnaire_TMNA2)
```

```
## Error in left_join(., teacher_questionnaire_TMNA2): object 'teacher_questionnaire_TMNA' not found
```

```r
teacher_questionnaire_TMNA <- teacher_questionnaire_TMNA %>%
  mutate(attendance_evaluated=if_else(m3sbq6_tmna==1,
                                      case_when(
                                        (m3sbq8_tmna__1==1) ~ 1,
                                        TRUE ~ 0
                                      ),
                                      0),
         attendance_rewarded=if_else(m3seq4_tatt==1,
                                     case_when(
                                       (m3seq5_tatt__1==1) ~ 1,
                                       TRUE ~ 0
                                     ),
                                     0),
         attendence_sanctions=case_when(
           (m3sbq2_tmna__1==1 | m3sbq2_tmna__2==1 | m3sbq2_tmna__3==1 | m3sbq2_tmna__4==1 | m3sbq2_tmna__97==1) ~ 1,
           (is.na(m3sbq2_tmna__1) & is.na(m3sbq2_tmna__2) & is.na(m3sbq2_tmna__3) & is.na(m3sbq2_tmna__4) & is.na(m3sbq2_tmna__97)) ~ as.numeric(NA),
           TRUE ~ 0
         ),
         miss_class_admin=case_when(
           (m3sbq1_tatt__1==1 | m3sbq1_tatt__2==1 | m3sbq1_tatt__3==1 | m3sbq1_tatt__97==1) ~ 1,
           (m3sbq1_tatt__1==0 & m3sbq1_tatt__2==0 & m3sbq1_tatt__3==0 & m3sbq1_tatt__97==0) ~ 0,
           (grepl('salud', teacher_questionnaire_TMNA$m3sbq1_other_tatt)) ~ 0, #some teachers reported missing for health reasons, we don't want these included.
           TRUE ~ as.numeric(NA)
         )
  )  %>%
  mutate(teacher_monitoring=1+attendance_evaluated + 1*attendance_rewarded + 1*attendence_sanctions + (1-miss_class_admin))
```

```
## Error in mutate(., attendance_evaluated = if_else(m3sbq6_tmna == 1, case_when((m3sbq8_tmna__1 == : object 'teacher_questionnaire_TMNA' not found
```

```r
final_indicator_data_TMNA <- teacher_questionnaire_TMNA %>%
  mutate(n_mssing_TMNA=n_miss_row(.)) %>%
  group_by(interview__key) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in mutate(., n_mssing_TMNA = n_miss_row(.)): object 'teacher_questionnaire_TMNA' not found
```

```r
#############################################
##### Teacher  Intrinsic Motivation ###########
#############################################

# 
# School Survey. This lever measures whether teachers are intrinsically motivated to teach. The question(s) aim to address this 
# phenomenon by measuring the level of intrinsic motivation among teachers as well as teacher values that may be relevant for 
# ensuring that the teacher is motivated to focus on all children and not just some. Average score (1 (worst) - 5 (best)) on items 
# given to teachers on intrinsic motivation.

intrinsic_motiv_q_rev <- c('m3scq1_tinm','m3scq2_tinm', 'm3scq3_tinm', 'm3scq4_tinm', 'm3scq5_tinm', 'm3scq6_tinm',
                           'm3scq7_tinm', 'm3scq10_tinm')

intrinsic_motiv_q <- c( 'm3scq11_tinm', 'm3scq14_tinm')

intrinsic_motiv_q_all <- c('m3scq1_tinm','m3scq2_tinm', 'm3scq3_tinm', 'm3scq4_tinm', 'm3scq5_tinm', 'm3scq6_tinm',
                           'm3scq7_tinm', 'm3scq10_tinm', 'm3scq11_tinm', 'm3scq14_tinm')

teacher_questionnaire_TINM2 <- teacher_questionnaire_TMNA %>%
  dplyr::select(school_code, preamble_info_teacher, m3sdq2_tmna)
```

```
## Error in dplyr::select(., school_code, preamble_info_teacher, m3sdq2_tmna): object 'teacher_questionnaire_TMNA' not found
```

```r
final_indicator_data_TINM <- teacher_questionnaire_TINM %>%
  left_join(teacher_questionnaire_TINM2) %>%
  mutate(n_mssing_TINM=n_miss_row(.)) %>%
  mutate(    
    SE_PRM_TINM_1 = 100*if_else(m3scq1_tinm>=3,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if the ~
    SE_PRM_TINM_2 = 100*if_else(m3scq2_tinm>=3,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if stud~
    SE_PRM_TINM_3 = 100*if_else(m3scq3_tinm>=3,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with It is acceptable for a teacher to be absent if the ~
    SE_PRM_TINM_4 = 100*if_else(m3scq4_tinm>=3,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they attend scho~
    SE_PRM_TINM_5 = 100*if_else(m3scq5_tinm>=3,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they come to sch~
    SE_PRM_TINM_6 = 100*if_else(m3scq6_tinm>=3,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students deserve more attention if they are motivat~
    SE_PRM_TINM_7 = 100*if_else(m3scq7_tinm>=3,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students have a certain amount of intelligence and ~
    SE_PRM_TINM_8 = 100*if_else(m3scq10_tinm>=3,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with To be honest, students can't really change how inte~
    SE_PRM_TINM_9 = 100*if_else(m3scq11_tinm>=3,1,0),  #(De Facto) Percent of teachers that agree or strongly agrees with Students can always substantially change how intell~
    SE_PRM_TINM_10 = 100*if_else(m3scq14_tinm>=3,1,0) #(De Facto) Percent of teachers that agree or strongly agrees with /"Students can change even their basic intelligence l~
  ) %>%
  mutate_at(intrinsic_motiv_q_rev, attitude_fun_rev ) %>%
  mutate_at(intrinsic_motiv_q, attitude_fun ) %>%
  mutate(acceptable_absent=(m3scq1_tinm+ m3scq2_tinm + m3scq3_tinm)/3,
         students_deserve_attention=(m3scq4_tinm+ m3scq5_tinm + m3scq6_tinm )/3,
         growth_mindset=(m3scq7_tinm + m3scq10_tinm + m3scq11_tinm + m3scq14_tinm)/4,
         motivation_teaching=case_when(
           m3scq15_tinm__3>=1 ~ 0,
           (m3scq15_tinm__3!=1 & (m3scq15_tinm__1>=1 | m3scq15_tinm__2>=1 | m3scq15_tinm__4>=1 & m3scq15_tinm__5>=1)) ~ 1,
           TRUE ~ as.numeric(NA)
         )) %>%
  mutate(intrinsic_motivation=1+0.8*(0.2*acceptable_absent + 0.2*students_deserve_attention + 0.2*growth_mindset + motivation_teaching+bin_var(m3sdq2_tmna,1))) %>%
  group_by(school_code) %>%
  summarise_all( ~(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  select(-drop_teacher_info)  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in left_join(., teacher_questionnaire_TINM2): object 'teacher_questionnaire_TINM' not found
```

```r
#############################################
##### School  Inputs and Infrastructure Standards ###########
#############################################
#   - 1 Point. Are there standards in place to monitor blackboard and chalk, pens and pencils, basic classroom furniture, computers, textbooks, exercise books, toilets, electricity, drinking water, accessibility for those with disabilities? (partial credit available) 

school_data_ISTD <- school_data_IMON %>%
  mutate(standards_monitoring_input=rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                    pattern="m1scq13_imon__")], na.rm=TRUE),
         standards_monitoring_infrastructure=rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                             pattern="m1scq14_imon__")], na.rm=TRUE) ) %>%
  mutate(standards_monitoring=(standards_monitoring_input*6+standards_monitoring_infrastructure*4)/2)
```

```
## Error in mutate(., standards_monitoring_input = rowMeans(.[grep(x = colnames(school_data_IMON), : object 'school_data_IMON' not found
```

```r
final_indicator_data_ISTD <- school_data_ISTD %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_ISTD=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in group_by(., school_code): object 'school_data_ISTD' not found
```

```r
#############################################
##### School  Inputs and Infrastructure Monitoring ###########
#############################################

# School Survey. A score of 1-5 based on 3 factors. Each factor has received an equal weight in terms of points. The factors are the following: 
```



```r
school_data_IMON <- school_data_IMON %>%
  mutate(m1scq3_imon=bin_var(m1scq3_imon,1),
         system_in_place=case_when(
           m1scq5_imon==0 ~ 0,
           m1scq5_imon==1 ~ 1,
           m1scq5_imon==2 ~ 0.5,
           TRUE ~ 0
         )) %>%
  mutate(monitoring_inputs=if_else(m1scq1_imon==1,
                                   rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                   pattern="m1scq4_imon__")], na.rm=TRUE),
                                   0),
         monitoring_infrastructure=if_else(m1scq7_imon==1,
                                           rowMeans(.[grep(x=colnames(school_data_IMON), 
                                                           pattern="m1scq9_imon__")], na.rm=TRUE),
                                           0),
  ) %>%
  mutate(parents_involved=if_else(m1scq3_imon==1,1,0,0)) %>%
  mutate(sch_monitoring=1+(monitoring_inputs+monitoring_infrastructure)/2+system_in_place+parents_involved)
```

```
## Error in mutate(., m1scq3_imon = bin_var(m1scq3_imon, 1), system_in_place = case_when(m1scq5_imon == : object 'school_data_IMON' not found
```

```r
final_indicator_data_IMON <- school_data_IMON %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_IMON=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in group_by(., school_code): object 'school_data_IMON' not found
```

```r
#############################################
##### School School Management Clarity of Functions  ###########
#############################################

school_data_SCFN <- school_data_PKNW %>%
  mutate(infrastructure_scfn=if_else((m7sfq15a_pknw__0==1 | m7sfq15a_pknw__98==1),0,1),
         materials_scfn=if_else((m7sfq15b_pknw__0==1 | m7sfq15b_pknw__98==1),0,1),
         hiring_scfn=if_else((m7sfq15c_pknw__0==1 | m7sfq15c_pknw__98==1),0,1),
         supervision_scfn=if_else((m7sfq15d_pknw__0==1 | m7sfq15d_pknw__98==1),0,1),
         student_scfn=if_else((m7sfq15e_pknw__0==1 | m7sfq15e_pknw__98==1),0,1),
         principal_hiring_scfn=if_else((m7sfq15f_pknw__0==1 | m7sfq15f_pknw__98==1),0,1),
         principal_supervision_scfn=if_else((m7sfq15g_pknw__0==1 | m7sfq15g_pknw__98==1),0,1)
  ) %>%
  mutate(sch_management_clarity=1+
           (infrastructure_scfn+materials_scfn)/2+
           (hiring_scfn + supervision_scfn)/2 +
           student_scfn +
           (principal_hiring_scfn+ principal_supervision_scfn)/2
  )
```

```
## Error in mutate(., infrastructure_scfn = if_else((m7sfq15a_pknw__0 == : object 'school_data_PKNW' not found
```

```r
final_indicator_data_SCFN <- school_data_SCFN %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SCFN=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in group_by(., school_code): object 'school_data_SCFN' not found
```

```r
#############################################
##### School School Management Attraction  ###########
#############################################

# This policy lever measures whether the right candidates are being attracted to the profession of school principals. The questions will aim to capture the provision of benefits to attract and maintain the best people to serve as principals. 
# 
# Scoring: 
#   -score is between 1-5 based on how satisfied the principal is with status in community. We will also add in component based on Principal salaries.
# For salary, based GDP per capita from 2018 World Bank  https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=ET.  

school_data_SATT <- school_data_SATT %>%
  mutate(principal_satisfaction=attitude_fun_rev(m7shq1_satt),
         principal_salary=12*m7shq2_satt/29351.46	) %>%
  mutate(
    principal_salary_score=case_when(
      between(principal_salary,0,0.5) ~ 1,
      between(principal_salary,0.5,0.75) ~ 2,
      between(principal_salary,0.75,1) ~ 3,
      between(principal_salary,1,1.5) ~ 4,
      between(principal_salary,1.5,15) ~ 5)) %>%
  mutate(sch_management_attraction=(principal_satisfaction+principal_salary_score)/2)
```

```
## Error in mutate(., principal_satisfaction = attitude_fun_rev(m7shq1_satt), : object 'school_data_SATT' not found
```

```r
final_indicator_data_SATT <- school_data_SATT %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SATT=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in group_by(., school_code): object 'school_data_SATT' not found
```

```r
#############################################
##### School School Management Selection and Deployment  ###########
#############################################


# This policy lever measures whether the right candidates being selected. These questions will probe what the recruitment process is like to 
# ensure that these individuals are getting the positions. The question would ultimately be based on: 1) there is a standard approach for selecting principals,
# 2) that approach relies on professional/academic requirements, and 3) those requirements are common in practice. 
# 
# Scoring: 
#   - 1 (lowest score) Most important factor is political affiliations or ethnic group. 
# - 2 Political affiliations or ethnic group is a consideration, but other factors considered as well. 
# - 3 Most important factor is years of experience, good relationship with owner/education department, and does not factor in quality teaching, demonstrated management qualities, or knowledge of local community. 
# - 4 Quality teaching, demonstrated management qualities, or knowledge of local community is a consideration in hiring, but not the most important factor 
# - 5 Quality teaching, demonstrated management qualities, or knowledge of local community is the most important factor in hiring.

school_data_SSLD <- school_data_SSLD %>%
  mutate(sch_selection_deployment=case_when(
    (m7sgq2_ssld==2 | m7sgq2_ssld==3 | m7sgq2_ssld==8) ~ 5,
    (m7sgq2_ssld==6 | m7sgq2_ssld==7) ~ 1,
    (!(m7sgq2_ssld==6 | m7sgq2_ssld==7) & (m7sgq1_ssld__2==1 | m7sgq1_ssld__3==1 | m7sgq1_ssld__8==1) ) ~ 4,
    (!(m7sgq2_ssld==6 | m7sgq2_ssld==7) & (m7sgq1_ssld__1==1 | m7sgq1_ssld__4==1 | m7sgq1_ssld__5==1 | m7sgq1_ssld__97==1) ) ~ 3,
    (m7sgq1_ssld__6==1 | m7sgq1_ssld__7==1 ) ~ 2, 
    TRUE ~ as.numeric(NA))
  )
```

```
## Error in mutate(., sch_selection_deployment = case_when((m7sgq2_ssld == : object 'school_data_SSLD' not found
```

```r
final_indicator_data_SSLD <- school_data_SSLD %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSLD=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in group_by(., school_code): object 'school_data_SSLD' not found
```

```r
#############################################
##### School School Management Support  ###########
#############################################


# This policy lever measures the extent to which principals receive training and/or exposure to other professional opportunities that could help them be better school leaders. 
# The questions aim to figure out if such programs are provided, and if they are, their level of quality. 
# 
# Scoring (sum of components below): 
#   - 1 Point. Principal has received formal training on managing school. 
# - 1/3 Point. Had management training for new principals. 
# - 1/3 Point. Had management in-service training. 
# - 1/3 Point. Had mentoring/coaching by experienced principals. 
# - 1 Point. Have used skills gained at training. 
# - 1 Point. Principals offered training at least once per year

school_data_SSUP <- school_data_SSUP %>%
  mutate(prinicipal_trained=bin_var(m7sgq3_ssup,1),
         principal_training=if_else(m7sgq3_ssup==1,
                                    rowMeans(.[grep(x=colnames(school_data_SSUP), 
                                                    pattern="m7sgq4_ssup__")], na.rm=TRUE),
                                    0),
         principal_used_skills=if_else(m7sgq3_ssup==1,
                                       bin_var(m7sgq5_ssup,1),0),
         principal_offered=if_else((m7sgq7_ssup==2 | m7sgq7_ssup==3 | m7sgq7_ssup==4 | m7sgq7_ssup==5),1,0)
  ) %>%
  mutate(sch_support=1+prinicipal_trained+principal_training+principal_used_skills+principal_offered)
```

```
## Error in mutate(., prinicipal_trained = bin_var(m7sgq3_ssup, 1), principal_training = if_else(m7sgq3_ssup == : object 'school_data_SSUP' not found
```

```r
final_indicator_data_SSUP <- school_data_SSUP %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SSUP=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in group_by(., school_code): object 'school_data_SSUP' not found
```

```r
#############################################
##### School School Management Evaluation  ###########
#############################################

# School Survey. This policy lever measures the extent to which principal performance is being monitored and enforced via accountability measures. 
# The idea is that the indicator will be based on: 1) there is a legislation outlining the need to monitor, 2) principals are being evaluated, 3) 
# principals are being evaluated on multiple things, and 4) there the accountability mechanisms in place.



school_data_SEVL<- school_data_SEVL %>%
  mutate(principal_formally_evaluated=bin_var(m7sgq8_sevl,1),
         principal_eval_tot=rowSums(.[grep(x=colnames(school_data_SEVL), 
                                           pattern="m7sgq10_sevl__")], na.rm=TRUE)-m7sgq10_sevl__98) %>%
  mutate(principal_evaluation_multiple=if_else(m7sgq8_sevl==1,
                                               case_when(
                                                 principal_eval_tot>=5 ~ 1,
                                                 (principal_eval_tot>1 & principal_eval_tot<5) ~ 0.666667,
                                                 principal_eval_tot==1 ~ 0.3333333,
                                                 TRUE ~ 0
                                               ),
                                               0),
         principal_negative_consequences=case_when(
           (m7sgq11_sevl__1==1 | m7sgq11_sevl__2==1 | m7sgq11_sevl__3==1 | m7sgq11_sevl__4==1 | m7sgq11_sevl__97==1) ~ 1,
           TRUE ~ 0),
         principal_positive_consequences=case_when(
           (m7sgq12_sevl__1==1 | m7sgq12_sevl__2==1 | m7sgq12_sevl__3==1 | m7sgq12_sevl__4==1 | m7sgq12_sevl__97==1) ~ 1,
           TRUE ~ 0)
  ) %>%
  mutate(principal_evaluation=1+principal_formally_evaluated+principal_evaluation_multiple+principal_negative_consequences+principal_positive_consequences)
```

```
## Error in mutate(., principal_formally_evaluated = bin_var(m7sgq8_sevl, : object 'school_data_SEVL' not found
```

```r
final_indicator_data_SEVL <- school_data_SEVL %>%
  group_by(school_code) %>%
  summarise_all(~first(na.omit(.))) %>%
  mutate(n_mssing_SEVL=n_miss_row(.))  %>%
  select( -starts_with('interview'), -starts_with('enumerator'))  
```

```
## Error in group_by(., school_code): object 'school_data_SEVL' not found
```

```r
#############################################
##### School Level Info ###########
#############################################

#Build school level database

#first create temp dataset with only required info (school_code + indicator info).  Main thing here is to drop enumerator code, interview ID, which mess up merges
#list additional info that will be useful to keep in each indicator dataframe
drop_info <- c('interview__id', 'interview__key',                    
               'school_name_preload', 'school_address_preload', 
               'school_province_preload', 'school_district_preload', 'school_code_preload', 'school_emis_preload',
               'school_info_correct', 'm1s0q2_name', 'm1s0q2_code', 'm1s0q2_emis',
               'survey_time', 'lat', 'lon' )

keep_info <-       c('school_code',
                     'school_name_preload', 'school_address_preload', 
                     'school_province_preload', 'school_district_preload', 'school_code_preload', 'school_emis_preload',
                     'school_info_correct', 'm1s0q2_name', 'm1s0q2_code', 'm1s0q2_emis',
                     'survey_time', 'lat', 'lon', 'total_enrolled')

if (exists('final_school_data')) {
  rm('final_school_data')
}

ind_dta_list<-c()

school_data_preamble_short<-school_data_preamble %>%
  group_by(school_code) %>%
  select(keep_info) %>%
  summarise_all(~first(na.omit(.)))
```

```
## Error in group_by(., school_code): object 'school_data_preamble' not found
```

```r
final_school_data <- school_data_preamble_short
```

```
## Error in eval(expr, envir, enclos): object 'school_data_preamble_short' not found
```

```r
for (i in indicator_names ) {
  if (exists(paste("final_indicator_data_",i, sep=""))) {
    #form temp data frame with each schools data
    temp<-get(paste("final_indicator_data_",i, sep="")) 
    
    #add element to list
    ind_dta_list<-c(ind_dta_list, paste("final_indicator_data_",i, sep=""))
    
    
    print(i)
    #Merge this to overall final_school_data frame
    if (!exists('final_school_data')) {
      final_school_data<-temp
      print(i)
      write.csv(temp, file = file.path(paste(confidential_folder,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
      if (backup_onedrive=="yes") {
        write.csv(temp, file = file.path(paste(save_folder_onedrive,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
      }
      
    } else {
      final_school_data<-final_school_data %>%
        left_join(temp, by='school_code') %>%
        select(-ends_with(".x"), -ends_with(".y"))
      
      write.csv(temp, file = file.path(paste(confidential_folder,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
      if (backup_onedrive=="yes") {
        write.csv(temp, file = file.path(paste(save_folder_onedrive,"/Indicators", sep=""), paste(i,"_final_indicator_data.csv", sep="")))
      }
    }
  }
}


#add male/female breakdowns to ind_data_list

ind_dta_list<-c(ind_dta_list, c("final_indicator_data_ATTD_M", "final_indicator_data_ATTD_F", 
                                "final_indicator_data_CONT_M", "final_indicator_data_CONT_F", 
                                "final_indicator_data_EFFT_M", "final_indicator_data_EFFT_F", 
                                "final_indicator_data_LCAP_M", "final_indicator_data_LCAP_F", 
                                "final_indicator_data_LERN_M", "final_indicator_data_LERN_F",
                                "final_indicator_data_OPMN_M", "final_indicator_data_OPMN_F",
                                "final_indicator_data_ILDR_M", "final_indicator_data_ILDR_F",
                                "final_indicator_data_PKNW_M", "final_indicator_data_PKNW_F",
                                "final_indicator_data_PMAN_M", "final_indicator_data_PMAN_F"))


#Create list of key indicators
ind_list<-c('student_knowledge', 'math_student_knowledge', 'literacy_student_knowledge', 
            'student_proficient', 'student_proficient_70', 'student_proficient_75',
            'literacy_student_proficient', 'literacy_student_proficient_70', 'literacy_student_proficient_75',
            'math_student_proficient', 'math_student_proficient_70', 'math_student_proficient_75',
            'presence_rate','absence_rate', 'sch_absence_rate', 'student_attendance',
            'content_knowledge', 'math_content_knowledge', 'literacy_content_knowledge', 
            'content_proficiency',  'content_proficiency_70', 'content_proficiency_75',
            'literacy_content_proficiency',  'literacy_content_proficiency_70', 'literacy_content_proficiency_75',
            'math_content_proficiency',  'math_content_proficiency_70', 'math_content_proficiency_75',
            'teach_score','classroom_culture','instruction','socio_emotional_skills',
            'teach_prof','classroom_culture_prof','instruction_prof','socio_emotional_skills_prof',
            'ecd_student_proficiency', 'ecd_math_student_proficiency', 'ecd_literacy_student_proficiency', 'ecd_exec_student_proficiency', 'ecd_soc_student_proficiency',
            'ecd_student_knowledge', 'ecd_math_student_knowledge', 'ecd_literacy_student_knowledge', 'ecd_exec_student_knowledge', 'ecd_soc_student_knowledge',
            'inputs', 'blackboard_functional', 'pens_etc', 'textbooks', 'share_desk', 'used_ict', 'access_ict',
            'infrastructure','drinking_water', 'functioning_toilet', 'internet', 'class_electricity','disability_accessibility','disab_road_access', 'disab_school_ramp', 'disab_school_entr', 'disab_class_ramp', 'disab_class_entr', 'disab_screening',
            'operational_management', 'vignette_1', 'vignette_1_resp', 'vignette_1_finance', 'vignette_1_address', 'vignette_2', 'vignette_2_resp', 'vignette_2_finance', 'vignette_2_address', 
            'intrinsic_motivation', 'acceptable_absent', 'students_deserve_attention', 'growth_mindset', 'motivation_teaching',
            'instructional_leadership', 'classroom_observed', 'classroom_observed_recent', 'discussed_observation', 'feedback_observation', 'lesson_plan_w_feedback',
            'principal_knowledge_score', 'add_triple_digit_pknw', 'multiply_double_digit_pknw', 'complete_sentence_pknw', 'experience_pknw', 'textbooks_pknw', 'blackboard_pknw',
            'principal_management', 'sch_goals_exist','sch_goals_clear','sch_goals_relevant','sch_goals_measured',
            'teacher_attraction', 'teacher_satisfied_job', 'teacher_satisfied_status', 'better_teachers_promoted' ,'teacher_bonus', 'salary_delays',
            'teacher_selection_deployment', 'teacher_selection','teacher_deployment',
            'teacher_support', 'pre_service','practicum','in_service','opportunities_teachers_share',
            'teaching_evaluation', 'formally_evaluated', 'evaluation_content', 'negative_consequences','positive_consequences',
            'teacher_monitoring','attendance_evaluated' , 'attendance_rewarded' , 'attendence_sanctions', 'miss_class_admin',
            'sch_management_clarity', 'infrastructure_scfn','materials_scfn','hiring_scfn', 'supervision_scfn', 'student_scfn' , 'principal_hiring_scfn', 'principal_supervision_scfn',
            'standards_monitoring',
            'sch_monitoring', 'monitoring_inputs','monitoring_infrastructure','system_in_place','parents_involved',
            'sch_management_attraction', 'principal_satisfaction',
            'sch_selection_deployment', 
            'sch_support', 'prinicipal_trained','principal_training','principal_used_skills','principal_offered',
            'principal_evaluation', 'principal_formally_evaluated','principal_evaluation_multiple','principal_negative_consequences','principal_positive_consequences'
)

################################
# Student & Teacher Weight Components
################################

g4_stud_weights<-school_dta %>%
  select(school_code,  m4scq4_inpt ) %>%
  group_by(school_code) %>%
  summarise(m4scq4_inpt=mean(m4scq4_inpt, na.rm=T)) %>%
  mutate(g4_stud_weight_component=1) %>%
  select(school_code, g4_stud_weight_component)
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
teacher_absence_weights <-school_dta %>%
  select(school_code,  numEligible ) %>%
  group_by(school_code) %>%
  summarise(numEligible=max(numEligible, na.rm=T)) %>%
  mutate(abs_weight_component=if_else(numEligible>=10,
                                      numEligible/10,
                                      1)) %>%
  select(school_code, abs_weight_component)
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
teacher_assessment_weights <-school_dta %>%
  select(school_code,  numEligible4th ) %>%
  group_by(school_code) %>%
  summarise(numEligible4th=max(numEligible4th, na.rm=T)) %>%
  mutate(teacher_weight_component=if_else(numEligible4th>=5,
                                          numEligible4th/5,
                                          1)) %>%
  select(school_code, teacher_weight_component)
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
g1_stud_weights<-school_dta %>%
  select(school_code,  m6_class_count ) %>%
  group_by(school_code) %>%
  summarise(m6_class_count=mean(m6_class_count, na.rm=T)) %>%
  mutate(g1_stud_weight_component=if_else(m6_class_count>=3,
                                          m6_class_count/3,
                                          1)) %>%
  select(school_code, g1_stud_weight_component)
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## x Column `school_code` doesn't exist.
```

```r
school_weights <- g4_stud_weights %>%
  left_join(teacher_absence_weights) %>%
  left_join(teacher_assessment_weights) %>%
  left_join(g1_stud_weights)
```

```
## Error in left_join(., teacher_absence_weights): object 'g4_stud_weights' not found
```

```r
#weights list
weights_list<-c('g4_stud_weight_component', 'abs_weight_component', 'teacher_weight_component','g1_stud_weight_component')


final_school_data <- final_school_data %>%
  left_join(school_data_preamble_short) %>%
  select(all_of(keep_info), one_of(ind_list), everything()) %>%
  left_join(school_weights)
```

```
## Error in left_join(., school_data_preamble_short): object 'final_school_data' not found
```

```r
write.csv(final_school_data, file = file.path(confidential_folder, "final_complete_school_data.csv"))
```

```
## Error in is.data.frame(x): object 'final_school_data' not found
```

```r
#write_dta(final_school_data, path = file.path(confidential_folder, "final_complete_school_data.dta"), version = 14)
write.csv(school_weights, file = file.path(confidential_folder, "school_weights.csv"))
```

```
## Error in is.data.frame(x): object 'school_weights' not found
```

```r
#write_dta(school_weights, path = file.path(confidential_folder, "school_weights.dta"), version = 14)
if (backup_onedrive=="yes") {
  write.csv(final_school_data, file = file.path(save_folder_onedrive, "final_complete_school_data.csv"))
  #write_dta(final_school_data, path = file.path(save_folder_onedrive, "final_complete_school_data.dta"), version = 14)
}
#If indicator in this list doesn't exists, create empty column with Missing values


for (i in ind_list ) {
  if(!(i %in% colnames(final_school_data))) {
    print(i)
    final_school_data[, i] <- NA
  }
}
```

```
## Error in colnames(final_school_data): object 'final_school_data' not found
```

```r
school_dta_short <- final_school_data %>%
  select(all_of(keep_info), one_of(ind_list), one_of(weights_list))
```

```
## Error in select(., all_of(keep_info), one_of(ind_list), one_of(weights_list)): object 'final_school_data' not found
```

```r
write.csv(school_dta_short, file = file.path(confidential_folder, "final_indicator_school_data.csv"))
```

```
## Error in is.data.frame(x): object 'school_dta_short' not found
```

```r
#write_dta(school_dta_short, path = file.path(confidential_folder, "final_indicator_school_data.dta"), version = 14)

if (backup_onedrive=="yes") {
  write.csv(school_dta_short, file = file.path(save_folder_onedrive, "final_indicator_school_data.csv"))
  #write_dta(school_dta_short, path = file.path(save_folder_onedrive, "final_indicator_school_data.dta"), version = 14)
}


extra_info='yes'

if (extra_info=='yes') {
#################################
# Read in Satellite Data on GDP
#################################
#Data downloaded from Here:
#https://datacatalog.worldbank.org/dataset/gross-domestic-product-2010 also see
#https://preview.grid.unep.ch/index.php?preview=data&events=socec&evcat=1&lang=eng
# In the distributed global GDP dataset sub-national GRP and national GDP data are allocated to 
# 30 arc second (approximately 1km) grid cells in proportion to the population residing in that cell. 
# The method also distinguishes between rural and urban population, assuming the latter to have a higher 
# GDP per capita. Input data are from 1) a global time-series dataset of GDP, with subnational gross regional 
# product (GRP) for 74 countries, compiled by the World Bank Development Economics Research Group (DECRG). 2) 
# Gridded population projections for the year 2009, based on a population grid for the year 2005 provided by 
# LandScanTM Global Population Database (Oak Ridge, TN: Oak Ridge National Laboratory). This dataset has been 
# extrapolated to year 2010 by UNEP/GRID-Geneva. Unit is estimated value of production per cell, in thousand of 
# constant 2000 USD. Cell level anomalies may occur due to poor alignment of multiple input data sources, and it 
# is strongly recommended that users attempt to verify information, or consult original sources, in order to determine 
# suitability for a particular application. This product was compiled by DECRG for the Global Assessment Report on Risk 
# Reduction (GAR). It was modeled using global data. Credit: GIS processing World Bank DECRG, Washington, DC, 
# extrapolation UNEP/GRID-Geneva.

#Load original sample of schools
currentDate<-c("2020-02-14")
sample_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/sampling/", sep="/"))
sample_frame_name <- paste(sample_folder,"/school_sample_",currentDate,".RData", sep="")

load(sample_frame_name)


#open the raster
raster_folder <- file.path(paste(project_folder,country,paste(country,year,"GEPD", sep="_"),paste(country,year,"GEPD_v01_RAW", sep="_"),"Data/Maps/GDP", sep="/")) 

gdp_raster <- raster::raster(paste(raster_folder, "/GDP.tif", sep="/"))

#add GDP to database
school_gdp <- school_dta_short %>%
  #mutate(Code_School=as.numeric(school_code_preload)) %>%
  #left_join(data_set_updated) %>%
  # mutate(longitude=as.character(lon)) %>%
  # mutate(latitude=as.character(lat)) %>%
  # mutate(lat=if_else(is.na(lat), as.numeric(latitude), lat),
  #        lon=if_else(is.na(lon), as.numeric(longitude), lon)) %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  dplyr::select(school_code, lon, lat)


sp::coordinates(school_gdp) <- c("lon","lat")
school_gdp$GDP <- raster::extract(gdp_raster, school_gdp, 
                                  buffer=1000, # 1000m radius
                                  fun=mean,na.rm=T,
                                  method='simple')


school_gdp <- as.data.frame(school_gdp) %>%
  mutate(GDP=as.numeric(GDP)) %>%
  select(school_code, GDP)

####################################
# Multiple Imputation of missing values
###################################

#use random forest approach to multiple imputation.  Some published research suggest this is a better approach than other methods.
#https://academic.oup.com/aje/article/179/6/764/107562
impdata<-mice::mice(school_dta_short, m=1,
           method='rf',
           maxit = 50, seed = 500)

school_dta_short_imp <- mice::complete(impdata, 1)

} else {
  school_dta_short_imp <- school_dta_short
  school_gdp <-school_dta_short
}
```

```
## Warning in readChar(con, 5L, useBytes = TRUE): cannot open compressed file 'C:/Users/wb469649/WBG/HEDGE Files - HEDGE Documents/GEPD-Confidential/CNT//SLE/SLE_2022_GEPD/
## SLE_2022_GEPD_v01_RAW/Data/sampling/school_sample_2020-02-14.RData', probable reason 'No such file or directory'
```

```
## Error in readChar(con, 5L, useBytes = TRUE): cannot open the connection
```

```r
################################
#Store Key Created Datasets
################################


#saves the following in R and stata format

data_list <- c(ind_dta_list, 'school_dta', 'school_dta_short', 'school_dta_short_imp', 'school_data_preamble', 'final_school_data', 'teacher_questionnaire','teacher_absence_final', 'ecd_dta', 'teacher_assessment_dta', 'teacher_roster', 
               "indicators", 'metadta', 'school_gdp', 'assess_4th_grade_anon', 'ecd_dta_anon')
save(list=data_list, file = file.path(confidential_folder, "school_survey_data.RData"))
```

```
## Error in save(list = data_list, file = file.path(confidential_folder, : objects 'final_indicator_data_ATTD_M', 'final_indicator_data_ATTD_F', 'final_indicator_data_CONT_M', 'final_indicator_data_CONT_F', 'final_indicator_data_EFFT_M', 'final_indicator_data_EFFT_F', 'final_indicator_data_LCAP_M', 'final_indicator_data_LCAP_F', 'final_indicator_data_LERN_M', 'final_indicator_data_LERN_F', 'final_indicator_data_OPMN_M', 'final_indicator_data_OPMN_F', 'final_indicator_data_ILDR_M', 'final_indicator_data_ILDR_F', 'final_indicator_data_PKNW_M', 'final_indicator_data_PKNW_F', 'final_indicator_data_PMAN_M', 'final_indicator_data_PMAN_F', 'school_dta_short', 'school_dta_short_imp', 'school_data_preamble', 'final_school_data', 'teacher_absence_final', 'school_gdp', 'assess_4th_grade_anon', 'ecd_dta_anon' not found
```

```r
save(list=c(ind_dta_list,"school_dta_short", 'school_dta_short_imp', "indicators", 'metadta',  'school_gdp' ), file = file.path(confidential_folder, "school_indicators_data.RData"))
```

```
## Error in save(list = c(ind_dta_list, "school_dta_short", "school_dta_short_imp", : objects 'final_indicator_data_ATTD_M', 'final_indicator_data_ATTD_F', 'final_indicator_data_CONT_M', 'final_indicator_data_CONT_F', 'final_indicator_data_EFFT_M', 'final_indicator_data_EFFT_F', 'final_indicator_data_LCAP_M', 'final_indicator_data_LCAP_F', 'final_indicator_data_LERN_M', 'final_indicator_data_LERN_F', 'final_indicator_data_OPMN_M', 'final_indicator_data_OPMN_F', 'final_indicator_data_ILDR_M', 'final_indicator_data_ILDR_F', 'final_indicator_data_PKNW_M', 'final_indicator_data_PKNW_F', 'final_indicator_data_PMAN_M', 'final_indicator_data_PMAN_F', 'school_dta_short', 'school_dta_short_imp', 'school_gdp' not found
```

```r
if (backup_onedrive=="yes") {
  save(list=data_list, file = file.path(save_folder_onedrive, "school_survey_data.RData"))
}
```

