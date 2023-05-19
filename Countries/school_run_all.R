# Run All School Level Files
# Brian Stacy
# May 19, 2023

library(tidyverse)
library(here)

dir <- here()

# Peru
source(file = paste0(dir, "/Countries/Peru/2019/School/01_data/01_school_run.R") )

#Jordan 
source(file = paste0(dir, "/Countries/Jordan/2019/School/01_data/01_school_run.R") )
source(file = paste0(dir, "/Countries/Jordan/2023/School/01_data/01_school_run.R") )

#Rwanda
source(file = paste0(dir, "/Countries/Rwanda/2019/School/01_data/01_school_run.R") )

#Ethiopia
source(file = paste0(dir, "/Countries/Ethiopia/2020/School/01_data/01_school_run.R") )
source(file = paste0(dir, "/Countries/Ethiopia/2021/School/01_data/01_school_run.R") )

#Madagascar
rmarkdown::render(paste0(dir, "/Countries/Madagascar/2021/School/01_data/01_school_cleaning.Rmd"))

# Sierra Leone
source(file = paste0(dir, "/Countries/Sierra Leone/2022/School/01_data/01_school_run.R") )

#Niger
source(file = paste0(dir, "/Countries/Niger/2022/School/01_data/01_school_run.R") )

#Gabon
source(file = paste0(dir, "/Countries/Gabon/2023/School/01_data/01_school_run.R") )

# Chad
source(file = paste0(dir, "/Countries/Chad/2023/School/01_data/01_school_run.R") )

# Pakistan
source(file = paste0(dir, "/Countries/Pakistan/2022/ICT/School/01_data/01_school_run.R") )
source(file = paste0(dir, "/Countries/Pakistan/2022/KP/School/01_data/01_school_run.R") )
