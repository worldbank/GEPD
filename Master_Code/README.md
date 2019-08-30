  # How to use the files in the Master Code directory
  
  Please read below for how to run and use the code for cleaning the school survey and survey of public officials data.
  
  After you clone this repo to your computer, if you want to use the code in this folder for a specific country, please copy all of the code and move it to a new directory.  Best practice is to do something like the following:
 
 1. If you are just starting out, clone the repo to your machine.  If you aren't sure where, I cloned mine to this folder (insert your own upi:
 C:\Users\wb{upi}\Documents\Github\GEPD
 2. Copy the code in the Master_Code directory to a new folder.  For example:
 C:\Users\wb{upi}\Documents\Github\GEPD\Peru\2019
 3. Then in the school_run.R and public_officials_run.R files, make the changes to the directories in those files.
 
 There is sampling code as well in this directory, but because they are stand-alone files and commented well, I will omit discussion of these files in this README.
 
 ## Github
 
 Given you have made it this far, you have some passing familiarity with Github.  If you would like more instructions on how to use Github, including cloning repos and the push/pull process, please contact me (bstacy@worldbank.org), or check out the [DIME Analytics info on Github](https://worldbank.github.io/dimeanalytics/git/).
 
  ## RStudio
  
  If you have not already, please install RStudio and the latest version of R.  If you are using a World Bank machine, you can install both by searching for the programs at [http://edapps](http://edapps).  RStudio and R are used to access the Survey Solutions API, download the data, and do data cleaning.  Some Stata code is available as well, and more will be added, but because interacting with APIs in Stata is more difficult, I mostly have used R to write code.  Additionally, the interactive reports that are created as part of high frequency checks are done in R, and it is not currently possible to do these type of reports in Stata.
  
    ## Passwords for API

It will be useful to create a password.R file, which stores the login information for the Survey Solutions API. To do this, go to the parent directory of the repo.  On my machine, it is  C:\Users\wb{upi}\Documents\Github\GEPD.  Create a new file called password.R in this directory.  In the file, you can enter your user names and passwords.  For example:

```
#Survey Solutions API password
user<-"username_1234"
password<-"pa$$word1234"
```  

That is all you need to do.  When you run the school_api.R or public_officials_api.R scripts, the script will check if this file exists and will read in your password and user info.  

You do not need to do this, but it will be more convenient if you do.  If you do not, you will be prompted each time you run the code for your user name and password when connecting to the API.

  ## Sequence of R scripts
  
  The R scrips in the School and Public_Officials sub-folders in this directory should be run in a specific order.   
  
  
  ##
 This file will run four R scripts in order.
 Each file can be run independently, but you will be prompted for certain paths that may not be specified
 This file will sequency the R scripts the correct way to produce an
 R markdown html file with high frequency and data quality checks for the school survey.
 1. school_api.R                       #This file will access the Survey Solutions API and pull rawdata and paradata  
 2. school_data_cleaner.R              #This file opens the raw data and cleans it to produce our indicators for the Dashboard
 3. school_paradata.R                  #This file opens paradata produced by Survey Solutions to calculate length 
                                        of time per module and other checks
 4. school_data_quality_checks.Rmd     #This file produces an R Markdown report containing several quality checks.

  
  
