<!-- MDTOC maxdepth:6 firsth1:1 numbering:0 flatten:0 bullets:1 updateOnSave:1 -->

- [How to use the files in the Master Code directory](#how-to-use-the-files-in-the-master-code-directory)   
   - [Github](#github)   
   - [RStudio](#rstudio)   
   - [Passwords for API](#passwords-for-api)   
   - [Sequence of R scripts](#sequence-of-r-scripts)   
   - [Common Errors](#common-errors)   

<!-- /MDTOC -->

  # How to use the files in the Master Code directory

  Please read below for how to run and use the code for cleaning the school survey and survey of public officials data.

  After you clone this repo to your computer, if you want to use the code in this folder for a specific country, please copy all of the code and move it to a new directory.  Best practice is to do something like the following:

 1. If you are just starting out, clone the repo to your machine.  If you aren't sure where, I cloned mine to this folder (insert your own upi):
 C:\Users\wb{upi}\Documents\Github\GEPD
 2. Copy the code in the Master_Code directory to a new folder.  For example:
 C:\Users\wb{upi}\Documents\Github\GEPD\Peru\2019
 3. Then in the school_run.R and public_officials_run.R files, make the changes to the directories in those files.

 There is sampling code as well in this directory, but because they are stand-alone files and commented well, I will omit discussion of these files in this README.

 ## Github

 Given you have made it this far, you have some passing familiarity with Github.  If you would like more instructions on how to use Github, including cloning repos and the push/pull process, please contact me (bstacy@worldbank.org).

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

  The R scrips in the School and Public_Officials sub-folders in this directory should be run in a specific order.  Each file can be run independently, but you will be prompted for certain paths that may not be specified.  If you run the R scripts the correct way you will produce a R markdown html file with high frequency and data quality checks for the school survey or survey of public officials. I am substituting school into the file names, but it is the same for the public_officials code.

  | File Name | What it does |
  | ---       | ---          |
  | 1. school_run.R                       | This file will set the directory paths where data will be downloaded from the API and where the cleaned data will be saved.  |
  | 2. school_api.R                       | This file will access the Survey Solutions API and pull rawdata           |
  | 3. school_data_cleaner.R              | This file opens the raw data and cleans it to produce our indicators for the Dashboard |
  | 4. school_paradata.R                  | This file pulls the paradata from the Survey Solutions API and opens paradata produced by Survey Solutions to calculate length of time per module and other checks                                                       |
  | 5. school_data_quality_checks.Rmd     | This file produces an R Markdown report containing several quality checks.             |

The code in the file ending in .R (school_run.R, school_api.R, school_data_cleaner.R, school_paradata.R) can be run in RStudio by highlighting the lines to be run and clicking the 'run' button in the top right corner of the script viewer.  Alternatively, you can run by using the keyboard short-cut Ctrl+Enter.

The R markdown file (school_data_quality_checks.Rmd) can be run by clicking on the 'knit' button near the top left of the script viewer or using 	Ctrl+Shift+K

  ## Common Errors

  Here is a list of common errors that I have experienced.  I will add more, or come up with better solutions to the code, as they come up.  If these solutions don't work, then contact me.

  | Error | What to do |
  | ------| -----------|
  <code> Error in library("blah blah") : there is no package called '[blah blah]' <code> | You need to install this package. Use the following command. <code> <br> install.packages('blah blah') |



  | Error | What to do |
  | ------| -----------|
  |Can't connect to Survey Solutions API | Run the following lines: |

  ```
  q<-GET(paste(server_add,"/api/v1/questionnaires", sep=""),
       authenticate(user, password))

str(content(q))
#pull data from version of our Education Policy Dashboard Questionnaire
POST(paste(server_add,"/api/v1/export/stata/06756cace6d24cc996ffccbfc26a2264$",quest_version,"/start", sep=""),
         authenticate(user, password))
```
Look in the output for Status: 404 or Status: 200.  If it is status code 200, then the connection was successful, and the error was somewhere beyond the connection.  If it was 404, then there was a problem with the connection, check the web address you entered for the server, the user name, and the password that you entered.  There can be a lot of problems here that are hard to diagnose in advance, so you might contact me here.

  | Error | What to do |
  | ------| -----------|
  | When I try to create RMarkdown Report, I get an error| This could be caused by a variety of reasons.  The most likely is that a file that used in the markdown document isn't available.  Please make sure the school_run.R, school_api.R, school_data_cleaner.R, and school_paradata.R are all run without errors before attempting to create report.|


  | Error | What to do |
  | ------| -----------|
  | <code> unzip(file.path(download_folder, tounzip), exdir=download_folder) <br>  Warning message: <br>  In unzip(file.path(download_folder, tounzip), exdir = download_folder) : <br>   error 1 in extracting from zip file <code> | If there is a previous zip file with the same name in the directory, try deleting it. |
