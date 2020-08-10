# Global Education Policy Dashboard Country Briefs

This directory contains the code to produce two and four page country reports based on the Global Education Policy Dashboard (GEPD).

In order produce the reports, please proceed to the /Code folder and then run the R script GEPD_briefs_run.R.  This will then compile two R Markdown documents and export country reports in PDF form. The R Markdown file associated with the two page report is called "GEPD_Brief_2page.Rmd" and the file for the four page reports is called "GEPD_Brief_4page.Rmd".

All necessary files to produce the reports can be found in the /Data folder.

To add a new country, the user will need to place a csv file containing data and matching the template of the other csv files in the folder.  Please name the file
GEPD_Indicators_API_{country_name}.csv.

In order to compile the report for this country, you can append the GEPD_briefs_run.R file to include a new block of code with the country name and year information modified to match the csv file that was uploaded.

Please feel free to contact bstacy@worldbank.org or educationdashboard@worldbank.org for questions or issues.
