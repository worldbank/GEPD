#function to create indicator data for a specified country and year
api_data <- function(data_dir1, data_dir2, data_dir3, cntry, yr) {
  
  
  school_df <- read_stata(paste(data_dir1, 'final_complete_school_data.dta', sep="/" ))
  public_officials_df <- read_stata(paste(data_dir2, 'public_officials_survey_data.dta', sep="/" ))
  expert_df <- read_stata(paste(data_dir3, 'expert_dta_final.dta', sep="/" ))
  
  
  #Tags
  practice_tags <- "SE.PRM.PROE|SE.LPV.PRIM|SE.PRM.LERN|SE.PRM.TENR|SE.PRM.EFFT|SE.PRM.CONT|SE.PRM.ATTD"
  
  
  #read in practice data
  api_p <- api_final %>%
    rename(Indicator.Name='Indicator Name') %>%
    filter(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name)) %>%
    rename(  'Indicator Name'=Indicator.Name) %>%
    select(Series, 'Indicator Name') %>%
    mutate(value=rbinom(n(), 100, 0.7)) %>%
    mutate(
      value_metadata=case_when(
        value <=50 ~ "Needs Improvement",
        value >50 & value<=80 ~ "Caution",
        value >80 ~ "On Target"
      ))
  
  #read in non-practice data
  api_c <- api_final %>%
    rename(Indicator.Name='Indicator Name') %>%
    filter(!(grepl(practice_tags, Series) | grepl("Percent", Indicator.Name))) %>%
    rename(  'Indicator Name'=Indicator.Name) %>%
    select(Series, 'Indicator Name') %>%
    mutate(value=rbinom(n(), 5, 0.7)) %>%
    mutate(
      value_metadata=case_when(
        value <=2 ~ "Needs Improvement",
        value >2 & value<4 ~ "Caution",
        value >=4 ~ "On Target"
      ))
  
  api_dummy_p %>%
    bind_rows(api_dummy_c) %>%
    arrange(Series) %>%
    mutate(year=yr,
           cty_or_agg="cty",
           countrycode=cntry)
}
