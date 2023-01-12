#########################
# raw_to_tables.R
# Accepts raw data from RedCap and checks for invalid values
# Cleans data and generates three tables:
# patients.csv: 1 row per patient containing demographic info
# prescriptions.csv: 1 row per prescription with appropriate dates
#
# Author: Sam Jaros (samjaros@stanford.edu)
# Date: June 2022
#########################

library(lubridate)
library(stringr)
library(tidyverse)

#########################
### Settings
# Raw data file path
raw_file_path = "data/SITT-MAT_simulated_patient_data.csv"
# Final output path
out_file_path = "data"
#########################

raw = read.csv(raw_file_path)

### Get only valid forms
# Look for incomplete surveys
complete = raw %>%
  filter(monthly_patient_information_form_complete==2)
incomplete = nrow(raw) - nrow(complete)
if (incomplete>0){
  warning(paste0(as.character(incomplete)," incomplete surveys will be discarded"))
}
valid = complete
rm(complete) #Release memory

### Get demographics
demogs = valid %>%
  filter(pr_new_admit==1) %>%
  select(pr_id, starts_with(c("pr_gender", "pr_race", "pr_ethnicity"))) %>%
  mutate(across(starts_with(c("pr_gender___", "pr_race___")),
         ~ if_else(.==1,str_sub(cur_column(),-1),NA_character_))) %>%
  unite("pr_gender", starts_with("pr_gender___"), na.rm=T) %>%
  unite("pr_race", starts_with("pr_race___"), na.rm=T)
# TODO: what to do with restarters
# TODO: what to do if a patient has an initial visit twice by mistake

### Get prescription info
mutate(pr_date_admission=mdy(pr_date_admission),
       pr_date_diagnosis=mdy(pr_date_diagnosis)) %>%







