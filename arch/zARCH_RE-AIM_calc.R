#########################
# RE-AIM_calc.R
# Calculates RE-AIM outcomes from patient data
#
# Author: Sam Jaros (samjaros@stanford.edu)
# Date: January 2022
#########################

library(lubridate)
library(tidyverse)

#########################
# Important variables
#  - Date to be used when deciding what was a month ago
#  - NOTE: mm/dd/yyyy
search_start = mdy("6/01/2022")
#  - OUD diagnosis codes
OUD_icds = c("305.51", "F11.10", "F11.20", "F11.21", "F11.90")
#  - Site code to be appended to final data frame
site_code = "001"
#  - Survey code to be appended to final data frame
survey_code = "baseline"
#  - Arm to be appended to final data frame
arm = "experimental"
#########################

search_interval = interval(search_start, search_start+months(1)-days(1))

# Gets names in R-friendly format
# Also checks that all columns are present
df_wide = read.csv("data/mock_patient_registry_01.27.22.csv") %>%
  rename(
    ID = ID,
    admit_date = Admission.Date,
    ICD_code = Diagnosis.Code..ICD.9.ICD.10.,
    diag_date = Date.of.OUD.Diagnosis,
    disc_status = Discharge...Status,
    disc_date = Discharge....Date..dd.mm.yyyy.,
    MOUD_prescribed = Was.MOUD.Prescribed..yes.no.,
    MOUD_type = MOUD.Type,
    init_prescriber = Initial.Prescription...Prescriber.Name,
    init_location = Initial.Prescription...Location,
    init_date = Initial.Prescription...Date..mm.dd.yyyy.,
    init_duration = Initial.Prescription...Duration..days.,
    refill1_prescriber = Refill.1...Prescriber.Name,
    refill1_location = Refill.1...Location,
    refill1_date = Refill.1...Date..mm.dd.yyyy.,
    refill1_duration = Refill.1...Duration..days.,
    refill2_prescriber = Refill.2...Prescriber.Name,
    refill2_location = Refill.2...Location,
    refill2_date = Refill.2...Date..mm.dd.yyyy.,
    refill2_duration = Refill.2...Duration..days.,
    refill3_prescriber = Refill.3...Prescriber.Name,
    refill3_location = Refill.3...Location,
    refill3_date = Refill.3...Date..mm.dd.yyyy.,
    refill3_duration = Refill.3...Duration..days.,
    refill4_prescriber = Refill.4...Prescriber.Name,
    refill4_location = Refill.4...Location,
    refill4_date = Refill.4...Date..mm.dd.yyyy.,
    refill4_duration = Refill.4...Duration..days.,
    refill5_prescriber = Refill.5...Prescriber.Name,
    refill5_location = Refill.5...Location,
    refill5_date = Refill.5...Date..mm.dd.yyyy.,
    refill5_duration = Refill.5...Duration..days.,
    refill6_prescriber = Refill.6...Prescriber.Name,
    refill6_location = Refill.6...Location,
    refill6_date = Refill.6...Date..mm.dd.yyyy.,
    refill6_duration = Refill.6...Duration..days.,
    notes = Additional.Notes
  ) %>%
  mutate(across(ends_with("_date"), mdy))

df_long = df_wide %>%
  rename(refill0_prescriber = init_prescriber,
         refill0_location = init_location,
         refill0_date = init_date,
         refill0_duration = init_duration) %>%
  pivot_longer(starts_with("refill"),
               names_to=c("refill",".value"),
               names_pattern="(.*)_(.*)") %>%
  mutate(refill=as.numeric(sub("refill","",refill))) %>%
  arrange(ID, refill) %>%
  group_by(ID) %>%
  mutate(days_off_rx = date - lag(date + days(duration), 1)) %>%
  ungroup() %>%
  mutate(restarer = days_off_rx>28)

#####
# Adoption measures
#####
# A1
# Num onsite perscribers actively prescribing MOUD
var_A1 = df_long %>%
  filter(location == "Onsite") %>%
  filter(date %within% search_interval) %>%
  select(prescriber) %>%
  unique() %>%
  nrow()
var_A1
# A2
# Num offsite perscribers who initiated someone on MOUD in last 30 days
var_A2 = df_long %>%
  filter(location == "Offsite") %>%
  filter(date %within% search_interval) %>%
  select(prescriber) %>%
  unique() %>%
  nrow()
var_A2
# A3
# Sum of A1 and A2
var_A3 = var_A1 + var_A2


#####
# Reach measures
#####

## Identification
# B1
# Number of patients with OUD admitted in the past month
var_B1 = df_long %>%
  filter(admit_date %within% search_interval) %>%
  select(ID) %>%
  unique() %>%
  nrow()
var_B1
# B2
# Number of patients with OUD admitted
var_B2 = df_long %>%
  filter(is.na(disc_date)) %>%
  select(ID) %>%
  unique() %>%
  nrow()
var_B2

## Treatment
# B3
# Number of patients with a Bup Rx in the last month
var_B3 = df_long %>%
  filter(MOUD_type=="Buprenorphine") %>%
  filter(date %within% search_interval) %>%
  select(ID) %>%
  unique() %>%
  nrow()
var_B3
# B4
# Number of patients with a naltrexone Rx in the last month
var_B3 = df_long %>%
  filter(MOUD_type=="Naltrexone") %>%
  filter(date %within% search_interval) %>%
  select(ID) %>%
  unique() %>%
  nrow()
var_B4
# B5
# Total prescribed MOUD in the past month
var_B5 = var_B3 + var_B4
var_B5
# B6
# Proportion of patients who were newly started in the past month
num_new = df_long %>%
  filter(refill==0) %>%
  filter(date %within% search_interval) %>%
  select(ID) %>%
  unique() %>%
  nrow()
var_B6 = num_new/var_B5
# B7
# Proportion of patients who restarted in the past month
num_restart = df_long %>%
  filter(restarted) %>%
  filter(date %within% search_interval) %>%
  select(ID) %>%
  unique() %>%
  nrow()
var_B7 = num_restart/var_B5


#####
# Effectiveness
#####

## Access
# C1
# Number of new patients started on MOUD within 3 days of diagnosis
var_C1 = df_long %>%
  filter(date - diag_date <= days(3)) %>%
  select(ID) %>%
  unique() %>%
  nrow()
var_C1
# C2
# Number of patients who restarted MOUD within 3 days of diagnosis
# TODO: Wouldn't these all have to be new patients?

var_C2

## Retention
# TODO: continuous calculation
# C3
# Number of patients who have stayed on for the past 6 months

var_C3
# C4
# Number of new patients who have stayed on for the past 6 months

var_C4
# C5
# Number of new patients who have stayed on for the past 6 months

var_C5






















calculate.re-aim = function(dataset, site_id, survey_id){
  library(tidyverse)
  
  # Function accepts either path to a dataset or a data.frame object
  if (is.character(dataset))
    df = read.csv(dataset)
  else
    df = data.frame(dataset)
}

