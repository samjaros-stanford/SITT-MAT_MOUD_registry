# Take raw inputs directly from EHR and turn it into counts for sites 53-62

# NOTE: IDs provided by the site are incorrect, so these patients will be issued
#         corrected id's in the format "c##-####"

require(lubridate)
require(readxl)
require(tidyverse)
require(here)

############
# Settings #
############
# PC group registry files
#   These files are sent directly to us by the data manager and transferred via
#   Stanford Medicine Box
visit_file = "raw_data/SITT-MAT - id53 - DE-ID OUD DIAGNOSIS 3.2022-3.2023.xlsx"
rx_file = "raw_data/SITT-MAT - id53- DE-ID MOUD 3.2022-3.2023.xlsx"

########
# Data #
########

# Import data & give more friendly column names
raw_visits = read_excel(here::here(visit_file)) %>%
  rename(id_from_site = "SITT-MAT ID",
         encounter_date = "cln enc date",
         provider = prvdr,
         site_name = "svc dprtmnt") %>%
  mutate(encounter_date = ymd(encounter_date))
raw_prescriptions = read_excel(here::here(rx_file)) %>%
  rename(id_from_site = "SITT-MAT ID",
         rx_order_date = "order chartdate",
         rx_name = "order name (single)",
         rx_quantity = "quantity prescribed",
         rx_num_refills =  "no. of refills",
         rx_provider = "ordr provdr fll nm",
         rx_site_name = "order dprtmnt",
         rx_icd10_dx = "cd-10 clncl rdr dgnss cd",
         patient_isHomeless = "homelessstatus",
         patient_sex = "patientsex",
         patient_gender = "pat gender",
         patient_gender_identity = "pat gender identity",
         patient_ethnicity = "ethnicity",
         patient_race = "race",
         patient_language = "patient lang",
         patient_sexual_orientation = "pat sex orientation") %>%
  mutate(rx_order_date = ymd(rx_order_date))

# Correct patient id's to reflect site
site_name_id = read.csv("data/53-62_decoder.csv")
# TODO: This should reference a file with previously stored IDs to make sure past patients keep their IDs
id_translation = raw_visits %>%
  arrange(id_from_site, encounter_date) %>%
  group_by(id_from_site) %>%
  mutate(visit_num = row_number()) %>%
  ungroup() %>%
  filter(visit_num==1) %>%
  left_join(site_name_id, by="site_name") %>%
  arrange(program_id, encounter_date) %>%
  group_by(program_id) %>%
  mutate(patient_num = row_number()) %>%
  ungroup() %>%
  mutate(pr_id = paste0(str_sub(program_id, start=-2),"-",str_pad(patient_num, width=4, side="left", pad="0"))) %>%
  select(id_from_site, pr_id)

# Analysis data sets
all_prescriptions = raw_prescriptions %>%
  left_join(id_translation, by="id_from_site") %>%
  mutate(program_id = paste0("id", substr(pr_id,1,2)),
         month_year = paste0(month(rx_order_date, label=T), " ", year(rx_order_date)),
         # Shots get an assumed 34 day effectiveness
         est_rx_days = case_when(
           grepl("VIVITROL", rx_name)  ~ 34,
           grepl("SUBLOCADE", rx_name) ~ 34,
           T                           ~ parse_number(rx_quantity)
         )) %>%
  arrange(pr_id, rx_order_date) %>%
  group_by(pr_id) %>%
  mutate(is_restart = case_when(
    row_number()==1 ~ F,
    rx_order_date-lag(rx_order_date)<34 ~ F,
    rx_order_date-lag(rx_order_date)<lag(est_rx_days)*(lag(rx_num_refills)+1) ~ F,
    T ~ T
  )) %>%
  mutate(is_break = if_else(row_number()==n(), T, lead(is_restart, n=1))) %>%
  ungroup()

all_visits = raw_visits %>%
  left_join(id_translation, by="id_from_site") %>%
  mutate(program_id = paste0("id", substr(pr_id,1,2)),
         month_year = paste0(month(encounter_date, label=T), " ", year(encounter_date))) %>%
  arrange(pr_id, encounter_date) %>%
  group_by(pr_id) %>%
  mutate(is_start = row_number()==1) %>%
  ungroup() %>%
  left_join(all_prescriptions %>%
              select(pr_id, rx_order_date, is_restart) %>%
              filter(is_restart),
            by=c("pr_id", "encounter_date"="rx_order_date")) %>%
  replace_na(list(is_restart=F))

# For study data, the patient must have been diagnosed after Sep 1 2022
after_sep2022_ids = all_visits %>%
  filter(is_start | is_restart) %>%
  filter(encounter_date>=ymd("2022-09-01")) %>%
  group_by(pr_id) %>%
  filter(row_number()==1) %>%
  rename(first_study_date=encounter_date) %>%
  select(pr_id, first_study_date) %>%
  ungroup()

study_prescriptions = after_sep2022_ids %>%
  left_join(all_prescriptions, by="pr_id") %>%
  filter(rx_order_date>=first_study_date)

study_visits = after_sep2022_ids %>%
  left_join(all_visits, by="pr_id") %>%
  filter(encounter_date>=first_study_date)

# Get months where the patient was on MOUD as defined in the PCHS data documentation
rx_start_stop = study_prescriptions %>%
  group_by(pr_id) %>%
  filter(row_number()==1 | is_restart | is_break) %>%
  ungroup() %>%
  mutate(continuous_tx_id = if_else(row_number()==1 | lag(is_break) | is_restart, row_number(), lag(row_number()))) %>%
  pivot_wider(id_cols=c(pr_id, program_id, continuous_tx_id),
              values_from=rx_order_date,
              names_from=is_break) %>%
  rename(start_date = "FALSE", end_date = "TRUE") %>%
  mutate(start_date = if_else(is.na(start_date), end_date, start_date)) %>%
  mutate(months=toString(unique(study_prescriptions$month_year))) %>%
  separate_rows(months, sep=", ") %>%
  mutate(is_rx_month = my(months) %within% interval(start_date, end_date) | (ceiling_date(my(months),"month")-days(1)) %within% interval(start_date, end_date))

# Get dataset that matches each visit to the subsequent prescription
find_nearest_rx = function(dataset, patient, visit_date){
  to_return = dataset %>%
    filter(pr_id==patient, rx_order_date >= visit_date) %>%
    filter(row_number()==1) %>%
    select(rx_order_date) %>%
    pull
    
  if(length(to_return)==0)
    return(NA)
  return(to_return)
}
visit_rx_match = study_visits %>%
  rowwise() %>%
  mutate(nearest_rx = find_nearest_rx(study_prescriptions, pr_id, encounter_date))

################
# Calculations #
################
# A1: Number of integrated prescribers per month
#     Number of prescribers employed or contracted by the agency who have 
#       prescribed medications for opioid use disorder to at least 1 patient in
#       the past month
A1 = study_prescriptions %>%
  group_by(rx_site_name, month_year) %>%
  summarize(value = length(unique(rx_provider)), .groups="keep") %>%
  ungroup() %>%
  mutate(variable = "reaim_a1") %>%
  left_join(site_name_id, by=join_by(rx_site_name==site_name)) %>%
  select(program_id, month_year, variable, value) %>%
  pivot_wider(names_from = month_year,
              values_from = value,
              values_fill = 0) %>%
  pivot_longer(cols = c(-program_id, -variable),
               names_to = "date",
               names_transform = list(date=my),
               values_to = "value")

# B1: Number of new and existing patients diagnosed with OUD
#     # OLD MEASURE #
B1 = study_visits %>%
  filter(row_number()==1, .by="pr_id") %>%
  select(pr_id, program_id, month_year) %>%
  left_join(rx_start_stop %>%
              select(pr_id, months, is_rx_month) %>%
              filter(is_rx_month), 
            by="pr_id") %>%
  mutate(months = if_else(is.na(months), month_year, months)) %>%
  group_by(program_id, months) %>%
  summarize(value = n(), .groups="keep") %>%
  ungroup() %>%
  mutate(variable = "reaim_b1") %>%
  select(program_id, months, variable, value) %>%
  pivot_wider(names_from = months,
              values_from = value,
              values_fill = 0) %>%
  pivot_longer(cols = c(-program_id, -variable),
               names_to = "date",
               names_transform = list(date=my),
               values_to = "value")

# B2: Number of new patients diagnosed with OUD
#     The total number of patients with a new ICD10 or DSM5 diagnosis of OUD in
#       the past month. *All patients listed will be assumed to have the correct
#       ICD-10/DSM-5 diagnosis*
B2 = study_visits %>%
  filter(is_start | is_restart) %>%
  group_by(program_id, month_year) %>%
  summarize(value = n(), .groups="keep") %>%
  ungroup() %>%
  mutate(variable = "reaim_b2") %>%
  select(program_id, month_year, variable, value) %>%
  pivot_wider(names_from = month_year,
              values_from = value,
              values_fill = 0) %>%
  pivot_longer(cols = c(-program_id, -variable),
               names_to = "date",
               names_transform = list(date=my),
               values_to = "value")

# B4: Number of patients prescribed MOUD
#     The total number of patients administered MOUD in the past month. Note: 
#       Include patients who may be new, restarted, or established.

# Start with dataset of each patient * month
B4 = rx_start_stop %>%
  # Filter for only rows where the patient was taking MOUD that month
  filter(is_rx_month) %>%
  # Get count of patients by site * month
  group_by(program_id, months) %>%
  summarize(reaim_b4 = n(), .groups="keep") %>%
  ungroup() %>%
  mutate(date = my(months)) %>%
  # Calculate new and existing MOUD as a percent of OUD diagnoses
  left_join(B1, by=c("program_id", "date")) %>%
  mutate(reaim_b4p = reaim_b4/value*100) %>%
  select(-variable, -value) %>%
  pivot_longer(cols=c(reaim_b4, reaim_b4p),
               names_to="variable",
               values_to="value") %>%
  select(program_id, months, variable, value) %>%
  pivot_wider(names_from = months,
              values_from = value,
              values_fill = 0) %>%
  pivot_longer(cols = c(-program_id, -variable),
               names_to = "date",
               names_transform = list(date=my),
               values_to = "value")
  
# B5: Number of new patients prescribed MOUD within 30 days of diagnosis
#     Of the total number of patients reported for B2, calculate the subset of 
#       opioid-naive patients who were newly started on the MOUD during the past
#       month. Note: Include patients who re-started MOUD after a break in 
#       treatment.
# B5P describes this as a percent of B2


# C1: Number of new patients prescribed MOUD within 72h
#     Of the total number of patients reported for B2, calculate the subset of
#       patients who were newly started on MOUD within 72 hours of OUD 
#       diagnosis. Note: Include patients who re-started MOUD after a break in
#       treatment.
# C1P describes this as a percent of B2
C1 = visit_rx_match %>%
  filter(is_start | is_restart) %>%
  filter(!is.na(nearest_rx)) %>%
  filter(encounter_date-nearest_rx<=3) %>%
  group_by(program_id, month_year) %>%
  summarize(reaim_c1_n = n(), .groups="keep") %>%
  ungroup() %>%
  mutate(date = my(month_year)) %>%
  left_join(B2, by=c("program_id", "date")) %>%
  mutate(reaim_c1p = reaim_c1_n/value*100) %>%
  select(-variable, -value) %>%
  pivot_longer(cols = c(reaim_c1_n, reaim_c1p),
               names_to = "variable",
               values_to = "value") %>%
  select(program_id, month_year, variable, value) %>%
  pivot_wider(names_from = month_year,
              values_from = value,
              values_fill = 0) %>%
  pivot_longer(cols = c(-program_id, -variable),
               names_to = "date",
               names_transform = list(date=my),
               values_to = "value")

# C3: Number of new patients retained on MOUD
#     Of the total number of patients reported in B5, the subset of patients who
#       had 2+ in-person outpatient clinical visits within 34 days of starting
#       MOUD.
# C3P describes this as a percent of B5

  
# C5: Number of patients transition care to outside your clinic
#     C5.1 - Referred and follow-up appointment confirmed
#     C5.2 - Referred only
#     C5.3 - No referral made by clinic
#     C5.4 - Patient refused referral
#     C5.5 - Other/not listed

# Unable to calculate with current data

##########
# Export #
##########

saveRDS(rbind(A1, B1, B2, B4, C1), "data/current_53-62_reaim.rds")



#####################################
# Temp summary stats to show Hannah #
#####################################
# Just first visits by site
# visits %>% 
#   group_by(id_from_site) %>%
#   mutate(n = row_number()) %>%
#   filter(n==1) %>%
#   group_by(site_name) %>%
#   summarize(first_count = n()) %>%
#   mutate(first_pct = first_count/sum(first_count)) %>%
#   left_join(visits %>% 
#               group_by(site_name) %>%
#               summarize(all_count = n()) %>%
#               mutate(all_pct = all_count/sum(all_count)),
#             by="site_name")
# # Shows that the % of first visits ~~ % of total visits
# # Get prescriptions to ask Albert about
# a = prescriptions %>%
#   select(rx_name, rx_quantity) %>%
#   distinct
# view(a)
### End temp ###