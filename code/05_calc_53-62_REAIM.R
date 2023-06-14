# Take raw inputs directly from EHR and turn it into counts for sites 53-62

# NOTE: IDs provided by the site are incorrect, so these patients will be issued
#         corrected id's in the format "c##-####"

require(lubridate)
require(readxl)
require(stringr)
require(tidyverse)

# Settings
visit_file = "raw_data/SITT-MAT - id53 - DE-ID OUD DIAGNOSIS 3.2022-3.2023.xlsx"
rx_file = "raw_data/SITT-MAT - id53- DE-ID MOUD 3.2022-3.2023.xlsx"

########
# Data #
########

# Import data & give more friendly column names
visits = read_excel(here::here(visit_file)) %>%
  rename(id_from_site = "SITT-MAT ID",
         encounter_date = "cln enc date",
         provider = prvdr,
         site_name = "svc dprtmnt") %>%
  mutate(encounter_date = ymd(encounter_date))
prescriptions = read_excel(here::here(rx_file)) %>%
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
id_translation = visits %>%
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


#####################################
# Temp summary stats to show Hannah #
#####################################
# Just first visits by site
visits %>% 
  group_by(id_from_site) %>%
  mutate(n = row_number()) %>%
  filter(n==1) %>%
  group_by(site_name) %>%
  summarize(first_count = n()) %>%
  mutate(first_pct = first_count/sum(first_count)) %>%
  left_join(visits %>% 
              group_by(site_name) %>%
              summarize(all_count = n()) %>%
              mutate(all_pct = all_count/sum(all_count)),
            by="site_name")
# Shows that the % of first visits ~~ % of total visits
# Get prescriptions to ask Albert about
a = prescriptions %>%
  select(rx_name, rx_quantity) %>%
  distinct
view(a)
### End temp ###

# A1: Number of integrated prescribers per month
#     If a prescriber gives any prescriptions at that site during the month, it
#       counts for the site
A1 = prescriptions %>%
  mutate(month_year = paste0(month(rx_order_date, label=T), " ", year(rx_order_date))) %>%
  group_by(rx_site_name, month_year) %>%
  summarize(A1 = length(unique(rx_provider)),
            .groups="keep") %>%
  ungroup() %>%
  mutate(date = my(month_year)) %>%
  left_join(id_decoder, by=join_by(rx_site_name==site_name)) %>%
  select(program_id, date, A1)

# A2: Number of external prescribers per month
## Invalid/ignored?

