##############
# Stopgap file while REAIM calculation pipeline is completed
#
# Takes manual REAIM calculation file and combines it with baseline data
# If a site has valid manual REAIM calculations, its baseline data are removed
##############
library(lubridate)
library(tidyverse)

manual_file = "raw_data/SITT-MAT_Quarterly Data Reports_MANUAL.csv"
manual_data = read.csv(manual_file) %>%
  select(date, program_id, variable, value, type) %>%
  mutate(date = ym(date))

# Exclude sites in manual data from baseline
trimmed_baseline = readRDS("data/current_baseline_reaim-imat.rds") %>%
  filter(startsWith(variable, "imat_") | !(program_id %in% unique(manual_data$program_id)))

# Combine manual and baseline
full_data = rbind(manual_data, trimmed_baseline) %>%
  arrange(program_id, date)

# Save data to file
write.csv(full_data, "data/reaim-imat_baseline_manual.csv", row.names=F)


##############
# Correct reaim_c1
#
# Sep-Nov c1 was calculated # <72h to MOUD/b2
# Dec-Feb c1 was calculated # <72h to MOUD/b4
# Need to correct Sep-Nov to match Dec-Feb
##############
library(lubridate)
library(tidyverse)

bad_manual_file = read.csv("raw_data/MANUAL_SITT-MAT_Quarterly Data Reports - Completed Manuals.csv") %>%
  select(-c(Maryam_cal, Hannah_cal, notes)) %>%
  pivot_wider(id_cols=c(program_id, date, type),
              names_from=variable,
              values_from=final.value,
              values_fn = as.numeric)

good_manual_file = bad_manual_file %>%
  filter(date %in% c("Sep-22", "Oct-22", "Nov-22")) %>%
  mutate(reaim_c1 = (reaim_c1/100)*reaim_b2/reaim_b4) 
  rbind(bad_manual_file %>% filter(date %in% c("Dec-22", "Jan-23", "Feb-23"))) %>%
  pivot_longer(cols=-c(date, program_id, type),
               names_to="variable",
               values_to="final_value") %>%
  arrange(program_id, variable, my(date)) %>%
  select(date, program_id, variable, final_value, type)

write.csv(good_manual_file, "data/corrected_manual_calculations_Apr23.csv", row.names=F)
