library(lubridate)
library(tidyverse)
library(xlsx)

############
# Settings #
############
# Site information file location
site_info_file = "data/program_info.rds"
# Long CDI location
cdi_long_file = "data/current_long_cdi.rds"
# Current RE-AIM/IMAT location
imat_reaim_file = "data/current_reaim-imat.rds"
# Output Excel file
excel_file = "data/interview_selection.xlsx"

########
# Data #
########
site_info = readRDS(site_info_file) %>%
  select(program_id, demo_goal, site_type, care_level)

cdi = readRDS(cdi_long_file) %>%
  filter(grepl("outer", item)) %>%
  mutate(value = if_else(value==8,NA_integer_,value)) %>%
  group_by(program_id) %>%
  summarize(mean_system_CDI = mean(value, na.rm=T))

# Variables of interest
  # IMAT total (imat_total)
  # IMAT care coordination (imat_d5)
  # IMAT workforce (imat_d6)
  # % of OUD on MOUD (reaim_b4p)
  # # of integrated MOUD prescribers (reaim_a1)
  # # of OUD patients (reaim_b1)
imat_reaim_of_interest = c("imat_total", "imat_d5", "imat_d6", "reaim_b4p", "reaim_a1", "reaim_b1")
imat_reaim = readRDS(imat_reaim_file) %>%
  filter(variable %in% imat_reaim_of_interest) %>%
  group_by(program_id, variable) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  pivot_wider(id_cols = program_id,
              names_from = variable,
              values_from = value)

# Assemble values
full_data = site_info %>%
  left_join(imat_reaim, by="program_id") %>%
  full_join(cdi, by="program_id")

write.xlsx(full_data, excel_file, showNA=F)
  
