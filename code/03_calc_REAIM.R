
#############################################################################
### This file does not do REAIM calculations right now                    ###
###   Currently, the Assemble Data section takes the manual calculations  ###
###   and saves them as current_reaim.rds. The manual calculations come   ###
###   from Google Sheets until the new REAIM registry is complete         ###
#############################################################################

library(lubridate)
library(stringr)
library(tidyverse)

# TODO: This file should process data from the new REAIM registry where sites give counts directly

############
# Settings #
############
# Patient registry
#Set the directory path
folder_path <- "raw_data/"

# List all the file names in the folder
file_names <- list.files(path = folder_path)

# Get the file names containing the search string
matching_files <- file_names[grepl("(?=.*Registry)(?=.*SUD)", file_names, perl = TRUE)]
SUD_registry_file <- paste0(folder_path, matching_files[1])

matching_files <- file_names[grepl("(?=.*Registry)(?=.*PC)", file_names, perl = TRUE)]
PC_registry_file = paste0(folder_path, matching_files[1])


####################
# Load & Prep Data #
####################
input_cleanup = function(file_name, type){
  read.csv(file_name) %>%
    mutate(site_type = type) %>%
    # Fill in site participation data for every entry
    group_by(program_id) %>%
    fill(imp_support, .direction="down") %>%
    # Remove sites no longer in study
    filter(imp_support != 5) %>%
    # Get survey month
    rowwise() %>%
    mutate(survey_month = my(str_extract(redcap_event_name, "^[A-Za-z]+_[0-9]+"))) %>%
    # Only want complete surveys
    filter(quarterly_moud_service_registry_complete==2) %>%
    # Force blanks to be NA's
    mutate(across(where(is.character), na_if, "")) 
}

raw_data = rbind(input_cleanup(SUD_registry_file, "SUD"),
                 input_cleanup(PC_registry_file, "PC"))

#####################
# Adoption Measures #
#####################

provider_data = raw_data %>%
  select(program_id, survey_month, starts_with("pr_moud_prescriber")) %>%
  # Get provider name, month, and relationship columns
  pivot_longer(cols = c(-program_id, -survey_month),
               names_to = ".value",
               names_pattern = "pr_moud_([a-z_]+)_[0-9]",
               values_transform = list(prescriber = as.character,
                                       prescriber_mo = paste),
               values_drop_na = T) %>%
  # Sites without providers excluded
  filter(!is.na(prescriber)) %>%
  # Get monthly rows
  separate_rows(prescriber_mo, sep = ",", convert = T) %>%
  # Calculate observation month for each row relative to the survey month
  mutate(month_diff = (prescriber_mo-2)*-1+2,
         month = survey_month-months(month_diff)) %>%
  # Get only relevant cols
  select(program_id, month, prescriber, prescriber_relat)

# A1: Number of prescribers employed or contracted by the agency who have 
#     prescribed medications for opioid use disorder (MOUD) to at least 1 
#     patient in the past month.
A1 = provider_data %>%
  #  TEMP: Temporarily removed filter while sites are inputting new data
  #  filter(prescriber_relat==1) %>%
  group_by(program_id, month) %>%
  summarise(A1 = n(),
            .groups = "keep")

# A2: Number of prescribers who are NOT employed or contracted by the agency who 
#     have prescribed medications for opioid use disorder (MOUD) to at least 1 
#     patient in the past month.
A2 = provider_data %>%
  filter(prescriber_relat==2) %>%
  group_by(program_id, month) %>%
  summarise(A2 = n(),
            .groups = "keep")

##################
# Reach Measures #
##################

patient_data = raw_data %>%
  select(-starts_with("pr_moud_prescriber")) %>%
  # Remove rows that do not contain a patient report
  filter(!is.na(pr_id))


#################
# Assemble Data #
#################

### TEMP ###
REAIM = read.csv("raw_data/MANUAL_SITT-MAT_Quarterly Data Reports - Completed Manuals.csv") %>%
  mutate(date = parse_date_time(date, c("b-y","b-Y","b y","b Y")),
         value = as.numeric(final.value)) %>%
  select(date, program_id, variable, value)
### /TEMP ###

saveRDS(REAIM, "data/current_reaim.rds")



