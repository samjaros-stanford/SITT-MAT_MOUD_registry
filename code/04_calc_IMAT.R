library(lubridate)
library(tidyverse)

############
# Settings #
############
# IMAT all items
#   Get these files from REDCap and put them in the raw_data folder
#     In REDCap, this is "IMAT - All Items" under "Reports" in the sidebar
#     Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
#   Change the file name to match format IMAT-all_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
#   Change the date in the string on the lines below
folder_path <- "raw_data/"

# List all the file names in the folder
file_names <- list.files(path = folder_path)

# Get the file names containing the search string 
matching_files <- file_names[grepl("(?=.*IMAT-all)(?=.*SUD)", file_names, perl = TRUE)]
sud_imat_file <- paste0(folder_path, matching_files[1])

# Get the file names containing the search string
matching_files <- file_names[grepl("(?=.*IMAT-all)(?=.*PC)", file_names, perl = TRUE)]
pc_imat_file <- paste0(folder_path, matching_files[1])


##########
# Import #
##########
raw_imat = read.csv(pc_imat_file) %>%
  bind_rows(read.csv(sud_imat_file)) %>%
  # Get survey date from redcap event
  mutate(date = my(substr(redcap_event_name, 1, 8))) %>%
  # Select only desired columns (no comment columns)
  select(program_id, date, starts_with("imat_d"), imat_total_mean, -ends_with("_c")) %>%
  # Remove rows where all data are NA
  filter(if_any(starts_with("imat"), ~!is.na(.x)))

#############
# All items #
#############
item_imat = raw_imat %>%
  select(-ends_with("_mean")) %>%
  pivot_longer(-c(program_id, date),
               names_to="variable",
               values_to="value")

##########################
# Dimensions & Subscales #
##########################
subscale_imat = raw_imat %>%
  select(date, program_id, ends_with("_mean")) %>%
  pivot_longer(ends_with("_mean"),
               names_pattern="(.*)_mean",
               names_to="variable",
               values_to="value") %>%
  # Define & merge in Low Barrier Care subscale
  rbind(raw_imat %>%
          rowwise() %>%
          mutate(imat_s1_mean = mean(c(imat_d3_11,imat_d3_12,imat_d3_14,imat_d4_9,
                                       #imat_d4_11, imat_d4_12, 
                                       imat_d5_5,imat_d5_6,imat_d6_1,imat_d6_2,imat_d7_3), na.rm=T)) %>%
          select(date, program_id, imat_s1_mean) %>%
          pivot_longer(c(-date, -program_id),
                       names_pattern="(.*)_mean",
                       names_to="variable",
                       values_to="value"))

##########
# Export #
##########
saveRDS(item_imat, "data/current_imat_item.rds")
saveRDS(subscale_imat, "data/current_imat_subscale.rds")

