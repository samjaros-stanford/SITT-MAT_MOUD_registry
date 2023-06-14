library(lubridate)
library(tidyverse)

##########
# Import #
##########
raw_imat = read.csv(here::here("raw_data/IMAT-all_20230613_SUD.csv")) %>%
  bind_rows(read.csv(here::here("raw_data/IMAT-all_20230613_PC.csv"))) %>%
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

saveRDS(item_imat, "data/current_imat_item.rds")

#############
# Subscales #
#############

# Flip to longer form
subscale_imat = raw_imat %>%
  select(date, program_id, ends_with("_mean")) %>%
  pivot_longer(ends_with("_mean"),
               names_pattern="(.*)_mean",
               names_to="variable",
               values_to="value")

saveRDS(subscale_imat, "data/current_imat_subscale.rds")

