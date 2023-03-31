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
