########################
# Accepts: REDCap CDI survey results
# Outputs: CDI scores to input into EMF google sheet
########################

# Required packages
require(lubridate)
require(tidyverse)
require(stringr)

############
# Settings #
############
# CDI Survey
#   Get these files from REDCap and put them in the raw_data folder
#     In REDCap, this is "CDI Scores" under "Reports" in the sidebar
#     Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
#   Change the file name to match format CDI_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
#   Change the date in the string on the lines below
pc_cdi_file = paste0("raw_data/CDI_20230619_PC.csv") # Path to REDCap report in .csv
sud_cdi_file = paste0("raw_data/CDI_20230619_SUD.csv") # Path to REDCap report in .csv
# How to deal with "Does not apply (8)"
dna = 0 #0 makes neutral, NA makes it missing
# Cutoff for neutral
neutral_cutoff = 0
# Import scoring scheme
cdi_scoring = read.csv("public_data/cdi_scoring.csv", colClasses=c("q_num"="character"))
# Output
output_file = paste0("data/CDI_",gsub("-","",as.character(today())),".csv") # Path to output .csv

##########
# Import #
##########

raw_cdi = rbind(data.frame(read.csv(sud_cdi_file), type="SUD"), 
                data.frame(read.csv(pc_cdi_file), type="PC")) %>%
  # Remove sites that have withdrawn
  filter(imp_support!=5) %>%
  select(-imp_support)

###########################
# Get long & wide formats #
###########################
# TODO: rewrite section with pivot_longer/pivot_wider for readibility

long_cdi = raw_cdi %>%
  select(program_id, redcap_event_name, type, starts_with("cdi_")) %>%
  pivot_longer(starts_with("cdi_"),
               names_to="item",
               values_to="value", values_drop_na=T) %>%
  # Put score in terms of centered around 0
  # Modify missing as needed
  mutate(scaled_value = if_else(value==8, dna, value-4)) %>%
  left_join(cdi_scoring, by="item") %>%
  # Flip scoring where needed
  # Categorize into barriers, neutral, and facilitators
  mutate(scaled_value = if_else(isReverse, scaled_value*-1, scaled_value),
         cat = case_when(
           scaled_value<neutral_cutoff*-1 ~ "barriers",
           scaled_value>neutral_cutoff    ~ "facilitators",
           is.na(scaled_value)            ~ NA_character_,
           T                              ~ "neutral"
         )) %>%
  mutate(date = fast_strptime(str_extract(redcap_event_name, "^([a-z]*_[0-9]*)"), "%b_%Y"))

# Need to use base version of pivot_wider due to error in vctrs::slice.c
long_cdi$unique = paste(long_cdi$date, long_cdi$program_id, long_cdi$type, 
                        long_cdi$subscale, sep=".")
long_cdi$cat = factor(long_cdi$cat)
long_cdi$num = 1
  
wide_cdi = as.data.frame.matrix(xtabs(num~unique + cat, data=long_cdi)) %>%
  rownames_to_column() %>%
  separate(col=rowname, into=c("date", "program_id", "type", "subscale"), sep="\\.") %>%
  select(date, program_id, type, subscale, barriers, neutral, facilitators)

total_cdi = wide_cdi %>%
  group_by(date, program_id, type) %>%
  summarise(barriers = sum(barriers),
            neutral = sum(neutral),
            facilitators = sum(facilitators),
            .groups = "keep") %>%
  mutate(subscale = "cdi_9total") %>%
  select(date, program_id, type, subscale, barriers, neutral, facilitators)

cdi_data = rbind(wide_cdi, total_cdi)

##########
# Export #
##########

write.csv(cdi_data, file=output_file, row.names=F)
saveRDS(long_cdi, file="data/current_long_cdi.rds")
