library(tidyverse)

############
# Settings #
############
# Site Info
#   Get these files from REDCap and put them in the raw_data folder
#     In REDCap, this is "Program Information" under "Reports" in the sidebar
#     Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
#   Change the file name to match format INFO_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
#   Change the date in the string on the lines below
SUD_info_file = "raw_data/INFO_20230619_SUD.csv"
PC_info_file = "raw_data/INFO_20230619_PC.csv"
# Program info destination file
prog_info_file = "data/program_info.rds"

#######################
# Load & Process Data #
#######################
raw_SUD = read_csv(SUD_info_file, show_col_types=F) %>% 
  filter(imp_support != 5) %>%
  mutate(site_type = "SUD",
         demo_level_of_care = as.character(demo_level_of_care),
         isOutpatient = grepl("1", demo_level_of_care) | grepl("2", demo_level_of_care),
         isInpatient = grepl("3", demo_level_of_care) | grepl("4", demo_level_of_care) | grepl("5", demo_level_of_care),
         care_level = case_when(
           isOutpatient & isInpatient ~ "Both",
           isOutpatient               ~ "Outpatient",
           isInpatient                ~ "Inpatient",
           NA ~ NA_character_
         ),
         demo_goal = factor(demo_goal, levels=c(1,2), labels=c("Start MOUD", "Expand MOUD")))

# Assuming all primary cares are outpatient
raw_PC = read_csv(PC_info_file, show_col_types=F) %>% 
  filter(imp_support != 5) %>%
  mutate(site_type="PC",
         isOutpatient = T,
         isInpatient = F,
         care_level = case_when(
           isOutpatient & isInpatient ~ "Both",
           isOutpatient               ~ "Outpatient",
           isInpatient                ~ "Inpatient",
           NA ~ NA_character_
         ),
         demo_goal = factor(demo_goal, levels=c(1,2), labels=c("Start MOUD", "Expand MOUD")))


##########
# Export #
##########
full_site_info = rbind(select(raw_SUD, program_id, demo_goal, site_type, care_level),
      select(raw_PC, program_id, demo_goal, site_type, care_level))

saveRDS(full_site_info,
        file = prog_info_file)
