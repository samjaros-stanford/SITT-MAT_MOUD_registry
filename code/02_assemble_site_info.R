library(tidyverse)

############
# Settings #
############
# SUD Site Info File
SUD_info_file = "raw_data/INFO_20221220_SUD.csv"
# PC Site Info File
PC_info_file = "raw_data/INFO_20221220_PC.csv"
# Program info destination file
prog_info_file = "data/program_info.rds"

#######################
# Load & Process Data #
#######################
raw_SUD = read_csv(SUD_info_file, show_col_types=F) %>% 
  filter(imp_support != 5) %>%
  mutate(site_type = "SUD",
         isOutpatient = demo_level_of_care___1==1 | demo_level_of_care___2==1,
         isInpatient = demo_level_of_care___3==1 | demo_level_of_care___4==1 | demo_level_of_care___5==1,
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



#########
# Write #
#########
full_site_info = rbind(select(raw_SUD, program_id, demo_goal, site_type, care_level),
      select(raw_PC, program_id, demo_goal, site_type, care_level))

saveRDS(full_site_info,
        file = prog_info_file)
