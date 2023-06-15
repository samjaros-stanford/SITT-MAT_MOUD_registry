########################
# Accepts: REDCap Baseline RE-AIM & IMAT outcome reports
# Outputs: Long form of baseline RE-AIM & IMAT outcomes
########################

# Required packages
require(htmlTable)
require(lubridate)
require(tidyverse)
require(stringr)

############
# Settings #
############
# RE-AIM
#   Get these files from REDCap and put them in the raw_data folder
#     In REDCap, this is "Baseline RE-AIM" under "Reports" in the sidebar
#     Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
#   Change the file name to match format REAIM_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
#   Change the date in the string on the lines below
pc_reaim_file = paste0("raw_data/REAIM_20230522_PC.csv") # Path to REDCap report in .csv
sud_reaim_file = paste0("raw_data/REAIM_20230522_SUD.csv") # Path to REDCap report in .csv
# IMAT
pc_imat_file = paste0("raw_data/IMAT_20230522_PC.csv") # Path to REDCap report in .csv
sud_imat_file = paste0("raw_data/IMAT_20230522_SUD.csv") # Path to REDCap report in .csv
# Output
output_file = paste0("data/baseline_EMF_",gsub("-","",as.character(today())),".csv") # Path to output .csv

##########
# RE-AIM #
##########

# Import and stack raw data
raw_reaim = rbind(data.frame(read.csv(sud_reaim_file), type="SUD"), 
                 data.frame(read.csv(pc_reaim_file), type="PC")) %>%
  filter(imp_support!=5) %>%
  select(-imp_support)

# Get long data
long_reaim = raw_reaim %>%
  select(-redcap_event_name) %>% # Unneeded column
  # Get 1 row per site per month per variable
  pivot_longer(starts_with("demo_moud_"),
               names_to=c("variable", "month"), names_prefix="demo_moud_", names_sep="_",
               values_transform=as.numeric, values_drop_na=T) %>%
  # Get variables as columns for calculating b4p
  pivot_wider(names_from=variable,
              values_from=value) %>%
  # Flip b1 and b2
  rename(b1=b2,
         b2=b1) %>%
  # Calculate new b4 and b4p
  mutate(
    b4 = if_else(is.na(b4) & is.na(b3),
                 NA_real_,
                 if_else(is.na(b4),0,b4) + if_else(is.na(b3),0,b3)),
    b4p = case_when(
      b1==0 ~ NA_real_,
      b4>b1 ~ 100,
      T     ~ b4/b1*100)) %>%
  # Get 1 row per site per month per variable
  pivot_longer(starts_with(c("a","b")),
               names_to="variable",
               values_to="value", values_drop_na=F) %>%
  mutate(variable = paste0("reaim_", variable)) %>%
  mutate(date = fast_strptime(month, "%b%Y"))

reaim_data = long_reaim %>%
  select(date, program_id, variable, value, type)

########
# IMAT #
########

# Import and stack raw data
raw_imat = rbind(data.frame(read.csv(sud_imat_file), type="SUD"), 
                 data.frame(read.csv(pc_imat_file), type="PC")) %>%
  filter(imp_support!=5) %>%
  select(-imp_support)

# Get long data
long_imat = raw_imat %>%
  pivot_longer(c(-program_id, -redcap_event_name, -type),
               names_to="variable", names_pattern="(.*)_mean$",
               values_transform=list(char_value=as.character), values_to="char_value") %>%
  filter(!is.na(char_value)) %>%
  mutate(value = as.numeric(char_value)) %>%
  mutate(date = fast_strptime(redcap_event_name, "%b_%Y_arm_1"))

imat_data = long_imat %>%
  select(date, program_id, variable, value, type)

##########
# Export #
##########

reaim_imat = rbind(reaim_data, imat_data)
write.csv(reaim_imat, file=output_file, row.names=F)
saveRDS(reaim_imat, file="data/current_baseline_reaim-imat.rds")

