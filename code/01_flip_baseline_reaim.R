########################
# Accepts: REDCap Baseline RE-AIM & IMAT outcome reports
# Outputs: Long form of RE-AIM & IMAT outcomes to input into EMF google sheet
#
# Author: Sam Jaros (samjaros@stanford.edu)
# Date: February 2023
########################

# Required packages
#require(ggplot2)
require(htmlTable)
require(lubridate)
require(tidyverse)

### Options
# Today string
today_string = gsub("-","",as.character(today()))
# RE-AIM
pc_reaim_file = paste0("raw_data/REAIM_", today_string, "_PC.csv") # Path to REDCap report in .csv
sud_reaim_file = paste0("raw_data/REAIM_", today_string, "_SUD.csv") # Path to REDCap report in .csv
# IMAT
pc_imat_file = paste0("raw_data/IMAT_", today_string, "_PC.csv") # Path to REDCap report in .csv
sud_imat_file = paste0("raw_data/IMAT_", today_string, "_SUD.csv") # Path to REDCap report in .csv
# Output
output_file = paste0("data/0_reaim_imat_", today_string, ".csv") # Path to output .csv
### End of options

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
# Output #
##########

reaim_imat = rbind(reaim_data, imat_data)
write.csv(reaim_imat, file=output_file, row.names=F)
saveRDS(reaim_imat, file="data/current_reaim-imat.rds")

############
# IMAT Viz #
############

imat_viz_data = imat_data %>%
  filter(date == max(date)) %>%
  group_by(variable, type) %>%
  summarise(mean_v = mean(value),
            max_v = max(value),
            min_v = min(value),
            .groups="keep")

dim_names = tribble(
  ~variable, ~label,
  "imat_d1",    "Infrastructure",
  "imat_d2",    "Culture &\nEnvironment",
  "imat_d3",    "Patient Identification\n& Initiating Care",
  "imat_d4",    "Care Delivery & Treatment\nResponse Monitoring",
  "imat_d5",    "Care Coordination",
  "imat_d6",    "Workforce",
  "imat_d7",    "Staff Training\n& Development",
  "imat_total", "Total"
)

site_names = list(
  "PC" = "Primary Care Average",
  "SUD" = "SUD Program Average"
)

colors = c("#4a86e8", "#ff9900")

### Lines
# imat_lines = ggplot(imat_viz_data, aes(x=variable, y=mean_v, ymax=max_v, 
#                                      ymin=min_v, group=type, color=type)) +
#   # Data
#   geom_point(size=3) + 
#   geom_line(aes(linetype=type), linewidth=0.8) + 
#   geom_errorbar(width=0.10) +
#   # Scales
#   scale_color_manual(name=NULL, labels=site_names, values=colors) +
#   scale_linetype_discrete(name=NULL, labels=site_names) +
#   scale_x_discrete(name=NULL, labels=dim_names$label) +
#   scale_y_continuous(name=NULL) +
#   # Aesthetics
#   annotate("text", x=8, y=5.1, vjust=0, color="grey",
#            label=paste0("Created: ", as.character(today()))) +
#   ggtitle("IMAT Dimension Averages, Maximums, and Minimums") +
#   theme_minimal() +
#   theme(legend.position="bottom",
#         plot.title=element_text(hjust=0.5))
# imat_lines
# 
# ggsave("imat_lines.jpeg", plot=imat_lines, width=12, height=5)

### Cols
# imat_cols = ggplot(imat_viz_data, aes(x=variable, y=mean_v, ymax=max_v, 
#                                       ymin=min_v, group=type)) +
#   # Data
#   geom_bar(aes(fill=type), stat="identity", position=position_dodge()) + 
#   geom_errorbar(width=0.25, color="black", position=position_dodge(0.9)) +
#   # Scales
#   scale_fill_manual(name=NULL, labels=site_names, values=colors) +
#   scale_x_discrete(name=NULL, labels=dim_names$label) +
#   scale_y_continuous(name=NULL) +
#   # Aesthetics
#   annotate("text", x=8, y=5.1, vjust=0, color="grey",
#            label=paste0("Created: ", as.character(today()))) +
#   ggtitle("IMAT Dimension Averages, Maximums, and Minimums") +
#   theme_minimal() +
#   theme(legend.position="bottom",
#         plot.title=element_text(hjust=0.5))
# imat_cols
# 
# ggsave("imat_cols.jpeg", plot=imat_cols, width=12, height=5)

### Table
site_count = imat_data %>%
  select(program_id, type) %>%
  unique() %>%
  group_by(type) %>%
  summarize(count=n())
site_count = as.list(setNames(site_count$count, site_count$type))

imat_viz_wide = imat_viz_data %>%
  left_join(dim_names, by="variable") %>%
  ungroup() %>%
  mutate(Dimension=gsub("\\n", " ", label),
         mean_v = round(mean_v, 2)) %>%
  select(Dimension, type, mean_v) %>%
  pivot_wider(names_from=type,
              values_from=mean_v) %>%
  rename_with(.fn = function(x){paste0(x, " (n=", site_count[x], ")")},
              .cols = -Dimension)

write.table(imat_viz_wide, "clipboard", sep="\t", row.names=F)
