library(ggplot2)
library(tidyverse)

imat_data = readRDS(file="data/current_imat_subscale.rds") %>%
  mutate(type=if_else(as.numeric(str_sub(program_id,-2,-1))<50,"SUD","PC"))

imat_item_data = readRDS(file="data/current_imat_item.rds")

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

### Summarize all items
april_item_summary = imat_data %>%
  select(date, program_id, variable, value) %>%
  rbind(imat_item_data) %>%
  filter(date=="2023-04-01") %>%
  group_by(variable) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            ptile25 = quantile(value, .25),
            ptile75 = quantile(value, .75))
write_excel_csv(april_item_summary, file="data/Apr23 IMAT Item Summary.csv")
