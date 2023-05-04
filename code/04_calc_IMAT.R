library(tidyverse)

# Until additional IMAT data are collected, the baseline IMAT data are current
IMAT = readRDS("data/current_baseline_reaim-imat.rds") %>%
  filter(startsWith(variable, "imat"))

saveRDS(IMAT, "data/current_imat.rds")