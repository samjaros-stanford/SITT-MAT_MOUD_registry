# Data need to be in long format for use in EMF
# This file can be changed to produce different data sets that update
#   alongside the current CDI, REAIM, and IMAT datasets

library(tidyverse)

# Create data set for "Quarterly REAIM/IMAT Reports" (QRIR)
write.csv(rbind(readRDS("data/current_reaim.rds"), 
                readRDS("data/current_imat.rds")),
          "data/current_QRIR.csv", row.names=F)
