library(tidyverse)
library(stringr)

sites = paste0("id",str_pad(1:74, 2, "left", "0"))

types = c("SUD", "PC")

dates = c("2022-09-01", "2022-10-01", "2022-11-01")

variables = c("date", "site", "type", "reaim_a1", "reaim_a2", "reaim_b1", 
              "reaim_b2", "reaim_b4", "reaim_b4p", "reaim_c1", "reaim_c2",
              "reaim_c2p", "reaim_c6", "reaim_c7", "reaim_c8", "reaim_c9",
              "reaim_c10", "reaim_c11",
              "imat_d1", "imat_d2", "imat_d3", 
              "imat_d4", "imat_d5", "imat_d6", "imat_d7", "imat_total")

## Generate noise for sites
set.seed(123)
full_wide = data.frame(matrix(ncol=length(variables), nrow=0))
for(i in 1:length(sites)){
  for(date in dates){
    full_wide[nrow(full_wide)+1,1:3] = c(date, sites[i], types[i%%2+1])
    full_wide[nrow(full_wide),4:length(variables)] = runif(length(variables)-3)
  }
}
colnames(full_wide) = variables

## Scale variables
full_wide$reaim_a1 = round(full_wide$reaim_a1*5)
full_wide$reaim_a2 = round(full_wide$reaim_a2*2)
full_wide$reaim_b1 = round(full_wide$reaim_b1*20)
full_wide$reaim_b2 = round(full_wide$reaim_b2*8)
full_wide$reaim_b4 = round(full_wide$reaim_b4*10)
full_wide$reaim_b4p = round(full_wide$reaim_b4p*100)
full_wide$reaim_c1 = round(full_wide$reaim_c1*100)
full_wide$reaim_c2 = round(full_wide$reaim_c2*25)
full_wide$reaim_c2p = round(full_wide$reaim_c2p*100)
full_wide$reaim_c6 = round(full_wide$reaim_c6*10)
full_wide$reaim_c7 = round(full_wide$reaim_c7*5)
full_wide$reaim_c8 = round(full_wide$reaim_c8*6)
full_wide$reaim_c9 = round(full_wide$reaim_c9*2)
full_wide$reaim_c10 = round(full_wide$reaim_c10*8)
full_wide$reaim_c11 = round(full_wide$reaim_c11*2)
full_wide$imat_d1 = round(full_wide$imat_d1*4)+1
full_wide$imat_d2 = round(full_wide$imat_d2*4)+1
full_wide$imat_d3 = round(full_wide$imat_d3*4)+1
full_wide$imat_d4 = round(full_wide$imat_d4*4)+1
full_wide$imat_d5 = round(full_wide$imat_d5*4)+1
full_wide$imat_d6 = round(full_wide$imat_d6*4)+1
full_wide$imat_d7 = round(full_wide$imat_d7*4)+1
full_wide$imat_total = round(full_wide$imat_total*4)+1

## Wide to long
full_long = full_wide %>%
  pivot_longer(-c(date, site, type), names_to="variable", values_to="value") %>%
  relocate(date, site, variable, value, type)

write.csv(full_long, "data/simulated_emf_data.csv", row.names=F)

