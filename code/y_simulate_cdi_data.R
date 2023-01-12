library(tidyverse)

sites = paste0("id",str_pad(1:74, 2, "left", "0"))

types = c("PC", "SUD")

dates = c("2022-09-01", "2022-10-01", "2022-11-01")

subscales = c("cdi_eep", "cdi_cs", "cdi_ls", "cdi_he", "cdi_sfm", "cdi_fea",
              "cdi_rtc", "cdi_ic", "cdi_total")

## Generate noise for sites
set.seed(123)
full_long = data.frame(expand.grid(sites, dates, subscales)) %>%
  rename_with(function(x){c("site", "date", "subscale")}) %>%
  left_join(data.frame(site=sites, type=types), by="site") %>%
  rowwise() %>%
  mutate(barriers=round(runif(1)*7)+1,
         neutral=round(runif(1)*7)+1,
         facilitators=round(runif(1)*7)+1) %>%
  relocate(date, site, type, subscale, barriers, neutral, facilitators)

write.csv(full_long, "data/simulated_cdi_data.csv", row.names=F)

