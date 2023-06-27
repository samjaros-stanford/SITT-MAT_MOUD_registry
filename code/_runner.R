##########
# Install all required packages
# Runs all scripts required for complete SITT-MAT analysis
##########

# Clear environemnt
rm(list=ls())

# TODO: implement renv?

####################
# Install packages #
####################
packages = c("htmlTable", "lubridate", "readxl", "stringr", "tidyverse", "here")
install.packages(packages)

##########################################
# Run all code that starts with a number #
##########################################
### NOTE: NOT FULLY WORKING, USE AT YOUR OWN RISK ###
files = list.files("code", pattern="^\\d.+\\.R")

for(file in files){
  source(paste0(getwd(),"/code/",file))
}
