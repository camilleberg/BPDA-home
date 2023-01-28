# redlining hypothesis testng script

# written on January 25, 2023
# written by Camille Bergeron

library(dplyr)
library(tidycensus)
library(readxl)

rm(list = ls())

# reading in 
redlined_tracts <- read_xlsx("RedlinedTracts2020.xlsx")

redlined_tracts <- redlined_tracts %>%
  rename(ct_red = `% of CT within Redlined Zone`) %>%
  mutate(prop50 = ifelse(ct_red > 50, "rl", "not")) %>%
  mutate(prop75 = ifelse(ct_red > 75, "rl", "not")) %>%
  mutate(prop90 = ifelse(ct_red > 90, "rl", "not")) 

# ownership var
