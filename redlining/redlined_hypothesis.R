# redlining hypothesis testng script

# written on January 25, 2023
# written by Camille Bergeron

library(dplyr)
library(tidycensus)
library(tidyverse)
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
VARS <- tidycensus::load_variables(dataset = 'acs5', year = 2021, cache = T)

vars_tenure <- VARS %>%
  filter(grepl("B25003", name))

census_vars <- tidycensus::get_acs(geography = "tract", 
                                     variable = c("B25003_001", "B25003_002"), 
                                     output = "wide",
                                     state = "MA",
                                     county = "Suffolk",
                                     geometry = TRUE,
                                     year = 2021,
                                     cache_table = T,
                                     show_call = TRUE) 

tracts_ownership <- redlined_tracts %>%
  left_join(y = census_vars %>%
              select(!ends_with("M")) %>% 
              rename(total_housing = B25003_001E) %>%
              rename(owner_occ = B25003_002E) %>%
              mutate(ownership_rate = owner_occ / total_housing) %>%
              select(GEOID, ownership_rate), by = c("GEOID20" = "GEOID"))

# hypothesis testing

# h0: m(nonredlined) - m(redlined) = 0
# hA: m(nonredlined) - m(redlined) > 0 

t.test(tracts_ownership %>% filter(prop50 == "rl") %>% select(ownership_rate), 
       tracts_ownership %>% filter(prop50 != "rl") %>% select(ownership_rate))

