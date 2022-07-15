# for mastercard graphs 
# July 14, 2022
# Camille Bergeron

# libraries
rm(list = ls())
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

# setting directories and looking at files 
proj_dir <-  "C:/Users/camilleb/Documents/GitHub/BPDA-home"
setwd("F:/Deptment/Research/Data/MasterCard")

# reading in data
dat <- read.csv("ALL_MA_blockgroup-level_monthly_indices_2022-04-01.csv")

# reading in the blockgroup nbhd
wb_name <- "C:/Users/camilleb/Box/Research/Active Projects/Greater_downtown_request_2022/All Boston (MA_blockgroup-level_weekly_indices_2020-01-01_2022-04-17).xlsm"
nbhds <- read_excel(wb_name, sheet = "neighborhoods")


# reformatting into data and adding month 
dat <- dat %>%
  mutate(do_date = as.Date(do_date)) %>%
  mutate(year = format(do_date, format = "%Y")) %>%
  mutate(month = lubridate::month(ymd(do_date))) %>%
  mutate(txn_amt = as.numeric(txn_amt)) %>%
  mutate(txn_cnt = as.numeric(txn_cnt)) 

#joining the tables for neighborhood analysis
nbhds <-  rename(nbhds, geoid = `GEOID(2010_Census)`)
dat_join <- left_join(dat, nbhds, "geoid")

rate_fxn <- function(df, ind) {
  test <- dat_join %>%
    # this reformats the data so we cna then calculate the rates 
    select(industry, month, year, txn_cnt) %>%
    filter(industry == paste0(ind)) %>%
    group_by(industry, month, year) %>%
    summarise(n = sum(txn_cnt)) %>%
    pivot_wider(names_from = year, values_from = n) %>%
    # this is the actual rate calculation
    mutate(rate2020 = `2020` / `2019` - 1) %>%
    mutate(rate2021 = `2021` / `2019` - 1) %>%
    mutate(rate2022 = `2022` / `2019` - 1) %>%
    select(industry, rate2020, rate2021, rate2022) %>%
    
    pivot_longer(cols = !industry, names_to = "rate", values_to = "count")
}

dat_join <- dat_join %>%
  select(industry, week_num, year, year, bg10_nbhd, txn_cnt) %>%
  filter(bg10_nbhd %in% c("Fenway", "Longwood", "South Boston Waterfront", "Back Bay")) %>%
  filter(industry %in% c("ret", "gro", "eap"))
  


# this is just going to replicate what's done in the pivot table
dat_pivot <- dat %>%
  filter(industry == "ret") %>%
  group_by(week_num, year) %>%
  summarise(n = sum(txn_amt)) %>%
  pivot_wider(names_from = year, values_from = n)

#c caluclating the rates 
dat_export <- dat_pivot %>%
  mutate(rate2020 = `2020` / `2019` - 1) %>%
  mutate(rate2021 = `2021` / `2019` - 1) %>%
  mutate(rate2022 = `2022` / `2019` - 1) %>%
  select(rate2020, rate2021, rate2022) 

dat_csv <- c(dat_export$rate2020, dat_export$rate2021, dat_export$rate2022)

# writing out the new csv 
write.csv(dat_csv, paste0("economic_dashboard_", today, "_rates.csv"))
# woudl it be better to simply rewrite the csv becasue this is a new one 

setwd(proj_dir)
