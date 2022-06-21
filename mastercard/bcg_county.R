# county level data for mastercard spending

# obj: 


# loading libraries and setting dir
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

rm(list =ls())

proj_dir <- getwd()

# setting new wd to get the data
setwd("C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Request")
list.files()

dat <- read.csv("bcg_2022.06.21.csv")

# reformatting data types
dat <- dat %>%
  mutate(do_date = as.Date(do_date)) %>%
  mutate(year = format(do_date, format = "%Y")) %>%
  mutate(week_num = lubridate::week(ymd(do_date))) %>%
  mutate(txn_amt = as.numeric(txn_amt))


# writing seperate files for each industry
industry_fxn <- function(dat, ind) {
  return(dat %>%
    filter(industry == paste(ind)))
}

sample <- industry_fxn(dat, "pst")

for(i in 1:length(unique(dat$industry))) {
  write.csv(industry_fxn(dat, unique(dat$industry)[i]), paste0("county-level_weekly_", unique(dat$industry)[i], "_2022.06.21.csv"))
}

# form mastercrad code, to export rates 
rate_fxn <- function(industry) {

  dat <- read.csv(paste0("county-level_weekly_", industry, "_2022.06.21.csv"))
  
  dat_pivot <- dat %>%
    group_by(week_num, year) %>%
    summarise(n = sum(txn_amt)) %>%
    pivot_wider(names_from = year, values_from = n)
  
  #c caluclating the rates 
  dat_export <- dat_pivot %>%
    mutate(rate2020 = `2020` / `2019` - 1) %>%
    mutate(rate2021 = `2021` / `2019` - 1) %>%
    mutate(rate2022 = `2022` / `2019` - 1) 
  
  dat_csv <- c(dat_export$rate2020, dat_export$rate2021, dat_export$rate2022)
  
  return(dat_csv)
}

rate_cnt_fxn <- function(industry) {
  
  dat <- read.csv(paste0("county-level_weekly_", industry, "_2022.06.21.csv"))
  
  dat_pivot <- dat %>%
    group_by(week_num, year) %>%
    summarise(n = sum(txn_cnt)) %>%
    pivot_wider(names_from = year, values_from = n)
  
  #c caluclating the rates 
  dat_export <- dat_pivot %>%
    mutate(rate2020 = `2020` / `2019` - 1) %>%
    mutate(rate2021 = `2021` / `2019` - 1) %>%
    mutate(rate2022 = `2022` / `2019` - 1) 
  
  dat_csv <- c(dat_export$rate2020, dat_export$rate2021, dat_export$rate2022)
  
  return(dat_csv)
}

# running through all the csvs to calculate rates 

for(i in 1:length(unique(dat$industry))) {
  write.csv(rate_cnt_fxn(unique(dat$industry)[i]), paste0("county-level_weekly_", unique(dat$industry)[i], "_2022.06.21_count_rates.csv"))
}
