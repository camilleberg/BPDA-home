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

# restting wd, so the new csv files can be written into the proj dir
setwd(proj_dir)

# this creates new csvs for each industry type 
for(i in 1:length(unique(dat$industry))) {
  write.csv(industry_fxn(dat, unique(dat$industry)[i]), paste0("county-level_weekly_", unique(dat$industry)[i], "_2022.06.21.csv"))
}

# from mastercrad code, to export rates 

# this is for txn_amt
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

# this is for txn_count
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

txn_amt_dat <- tibble()
txn_cnt_dat <- tibble()

# running through all the csvs to calculate rates and exporting it to one csv

for(i in 1:length(unique(dat$industry))) {
  if(i == 1) {
    txn_amt_dat <- rate_fxn(unique(dat$industry)[i])
  } else {
    txn_amt_dat<-  cbind(txn_amt_dat, rate_fxn(unique(dat$industry)[i]))
  }
  # write.csv(rate_fxn(unique(dat$industry)[i]), paste0("county-level_weekly_", unique(dat$industry)[i], "_2022.06.21_rates.csv"))
}

# renaming columns to propeor industry title
colnames(txn_amt_dat) <- unique(dat$industry)
write.csv(txn_amt_dat, "county-weekly_all_rates_2022.06.21.csv")


## for count rates 
for(i in 1:length(unique(dat$industry))) {
  if(i == 1) {
    txn_cnt_dat <- rate_cnt_fxn(unique(dat$industry)[i])
  } else {
    txn_cnt_dat <- cbind(txn_cnt_dat, rate_cnt_fxn(unique(dat$industry)[i]))
  }
  # write.csv(rate_fxn(unique(dat$industry)[i]), paste0("county-level_weekly_", unique(dat$industry)[i], "_2022.06.21_rates.csv"))
}

# renaming columns to propeor industry title
colnames(txn_cnt_dat) <- unique(dat$industry)
write.csv(txn_cnt_dat, "county-weekly_all_cnt_rates_2022.06.21.csv")

