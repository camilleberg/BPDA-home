# this is to look for the data anomaly

# for mastercard graphs 
# July 15, 2022
# Camille Bergeron

# libraries
rm(list = ls())
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(xts)
library(plotly)

# setting directories and looking at files 
proj_dir <-  "C:/Users/camilleb/Documents/GitHub/BPDA-home"
proj_folder_dir <- "C:/Users/camilleb/Box/Research/Active Projects/Greater_downtown_request_2022"
setwd("F:/Deptment/Research/Data/MasterCard")

# reading in data
dat <- read.csv("ALL_MA_blockgroup-level_daily_indices_2022-05-31.csv")

# filtering for sep2021
dat_sep2021 <- dat %>%
  mutate(do_date = as.Date(do_date)) %>%
  mutate(month = lubridate::month(ymd(do_date))) %>%
  mutate(year = lubridate::year(ymd(do_date))) %>%
  filter((year == 2021 | year == 2019)  & (month >= 8 & month <= 10)) 

dat_sep2021_filt <- dat_sep2021 %>% 
  mutate(date = format(do_date, "%d-%m")) %>%
  mutate(date = as.Date(date, format = "%d-%m")) %>%
  group_by(date, industry, year) %>%
  summarise(n = sum(txn_cnt)) %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(rate = `2021` / `2019` - 1)

dat_sep2021_filt <- dat_sep2021_filt[order(dat_sep2021_filt$date), ]


plot_ly(data = dat_sep2021_filt, 
        x = ~date, 
        y = ~rate, 
        split = ~industry, 
        type = 'scatter', mode  = "line")

