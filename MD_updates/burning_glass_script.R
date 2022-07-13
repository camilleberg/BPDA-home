# this is for burning glass R script 

library(readxl)
library(tidyverse)
library(writexl)


rm(list = ls())

# setting wd 
proj_dir <- "C:/Users/camilleb/Documents/MD Consumer Spending"
setwd("C:/Users/camilleb/Box/Research/Active Projects/Mayor's_Dashboard/01.Summary_Dashboard/Data/Burning_Glass/weekly_update")

## for the industries 
today <- format(Sys.Date(), "%Y.%m.%d")

wb_name <- paste0("MD ", today, " week.xlsx")
dat <- read_excel(wb_name, sheet = "Report3_Data")

# sorting and rearranging the data to fit the dashbaord thing 
dat <- dat %>%
  as_tibble(dat) %>%
  select(`NAICS Code`, `Industry`, `Job Postings`) %>%
  mutate(`Job Postings` = as.numeric(`Job Postings`)) %>%
  arrange(`NAICS Code`) %>% t() %>% as_tibble()
colnames(dat) <- dat[1, ]
dat <- dat[-1, ]

## for the total number 
dat_total <- read_excel(wb_name, sheet = "Filters")
colnames(dat_total) <- c("Name", "Value")

# organizing and cleaning 
dat_total <- dat_total %>%
  filter(Name %in% c("Postings available with the current filters applied:", "Top Detailed Industries")) %>%
  mutate(Value = as.numeric(Value))


write_xlsx(list(industries = dat, totals = dat_total), paste0("MD ", today, " week_sorted.xlsx"))

