# brnign glass script for the adjustment 

# this is for burning glass R script 

library(readxl)
library(tidyverse)
library(writexl)


rm(list = ls())

# setting wd 
proj_dir <- "C:/Users/camilleb/Documents/MD Consumer Spending"
setwd("C:/Users/camilleb/Box/Research/Active Projects/Mayor's_Dashboard/01.Summary_Dashboard/Data/Burning_Glass/weekly_update")

#### added Sept 1, 2022 to adjust all numbers from May until August 31, 2022
library(readxl)
dates <- read_xlsx("Historical Data Pre May 2022 Adjustment/May Job Posting Update.xlsx", sheet = "weeks") 
colnames(dates) <- "Dates"
dates <- dates %>%
  mutate(Dates = format(Dates, "%Y.%m.%d")) # reformatting to fit new type 


# creating a function to loop through
date <- dates[1, 1]

job_posting_fxn <- function(date) {

  ## for the industries 
  today <- date
  
  wb_name <- paste0("MD ", today, " week_new.xlsx")
  dat <- read_excel(wb_name, sheet = "Report3_Data")
  
  # sorting and rearranging the data to fit the dashbaord thing 
  dat <- dat %>%
    as_tibble(dat) %>%
    select(`NAICS Code`, `Industry`, `Job Postings`) %>%
    mutate(`Job Postings` = as.numeric(`Job Postings`)) %>%
    arrange(`NAICS Code`) %>% t() %>% as_tibble()
  colnames(dat) <- dat[1, ]
  dat <- dat[-1, ]
  
  # this is to just grab the data
  return(dat[2,])
}

job_posting_totals_fxn <- function(date) {
  ## for the total number 
  ## for the industries 
  today <- date
  
  wb_name <- paste0("MD ", today, " week_new.xlsx")
  dat_total <- read_excel(wb_name, sheet = "Filters")
  colnames(dat_total) <- c("Name", "Value")
  
  # organizing and cleaning 
  dat_total <- dat_total %>%
    filter(Name %in% c("Postings available with the current filters applied:", "Top Detailed Industries")) %>%
    mutate(Value = as.numeric(Value))
  
  return(as_tibble(t(dat_total[, 2])))

}

# initializing dataframes

job_ind <- job_posting_fxn(dates[1, 1])
for(i in 2:nrow(dates)) {
  job_ind <- rbind(job_ind, job_posting_fxn(dates[i, 1]))
}

job_ind_total <- job_posting_totals_fxn(dates[1, 1])
for(i in 2:nrow(dates)) {
  job_ind_total <- rbind(job_ind_total, job_posting_totals_fxn(dates[i, 1]))
}

write_xlsx(list(industries = job_ind, totals = job_ind_total), "Updated Data 05.01.2022 to 08.27.2022.xlsx")
