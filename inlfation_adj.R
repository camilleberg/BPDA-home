# this is to load the txn data so it doesn;t have t o be in the graph code
# written august 22, 2022

#libraries
rm(list =ls())
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)


dat <- read.csv("F:/Deptment/Research/Data/MasterCard/MA_blockgroup-level_monthly_indices_2020-01-01_2022-04-17.csv")

# this is to select for boston and not suffolk
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
dat_join <- left_join(dat, nbhds, "geoid") %>% filter(!is.na(bg10_nbhd)) # do later 
  # this filters for just Boston and not Suffolk County

# inflation adjusting the data
inflation_dat <- read_xlsx("C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Program/Inflation/inflation_idx_jan2019_june2022.xlsx")
# this is to format them the same

# reading in industry data to join 
industry_dat <- readxl::read_excel("C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Program/Inflation/cpi-u-202206.xlsx", sheet = "index_wanted")

# adjusting the inflation raw data
inflation_dat_full <- inflation_dat %>%
  mutate(serial_no_1 = V1) %>%
  left_join(industry_dat %>% select(serial_no_1, industry), by = "serial_no_1") %>%
  select(-V1)

# weighting the mwans for multi-item baskets 
inflation_dat_full <-  inflation_dat_full %>%
  group_by(industry, date) %>%
  summarise(inflation = weighted.mean(norm_value, rel_imp)) 

# plotting the inlfation dat
library(plotly)
inflation_dat_full %>%
  plot_ly(x = ~date, 
          y = ~inflation, 
          split = ~industry, 
          type = 'scatter', 
          mode = 'lines')

# length(unique(inflation_dat_full$industry)) * length(unique(inflation_dat_full$date))
# checking the length =of the df is correct

df <- dat_join
rm(df)

rate_fxn <- function(df, txn_type, nbhd = "all") {
  # if(nbhd == "all") {df <- df %>% filter(!is.na(bg10_nbhd))} else {df <- df %>% filter(bg10_nbhd %in% paste(nbhd))}
  
  if(txn_type == "amt_adj") {
    dat <- df %>%  
      group_by(industry, month, year) %>%
      summarise(n = sum(txn_amt)) %>%
      
      # this is to match the inflation
      mutate(date = lubridate::as_date(mdy(paste0(month, "/1/", year)))) %>%
        # this is for the dates to join the inflation data
      
      # inflation adjustments
      left_join(inflation_dat_full, by = c("date", "industry")) %>%
      mutate(n_adj = n / inflation * 100) %>%
      select(-c(n, inflation, date)) %>%
      
      # time to calculate the rates
      pivot_wider(names_from = year, values_from = n_adj) %>%
      
      # this is the actual rate calculation
      mutate(`2020` = `2020` / `2019` - 1) %>%
      mutate(`2021` = `2021` / `2019` - 1) %>%
      mutate(`2022` = `2022` / `2019` - 1) %>%
      select(-`2019`) %>%
      pivot_longer(cols = c(`2020`, `2021`, `2022`), names_to = "year_rate", values_to = "count") %>%
      # fixing labels
      mutate(date = paste0(month, "/1/", year_rate)) %>%
      mutate(date = lubridate::as_date(mdy(date)))
  } else if(txn_type == "cnt") {
  
    dat <- df %>%  
      # this reformat the data so we can then calculate the rates 
      select(industry, month, year, txn_cnt) %>%   # change back to cnt
      # filter(industry == paste0(ind)) %>%               # this was commented out for edits 
      group_by(industry, month, year) %>%
      summarise(n = sum(txn_cnt)) %>%   # change back to count
      pivot_wider(names_from = year, values_from = n) %>%
      # this is the actual rate calculation
      mutate(`2020` = `2020` / `2019` - 1) %>%
      mutate(`2021` = `2021` / `2019` - 1) %>%
      mutate(`2022` = `2022` / `2019` - 1) %>%
      select(-`2019`) %>%
      pivot_longer(cols = c(`2020`, `2021`, `2022`), names_to = "year_rate", values_to = "count") %>%
      # fixing labels
      mutate(date = paste0(month, "/1/", year_rate)) %>%
      mutate(date = lubridate::as_date(mdy(date))) 
  } else {
    dat <- df %>%  
      # this reformat the data so we can then calculate the rates 
      select(industry, month, year, txn_amt) %>%   # change back to cnt
      # filter(industry == paste0(ind)) %>%               # this was commented out for edits 
      group_by(industry, month, year) %>%
      summarise(n = sum(txn_amt)) %>%   # change back to count
      pivot_wider(names_from = year, values_from = n) %>%
      # this is the actual rate calculation
      mutate(`2020` = `2020` / `2019` - 1) %>%
      mutate(`2021` = `2021` / `2019` - 1) %>%
      mutate(`2022` = `2022` / `2019` - 1) %>%
      select(-`2019`) %>%
      pivot_longer(cols = c(`2020`, `2021`, `2022`), names_to = "year_rate", values_to = "count") %>%
      # fixing labels
      mutate(date = paste0(month, "/1/", year_rate)) %>%
      mutate(date = lubridate::as_date(mdy(date))) 
  }
  
  dat <- dat[with(dat, order(industry, date)), ] %>% select(industry, date, count)
  
  return(dat)
}


write_xlsx(list(txn_amt = rate_fxn(dat_join, "amt"), txn_amt_adj = rate_fxn(dat_join, "amt_adj"), txn_cnt = rate_fxn(dat_join, "cnt")), "C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Program/Inflation/in_person_spending.xlsx")
