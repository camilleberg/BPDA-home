# TRYING DIFF IN DIFF ANALYISYS
# looking fo rsimilar tracts

rm(list = ls())

# libraries
{
  library(tidyverse)
  library(readxl)
  library(xts)
  library(zoo)
  library(lubridate)
  library(ggpubr)
  library(plotly)
  library(purrr)
}

## LOADING DATA ---------------------------------------------------

dat <- read_xlsx("C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Request/Troubleshooting/tract_weekly_comp_MC_2023.09.05.xlsx")

dat_ret <-  dat %>% 
  filter(industry == "Total Retail", geo_type == "Msa") %>%
  mutate(do_date = as.Date(paste(wk, yr, 'Sun'), '%U %Y %a')) %>%
  group_by(do_date, tract_id) %>%
  summarise(txn_cnt = sum(txn_cnt), txn_amt = sum(txn_amt))

# trial ts 
dat_filtered <- dat %>% 
  mutate(tract_id = ifelse(tract_id %in% c(25025010701,25025010702), "study_area", tract_id), 
         do_date = as.Date(paste(wk, yr, 'Sun'), '%U %Y %a')) %>%
  filter(industry == "Total Retail", geo_type == "Msa", tract_id == "study_area") %>%
  group_by(tract_id, do_date) %>%
  summarise(txn_cnt = sum(txn_cnt), txn_amt = sum(txn_amt))
         
ts_newbury <- xts(dat_filtered$txn_cnt, order.by = dat_filtered$do_date)

# making list of Xts for all geoids
dat_ts <- lapply(split(dat_ret, dat_ret$tract_id), 
                 function(x) xts(x$txn_cnt, order.by = x$do_date))
str(dat_ts)

## EUCLIDEAN SUBTRACTION ---------------------------------------------------

# extracting the base / study and random comparison
study_dat <- coredata(ts_newbury)
comp_dat <- coredata(dat_ts[6][[1]])

ts_newbury - dat_ts[6][[1]]

# generate graph function #

# filtering for list that are the same length as the study area 
# this is done for comparison's sake)
dat_ts_comp = dat_ts[lengths(dat_ts) == length(study_dat)]

# creating 'residuals' df to map out
df_comp <- lapply(dat_ts_comp, function(x) {x - ts_newbury}) %>% 
  lapply(fortify.zoo) %>%
  map_df(~as_tibble(.), .id = 'tract_id') %>%
  rename(diff = `X[[i]]`)

# graphing to see
df_comp %>%
  group_by(tract_id) %>%
  summarise(avg  = mean(diff)) %>%
  ggplot() +
  geom_histogram(aes(x = avg)) +
  labs(title = "Dist of avg diff in spending from study area")

df_comp %>%
  group_by(tract_id) %>%
  summarise(avg  = mean(diff)) %>%
  filter(avg > 0)


spread(df_comp, key = tract_id, value = diff) 


# basic correlation
dat_ts_raw <- lapply(dat_ts_comp, fortify.zoo) %>%
  map_df(~as_tibble(.), .id = 'tract_id') %>%
  rename(raw = `X[[i]]`) 

# reformatting to graph
geoids <- dat_ts_raw %>%
  select(tract_id) %>%
  unique() 

dat_ts_raw %>%
  mutate(study_area = rep(coredata(ts_newbury), nrow(dat_ts_raw) / nrow(coredata(ts_newbury)))) %>%
  left_join(geoids %>%
              mutate(idx = as.character(1:nrow(geoids))), by = 'tract_id')  %>%  
  filter(tract_id == "25025010204") %>% plot_ly(
    x=~study_area, 
    y=~raw, 
    group=~tract_id, 
    color = ~tract_id,
    type = 'scatter',
    mode = 'markers'
  )

# potentials
good <- c(7, 8, 100, 102, 115, 116, 117, 135, 142, 154, 168, 180, 46, 50, 52)
ok <- c(103, 105, 110, 113, 140, 153, 157, 164, 192, 34, )
the study <- c(49, v)

