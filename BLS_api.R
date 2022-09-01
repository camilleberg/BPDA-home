# BLS API for inflation
# Written by Camille Bergeron
# August 3, 2022

# this is basically to pull all the series data at one time so it doesn't have to be manually
# using https://www.bls.gov/developers/api_r.htm as reference 

# setting up workspace
rm(list = ls())
library(tidyverse)
library(rjson)
library(readxl)
library(lubridate)
library(writexl)

proj_dir <-  "C:/Users/camilleb/Documents/GitHub/BPDA-home"
proj_folder_dir <- "C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Program/Inflation"

####################### INSTALLATION
library(devtools)
# install_github("mikeasilva/blsAPI")
library(blsAPI)

####################### SAMPLE CODE
# single series request 
response <- blsAPI('CUUR0000SEHB02') 
json_single <- fromJSON(response) 

# multiple series 
payload <- list('seriesid'=c('CUUR0000SAA1','CUUR0000SAA2', 'CUUR0000SEAE', 'CUUR0000SEAF')) 
response <- blsAPI(payload) 
json <- fromJSON(response) 

## One or More Series, Specifying Years 
payload <- list('seriesid'=c('CUUR0000SAA1','CUUR0000SAA2', 'CUUR0000SEAE', 'CUUR0000SEAF'), 'startyear'='2019', 'endyear'= '2022') 
response <- blsAPI(payload) 
json <- fromJSON(response) 

####################### USING FOR MY PURPOSES 
# basically, I want to pull all the series from 2019 t 

##### Loading the data

# loading in the serial numbers from the spreadsheet 
# these are the seria; nos that have been collected and decided upon to 
serial_nos <- readxl::read_excel(paste0(proj_folder_dir, "/cpi-u-202206.xlsx"), sheet = "index_wanted")

# loading the series from the API
payload <- list('seriesid' = serial_nos$serial_no_1, 'startyear' = '2019', 'endyear'= '2022')
response <- blsAPI(payload) 
inflation <- fromJSON(response) 

inflation[['Results']][['series']][[1]][['data']][[1]]$value
# the first "one" is the doff serial numbers
# the 2nd "one" is the different months 

##### Chanigng the data

# now time to renorm all of the data to base period Jan 2019

sapply(inflation[['Results']][['series']][[1]][['data']], '[[', 'value')
# this gives all of the values for the first expenditure
# using https://stackoverflow.com/questions/13016359/how-to-directly-select-the-same-column-from-all-nested-lists-within-a-list as ref 

num_expend <- length(inflation[['Results']][['series']])
num_periods <- length(inflation[['Results']][['series']][[1]][['data']])
num_elements <- num_expend * num_periods
  # this is the number of serial nos times the number of periods per each serial no 


# function to extract the data
series_list <- inflation[['Results']][['series']][[1]]

# this is a function to take out variables of int
combine_fxn <- function(series_list) {
  serial_no <- series_list$seriesID
  
  # extracting the data
  period <- sapply(series_list[['data']], '[[', 'periodName')
  year <- sapply(series_list[['data']], '[[', 'year')
  value <- sapply(series_list[['data']], '[[', 'value')
  
  # putting it together to export 
  df <- as_tibble(cbind(rep(serial_no, length(period)), period, year, value))
  
  df <- df %>% 
    mutate(date = my(paste(period, year)))
  
  return(df)
}

# initializing a tibble to run through 
inflation_dat <- combine_fxn(inflation[['Results']][['series']][[1]])

# looping through except the first one 
for(i in 2:num_expend) {
  inflation_dat <-  rbind(inflation_dat, combine_fxn(inflation[['Results']][['series']][[i]]))
}

dates_less <- inflation_dat %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  filter(n < 22) %>%
  select(date) 

# expected 924 rows, only have 901 - figure out why later 

# inspect <- inflation_dat[which(inflation_dat$date %in% dates_less$date), ]
# 
# checck <- inspect %>%
#   group_by(V1) %>%
#   summarise(n = n())
#
# fixed, refer to the bottom of the page, has to do with missing time series data 

##### Analyzing the data

# renormign the data

# 2019 January values to renorm 
val_2019 <- inflation_dat %>%
  filter(year == 2019 & period == "January") %>%
  select(V1, value) %>%
  mutate(value = as.numeric(value))

# combining and subtracting 
inflation_dat <- inflation_dat %>%
  left_join(val_2019, by = "V1", suffix = c(".all", ".2019")) %>%
  mutate(value.all = as.numeric(value.all)) %>%
  mutate(norm_value = value.all / value.2019 * 100) %>%
  select(-c(value.all, value.2019))

# graphign to see if there are msiing values   
library(plotly)

p_expend <- inflation_dat %>% 
  mutate(serial_no_1 = V1) %>%
  left_join(serial_nos, by = "serial_no_1") %>%
  select(V1, date, norm_value, `Expenditure Category`) %>%
  plot_ly(x = ~date, 
          y = ~norm_value, 
          split = ~`Expenditure Category`, 
          type = 'scatter', 
          mode = 'lines') 
p_ind <-  inflation_dat %>% 
  mutate(serial_no_1 = V1) %>%
  left_join(serial_nos, by = "serial_no_1") %>%
  select(V1, date, norm_value, industry) %>%
  plot_ly(x = ~date, 
          y = ~norm_value, 
          split = ~industry, 
          type = 'scatter', 
          mode = 'lines')  
subplot(p_expend, p_ind, nrows = 2, shareX= TRUE) %>%
  layout(xaxis = list(title = "Period"), 
         yaxis = list(title = "Inflationary Index Based on January 2019"), 
         title = "Non-Seasonally Adjusted Inflationary Indices from January 2019 to June 2022")

# writing out the data 
inflation_dat %>% 
  mutate(serial_no_1 = V1) %>%
  left_join(serial_nos, by = "serial_no_1") %>%
  select(V1, date, norm_value, `Expenditure Category`, rel_imp) %>% 
  write_xlsx(paste0(proj_folder_dir, "/inflation_idx_jan2019_june2022.xlsx"))

# writing out the non html plots
# kaleido(p_expend, file = paste0(proj_folder_dir, "/ind_expend.png"))
p_expend
p_ind

###### Looking at weird data

inflation_dat %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  filter(n!=42)

carsData <- blsAPI(list('seriesid'='CUUR0000SETA03','startyear' = '2019', 'endyear'= '2022') )
carsDataSeries <- fromJSON(carsData) 

