# BLS API for inflation
# Written by Camille Bergeron
# August 3, 2022

# this is basically to pull all the series data at one time so it doesn't have to be manually
# using https://www.bls.gov/developers/api_r.htm as reference 

# setting up workspace
rm(list = ls())
library(tidyverse)
library(rjson)

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