# this is to parse the pob recode from 
# the 2016 - 2020 ACS 

rm(list = ls())
library(tidyverse)
library(pdftools)

proj_dir <- "C:/Users/camilleb/Box/Research/Active Projects/Young Adults_2022/03. Data/2020"
setwd(proj_dir)

# reading in the pdf 
pob <- pdftools::pdf_text(pdf =  "PUMS_Data_Dictionary_2016-2020-pob.pdf")
pob_line <- strsplit(pob, "\\n")

# fixing / cutting the first page
first_page <- str_trim(pob_line[[1]][22:length(pob_line[[1]])])
last_page <- str_trim(pob_line[[5]][1:18])

# putting into one csv
write.csv(c(first_page, pob_line[[2]], pob_line[[3]], pob_line[[4]], last_page), "pobp_recode.csv")
