# this script is for grabbing hotel data

## the objective 
#  the hotel properties, rooms, 
# occupancy rate and average daily rate and revpar 
# for the different areas of Boston 
# every month January 2019 to April 2022

# loading libraries and setting dir
rm(list =ls())
library(tidyverse)
library(readxl)
library(writexl)

proj_dir <- getwd()
setwd("C:/Users/camilleb/Box/Research/Active Projects/Economic Indicators/01. Other Indicators/Hotels/05. Pinnacle Data")
list.files()

reading_fxn <- function(this_year) {

  if(this_year < 2022) {
    dat <- read_xlsx(paste0("Historical Tracking Data/Pinnacle Jan-Dec_", this_year, "_Recalculate.xlsx"), sheet = paste0(this_year, " Database"))
  } else {
    # reading in the files
    dat <- read_xlsx(paste0("Pinnacle Jan-Dec_", this_year, "_Recalculate.xlsx"), sheet = paste0(this_year, " Database"))
  }
  # dropping unnec rows
  
  dat <- dat[, 1:9]
    # this is to make sure the drop_na does what it needs to 
  
  dat <- dat %>% 
    drop_na() %>% # this gets rid of the explainer titles as well as the Bosotn+cambridge, which we don't want
    mutate(Year = this_year)
  
  # renaming colnames
  colnames(dat) <- c("Month", "Submarket", "Properties", "avail_rooms", "occ_rate", "rooms_sold", "avg_daily_rate", 
                     "RevPAR", "total_rev", "Year")
  
  # selecting out the vars of interest and getting rod of total boston numbers 
  dat <- dat %>%
    mutate(Date = paste(Month, Year)) %>%
    select(Date, Submarket, Properties, avail_rooms, occ_rate, avg_daily_rate, RevPAR) %>%
    filter(Submarket != "Boston Total")
  
  return(dat)

}

# going to loop through function of rhistoricla data 
years_of_int <- as.character(2019:2022)

# initializing tibble
df <- tibble("Date" = NA, "Submarket"= NA, "Properties"= NA, "avail_rooms"=NA, "occ_rate"=NA, "avg_daily_rate"=NA, 
             "RevPAR"=NA)

# looping through the years 
for(i in 1:length(years_of_int)) {
  df <- rbind(df, reading_fxn(years_of_int[i]))
}

# dropping the first NA initialization row 
df <- df[-1, ]
# small erorr in formatting, but it doesn't really matter 

write_xlsx(df, "Pinnacle Hotel Historical Request 2019 to April 2022_data.xlsx")

setwd(proj_dir)