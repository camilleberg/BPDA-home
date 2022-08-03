# for mastercard graphs 
# July 14, 2022
# Camille Bergeron

# edited august 1, 2022 to include all industries 

# libraries
rm(list = ls())
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(hash)
library(plotly)

# setting directories and looking at files 
proj_dir <-  "C:/Users/camilleb/Documents/GitHub/BPDA-home"
proj_folder_dir <- "C:/Users/camilleb/Box/Research/Active Projects/Greater_downtown_request_2022"
setwd("F:/Deptment/Research/Data/MasterCard")

# reading in data
dat <- read.csv("ALL_MA_blockgroup-level_monthly_indices_2022-04-01.csv")

# reading in the blockgroup nbhd
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
dat_join <- left_join(dat, nbhds, "geoid") 

# filtering out the data
# to use for filtering 
`%notin%` <- Negate(`%in%`)

# dat_join <- dat_join[-which((dat_join$industry %in% c("bar", "jgw")) &  dat_join$month == 9 & dat_join$year == 2021), ]

# names of industries 
# ind_names <- hash()
# ind_names[["acc"]] <- "Accommodation"
# ind_names[["app"]] <- "Apparel"
# ind_names[["aut"]] <- "Auto and Fuel"
# ind_names[["bar"]] <- "Bars, Taverns, and Nightclubs"
# ind_names[["cbs"]] <- "Cosmetic and Beauty Services"
# ind_names[["cel"]] <- "Consumer Electronics"
# ind_names[["dgm"]] <- "Department and General Merchendise"
# ind_names[["eap"]] <- "Eating places"
# ind_names[["ent"]] <- "Art, Entertainment, and Recreation"
# ind_names[["gro"]] <- "Grocery"
# ind_names[["hfn"]] <- "Home Furnishings"
# ind_names[["jgw"]] <- "Jewelry and Giftware"
# ind_names[["pst"]] <- "Proffesional and Scientific Services"
# ind_names[["mvf"]] <- "Unsure"


# function to calculate and return the rates 
rate_fxn <- function(df, ind) {
  dat <- dat_join %>%  #df 
    # this reformat the data so we can then calculate the rates 
    select(industry, month, year, txn_cnt) %>%
    # filter(industry == paste0(ind)) %>%               # this was commented out for edits 
    group_by(industry, month, year) %>%
    summarise(n = sum(txn_cnt)) %>%
    pivot_wider(names_from = year, values_from = n) %>%
    # this is the actual rate calculation
    mutate(`2020` = `2020` / `2019` - 1) %>%
    mutate(`2021` = `2021` / `2019` - 1) %>%
    mutate(`2022` = `2022` / `2019` - 1) %>%
    select(industry, `2020`, `2021`, `2022`) %>%
    pivot_longer(cols = c(`2020`, `2021`, `2022`), names_to = "year_rate", values_to = "count") %>%
    # fixing labels
    mutate(date = paste0(month, "/1/", year_rate)) %>%
    mutate(date = lubridate::as_date(mdy(date)))
  
  dat <- dat[order(dat$date), ] %>% select(date, count)
  
  return(dat)
}


# this is for all boston
rate_ret <- rate_fxn(dat_join %>% filter(bg10_nbhd %notin% "None"), "ret")
rate_gro <- rate_fxn(dat_join %>% filter(bg10_nbhd %notin% "None"), "gro")
rate_eap <- rate_fxn(dat_join %>% filter(bg10_nbhd %notin% "None"), "eap")

# making all bosotn csv
# all_boston <- rbind(rate_ret, rate_eap, rate_gro)
all_boston <- rate_fxn(dat_join %>% filter(bg10_nbhd %notin% "None"), "ret")

# for all the neighborhoods now 

rate_ret_backbay <- rate_fxn(dat_join %>% filter(bg10_nbhd %in% "Back Bay"), "ret")
rate_gro_backbay <- rate_fxn(dat_join %>% filter(bg10_nbhd == "Back Bay"), "gro")
rate_eap_backbay <- rate_fxn(dat_join %>% filter(bg10_nbhd == "Back Bay"), "eap")
# backbay <- rbind(rate_ret_backbay, rate_eap_backbay, rate_gro_backbay)
backbay <- rate_fxn(dat_join %>% filter(bg10_nbhd %in% "Back Bay"), "ret")

rate_ret_sbw <- rate_fxn(dat_join %>% filter(bg10_nbhd == "South Boston Waterfront"), "ret")
rate_gro_sbw <- rate_fxn(dat_join %>% filter(bg10_nbhd == "South Boston Waterfront"), "gro")
rate_eap_sbw <- rate_fxn(dat_join %>% filter(bg10_nbhd == "South Boston Waterfront"), "eap")
# sbw <- rbind(rate_ret_sbw, rate_eap_sbw, rate_gro_sbw)
sbw <- rate_fxn(dat_join %>% filter(bg10_nbhd == "South Boston Waterfront"), "ret")

rate_ret_fl <- rate_fxn(dat_join %>% filter(bg10_nbhd %in%  c("Fenway", "Longwood")), "ret")
rate_gro_fl <- rate_fxn(dat_join %>% filter(bg10_nbhd %in%  c("Fenway", "Longwood")), "gro")
rate_eap_fl <- rate_fxn(dat_join %>% filter(bg10_nbhd %in%  c("Fenway", "Longwood")), "eap")
# fl <- rbind(rate_ret_fl, rate_eap_fl, rate_gro_fl)
fl <- rate_fxn(dat_join %>% filter(bg10_nbhd %in%  c("Fenway", "Longwood")), "ret")


# for greater downtwn 
greater_down <- read_excel(wb_name, sheet = "downtown_block_grp") %>% mutate(geoid = GEOID10) %>% select(geoid)
dat_down <- left_join(greater_down, dat_join, "geoid")
rate_ret_down <- rate_fxn(dat_down, "ret")
rate_gro_down <- rate_fxn(dat_down, "gro")
rate_eap_down <- rate_fxn(dat_down, "eap")
# down <- rbind(rate_ret_down, rate_eap_down, rate_gro_down)
down <- rate_fxn(dat_down, "ret")

# library(plotly)
# rate_fxn(dat_join, "jgw") %>%
#   plot_ly(x = ~date, y = ~count, type = 'scatter') 
# 
# rate_fxn(dat_join, "bar") %>%
#   plot_ly(x = ~date, y = ~count, type = 'scatter') 

# graphs for kevin

# individual graphs 
plot_ly(data = down, 
        x = ~date, 
        y = ~count, 
        split = ~industry, 
        type = 'scatter', mode  = "line") %>%
  layout(title = "Transaction Counts for Greater Downtown", 
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Comparison Rate to 2019"))

# split by industry
p1 <- all_boston %>%
  ggplot(aes(x = date, y = count, color = industry)) +
  geom_line() +
  facet_wrap(~industry) +
  theme(legend.position="none") +
  labs(title = "Transaction Counts for All Boston", x = "Date", y = "Comparison Rate to 2019")
ggplotly(p1)

# for all the hubs
# joining function
join_fxn <- function(df, nbhd_name) {
  return(df %>%
    mutate(hub = paste(nbhd_name)))
}

commercial_hubs <- rbind(join_fxn(down, "Greater Downtown"), 
                         join_fxn(all_boston, "All Boston"), 
                         join_fxn(backbay, "Back Bay"), 
                         join_fxn(fl, "Fenway/Longwood"), 
                         join_fxn(sbw, "South Boston Waterfront"))

p2 <- commercial_hubs %>%
  ggplot(aes(x = date, y = count, color = hub)) +
  geom_line() +
  facet_wrap(~industry) +
  labs(title = "Transaction Counts for Boston's Commercial Hubs", x = "Date", y = "Comparison Rate to 2019")
ggplotly(p2)

# writing out the new csv 
setwd(proj_folder_dir)
write_xlsx(list(boston = all_boston, backbay = backbay, southbostonwater= sbw, fenway_longwood = fl, downtown = down), "in_person_spending_commercial_hubs_all_industries.xlsx")



setwd(proj_dir)
