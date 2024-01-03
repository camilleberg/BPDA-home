
string <- '=VLOOKUP(hholder_movers_2015[@[Year_text]:[Year_text]]& ", " & hholder_movers_2015[@[helper_text]:[helper_text]],  nbhd_hhtype_raking_age[[#All],[helper_text]:[West Roxbury]], MATCH(hholder_movers_2015[[#Headers],[Allston]], nbhd_hhtype_raking_age[[#Headers],[helper_text]:[West Roxbury]], 0), FALSE) + VLOOKUP(hholder_movers_2015[@[Year_text]:[Year_text]]& ", " & hholder_movers_2015[@[helper_text]:[helper_text]],  nbhd_hhtype_remainder_reassigned[[#All],[helper_text]:[West Roxbury]], MATCH(hholder_movers_2015[[#Headers],[Allston]], nbhd_hhtype_remainder_reassigned[[#Headers],[helper_text]:[West Roxbury]], 0), FALSE)'

library(tidyverse)
library(writexl)

rm(list = ls())
write_xlsx(as_tibble(rbind(gsub("2015", "2020", string), 
                           gsub("2015", "2025", string), 
      gsub("2015", "2030", string), 
      gsub("2015", "2035", string), 
      gsub("2015", "2040", string), 
      gsub("2015", "2045", string), 
      gsub("2015", "2050", string))), "text_replacement.xlsx")

# creating table names
yrs <- c("2010", "2011-2014", "2015", "2016-2019", "2020", 2021:2050)
qrts <- c("Q1", "Q2", "Q3", "Q4")
bdrms <- c("0/1 bedrooms", "2 bedrooms", "3+ bedrooms")

bdrms_no <- c(1:3)

k <- 0

labels_no <- tibble(yr = rep(yrs, each = 3,length(bdrms)), hhtype = rep(bdrms_no, length(yrs)*length(bdrms)))
labels_no <- labels_no %>%
   mutate(label = paste0(yr, "_", hhtype))


labels <- tibble(yr = rep(yrs, each = 3,length(bdrms)), hhtype = rep(bdrms, length(yrs)*length(bdrms)))




yrs <- c(2023:2050)
status <- c("Actual", "Projected")

labels <- tibble(year = rep(yrs, each = length(bdrms) * length(qrts)))

labels <- labels %>%
   mutate(status = rep(qrts, 28, each = length(bdrms)), 
          bed = rep(bdrms, 112))

update <- 'B'
string <- paste0('=IF(HOUSING_projection!$B$4 & "_" &HOUSING_projection!$', update, '$5 < $H$2, SUMIFS(INDIRECT("housing_development_q[[#All],[" &$A6 &"]]"),housing_development_q[[#All],[construction_helper]], "=" & HOUSING_projection!$B$4 & "_" &HOUSING_projection!$', update, '$5), "")')

string2 <- paste0('=IF(HOUSING_projection!$B$4 & "_" & HOUSING_projection!$', update, '$5 < $H$2, SUMIFS(INDIRECT("housing_development_q[[#All],[" &$A6 &"]]"),housing_development_q[[#All],[construction_helper]], "=" & HOUSING_projection!$B$4 & "_" & HOUSING_projection!$', update, '$5),  IF(AND(HOUSING_projection!$B$4 & "_" & HOUSING_projection!$', update, '$5>=H2, HOUSING_projection!$B$4 & "_" & HOUSING_projection!$', update, '$5<=$J$2), VLOOKUP($A6, under_construction_sum_all[#All], MATCH(HOUSING_projection!$B$4 & "_" & HOUSING_projection!$', update, '$5,under_construction_sum_all[#Headers], 0), FALSE), ""))')

str_replace_all(string2, "HOUSING_projection!$B$5", "HOUSING_projection!$C$5")
strsplit(string2, "HOUSING_projection!$B$5")

labels <- c()

## for A- Z
year <- LETTERS[2]

for(i in 1:length(LETTERS)) {
   if(i %% 4 == 1) {
      year <- LETTERS[i+1]
   }
   
   update <- LETTERS[i+1]
   string2 <- paste0('=IF(HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81 < $H$2, SUMIFS(INDIRECT("housing_development_q[[#All],[" &$A82 &"]]"),housing_development_q[[#All],[construction_helper]], "=" & HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81),  IF(AND(HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81>=$H$2, HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81<=$J$2), VLOOKUP($A82, under_construction_sum_all[#All], MATCH(HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81,under_construction_sum_all[#Headers], 0), FALSE), ""))')
   
      
   labels <- c(labels, string2)
}

# For AA - AZ
year <- 'Z'

for(i in 1:length(LETTERS)) {
   if(i %% 4 == 0) {
      year <- paste0('A', LETTERS[i])
   }
   
   update <- paste0('A', LETTERS[i])
   string2 <- paste0('=IF(HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81 < $H$2, SUMIFS(INDIRECT("housing_development_q[[#All],[" &$A82 &"]]"),housing_development_q[[#All],[construction_helper]], "=" & HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81),  IF(AND(HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81>=$H$2, HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81<=$J$2), VLOOKUP($A82, under_construction_sum_all[#All], MATCH(HOUSING_projection!$', year, '$80 & "_" & HOUSING_projection!$', update, '$81,under_construction_sum_all[#Headers], 0), FALSE), ""))')
   
   
   labels <- c(labels, string2)
}

year <- 2025

labels <- c()

for(i in 2025:2049) {
   year <- i
   string3 <- paste0('=IF(AND(housing_projected_nbhd[[#Headers],[', year, '-Actual]]>=$G$2, housing_projected_nbhd[[#Headers],[', year,'-Actual]] <$I$3), "Construction %", "Forecast %")')
   
   labels <- c(labels, string3)
}

labels <- c()

year <- '2023'
idx <- 'C'
y <- 3

for(i in 2023: 2049) {
   year <- i
   
   idx <- LETTERS[y]
   y <- y+2
   string4 <- paste0('=IF(VLOOKUP(TEXT(LEFT(housing_projected_nbhd[[#Headers],[', year, '-Projected]], 4),0), under_construction_status[[Year_text]:[Status]], 2, FALSE)="Under Construction", [@[', year, '-Actual]], IF(VLOOKUP(TEXT(LEFT(housing_projected_nbhd[[#Headers],[', year, '-Projected]], 4), 0), under_construction_status[[Year_text]:[Status]], 2) = "Partially Under Construction", [@[', year, '-Actual]]+VLOOKUP(housing_projected_nbhd[@[Neighborhood]:[Neighborhood]], under_construction_add_units[#All], 2, FALSE), ', idx, '$61*VLOOKUP([@Neighborhood], completion_adjust[#All], MATCH(VLOOKUP(TEXT(LEFT(housing_projected_nbhd[[#Headers],[', year, '-Projected]], 4),0), under_construction_status[[Year_text]:[Status]], 2, FALSE), completion_adjust[#Headers], 0), FALSE)))')
   
   labels <- c(labels, string4)
      
}

write_xlsx(as_tibble(labels), "labels.xlsx")
