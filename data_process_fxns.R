# functions


###### FUNCTIONS  ------------------------------------------------

assign_address <- function(df) {
  # creates address column for coding later 
  # also cleans data / gets rid of irrelevant columns 
  # also reformat date to proper datetime format 
  df_address = df %>%
    mutate(address = paste0(`St.Number`, " ", Street, " ", ifelse(is.na(`St.Suffix`), "", St.Suffix), ", Boston, MA"), # making address
           LAT = as.numeric(LAT), LONG = as.numeric(LONG), # converting lat/long to int
           Complete.Date = excel_numeric_to_date(as.numeric(Complete.Date))) %>% # making date column 
    mutate(complete.year = format(Complete.Date, "%Y"), 
           complete.quarter = quarter(Complete.Date)) %>% 
    as_tibble() %>%
    select(Project, address, LAT, LONG, Complete.Date, complete.year, 
           complete.quarter, New.SF, `0BR`, `1BR`, `2BR`, `3+BR`, 
           Current.Status, Reporting.Category, New.Units, `Permit.#`)
  
  return(df_address)
}


assign_lat_long <- function(df) {
  # this is the geo coding script (takes addresses and goes to lat / long)
  print(paste("There are", df %>%
                filter(is.na(LAT) | is.na(LONG)) %>% nrow(), "addresses to be geocoded"))
  
  df_geocoded <- df %>%
    filter(is.na(LAT) | is.na(LONG)) %>%
    geocode(address, method = 'osm', lat = lat_new , long = long_new) %>%
    mutate(LAT = lat_new, 
           LONG = long_new) %>%
    select(-c(lat_new, long_new))
  
  df[which(is.na(df[["LAT"]])),] <- df_geocoded
  
  # number of address not geode (throwing errors)
  print(paste0("There are still ", sum(is.na(df[["LAT"]])), " projects left to be edited"))
  
  return(df)
}

# changing data types and dealing  bedroom / size classifications
assign_size <- function(df) {
  df_size <- df %>%     # determining size (small / large)
    mutate_at(vars(ends_with("BR") | starts_with("New.")), as.numeric) %>%
    mutate(size = 
             case_when(
               New.SF < 50000 ~ "small", 
               New.SF >= 50000 ~ "large", 
               is.na(New.SF) & New.Units <50 ~ "small", 
               is.na(New.SF) & New.Units >= 50 ~ "large"
             ))
  return(df_size)
}

assign_bdrm <- function(df) {
  # assigns size to bedrooms
  df_cleaned <- df %>%
    
    # summing on 
    mutate(hhtype_1 = ifelse(is.na(`0BR`), 0, `0BR`) + ifelse(is.na(`1BR`), 0, `2BR`), 
           hhtype_2 = ifelse(is.na(`2BR`), 0, `2BR`), 
           hhtype_3 = ifelse(is.na(`3+BR`), 0, `3+BR`)) %>%
    select(!(ends_with("BR") | ends_with("20"))) %>%
    select(-c(address, Reporting.Category, LAT, LONG, New.SF)) 
  
  return(df_cleaned)
}

get_new_bdrm_dist <- function(df) {
  df_dist <- df %>%
    
    # assigning tracts to nbhds and cleaning -- aggregates by project to get counts by nbhd 
    # for new units  
    pivot_longer(cols = starts_with("hhtype"), names_to = "hhtype", values_to = "bdrm.units") %>%
    
  group_by(nbhd, hhtype) %>%
    summarize(new_bdrm_units = sum(bdrm.units), 
              new_units = sum(New.Units)) %>%
    pivot_wider(names_from = c(hhtype), values_from = new_bdrm_units) %>%
    mutate(hhtype_units = rowSums(across(contains("hhtype"))), 
           diff_units = new_units - hhtype_units) %>%
    
    # aggregating to get citywide dist
    mutate(all_1 = sum(.[[3]]), 
           all_2 = sum(.[[4]]), 
           all_3 = sum(.[[5]]), 
           all_units = sum(.[[2]])) %>%
    
    # getting dist for each nbhd
    mutate(across(contains("hhtype"), ~. / hhtype_units * new_units), 
           across(contains("all"), ~. / all_units * new_units)) %>%
    
    # if there's a divide by 0 isntance (missing data), using city dist 
    mutate(hhtype_1 = ifelse(is.nan(hhtype_units), all_1, hhtype_1), 
           hhtype_2 = ifelse(is.nan(hhtype_units), all_2, hhtype_2), 
           hhtype_3 = ifelse(is.nan(hhtype_units), all_3, hhtype_3)) %>%
    select(!(ends_with("units") | starts_with("all"))) 
  
  return(df_dist)
}