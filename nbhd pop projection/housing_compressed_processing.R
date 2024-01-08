
# script to pull input data for nbhd project


rm(list = ls())

# necessary libraries
{
  library(tidyverse)
  library(openxlsx)
  library(tidycensus)
  library(sf)
  library(tidygeocoder)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(tigris)
  #remotes::install_github("walkerke/tigris")
  library(MazamaLocationUtils)
  library(sjmisc)
  library(writexl)
  
}


# reading in crosswalk files
username <- Sys.info()["user"]

box_dir = paste0("C:/Users/", username, "/Box/Research/Active Projects/Neighborhood Projection_2023/03. Data/Source Data/Development (Completions) Data")
working_dir = paste0("/Users/", username, "/Documents/GitHub/BPDA-home/nbhd pop projection")

TRACT_20 = read.xlsx(paste0(working_dir, '/tract20_nbhd20_crosswalk_DOT_split.xlsx'))
NBHD <- tibble(nbhd = unique(TRACT_20$nbhd))

# reading in housing data
# getting most recent housing file

### CHANGE #####
recent_housing_file <- "Compressed Housing Master 11-30-23.xlsx"
################
output_file_name <- paste0("nbhd_input_updated", str_split(recent_housing_file, " ")[[1]][4])

housing_df <- read.xlsx(paste0(working_dir, "/housing_monthly/", recent_housing_file))
pipeline_df <- read.xlsx(paste0(working_dir, "/housing_monthly/", recent_housing_file), sheet = "Pipeline")


# first filtering got correct reporting agency 
rep_cat <- c("G/N", "N")


###### FUNCTIONS  ------------------------------------------------

assign_address <- function(df, type = "all") {
  # creates address column for coding later 
  # also cleans data / gets rid of irrelevant columns 
  # also reformat date to proper datetime format 
  df_address = df %>%
    mutate(address = paste0(`St.Number`, " ", Street, " ", ifelse(is.na(`St.Suffix`), "", St.Suffix), ", Boston, MA"), # making address
           LAT = as.numeric(LAT), LONG = as.numeric(LONG), # converting lat/long to int
           Complete.Date = excel_numeric_to_date(as.numeric(Complete.Date))) %>% # making date column 
    mutate(complete.year = format(Complete.Date, "%Y"), 
           complete.quarter = quarter(Complete.Date)) %>% 
    as_tibble() 
  
  if(type == "completions") {
    df_address <- df_address %>%
      select(Project, address, LAT, LONG, Complete.Date, complete.year, 
             complete.quarter, New.SF, `0BR`, `1BR`, `2BR`, `3+BR`, 
             Current.Status, Reporting.Category, New.Units)
  } else {
    df_address <- df_address %>%
      select(Project, address, LAT, LONG, Complete.Date, complete.year, 
             complete.quarter, New.SF, `0BR`, `1BR`, `2BR`, `3+BR`, 
             Current.Status, Reporting.Category, New.Units, `Permit.#`)
  }
  
  return(df_address)
}

  
###### geocoding non-existent lat/long pairs ------------------------------------------------


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


###### pairing the lat/long with associated tracts ------------------------------------------------


assign_bg20 <- function(df) {
  # looping over to get geoid
  df[["bg20"]] <- NA
  
  for(i in 1:nrow(df)) {
    # getting geoid
    geoid <- location_getCensusBlock(latitude = df[["LAT"]][i], longitude = df[["LONG"]][i], censusYear =  2020)[3][[1]]
    
    # if unavailable, like iferror(na)
    if(length(geoid) >= 1) {
      df[["bg20"]][i] = geoid
    }
  }
  
  return (df)
}



###### assigning tracts to nbhds and cleaning------------------------------------------------

# joining to nbhd table
assign_nbhd <- function(df){
  df_nbhd <- df %>%
    mutate(tract20 = substr(bg20, 1, 11)) %>%
    left_join(TRACT_20 %>%
                mutate(tract20 = as.character(geoid20)) %>%
                select(!geoid20), by = "tract20") 
  return(df_nbhd)
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

assign_bdrm <- function(df, type = "all", agg = TRUE) {
  df_cleaned <- df %>%
    
    # summing on 
    mutate(hhtype_1 = ifelse(is.na(`0BR`), 0, `0BR`) + ifelse(is.na(`1BR`), 0, `2BR`), 
           hhtype_2 = ifelse(is.na(`2BR`), 0, `2BR`), 
           hhtype_3 = ifelse(is.na(`3+BR`), 0, `3+BR`))  %>%
    select(!(ends_with("BR") | ends_with("20"))) %>%
    select(-c(address, Reporting.Category, LAT, LONG, New.SF))
  
  # assigning tracts to nbhds and cleaning -- aggregates by project to get counts by nbhd 
  # for new units 
  if(agg == TRUE) {
    df_cleaned <- df_cleaned %>%
      pivot_longer(cols = starts_with("hhtype"), names_to = "hhtype", values_to = "bdrm.units")
  }
  
  # grouping by year for completions
  if(type == "completions") {
    df_cleaned <- df_cleaned %>%
      group_by(complete.year, complete.quarter, nbhd, size, hhtype) %>%
      summarise(new_bdrm_units = sum(bdrm.units), new_units = sum(New.Units)) 
  } else {
    df_cleaned <- df_cleaned %>%
      group_by(nbhd, size, hhtype) %>%
      summarise(new_bdrm_units = sum(bdrm.units), new_units = sum(New.Units)) 
  }
  
  return(df_cleaned)
}


###### assigning tracts to nbhds and cleaning------------------------------------------------

get_last_quarter_completions <- function(df_cleaned, max_date) {
  # financial quarter and year to pull
  current_completion <- as.Date(cut(max_date, "quarter"))-1
  # this gets the most recently COMPLETED financial quarter 
  
  update_time <- c(format(current_completion, "%Y"), quarter(current_completion))
  # reformatting to filter the data set later on 
  
  
  # pulls all completion the last fully completed quarter 
  new_construction <- df_cleaned %>% 
    filter(complete.year == update_time[1], complete.quarter == update_time[2]) %>%
    ungroup() %>%
    group_by(across(!(starts_with("new") | starts_with("size")))) %>%
    summarize(new_bdrm_units = sum(new_bdrm_units), 
              new_units = sum(new_units)) %>%
    pivot_wider(names_from = c(hhtype), values_from = new_bdrm_units) %>%
    mutate(hhtype_units = rowSums(across(contains("hhtype"))), 
           diff_units = new_units - hhtype_units) 
  
  return(new_construction)
}

get_last_quarter_completions_raked <- function(df_cleaned, max_date) {
  # pulls all completion the last fully completed quarter and then rakes to new units 
  
  current_completion <- as.Date(cut(max_date, "quarter"))-1
  update_time <- c(format(current_completion, "%Y"), quarter(current_completion))
  # reformatting to filter the data set later on 
  
  new_construction <- df_cleaned %>% 
    ## same as get_last_q_completions
    filter(complete.year == update_time[1], complete.quarter == update_time[2]) %>%
    
    # agg both small and large
    ungroup() %>%
    group_by(across(!(starts_with("new") | starts_with("size")))) %>%
    summarize(new_bdrm_units = sum(new_bdrm_units), 
              new_units = sum(new_units)) %>%
    
    # pivots out to bdrm type
    pivot_wider(names_from = c(hhtype), values_from = new_bdrm_units) %>%
    mutate(hhtype_units = rowSums(across(contains("hhtype"))), 
           diff_units = new_units - hhtype_units) %>%
    
    # becasue of missing bdrm info, this part of the function rakes 
    mutate(all_1 = sum(.[[6]]), all_2 = sum(.[[7]]), all_3 = sum(.[[8]]), all_units = sum(.[[5]])) %>%
    mutate(across(contains("hhtype"), ~. / hhtype_units * new_units)) %>%
    mutate(across(contains("all"), ~. / all_units * new_units)) %>%
    mutate(hhtype_1 = ifelse(is.nan(hhtype_units), all_1, hhtype_1), 
           hhtype_2 = ifelse(is.nan(hhtype_units), all_2, hhtype_2), 
           hhtype_3 = ifelse(is.nan(hhtype_units), all_3, hhtype_3)) %>%
    select(!(ends_with("units") | starts_with("all"))) %>%
    full_join(NBHD, by = "nbhd") %>%
    arrange(nbhd) 
  
  return(new_construction)
}

get_under_construction_raked <- function(df_cleaned) {
  # pulls all "completion the last fully completed quarter and then rakes to new units "under construction"
  # permitted but not under construction
  
  under_construction <- df_cleaned %>% 
    ## same as get_last_q_completions
    filter(is.na(complete.year)) %>%
    # agg both small and large
    ungroup() %>%
    group_by(across(!(starts_with("new") | starts_with("size")))) %>%
    summarize(new_bdrm_units = sum(new_bdrm_units), 
              new_units = sum(new_units)) %>%
    
    # pivots out to bdrm type
    pivot_wider(names_from = c(hhtype), values_from = new_bdrm_units) %>%
    mutate(hhtype_units = rowSums(across(contains("hhtype"))), 
           diff_units = new_units - hhtype_units) %>%
    
    # becasue of missing bdrm info, this part of the function rakes 
    mutate(all_1 = sum(.[[6]]), all_2 = sum(.[[7]]), all_3 = sum(.[[8]]), all_units = sum(.[[5]])) %>%
    mutate(across(contains("hhtype"), ~. / hhtype_units * new_units)) %>%
    mutate(across(contains("all"), ~. / all_units * new_units)) %>%
    mutate(hhtype_1 = ifelse(is.nan(hhtype_units), all_1, hhtype_1), 
           hhtype_2 = ifelse(is.nan(hhtype_units), all_2, hhtype_2), 
           hhtype_3 = ifelse(is.nan(hhtype_units), all_3, hhtype_3)) %>%
    select(!(ends_with("units") | starts_with("all"))) %>%
    full_join(NBHD, by = "nbhd") %>%
    arrange(nbhd) 
  
  return(new_construction)
}

  
### FORECAST %

get_hist_small_completions <-  function(df_cleaned) {
  # small completions for weighted average of forecast %
  small_hist_completions <- df_cleaned %>%
    filter(size == "small", !is.na(complete.year)) %>%
    group_by(nbhd) %>%
    summarise(new_units = sum(new_units)) %>%
    full_join(NBHD, by = "nbhd") %>%
    arrange(nbhd)
  
  return(small_hist_completions)
}

get_small_pipeline <- function(df) {
  df_small_pipeline <- df %>%
    filter(size == "small") %>%
    group_by(nbhd) %>%
    summarise(all_new_units = sum(New.Units, na.rm = T)) %>%
    full_join(NBHD, by = "nbhd") %>%
    arrange(nbhd)
  
  return(df_small_pipeline)
}

get_large_pipeline_not_permit <- function(df){
  df_large_pipe_not_permit <- df %>%
    filter(size == "large", 
           is.na(`Permit.#`)) %>%
    group_by(nbhd) %>%
    summarise(all_new_units = sum(New.Units, na.rm = T)) %>%
    full_join(NBHD, by = "nbhd") %>%
    arrange(nbhd)
  
  return(df_large_pipe_not_permit)
}

### PULLING DATA  ----------------------------------------------------------

##### HOUSING ----------

housing_address <-  assign_address(housing_df, type = "completions") %>%
  filter(Reporting.Category %in% rep_cat)

housing_max_date <- max(housing_address$Complete.Date, na.rm = TRUE)

# geocoding all lat/long pairs
housing_geocoded <- assign_lat_long(housing_address)

# extracting addresses to be fixed 
housing_edit <- housing_geocoded[which(is.na(housing_geocoded$LAT)),]
housing_bg <- assign_bg20(housing_geocoded %>% 
                               filter(!(is.na(LAT) | is.na(LONG)))
                             )


# handling NAs -- geocoding new lat/long pairs for NA lat/long bgs
# that is, the addreses dwerrem't righ to go to lat/long
housing_edit[grepl("Showa", housing_edit$Project),]$address <- "420 Pond St, Boston, MA"
housing_edit[grepl("2 Northdale TE", housing_edit$Project),]$address <- "2 Northdale Terrace, Boston, MA"


# cleaning data for incorrect lat/long pairs 
housing_edit_geocoded <- rbind(housing_edit %>% mutate(bg20 = NA), housing_bg %>% filter(is.na(bg20))) %>%
  geocode(address, method = 'osm', lat = lat_new , long = long_new) %>%
  mutate(LAT = lat_new, LONG = long_new) %>%
  select(-c(lat_new, long_new)) %>%
  filter(!is.na(LAT)) # this is an NA thing 

# assigning bgs based on lat long
housing_edit_bg20 <- assign_bg20(housing_edit_geocoded)

# appending
housing_bg20_final <- rbind(housing_bg, housing_edit_bg20)

# assigning nbhds and bdrms for aggregation
housing_agg <- housing_bg20_final %>%
  assign_nbhd() %>%
  assign_size() %>%
  assign_bdrm(agg = TRUE, type = "completions")

write_xlsx(housing_agg, "comp.xlsx")

last_quarter_compl <- housing_agg %>%
  get_last_quarter_completions_raked(max_date = housing_max_date) 

hist_small <- housing_agg %>%
  get_hist_small_completions()

write.xlsx(last_quarter_compl, file = output_file_name, sheetname = "recent quarterly completions", append = T)

# also need under construction numbers 
# aka not completed and has a permit number 

##### PIPELINE ----------

pipeline_clean <- assign_address(pipeline_df %>% mutate(Street = Street.Name)) %>%
  filter(Reporting.Category %in% rep_cat)
pipeline_geocoded <- assign_lat_long(pipeline_clean)

pipeline_edit <- pipeline_geocoded[which(is.na(pipeline_geocoded$LAT)),]
pipeline_bg <- assign_bg20(pipeline_geocoded %>% 
                            filter(!(is.na(LAT) | is.na(LONG)))
              )

pipeline_edit[grepl("Olmst", pipeline_edit$Project),]$address <- "618 Harvard St, Boston, MA"
  # this is not the exact address but the building next to it

# cleaning data for incorrect lat/long pairs 
pipeline_edit_geocoded <- rbind(pipeline_edit %>% mutate(bg20 = NA), pipeline_bg %>% filter(is.na(bg20))) %>%
  geocode(address, method = 'osm', lat = lat_new , long = long_new) %>%
  mutate(LAT = lat_new, LONG = long_new) %>%
  select(-c(lat_new, long_new))

# assigning bgs based on lat long
pipeline_edit_bg20 <- assign_bg20(pipeline_edit_geocoded)

# appending
pipeline_bg20_final <- rbind(pipeline_bg, pipeline_edit_bg20)

# assigning nbhds and bdrms for aggregation
pipeline_agg <- pipeline_bg20_final %>%
  assign_nbhd() %>%
  assign_size()

# getting small pipeline and large pipeline not yet permitted
pipeline_forecast <- pipeline_agg %>% get_small_pipeline() %>%
  left_join(pipeline_agg %>% get_large_pipeline_not_permit(), by = "nbhd", suffix = c(".small", ".large"))


##### OUTPUT ----------

# writing out in output sheet 


