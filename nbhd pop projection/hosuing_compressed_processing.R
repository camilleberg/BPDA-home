
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
}

# reading in crosswalk files
username <- Sys.info()["user"]

box_dir = paste0("C:/Users/", username, "/Box/Research/Active Projects/Neighborhood Projection_2023/03. Data/Source Data/Development (Completions) Data")
working_dir = paste0("/Users/", username, "/Documents/GitHub/BPDA-home/nbhd pop projection")

TRACT_20 = read.xlsx(paste0(working_dir, '/tract20_nbhd20_crosswalk_DOT_split.xlsx'))

# reading in housing data
housing_df = read.xlsx(paste0(working_dir, "/Compressed Housing Master 11-30-23.xlsx"))

# pulling census data
# census tract shape files in boston
VARS <- tidycensus::load_variables(dataset = 'acs5', year = 2020, cache = T)
acs2020_pop <- tidycensus::get_acs(geography = "tract", 
                                   variable = "B01001_001",
                                   output = "wide",
                                   state = "MA",
                                   county = "Suffolk",
                                   geometry = TRUE,
                                   year = 2022,
                                   cache_table = T,
                                   show_call = TRUE) %>% 
  filter(!(str_detect(NAME,"Census Tract 99")|str_detect(NAME,"Census Tract 18")
           |str_detect(NAME,"Census Tract 17")|str_detect(NAME,"Census Tract 16")
           |str_detect(NAME,"Census Tract 9812.01")|str_detect(NAME,"Census Tract 9801.01")))

# checking the projection coordinate
st_crs(acs2020_pop)

# merging nbhd crosswalk with the shape files
acs2020_pop = acs2020_pop %>%
  left_join(TRACT_20 %>%
              mutate(NAME = str_replace_all(Geographic.Area.Name, ",", ";")), by = 'NAME') %>%
  select(-c(ends_with("M"), Geographic.Area.Name))

# cleaning data from housing
housing_df_clean = housing_df %>%
  mutate(address = paste0(`St.Number`, " ", Street, " ", ifelse(is.na(`St.Suffix`), "", St.Suffix), ", Boston, MA"), # making address
         LAT = as.numeric(LAT), LONG = as.numeric(LONG), # converting lat/long to int
         Complete.Date = excel_numeric_to_date(as.numeric(Complete.Date))) %>% # making date column 
  filter(!((is.na(LAT) | is.na(LONG)) & (is.na(Street) | is.na(St.Number)))) %>%
  select(Project, address, LAT, LONG, Complete.Date, New.SF, `0BR`, `1BR`, `2BR`, `3+BR`, Current.Status, Reporting.Category, New.Units) %>%
  as_tibble()
  
###### geocoding non-existent lat/long pairs ------------------------------------------------

# first filtering got correct reporting agency 
rep_cat <- c("G/N", "N")

housing_df_filt <- housing_df_clean %>%
  filter(Reporting.Category %in% rep_cat)

# number of rows to be geocoded
sum(is.na(housing_df_filt$LAT))

# this is the geocoding script (takes adresses and goes to lat / long)
housing_geocoded <- housing_df_filt[which(is.na(housing_df_filt$LAT) | is.na(housing_df_filt$LONG)),] %>%
  geocode(address, method = 'osm', lat = lat_new , long = long_new) %>%
  mutate(LAT = lat_new, 
         LONG = long_new) %>%
  select(-c(lat_new, long_new))

# I could also use call_geocoder_lation via tigris but this is vectorized

# adding back in the geocoded rows 
housing_df_filt[which(is.na(housing_df_filt$LAT)),] <- housing_geocoded

# number of address not geocded (throwign errors)
sum(is.na(housing_df_filt$LAT))

housing_edit <- housing_df_filt[which(is.na(housing_df_filt$LAT)),]
# none of these are in the reporting category if interest though so we can ignore

# filtering out the ones that are still NA so those can be edited manually 
housing_df_filt <- housing_df_filt %>%
  filter(!(is.na(LAT) | is.na(LONG)))

###### pairing the lat/long with associated tracts ------------------------------------------------

housing_df_filt$bg20 <- NA

# looping over to get geoid
assign_bg20 <- function(df) {
  df[["bg20"]] <- NA
  
  for(i in 1:nrow(df)) {
    # getting geoid
    geoid <- location_getCensusBlock(latitude = df[["LAT"]][i], longitude = df[["LONG"]][i], censusYear =  2020)[3][[1]]
    
    # if unavailable, like iferror(/na)
    if(length(geoid) >= 1) {
      df[["bg20"]][i] = geoid
    }
  }
  
  return (df)
}

housing_df_bg <- assign_bg20(housing_df_filt)


# handling NAs -- geocoding new lat/long pairs for NA lat/long bgs
housing_edit[grepl("Showa", housing_edit$Project),]$address <- "420 Pond St, Boston, MA"
housing_edit[grepl("2 Northdale TE", housing_edit$Project),]$address <- "2 Northdale Terrace, Boston, MA"

#### addd in number needed to be manually fixed (as seperate output sheet)


# cleaning data for incorrect lat/long pairs 
housing_edit_geocoded <- rbind(housing_edit, housing_df_bg[which(is.na(housing_df_bg$bg20)), ]) %>%
  geocode(address, method = 'osm', lat = lat_new , long = long_new) %>%
  mutate(LAT = lat_new, LONG = long_new) %>%
  select(-c(lat_new, long_new))

# assigning bgs based on lat long
housing_edit_bg20 <- assign_bg20(housing_edit_geocoded)

# replacing the new rows with their corresponding old ones
housing_df_bg[(housing_df_bg$Project %in% housing_edit_bg20$Project) & 
                !(housing_df_bg$Project %in% housing_edit$Project), ] <- housing_edit_bg20[!(housing_edit_bg20$Project %in% housing_edit$Project), ]

# adding back in the edited one from earlier 
housing_all_bg <- housing_df_bg %>%
  rbind(housing_edit_bg20[(housing_edit_bg20$Project %in% housing_edit$Project), ])

###### assigning tracts to nbhds ------------------------------------------------

housing_nbhd <- housing_all_bg %>%
  mutate(tract20 = substr(bg20, 1, 11)) %>%
  left_join(TRACT_20 %>%
              mutate(tract20 = as.character(geoid20)) %>%
              select(!geoid20), by = "tract20")

housing_nbhd %>%
  mutate(hhtype_1 = ifelse(`0BR` == "-", 0, as.numeric(`0BR`)) + ifelse(`1BR` == "-", 0, as.numeric(`1BR`)), 
         hhtype_2 = ifelse(`2BR` == "-", 0, as.numeric(`2BR`)), 
         hhtype_3 = ifelse(`3+BR` == "-", 0, as.numeric(`3+BR`)), 
         New.Units = ifelse(New.Units == "-", 0, as.numeric(New.Units)), 
         New.SF = ifelse(New.SF == "-", 0, as.numeric(New.SF))) %>%
  group_by(nbhd) %>%
  summarize(n = sum(New.Units))

df %>% mutate_at(c('col1', 'col2'), as.numeric)
