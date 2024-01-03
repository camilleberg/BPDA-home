
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
}

# reading in crosswalk files
box_dir = "C:/Users/camilleb/Box/Research/Active Projects/Neighborhood Projection_2023/03. Data/Source Data/Development (Completions) Data"
working_dir = "C:/Users/camilleb/Documents/GitHub/BPDA-home/nbhd pop projection"

TRACT_20 = read.xlsx(paste0(box_dir, '/tract20_nbhd20_crosswalk_DOT_split.xlsx'))

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
  mutate(address = paste(`St.Number`, Street, `St.Suffix`), # making address
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
housing_geocoded <- housing_df_filt[which(is.na(housing_df_filt$LAT)),] %>%
  geocode(address, method = 'osm', lat = lat_new , long = long_new) %>%
  mutate(LAT = lat_new, 
         LONG = long_new) %>%
  select(-c(lat_new, long_new))

# I could also use call_geocoder_lation via tigris but this is vectorized

# adding back in the geocoded rows 
housing_df_filt[which(is.na(housing_df_filt$LAT)),] <- housing_geocoded

# number of address not geocded (throwign errors)
sum(is.na(housing_df_filt$LAT))

housing_df_filt[which(is.na(housing_df_filt$LAT)),]
# none of these are in the reporting category if interest though so we can ignore

###### pairing the lat/long with shapefiles  ------------------------------------------------

# finding census tract from lat lomg


# looping over to get geoid
for(i in 1:nrow(housing_df_filt)) {
  housing_df_filt[i, 'geoid20'] = call_geolocator_latlon(housing_df_filt$LAT[i], housing_df_filt$LONG[i], vintage = "Census2020_Current")
  
  if(is.na(housing_df_filt[i, 'geoid20'])) {
    housing_df_filt[i, 'geoid20'] = call_geolocator(housing_df_filt[i, 'address'], city = "Boston", state = "MA")
  }
}

# handling NAs
housing_df_filt[which(is.na(housing_df_filt$geoid20)), ]

# trying to geolocat based on address
call_geolocator(housing_df_filt[which(is.na(housing_df_filt$geoid20)), ][1, 'address'], city = "Boston", state = "MA")

