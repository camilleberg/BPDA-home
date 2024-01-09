# new try because I"m stupid

# reading in data 


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
  library(sf)
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

output_file_date <- str_split(recent_housing_file, " ")[[1]][4]

permitted_df <- read.xlsx(paste0(working_dir, "/housing_monthly/", recent_housing_file)) %>%
  as_tibble() 
pipeline_df <- read.xlsx(paste0(working_dir, "/housing_monthly/", recent_housing_file), sheet = "Pipeline") %>%
  as_tibble()

# RUNNING FUNCTIONS #####3 ---------

source('data_process_fxns.R')

## processing the data

housing_df <- rbind(permitted_df, pipeline_df)

# first filtering got correct reporting agency (new units bascially)
rep_cat <- c("G/N", "N")

# assigning addressee to entire df and creating address column 
housing_df <- rbind(permitted_df %>% assign_address() %>% mutate(df = "permitted"), 
      pipeline_df %>% mutate(Street = Street.Name) %>% assign_address() %>% mutate(df = "pipeline")) %>%
      filter(Reporting.Category %in% rep_cat)

# assigning new lat longs
housing_xy <- housing_df %>% assign_lat_long()

# manually cleaning
housing_edit <- housing_xy[which(is.na(housing_xy$LAT)),]
housing_edit[grepl("Showa", housing_edit$Project),]$address <- "420 Pond St, Boston, MA"
housing_edit[grepl("2 Northdale TE", housing_edit$Project),]$address <- "2 Northdale Terrace, Boston, MA"
housing_edit[grepl("Olmst", housing_edit$Project),]$address <- "618 Harvard St, Boston, MA"
  # this one isn't the actual address - just close enogugh to be in the same tract
housing_edit[grepl("Quincy & Warr", housing_edit$Project),]$address <- "436 Warren Street, Boston, MA"
  # this is one of the adresses
housing_edit[grepl("Welcome Home", housing_edit$Project),]$address <- "241 Geneva Ave, Boston, MA"
# this is based on their filed RFP - subject to change 
  # https://drive.google.com/drive/folders/17zgTd-Ok7z0Hlc1wolUaLbpVJyPegmhr
housing_edit[grepl("Dewey", housing_edit$Project),]$address <- "21 Danube St, Boston, MA"
# this is one of the adresses
housing_edit <- housing_edit %>%
  filter(Project != "Suffolk Downs Phase 4", # this is added later on 
         Project != "Safe Haven") # no available address

# geocoding the new address
housing_edit_xy <- housing_edit %>% assign_lat_long()

# joining back in to the original df and transforming to shapefile
housing_coords <- housing_xy %>%
  filter(!is.na(LAT)) %>%
  rbind(housing_edit_xy) %>%
  st_as_sf(coords=c("LONG","LAT"), crs=4269, remove=FALSE)  
  # this is the same projection as the  tract data
  
# pulling census data for polygons 
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
           |str_detect(NAME,"Census Tract 9812.01")|str_detect(NAME,"Census Tract 9801.01"))) %>%
  select(!ends_with("M"))

write_rds(acs2020_pop, paste0(working_dir, "boston_ct2020.RDS"))

# checking the projection coordinate
st_crs(acs2020_pop)

# joining the two spatial objects
housing_tracts <- st_join(housing_coords, acs2020_pop, left = TRUE) %>%
  left_join(TRACT_20 %>% mutate_all(as.character) %>% rename(GEOID = geoid20))

# writing out excel file with geocoded tracts
write_xlsx(housing_tracts, paste0(working_dir, "/housing_geocoded_", output_file_date))

# plotting! 
ggplot(acs2020_pop) +
  geom_sf() 


####### OUTPUT FOR NBHD PROJ ----------

# assigning sixe and bedroom variables to entire df 
housing_with_vars <- housing_tracts %>% assign_size() %>% assign_bdrm() %>%
  select(-c(geometry, GEOID, NAME, B01001_001E, ))

# last recently completed quarter
completed_date <- max(hosuing_with_vars$Complete.Date, na.rm = TRUE)
completed_quarter <- as.Date(cut(completed_date, "quarter"))-1
completed_quarter_filt <- c(format(completed_quarter, "%Y"), quarter(completed_quarter))

## 1. under construction (permitted but not yet completed) for all nbhds by bedroom
output_constr <- housing_with_vars %>%
  select(-df) %>%
  filter((is.na(Complete.Date) & !is.na(`Permit.#`))) %>%
  get_new_bdrm_dist() %>%
  full_join(NBHD) %>% arrange(nbhd)

## 2. Recent Full Quarter completions for all nbhds by bedrooom
output_quartely_compl <- housing_with_vars %>%
  filter(complete.year == completed_quarter_filt[1], 
         complete.quarter == completed_quarter_filt[2], )%>%
  get_new_bdrm_dist() %>%
  full_join(NBHD) %>% arrange(nbhd)

## 3. Forecast % numbers 
### 3.a. Small historical (< 50,000 sqft or < 50 new units) (2011+) completions by nbhd (completed)
output_small_hist <- housing_with_vars %>%
  filter(size == "small") %>%
  filter(!is.na(Complete.Date)) %>%
  group_by(nbhd) %>%
  summarise(new_units = sum(New.Units))

### 3.b. Small (< 50,000 sqft or < 50 new units) permitted projects 
output_small_permit <- housing_with_vars %>%
  filter(size == "small") %>%
  filter(is.na(Complete.Date) & !is.na(`Permit.#`)) %>%
  group_by(nbhd) %>%
  summarise(new_units = sum(New.Units))

### 3.c. large projects in pipeline and not not yet permitted
output_large_pipeline <- housing_with_vars %>%
  filter(size == "large") %>%
  filter(df == "pipeline" & is.na(`Permit.#`)) %>%
  group_by(nbhd) %>%
  summarise(new_units = sum(New.Units))

### 3.d. making one sheet
output_forecast <- output_small_hist %>% rename(sm_hist = new_units) %>%
  left_join(output_small_permit %>% rename(sm_permit = new_units)) %>%
  left_join(output_large_pipeline %>% rename(lg_pipeline = new_units)) %>%
  mutate_at(c('sm_hist','sm_permit', 'lg_pipeline'), ~tidyr::replace_na(.,0)) %>%
  mutate(sm = sm_hist + sm_hist, 
         forecast_num = sm * 0.28 + lg_pipeline * 0.72, 
         all = sum(forecast_num), 
         forecast_share = forecast_num / all) %>%
  select(nbhd, sm, lg_pipeline, forecast_num, forecast_share)

# writing out to one excel sheet 

  file_name <- paste0(working_dir, "/output_using_", output_file_date)

  write.xlsx("dkfhkjdfh", file = file_name, sheetName = "Explanation")
  write.xlsx(output_constr, file = file_name, sheetName = "Under Construction", append = TRUE)
  write.xlsx(output_quartely_compl, file = file_name, 
             sheetName = paste0("Quarterly Completions ", completed_quarter_filt[1], " Q", 
                                completed_quarter_filt[2]), append = TRUE)
  write.xlsx(output_forecast, file = file_name, sheetName = "Forecast %", append = TRUE)

