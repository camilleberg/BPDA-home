{
library(tidyverse)
library(leaflet)
library(leafpop)
library(mapview)
library(sf)
}

rm(list = ls())

library(viridis) 
pal_help <- colorFactor(viridis(7), dat$extra_MC)

dat = read_csv('C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Raw Data/geoid_comp.csv')
dat1 <- read_csv("C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Raw Data/2010tracts_MA.csv")

df_ct2020_MC = read_csv('C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Raw Data/df_ct2020_MC.csv')
df_ct2010_MC = read_csv('C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Raw Data/df_ct2010_MC.csv')

# 2020 tracts in Boston not in MC
dat %>%
  left_join(df_ct2020_MC %>% mutate(GEOID = as.numeric(GEOID)) %>% rename(idx = `...1`), by = 'GEOID') %>%
  filter(!is.na(idx)) %>%
  st_as_sf(wkt = 'geometry') %>%
  leaflet() %>%
  addTiles()  %>%
  addProviderTiles(provider = "CartoDB.Positron")  %>%
  addPolygons(
    stroke = F
  ) 

dat1 %>%
  left_join(df_ct2010_MC %>% mutate(GEOID10 = as.numeric(GEOID)) %>% rename(idx = `...1`), by = 'GEOID10') %>%
  filter(!is.na(idx)) %>%
  st_as_sf(wkt = 'geometry') %>%
  leaflet() %>%
  addTiles()  %>%
  addProviderTiles(provider = "CartoDB.Positron")  %>%
  addPolygons(
    stroke = F
  ) 
