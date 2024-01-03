# new carto comparison script
{
  library(readxl)
  library(tidyverse)
  library(plotly)
  library(cluster)
  library(dbscan)
  }


rm(list = ls())

# reading in carto data 
carto <- read_xlsx("C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Request/Troubleshooting/tract_weekly_comp_2023.09.05.xlsx")
MC <- read_xlsx("C:/Users/camilleb/Box/Research/Active Projects/Mastercard/Request/Troubleshooting/tract_weekly_comp_MC_2023.09.05.xlsx")

crosswalk <- read_delim('https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_st25.txt', delim = "|")

boston_tracts <- read_csv("C:/Users/camilleb/Documents/census/Census_R_Tool/Output/B01001_acs5_2021_tract20_boston.csv")

crosswalk_boston <- left_join(crosswalk, boston_tracts %>% mutate(GEOID_TRACT_20 = as.double(GEOID)) %>%
                                select(GEOID_TRACT_20, tract20, tract20_nbhd), by = 'GEOID_TRACT_20') %>%
  filter(!is.na(tract20_nbhd))

# making merge file with just values and geoids
df <- full_join(carto %>%
                  filter(industry == "ret") %>%
             mutate(tract_id = geoid) %>%
               mutate(do_date = as.Date(do_date)) %>%
             select(tract_id, do_date, txn_amt, txn_cnt), 
           MC %>%
             filter(geo_type == 'Msa', 
                    industry == 'Total Retail') %>%
             mutate(do_date = as.Date(paste(wk, yr, 'Sun'), '%U %Y %a')) %>%
             select(tract_id, do_date, txn_amt, txn_cnt), by = c('do_date', 'tract_id'), suffix = c(".carto", ".MC"))


# this joins with 2010 and 202 tracts to check if there's any difference 
x <- left_join(df, crosswalk_boston %>%
              select(GEOID_TRACT_20, NAMELSAD_TRACT_20), by = c('tract_id'='GEOID_TRACT_20')) %>%
   left_join(crosswalk_boston %>% 
               mutate(GEOID_TRACT_10 = as.double(GEOID_TRACT_10)) %>% 
               select(GEOID_TRACT_10, NAMELSAD_TRACT_10), 
             by = c('tract_id'='GEOID_TRACT_10')) %>%
  filter_at(vars(NAMELSAD_TRACT_20,NAMELSAD_TRACT_10),all_vars(!is.na(.)))

# the tracts are the samme so it's all good
length(x$NAMELSAD_TRACT_10 == x$NAMELSAD_TRACT_20) == nrow(x)

# plotting the diff for txn
x %>%
  ggplot() +
  geom_point(aes(x = txn_amt.MC, y = txn_amt.carto), color = "blue") +
  geom_abline(intercept = 0, slope = 1) +
  geom_ribbon(aes(aes()))
  labs(y = "Carto Index", x = "Mastercard Index") 
  
x %>%
  plot_ly() %>%
  add_trace(x= ~txn_amt.MC, 
          y = ~ txn_amt.carto, name = 'Amount') %>%
  add_trace(x= ~txn_cnt.MC, 
            y = ~ txn_cnt.carto, name = 'Count')%>%
  add_trace(x = ~txn_amt.MC, 
            y = ~txn_amt.MC, mode = 'lines', name = "Line of equivalence")

# it seems like there's three/four groups of comparisons between carto and MAstercard 
x<- x %>%
  mutate(amt_comp = txn_amt.carto/txn_amt.MC, 
         cnt_comp = txn_cnt.carto/txn_cnt.carto)

x.model <-  kmeans(x %>% select(txn_amt.carto, txn_amt.MC) %>% drop_na(), 4)
clusplot(x %>% select(txn_amt.carto, txn_amt.MC) %>% drop_na(), x.model$cluster)


## SVM model
x_sample = x[sample(1:nrow(x), round(nrow(x)*0.1, 0)), ]

# refomratting data based on tutorial
x.lsv <- x_sample %>% mutate(txn_amt.carto = as.factor(txn_amt.carto)) %>% select(txn_amt.carto, txn_amt.MC)
x.svmfit <- svm(txn_amt.carto ~txn_amt.MC, data = x.lsv, kernal = "linear", scale = FALSE, probability = TRUE)
x.svmfit

saveRDS(x.svmfit, "x.svmfit.RDS")
