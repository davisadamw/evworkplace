library(tidyverse)

# load all datasets
vars_from_old <- read_rds("Data/Inputs/commute_ml_pev_from_old.rds") %>% 
  select(GEOID, starts_with("WgtAvg"))
htype_income  <- read_rds("Data/Inputs/pa_nj_bg_data_from_acs.rds") %>% select(-vehicles)
neighborhood  <- read_rds("Data/Intermediates/dvrpc_bg_neighborvals.rds")
vehicles_own  <- read_rds("Data/Intermediates/ev_ownership_start_old_toolbox_nos.rds")

# combine all datasets, remove NA rows, and store
list(vehicles_own, htype_income, vars_from_old, neighborhood) %>% 
  reduce(inner_join, by = "GEOID") %>% 
  drop_na() %>% 
  write_csv("Data/Model/dvrpc_bg_data_raw.csv") %>% 
  write_rds("Data/Model/dvrpc_bg_data_raw.rds", compress = "gz")
