library(tidyverse)

# NOTE: this part will need to be changed a little to incorporate new EV ownership data

# load starting vehicle totals and ev ownership
all_vehs_start <- read_rds("Data/Inputs/pa_nj_bg_data_from_acs.rds") %>% 
  select(GEOID, vehicles)

ev_start <- read_rds("Data/Inputs/commute_ml_pev_from_old.rds") %>% 
  select(GEOID, evs_start = CurrentPEVsales)

# combine totals to get starting market penetration, remove block groups with very small numbers of vehicles
# this removes 30 small block groups
ev_frac_start <- all_vehs_start %>% 
  inner_join(ev_start, by = "GEOID") %>% 
  mutate(ev_frac = evs_start / vehicles) %>% 
  filter(vehicles >= 15)

ev_frac_start %>% 
  write_rds("Data/Intermediates/ev_ownership_start_old_toolbox_nos.rds", compress = "gz")
