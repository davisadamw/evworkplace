library(tidyverse)

# pull relevant columns from old input
# not all columns could or should be recomputed for this run
old_inputs_full <- read_rds("Data/from_old_version/dvrpc_inputs_2019_nogeom.rds")

# required columns are:
# ... identifiers (state / geoid)
# ... commute variables
# ... CurrentPEVsales (will be used as starting values unless anything newer is available)
# ... MarketLimit??? (might recompute based on #vehicles, depending on available data)
old_inputs_full %>% 
  select(STATEFP, GEOID, WgtAvgHOVShare_Mi, WgtAvgCmtDst_Mi, MarketLimit, CurrentPEVsales) %>% 
  write_rds("Data/Inputs/commute_ml_pev_from_old.rds", compress = "gz")
