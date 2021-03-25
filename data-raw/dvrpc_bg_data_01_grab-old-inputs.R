# NOTE: this code is included to document the workflow but relies on input files much too large to be included in this package
# as a result, code will not run as is

library(tidyverse)
library(sf)

# GDB with all inputs ... lots of redundant columns
dvrpc_old_ins <- "EV_Toolbox PA_NJ_DVRPC-20190110T172554Z-001/EV_Toolbox PA_NJ_DVRPC/DVRPC_input_output/New_Input_Market_DVRPC.gdb"

# the layer TotalPEVsLayer appears to have everything but the raw LODES data, which we won't modify for now
market_assignment_oldvars <- read_sf(dvrpc_old_ins, "TotalPEVsLayer")

# remove a couple unneeded columns and store in this directory in RDS format (and as csv without spatial data)
market_assignment_oldvars %>%
  select(STATEFP, GEOID, ALAND, AWATER, population:WgtAvgCmtDst_Mi, CurrentPEVsales:TotalPEVSales) %>%
  st_drop_geometry() %>%
  write_rds("Data/from_old_version/dvrpc_inputs_2019_nogeom.rds", compress = "gz") %>%
  write_csv("Data/from_old_version/dvrpc_inputs_2019_nogeom.csv")

# and save geometry too (though this can be gotten directly from census.gov if necessary)
market_assignment_oldvars %>%
  select(GEOID, Shape) %>%
  write_rds("Data/from_old_version/dvrpc_bg_bounds.rds", compress = "gz")

