library(tidyverse)
library(sf)
library(dbscan)

source("98_constants.R")

# what's the farthest we should look for neighbors? (higher distance -> takes longer and uses more memory ~ n^2)
max_distance_mi <- 20

# load data ####
 
# load block group starting ownership
ev_start <- read_rds("Data/Intermediates/ev_ownership_start_old_toolbox_nos.rds") %>% 
  select(GEOID, ev_frac)

# load block group centroids and grab projected coordinates
dvrpc_bg_cents <- read_rds("Data/Inputs/pa_nj_centroids.rds")

dvrpc_bg_cents_utm <- dvrpc_bg_cents %>% 
  # remove block groups outside of DVRPC region
  semi_join(ev_start, by = "GEOID") %>% 
  # reproject to whatever coordinate system we're gonna use
  st_transform(dvrpc_crs) %>% 
  # attach coordinate columns (XY in meters)
  bind_cols(as_tibble(st_coordinates(.))) %>% 
  # and drop the explicit spatial info (all we need are coords in meters)
  st_drop_geometry()

# identify neighbors ####

# to compute Inverse Distance Weighted ev ownership densities, we need to identify all neighbors (within 20 miles in this case)
# the starting data structure here is a fixed radius neighbors object with two lists we care about:
# ... neighbs_20mi$id a list with one entry per row in dvrpc_bg_cents_utm
# ... ... each entry in this list is a vector with the indices of block groups within 20mi of that block group
# ... neighbs_20mi$dist has the same structure and stores distances (in miles)
neighbs_20mi <- frNN(select(dvrpc_bg_cents_utm, X, Y), 1609 * max_distance_mi)

# convert fixed radius neighbors dataset into data frame
# ... the purrr package is great for converting weird nested data structures
# start by making a dataset with the original GEOIDs and two list columns
neighbs_tbl <- tibble(GEOID      = dvrpc_bg_cents_utm$GEOID,
                      index_t    = neighbs_20mi$id,
                      distance_m = neighbs_20mi$dist) %>% 
  # expand the list columns into rows
  unnest(c(index_t, distance_m)) %>% 
  # get the geoids corresponding to the indices
  mutate(GEOID_t = dvrpc_bg_cents_utm$GEOID[index_t],
         .after = GEOID, .keep = "unused")

# compute weighted adoption rate from neighbors ####

# function for computing a weighted mean (as long as there are no NAs)
wmean <- function(x, w) sum(x * w) / sum(w)

local_adoption_rate <- neighbs_tbl %>% 
  # compute weights
  mutate(wt = 1 / distance_m) %>% 
  # attach EV ownership
  left_join(ev_start, by = "GEOID") %>% 
  # compute average adoption rate, weighted by inverse distance
  with_groups(GEOID, summarize,
              nb_ev_ownership = wmean(ev_frac, wt))

# save neighborhood adoption rates
local_adoption_rate %>% 
  write_rds('Data/Intermediates/dvrpc_bg_neighborvals.rds', compress = "gz")



