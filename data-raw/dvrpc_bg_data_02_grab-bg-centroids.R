library(tidyverse)
library(sf)

# using census block group tiger line datasets downloaded from: https://www2.census.gov/geo/tiger/TIGER2019/BG/
nj_bounds <- read_sf("~/GIS DataBase/DVRPC_bgs/tl_2019_34_bg.shp")
pa_bounds <- read_sf("~/GIS DataBase/DVRPC_bgs/tl_2019_42_bg.shp")

# for this, all we need are centroids / interior points
centroid_cols <- bind_rows(nj_bounds, pa_bounds) %>%
  st_drop_geometry() %>%
  select(GEOID, INTPTLAT, INTPTLON)

# extract them, read as coordinates
centroid_cols %>%
  mutate(across(INTPTLAT:INTPTLON, as.numeric)) %>%
  st_as_sf(coords = c(3, 2), crs = st_crs(nj_bounds)) %>%
  write_rds("Data/Inputs/pa_nj_centroids.rds", compress = "gz")
