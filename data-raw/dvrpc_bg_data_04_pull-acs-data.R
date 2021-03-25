library(tidyverse)
library(tidycensus)

# you will need to register for your own census api key here: https://api.census.gov/data/key_signup.html
# and add it to R using tidycensus::census_api_key(<your key>, install = TRUE)

# assign constants (just states for now)
states <- c("PA", "NJ")

# start by loading a table of variables and a couple useful functions (may take a little while the first time you run it on your computer)
# load variable names for all ACS 5 year tables ... this may take a while the first time you run it on a computer
cached_acs5 <- load_variables(year = 2018, dataset = 'acs5', cache = TRUE)

# cached_acs5 created above, tbl_name is everything to the left of the _ in the first column of cached_acs5
# var_cols contains the new columns the data is broken into, typically two or three levels
extract_variables <- function(cached_acs5, tbl_name, var_cols) {
  rep_na_vals <- set_names(rep("total", length(var_cols)), var_cols)
  cached_acs5 %>%
    filter(str_starts(name, tbl_name)) %>%
    select(-concept) %>%
    separate(label,
             into = c(NA, NA, var_cols),
             sep = "!!",
             fill = "right") %>%
    replace_na(as.list(rep_na_vals)) %>%
    rename(variable = name)
}

get_values <- function(table, variable_info, state, geography = "tract") {
  get_acs(geography = geography,
          state = state,
          table = table,
          cache_table = TRUE) %>%
    select(-NAME, -moe) %>%
    inner_join(variable_info, by = "variable")
}




# housing units by type ####
housing_table  <- "B25032"

# identify the variables we want
housing_varnames <- cached_acs5 %>%
  extract_variables(housing_table, c("owner_renter", "housing_type")) %>%
  filter(housing_type != "total")

# and attach the variable names, drop TOTALS rows ... result has one row per block group x variable extracted
housing_values <- get_values(housing_table, housing_varnames, state = states, geography = "block group")

# simplify housing types into single attached, single detached, mobile/other, and apartment
# partial table of house types ... all others will be apartments
htypes <- tribble(~house_type_general, ~housing_type,
                  'single_attached',   '1, detached',
                  'single_detached',   '1, attached',
                  'mobile_home',       'Mobile home',
                  'mobile_home',       'Boat RV Van etc.')

housing_simplified <- housing_values %>%
  # attach simplified housing categories
  left_join(htypes, by = 'housing_type') %>%
  mutate(house_type_general = replace_na(house_type_general, "apartment")) %>%
  # get total number of housing units of each type
  with_groups(c(GEOID, house_type_general), summarize, tot_units = sum(estimate)) %>%
  # convert to wide format, one row per block group
  pivot_wider(names_from  = house_type_general,
              values_from = tot_units)


# median income ####
# median income is missing for many block groups, use tract-level values as override
# the income table only has one variable
income_varnames <- tibble(variable = "B19113_001", name = "median_income")

bg_income <- get_values('B19113', income_varnames, state = states, geography = "block group")

tr_income <- get_values('B19113', income_varnames, state = states, geography = "tract") %>%
  select(GEOID_tr = GEOID, estimate_tr = estimate)

# attach tract income to block groups to fix NAs
bg_income_combined <- bg_income %>%
  mutate(GEOID_tr = str_sub(GEOID, end = -2)) %>%
  left_join(tr_income, by = 'GEOID_tr') %>%
  mutate(med_income = if_else(is.na(estimate), estimate_tr, estimate)) %>%
  select(GEOID, med_income)


# total vehicles available ####
# also total vehicles available, at census tract level
# ... these will be combined with EV counts and allocated to block groups
vehicle_table <- 'B08201'

# this table has totals by number of vehicles available AND broken down by household size ... we only want vehicles available
vehicles_varnames <- extract_variables(cached_acs5, vehicle_table, var_cols = c("veh_or_hh", "veh")) %>%
  filter(str_detect(veh_or_hh, "vehicle"),
         veh == "total")

vehicles_values <- get_values(vehicle_table, vehicles_varnames, state = states, geography = "tract")

# table has number of households in each vehicle category ... convert to just total vehicles in census tract

# number of vehicles in household starts with "No vehicle available", then goes "1 vehicle available" up to "4 or more vehicles available"
# for simplicity, we're going to say "4 or more vehicles" means 5 to (hopefully) account for the long tail
# this function will convert a vector of characters to integers silently, with 0 as the default value (e.g., if the character is "N)
as_integer_silent <- function(x, default = 0L) {
  int_start <- quietly(as.integer)(x)

  replace_na(int_start$result, default)
}


tract_total_vehicles <- vehicles_values %>%
  # extract number of vehicles in each housing type
  mutate(veh_char1 = str_sub(veh_or_hh, 1, 1),
         hh_vehs   = if_else(veh_char1 == "4", 5L, as_integer_silent(veh_char1))) %>%
  # compute total numbers of households and vehicles in each tract
  with_groups(GEOID, summarize, households = sum(estimate), vehicles = sum(estimate * hh_vehs)) %>%
  mutate(vehs_per_hh = vehicles / households)

# to convert to block group numbers, we need number of households in each block group
bg_hhs <- get_acs("block group", variable = "B11001_001", state = states) %>%
  select(GEOID, households = estimate)

bg_total_vehicles <- bg_hhs %>%
  mutate(GEOID_tr = str_sub(GEOID, end = -2)) %>%
  left_join(select(tract_total_vehicles, GEOID, vehs_per_hh), by = c("GEOID_tr" = "GEOID")) %>%
  mutate(vehicles = vehs_per_hh * households) %>%
  select(GEOID, households, vehicles)

# tract vehicle totals will be kept separately for better estimate of starting EV%
tract_total_vehicles %>%
  write_rds("Data/Inputs/pa_nj_tr_meanvehs.rds", compress = "gz")

# combine all block group variables grabbed from acs and store
bg_total_vehicles %>%
  left_join(bg_income_combined, by = "GEOID") %>%
  left_join(housing_simplified, by = "GEOID") %>%
  write_rds("Data/Inputs/pa_nj_bg_data_from_acs.rds", compress = "gz")

