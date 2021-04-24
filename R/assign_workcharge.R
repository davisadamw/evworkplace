#' Create work charging events and demand
#'
#' Create work charging events and demand for a given EV adoption and charging scenario
#'
#' @param prepped_data EV commute counts produced by \code{\link{prep_workcharge}}
#' @param commute_ev_col Unquoted name of column in `prepped_data` that contains the EV count for this scenario
#' @param charging_probs Work charging probabilities produced by \code{\link{make_workcharge_probs}}
#' @param scenario_name Text name of the combined adoption-charging scenario, used as prefix for column names in the output
#' @param extra_daily_miles Non-commute travel per day, miles
#' @param miles_per_kWh Vehicle miles per kilowatt hour
#'
#' @return A data frame of work charging events and kWh for the scenario in each block group
#' @export
#'
#' @examples
#' # First, create a work charging dataset using only 50% of EVs for commuting:
#' ev_commuter_count <- ev_totals_sample %>%
#'   prep_workcharge(bevs_20k, phevs_10k, .ev_commute_frac = 0.5)
#'
#' # create a charging scenario each for BEVs and PHEVs
#' bev_chargeprob  <- make_workcharge_probs(use_BEV = TRUE)
#' phev_chargeprob <- make_workcharge_probs(use_BEV = FALSE)
#'
#' # run the workplace charging calculator
#' ev_workplace_demand_bev20 <- ev_commuter_count %>%
#'   assign_workcharge(bevs_20k, bev_chargeprob, "bev20k_basic")
#'
#' ev_workplace_demand_phev10 <- ev_commuter_count %>%
#'   assign_workcharge(phevs_10k, phev_chargeprob, "phev10k_basic")
assign_workcharge <- function(prepped_data,
                              commute_ev_col,
                              charging_probs,
                              scenario_name,
                              extra_daily_miles = 10,
                              miles_per_kWh = 4) {

  # use approxfun to make functions which identify the nearest higher and lower computed distance
  dist_lo_fun <- stats::approxfun(charging_probs$cmtdist,
                                  charging_probs$cmtdist,
                                  method = "constant", rule = 2, f = 0)
  dist_hi_fun <- stats::approxfun(charging_probs$cmtdist,
                                  charging_probs$cmtdist,
                                  method = "constant", rule = 2, f = 1)

  # grab only the columns that matter: work location, distance, and number of EVs
  per_vehicle_info <- prepped_data %>%
    dplyr::select(.data$Work_BlkGrp, .data$Distance, vehs = {{ commute_ev_col }}) %>%
    dplyr::mutate(probs_dist_lo = dist_lo_fun(.data$Distance),
                  probs_dist_hi = dist_hi_fun(.data$Distance),
                  # compute the weight for the lower value to use for interpolation
                  weight_lo = (.data$probs_dist_hi - .data$Distance) /
                    (.data$probs_dist_hi - .data$probs_dist_lo),
                  # when distance is out of range, weight_lo will be 0/0=NaN, so fix to 1
                  weight_lo = dplyr::if_else(
                    .data$probs_dist_lo == .data$probs_dist_hi, 1, .data$weight_lo)) %>%
    # attach the appropriate charging probabilities
    dplyr::left_join(charging_probs, by = c("probs_dist_lo" = "cmtdist")) %>%
    dplyr::left_join(charging_probs, by = c("probs_dist_hi" = "cmtdist"),
                     suffix = c("_lo", "_hi")) %>%
    # use the weight to interpolate the event values between the commute distances
    dplyr::mutate(tot_events = .data$tot_events_lo * .data$weight_lo +
                    .data$tot_events_hi * (1 - .data$weight_lo),
                  work_events = .data$work_events_lo * .data$weight_lo +
                    .data$work_events_hi * (1 - .data$weight_lo),
                  .keep = "unused") %>%
    # compute daily miles and miles per event
    dplyr::mutate(daily_miles     = .data$Distance + extra_daily_miles,
                  daily_kWh       = .data$daily_miles / miles_per_kWh,
                  kWh_per_event   = .data$daily_kWh / .data$tot_events,
                  daily_work_kWh  = .data$work_events * .data$kWh_per_event) %>%
    # scale everything by number of vehicles
    dplyr::mutate(work_charging_events = .data$vehs * .data$work_events,
                  work_charging_kWh    = .data$vehs * .data$daily_work_kWh)

  per_vehicle_info %>%
    # # and sum up per work block group
    dplyr::with_groups(.data$Work_BlkGrp, dplyr::summarise,
                       work_vehicles = sum(.data$vehs),
                       work_events   = sum(.data$work_charging_events),
                       work_kWh      = sum(.data$work_charging_kWh)) %>%
    # add scenario name prefix to the new columns
    dplyr::rename_with(~ paste(scenario_name, .x, sep = "_"), -1)

}


#' Assign EVs to commute routes
#'
#' @param ev_totals Dataset containing home block groups and one or more columns of EV counts.
#' @param ... Unquoted names of variables in `ev_totals` containing number of EVs in a block group
#' @param .ev_commute_frac What fraction of EVs are used for commuting? Default = 60%
#'
#' @return A dataset with one row for each home-work pair of block groups in the study area, probably for use in \code{\link{assign_workcharge}}
#' @export
#'
#' @examples
#' # Take an output from the assign_evs() function and assign to commute destinations:
#' ev_totals_sample %>%
#'   prep_workcharge(bevs_20k, phevs_10k)
#'
#' # Run it again, using only 50% of EVs for commuting:
#' ev_totals_sample %>%
#'   prep_workcharge(bevs_20k, phevs_10k, .ev_commute_frac = 0.5)
prep_workcharge <- function(ev_totals, ..., .ev_commute_frac = 0.6) {
  # First, grab only the home bg identifier and any EV columns that are specified
  # then, adjust vehicle total by commute fraction
  ev_totals_only <- ev_totals %>%
    dplyr::select(Home_BlkGrp = .data$GEOID, ...) %>%
    dplyr::mutate(dplyr::across(-.data$Home_BlkGrp, ~ . * .ev_commute_frac))

  # attach commute data and assign commute vehicles to job locations in proportion to total jobs
  ev_totals_only %>%
    dplyr::left_join(evworkplace::dvrpc_od, by = "Home_BlkGrp") %>%
    dplyr::with_groups(.data$Home_BlkGrp, dplyr::mutate, job_frac = .data$total_Jobs / sum(.data$total_Jobs)) %>%
    dplyr::select(.data$Home_BlkGrp, .data$Work_BlkGrp, .data$Distance, .data$job_frac, ...) %>%
    dplyr::mutate(dplyr::across(-c(1:4), ~ . * .data$job_frac), .keep = "unused")
}


#' Create charging probabilities
#'
#' This function subsets the included charging probabilities to create a single charging scenario.
#'
#' Note: All fractions should be kept between 0-1, but the vehicle range weights can also be expressed as any positive number
#'
#' @param use_BEV Use BEV charging probabilities? Otherwise use PHEV probabilities.
#' @param single_family_home_frac Fraction of all electric vehicles owned by households in single-family homes
#' @param sfh_homecharge Home charging availability in single-family homes, keep it between 0-1
#' @param mud_homecharge Home charging availability in multi-unit dwellings, keep it between 0-1
#' @param is_workcharge_free Is charging free at commute destinations? `TRUE` or `FALSE`
#' @param rate_paid Electrical rate paid for home charging, keep it between 10-25 cents/kWh
#' @param long_range_wt Share of long-range vehicles, BEV range 220+ miles, PHEV range 80+ miles
#' @param medium_range_wt Share of medium-range vehicles, BEV range 120 miles, PHEV range 35 miles
#' @param short_range_wt Share of short-range vehicles, BEV range 85 miles, PHEV range 20 miles
#'
#'   Fraction of short range vehicles, BEV range 75 miles, PHEV range 15 miles
#'
#' @return A data frame of charging probabilities per commute distance to use in \code{\link{assign_workcharge}}
#' @export
#'
#' @examples
#' # Make a set of charging probabilities each for BEVs and PHEVs using the default values
#' bev_chargeprob  <- make_workcharge_probs()
#' phev_chargeprob <- make_workcharge_probs(use_BEV = FALSE)
#'
#' # create a set of probabilities for free work charging
#' bev_chargeprob_wfree <- make_workcharge_probs(is_workcharge_free = TRUE)
make_workcharge_probs <- function(use_BEV = TRUE,
                                  single_family_home_frac = 0.9,
                                  sfh_homecharge = 0.9,
                                  mud_homecharge = 0.2,
                                  is_workcharge_free = FALSE,
                                  rate_paid = 12.97,
                                  long_range_wt   = 0.5,
                                  medium_range_wt = 0.4,
                                  short_range_wt  = 0.1) {
  # first, check all arguments for the appropriate range
  if(min(single_family_home_frac, sfh_homecharge, mud_homecharge) < 0 |
     max(single_family_home_frac, sfh_homecharge, mud_homecharge) > 1) {
    stop("single_family_home_frac, sfh_homecharge, and mud_homecharge ",
         "must be fractions between 0 and 1")
  }
  if(min(long_range_wt, medium_range_wt, short_range_wt) < 0) {
    stop("range fraction values must all be positive")
  }
  if(rate_paid < 10) {
    warning("Cost too low, 10 cents per kWh will be used instead")
    rate_paid <- 10
  }
  if(rate_paid > 25) {
    warning("Cost too high, 25 cents per kWh will be used instead")
    rate_paid <- 25
  }
  if(!is.logical(use_BEV)) stop("use_BEV should be either TRUE for BEV or FALSE for PHEV")
  if(!is.logical(is_workcharge_free)) stop("is_workcharge_free should be either TRUE for free or FALSE for costly")

  # convert the weights to fractions
  tot_rng_wt <- long_range_wt + medium_range_wt + short_range_wt
  lrw <- long_range_wt   / tot_rng_wt
  mrw <- medium_range_wt / tot_rng_wt
  srw <- short_range_wt  / tot_rng_wt

  # grab the appropriate starting dataset and cut it down to potential values ... account for basic range stuff here too
  if (isTRUE(use_BEV)) {
    range_cuts <- sort(unique(charge_probs_bev$actualrange))

    # use only the top three ranges for BEVs
    all_probs <- charge_probs_bev %>%
      dplyr::mutate(range_wt = dplyr::case_when(.data$actualrange == max(range_cuts) ~ lrw,
                                                .data$actualrange == range_cuts[3]   ~ mrw,
                                                .data$actualrange == range_cuts[2]   ~ srw,
                                                TRUE                                 ~ 0)) %>%
      dplyr::group_by(.data$location, .data$dwell_type, .data$cmtdist, .data$rate_type,
                      .data$work_free, .data$rate_paid) %>%
      dplyr::summarise(charge_prob = sum(.data$range_wt * .data$charge_prob),
                       .groups = "drop")
  } else {
    range_cuts <- sort(unique(charge_probs_phev$erange))

    # use the top two ranges for PHEVs and combine the next two to make the short range
    all_probs <- charge_probs_phev %>%
      dplyr::mutate(range_wt = dplyr::case_when(.data$erange == max(range_cuts) ~ lrw,
                                                .data$erange == range_cuts[4]   ~ mrw,
                                                .data$erange == range_cuts[3]   ~ srw / 2,
                                                .data$erange == range_cuts[2]   ~ srw / 2,
                                                TRUE                            ~ 0)) %>%
      dplyr::group_by(.data$location, .data$dwell_type, .data$cmtdist, .data$rate_type,
                      .data$work_free, .data$rate_paid) %>%
      dplyr::summarise(charge_prob = sum(.data$range_wt * .data$charge_prob),
                       .groups = "drop")
  }

  # subset charging locations
  probs_simplified_location <- all_probs %>%
    # drop no-charge days
    # for now, we're assuming everyone's on the middle rate category
    # also grab only the appropriate work free / cost category
    # also grab only single family detached and mud
    dplyr::filter(.data$rate_type == 2,
                  .data$location != 0,
                  .data$work_free == is_workcharge_free,
                  .data$dwell_type == 1 | .data$dwell_type == 3) %>%
    # double count days where multiple charging locations used for total events
    # home onlys will get switched to work for people without home charging
    # count 70% of multi-location work charge events as work-home or work-public (the rest are home-public)
    dplyr::mutate(all_events  = dplyr::if_else(.data$location == 4, 2 * .data$charge_prob, .data$charge_prob),
                  home_onlys  = (.data$location == 1) * .data$charge_prob,
                  work_events = dplyr::case_when(.data$location == 2 ~ .data$charge_prob,
                                                 .data$location == 4 ~ 0.7 * .data$charge_prob,
                                                 TRUE                ~ 0)) %>%
    dplyr::group_by(.data$dwell_type, .data$cmtdist, .data$rate_paid) %>%
    dplyr::summarize(tot_events  = sum(.data$all_events),
                     home_onlys  = sum(.data$home_onlys),
                     work_events = sum(.data$work_events),
                     .groups = "drop")

  # compute weights for electricity rate by interpolating two nearest values
  all_rates <- unique(probs_simplified_location$rate_paid)
  if (rate_paid > max(all_rates)) {

    # if rate_used is above max, just use max
    probs_simplified_rate <- probs_simplified_location %>%
      dplyr::filter(.data$rate_paid == max(all_rates)) %>%
      dplyr::select(-.data$rate_paid)

  } else if (rate_paid < min(all_rates)) {

    # if rate_used is below min, just use min
    probs_simplified_rate <- probs_simplified_location %>%
      dplyr::filter(.data$rate_paid == min(all_rates)) %>%
      dplyr::select(-.data$rate_paid)

  } else if (rate_paid %in% all_rates) {

    # if rate_used is in the computed data, just use that
    rp <- rate_paid
    probs_simplified_rate <- probs_simplified_location %>%
      dplyr::filter(.data$rate_paid == rp) %>%
      dplyr::select(-.data$rate_paid)

  } else {

    # otherwise linearly interpolate between the two nearest computed rates
    lo_rate <- max(all_rates[all_rates < rate_paid])
    hi_rate <- min(all_rates[all_rates > rate_paid])
    lo_wt   <- (hi_rate - rate_paid) / (hi_rate - lo_rate)

    probs_simplified_rate <- probs_simplified_location %>%
      dplyr::mutate(rate_weight = dplyr::case_when(
        .data$rate_paid == lo_rate ~ lo_wt,
        .data$rate_paid == hi_rate ~ 1 - lo_wt,
        TRUE                       ~ 0)) %>%
      dplyr::group_by(.data$dwell_type, .data$cmtdist) %>%
      dplyr::summarise(tot_events  = sum(.data$tot_events  * .data$rate_weight),
                       home_onlys  = sum(.data$home_onlys  * .data$rate_weight),
                       work_events = sum(.data$work_events * .data$rate_weight),
                       .groups = "drop")

  }

  probs_simplified_rate %>%
    # apply home charge rate by housing type (move home-only days to work)
    # separate home charging access rates by dwell type
    # then apply single family home % weighting
    dplyr::mutate(work_events_combi = .data$work_events +
                    dplyr::if_else(.data$dwell_type == 1,
                                   .data$home_onlys * (1 - sfh_homecharge),
                                   .data$home_onlys * (1 - mud_homecharge)),
                  dwell_wt =
                    dplyr::if_else(.data$dwell_type == 1,
                                   single_family_home_frac,
                                   1 - single_family_home_frac),
                  tot_events_wtd  = .data$tot_events * .data$dwell_wt,
                  work_events_wtd = .data$work_events_combi * .data$dwell_wt) %>%
    dplyr::group_by(.data$cmtdist) %>%
    dplyr::summarise(tot_events = sum(.data$tot_events_wtd),
                     work_events = sum(.data$work_events_wtd),
                     .groups = "drop")

}
