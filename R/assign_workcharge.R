#' Create work charging events and demand
#'
#' Create work charging events and demand for a given EV adoption and charging scenario
#'
#' @param prepped_data EV commute counts produced by \code{\link{prep_workcharge}}
#' @param commute_ev_col Unquoted name of column in `prepped_data` that contains the EV count for this scenario
#' @param charging_probs Work charging probabilities produced by \code{\link{make_workcharge_probs}}
#' @param commute_miles_frac What fraction of total travel is for commute?
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
#'   assign_workcharge(bevs_20k, bev_chargeprob)
#'
#' ev_workplace_demand_phev10 <- ev_commuter_count %>%
#'   assign_workcharge(bevs_10k, phev_chargeprob)
assign_workcharge <- function(prepped_data,
                              commute_ev_col,
                              charging_probs,
                              commute_miles_frac = 0.5,
                              miles_per_kWh = 4) {

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
    dplyr::mutate(dplyr::across(-c(1:4), ~ . * .data$job_frac))
}


#' Create charging probabilities
#'
#' This function subsets the included charging probabilities to create a single charging scenario.
#'
#' Note: All fractions should be kept between 0-1, but the vehicle range fractions can also be expressed as relative weights.
#'
#' @param use_BEV Use BEV charging probabilities? Otherwise use PHEV probabilities.
#' @param single_family_home_frac Fraction of all electric vehicles owned by households in single-family homes
#' @param sfh_homecharge Home charging availability in single-family homes, keep it between 0-1
#' @param mud_homecharge Home charging availability in multi-unit dwellings, keep it between 0-1
#' @param work_free Is charging free at commute destinations? `TRUE` or `FALSE`
#' @param rate_paid Electrical rate paid for home charging, keep it between 10-25 cents/kWh
#' @param long_range_frac Fraction of long range vehicles, BEV range 220+ miles, PHEV range 80+ miles
#' @param medium_range_frac Fraction of medium range vehicles, BEV range 140 miles, PHEV range 35 miles
#' @param short_range_frac Fraction of short range vehicles, BEV range 75 miles, PHEV range 15 miles
#'
#' @return A data frame of charging probabilities to use in \code{\link{assign_workcharge}}
#' @export
#'
#' @examples
#' # Make a set of charging probabilities each for BEVs and PHEVs using the default values
#' bev_chargeprob  <- make_workcharge_probs()
#' phev_chargeprob <- make_workcharge_probs()
#'
#' # create a set of probabilities for free work charging
#' bev_chargeprob_wfree <- make_workcharge_probs(work_free = TRUE)
make_workcharge_probs <- function(use_BEV = TRUE,
                                  single_family_home_frac = 0.9,
                                  sfh_homecharge = 0.9,
                                  mud_homecharge = 0.2,
                                  work_free = FALSE,
                                  rate_paid = 12.97,
                                  long_range_frac   = 0.5,
                                  medium_range_frac = 0.4,
                                  short_range_frac  = 0.1) {
  # first, check all arguments for the appropriate range
  if(min(single_family_home_frac, sfh_homecharge, mud_homecharge) < 0 |
     max(single_family_home_frac, sfh_homecharge, mud_homecharge) > 1) {
    stop("single_family_home_frac, sfh_homecharge, and mud_homecharge ",
         "must be fractions between 0 and 1")
  }
  if(min(long_range_frac, medium_range_frac, short_range_frac) < 0) {
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
  if(!is.logical(work_free)) stop("work_free should be either TRUE for free or FALSE for costly")

  # grab the appropriate starting dataset and cut it down to potential values
  if (isTRUE(use_BEV)) all_probs <- evworkplace::charge_probs_bev
  else all_probs <- evworkplace::charge_probs_phev

}
