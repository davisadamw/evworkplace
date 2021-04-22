#' Title
#'
#' @param ev_tots
#'
#' @return
#'
#' @examples sum(1)
assign_workcharge <- function(ev_tots) {

}


#' Assign EVs to commute routes
#'
#' @param ev_totals Dataset containing home block groups and one or more columns of EV counts.
#' @param ... Unquoted names of variables in `ev_totals` containing number of EVs in a block group
#' @param .ev_commute_frac What fraction of EVs are used for commuting? Default = 60%
#'
#' @return A dataset with one row for each home-work pair of block groups in the study area.
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
    dplyr::mutate(dplyr::across(-Home_BlkGrp, ~ . * .ev_commute_frac))

  # attach commute data and assign commute vehicles to job locations in proportion to total jobs
  ev_totals_only %>%
    dplyr::left_join(dvrpc_od, by = "Home_BlkGrp") %>%
    dplyr::with_groups(.data$Home_BlkGrp, dplyr::mutate, job_frac = total_Jobs / sum(total_Jobs)) %>%
    dplyr::select(.data$Home_BlkGrp, .data$Work_BlkGrp, .data$Distance, .data$job_frac, ...) %>%
    dplyr::mutate(dplyr::across(-c(1:4), ~ . * .data$job_frac))
}
