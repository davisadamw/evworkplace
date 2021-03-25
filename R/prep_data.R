#' Prep block-group-level predictors of EV adoption.
#'
#' @param max_evs Maximum proportion of vehicles in any block group to be converted to EVs.
#'
#' @return A dataset of block group predictors that have been centered and scaled for use in the assignment model, defaults to 70%
#' @export
#'
#' @examples
#' prepped_data <- prep_bg_data(max_evs = 0.5)
prep_bg_data <- function(max_evs = 0.7) {
  # prep dataset for projection
  # ... this does two things:
  # ... ... computes market limit / makes sure all variables have the names the model expects them to
  # ... ... transforms all predictor variables to have mean 0 and sd 1
  evworkplace::dvrpc_bg_data %>%
    dplyr::rename(avg_commute = .data$WgtAvgCmtDst_Mi,
                  hov_share   = .data$WgtAvgHOVShare_Mi) %>%
    # first, compute the market limit and convert housing #'s to %s
    dplyr::mutate(market_limit    = .data$vehicles * max_evs,
                  housing_tot     = .data$single_attached + .data$single_detached + .data$apartment + .data$mobile_home,
                  single_det_frac = .data$single_detached / .data$housing_tot,
                  single_att_frac = .data$single_attached / .data$housing_tot,
                  apartment_frac  = .data$apartment / .data$housing_tot) %>%
    # data prep function in the package stocked
    # bump_ml=TRUE as a bug workaround ... doesn't actually affect anything in this case
    stocked::prep_data(id_cols = .data$GEOID,
                       targ_cols = .data$evs_start,
                       predictor_cols = c(.data$med_income,
                                          .data$single_att_frac, .data$single_det_frac, .data$apartment,
                                          .data$avg_commute, .data$hov_share,
                                          .data$nb_ev_ownership),
                       ml_col = .data$market_limit,
                       bump_ml = TRUE) %>%
    # note: hov share currently all 0's, so it got converted to NaNs ... let's fix that
    dplyr::mutate(hov_share = 0)
}
