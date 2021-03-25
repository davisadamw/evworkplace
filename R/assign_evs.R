#' Run EV assignment model
#'
#' Assign EVs to block groups with baseline adoption rates computed using
#' coefficients included in this package. Typically will be run AFTER calling
#' \code{\link{prep_bg_data}}
#'
#' @param prepped_data Dataset prepped for model use (all continuous variables
#'   centered and scaled), probably produced by \code{\link{prep_bg_data}}
#' @param evs_to_add Number of additional EVs to add
#' @param model_coefficients A data frame of coefficients to use, with one
#'   column of variable names and one column of values ... Current choices
#'   `CA_bev` and `CA_phev` are stored in
#'   \code{\link{assignment_model_coefficients}}
#' @param prediction_colname Name for the new column of predicted PEVs
#'
#' @return A dataframe identical to prepped data with an additional column added
#'   for number of predicted PEVs
#' @export
#'
#' @examples
#' # First, prepare the input data for this functino
#' prepped_data <- prep_bg_data()
#'
#' # Then, run a scenario where 20,000 BEVs and 10,000 PHEVs are assigned:
#' prepped_data %>%
#'   assign_evs(20000, evworkplace::assignment_model_coefficients$CA_bev,  "bevs_20k") %>%
#'   assign_evs(10000, evworkplace::assignment_model_coefficients$CA_phev, "phevs_10k")
assign_evs <- function(prepped_data,
                       evs_to_add,
                       model_coefficients,
                       prediction_colname) {

  # load model coefficients
  coefficients_use <- model_coefficients %>%
    tibble::deframe()

  # compute the bp rate based on these coefficients
  base_rates <- calculate_bp_simplified(
    fixed_predictor = prepped_data$med_income,
    other_predictors = dplyr::select(prepped_data, dplyr::any_of(names(coefficients_use))),
    coefficients = coefficients_use)

  # assign vehicles ... another function in stocked, details in help
  pred_evs <- stocked::run_assignment(
    market_current = prepped_data$evs_start,
    market_limit   = prepped_data$market_limit,
    base_rate = base_rates,
    n_to_add = evs_to_add,
    p = coefficients_use["p"],
    q = coefficients_use["q"])

  prepped_data %>%
    dplyr::mutate({{ prediction_colname }} := pred_evs)
}
