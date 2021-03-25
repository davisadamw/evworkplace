#' Calculate base penetration rate
#'
#' This is basically a version of \code{\link[stocked]{calculate_bp}} that works
#' inside mutate
#'
#' @param fixed_predictor Vector of values to be used as the fixed predictors
#' @param other_predictors Data frame of values to that match names in
#'   coefficients
#' @param coefficients Named vector of coefficients, probably derived from (
#'   \code{\link{assignment_model_coefficients}})
#' @param frame numeric; maximum divergence allowed for calculated BP. By
#'   default, the smallest BP will be 0.1\*mean, and the largest will be
#'   10\*mean)
#'
#' @return Vector of bp values
calculate_bp_simplified <- function(fixed_predictor, other_predictors, coefficients, frame = 10) {

  # add a coefficient for the fixed predictor
  cd <- coefficients
  cd["fixed"] <- 1

  # make a matrix of the variables used in computation
  pred_vars <- other_predictors %>%
    dplyr::mutate(intercept = 1,
                  fixed = fixed_predictor,
                  .before = 1) %>%
    as.matrix()

  # use matrix multiplication to efficiently calculate initial bp values
  bps_init <- pred_vars %*% as.matrix(cd[colnames(pred_vars)])

  # correct the base rate to within the frame (all bps must be positive and within a factor of frame from the median)
  med_bp <- stats::median(bps_init)
  bp_lo  <- max(med_bp / frame, 0.001)
  bp_hi  <- max(med_bp * frame, 0.001)


  dplyr::case_when(bps_init < bp_lo ~ bp_lo,
                   bps_init > bp_hi ~ bp_hi,
                   TRUE             ~ bps_init)
}
