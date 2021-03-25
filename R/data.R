#' EV adoption data for the DVRPC region.
#'
#' A dataset containing block-group-level information for the Delaware Valley Region relevant to EV adoption.
#'
#' @format A data frame with 7128 rows and 13 variables:
#' \describe{
#'   \item{GEOID}{FIPS code for each block group in the region}
#'   \item{vehicles}{Number of privately owned vehicles in the block group}
#'   \item{evs_start}{Number of electric vehicles in the block group at the start of our analysis}
#'   \item{households}{Number of households in the block group}
#'   \item{med_income}{Median household income of the block group, dollars}
#'   \item{apartment}{Number of households living in apartments}
#'   \item{mobile_home}{Number of households living in mobile homes}
#'   \item{single_attached}{Number of households living in attached single family houses}
#'   \item{single_detached}{Number of households living in detached single family houses}
#'   \item{WgtAvgHOVShare_Mi}{Ignore this column}
#'   \item{WgtAvgCmtDst_Mi}{Average commute distance of people living in this block group, miles}
#'   \item{nb_ev_ownership}{Weighted average EV adoption rate in surrounding block groups}
#' }
#' @source Most data from American Community Survey 2015-2019 5-year averages.
"dvrpc_bg_data"

#' Adoption model coefficients for this study.
#'
#' A dataset containing two sets of model coefficients to use for EV assignment.
#'
#' Currently included coefficients are derived from models for BEV and PHEV adoption in California.
#' A paper from this study is in review in *Transportation Research Record* as
#' "Investigating the sensitivity of electric vehicle out-of-home charging demand to changes in light-duty vehicle fleet makeup and usage, a case study for California 2030"
#'
#' @format A named of two data frames with model coefficients, with entries named `BEV` and `PHEV`. Each data frame has 8 rows and 2 variables:
#' \describe{
#'   \item{Variable}{Variable name, all but intercept, p, and q must match the name of a column in the block group dataset}
#'   \item{Coefficient}{Coefficient of this column in model for base adoption rate}
#' }
"assignment_model_coefficients"

