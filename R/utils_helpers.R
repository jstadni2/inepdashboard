#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# utils function for calculating percent
# used in census.R and profile server script
# eg:
# snap_ed_eligibility_tracts$snap_eligibility_percent <-
#   round(
#     100 * (
#       snap_ed_eligibility_tracts$individuals_income_below_185_percent_poverty_level / snap_ed_eligibility_tracts$total_population
#     )
#   )
