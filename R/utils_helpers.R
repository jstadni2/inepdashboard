#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

#' Create percentage variable in data frame
#' 
#' \code{percent} returns a data frame with a percentage variable for the given numerator and denominator.
#' Used in census.R and profile server script
#'
#' @param df A data.frame containing numerator and denominator variable used for the percent variable.
#' @param percent_name A name for the resulting percent variable.
#' @param num The name of a numeric variable in \code{df} used as the numerator for calculating the percent variable.
#' @param denom The name of a numeric variable in \code{df} used as the denominator for calculating the percent variable.
#'
#' @return A data frame containing the percent variable.
#' @export # export not necessary? 
#'
#' @examples
percent <- function(df, percent_name, num, denom) {
  df[[percent_name]] <- round(100 * (df[[num]] / df[[denom]]))
  df
}


