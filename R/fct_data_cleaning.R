#' data_cleaning 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# pears.R functions

#' Count the number of module entries by Program Area.
#'
#'
#'
#' @param df A data.frame of a PEARS module export containing \code{program_area} and the module name as the second variable.
#' @param fields 
#' @param module 
#'
#' @return
#' @export
#'
#' @examples
program_area_counts <- function(df, fields, module) {
  out_df <-
    df[df$program_area %in% c("SNAP-Ed", "EFNEP") & # program_area column must be present
         !grepl("TEST", df[[fields[2]]]), fields] # second field must be name
  out_df <-
    full_join(out_df[out_df$program_area == "SNAP-Ed", ] %>% count(site_id, name = paste0("snap_ed_", module)),
              out_df[out_df$program_area == "EFNEP", ] %>% count(site_id, name =  paste0("efnep_", module)),
              by = "site_id")
  out_df[[module]] <-
    rowSums(out_df[, c(paste0("snap_ed_", module), paste0("efnep_", module))],  na.rm = TRUE)
  out_df
}