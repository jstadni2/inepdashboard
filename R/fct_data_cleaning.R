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
#' @param df A data frame of a PEARS module export containing \code{program_area}, \code{site_id}, and the module name as the second variable.
#' @param fields A character vector of variables to be included in the output data frame.
#' @param module A snake-case string of the PEARS module used for the count variable names.
#'
#' @return A data frame with module count variables for each program area and a total count by \code{site_id}.
#' @export
#'
#' @examples
program_area_counts <- function(df, fields, module) {
  out_df <-
    df[df$program_area %in% c("SNAP-Ed", "EFNEP") & # program_area column must be present
         !grepl("TEST", df[[fields[2]]]), fields] # second field must be name
  out_df <-
    dplyr::full_join(out_df[out_df$program_area == "SNAP-Ed", ] %>% dplyr::count(site_id, name = paste0("snap_ed_", module)), #create string var
              out_df[out_df$program_area == "EFNEP", ] %>% dplyr::count(site_id, name =  paste0("efnep_", module)), #create string var
              by = "site_id")
  out_df[[module]] <-
    rowSums(out_df[, c(paste0("snap_ed_", module), paste0("efnep_", module))],  na.rm = TRUE)
  out_df
}


#' Extract unique module sites from child records.
#'
#' @param parent_df A data frame of a PEARS module export containing module id, module name, and \code{program_area}.
#' @param child_df A data frame of the PEARS module's child records containing module id, child_id, and \code{site_id}.
#' @param parent_id 
#' @param parent_name 
#' @param child_id 
#'
#' @return
#' @export
#'
#' @examples
unique_child_sites <- function(parent_df, child_df, parent_id, parent_name, child_id) {
  out_df <-
    dplyr::left_join(parent_df,
                     dplyr::select(child_df, {{ parent_id }}, {{ child_id }}, site_id),
                     by = {{ parent_id }}) %>%
    dplyr::filter(!is.na(site_id)) %>%
    dplyr::distinct({{ parent_id }}, {{ parent_name }}, program_area, site_id)
  out_df
}