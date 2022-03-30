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
#' @param child_df A data frame of the PEARS module's child records containing module id, and \code{site_id}.
#' @param parent_id 
#' @param parent_name 
#'
#' @return
#' @export
#'
#' @examples
unique_child_sites <- function(parent_df, child_df, parent_id, parent_name) {
  by <-
    setNames(dplyr::quo_name(dplyr::enquo(parent_id)),
             dplyr::quo_name(dplyr::enquo(parent_id)))
  out_df <-
    dplyr::left_join(parent_df,
                     dplyr::select(child_df, {{ parent_id }}, site_id),
                     by = by) %>%
    dplyr::filter(!is.na(site_id)) %>%
    dplyr::distinct({{ parent_id }}, {{ parent_name }}, program_area, site_id)
  out_df
}

#' Create Boolean variable for site program areas
#'
#' @param site_programming A data frame of a PEARS sites with program area counts for each module.
#' @param program A snake case character variable for the program area used to create the Boolean variable.
#'
#' @return
#' @export
#'
#' @examples
program_bool <- function(site_programming, program) {
  out_df <- site_programming
  cols <- colnames(out_df)
  program_cols <- cols[grepl(paste0("^", program), cols)]
  out_df[program] <-rowSums(out_df[, program_cols],  na.rm = TRUE)
  out_df[program] <- ifelse(out_df[program] > 0, "Yes", "No")
  out_df
}

#' Generate queries for the FoodFinder API
#'
#' @param key 
#' @param limit 
#' @param offset 
#' @param format 
#' @param min_lat 
#' @param max_lat 
#' @param min_lon 
#' @param max_lon 
#'
#' @return A string for the FoodFinder API query.
#' @export
#'
#' @examples
ff_query <-
  function(key,
           limit = 10000,
           offset = 1,
           format = 1,
           min_lat = 36.73359537141243,
           max_lat = 42.81674576662397,
           min_lon = -91.91983678325361,
           max_lon = -87.36749173318934) {
    query = paste0(
      "https://api-v2-prod-dot-foodfinder-183216.uc.r.appspot.com/partners/providers?",
      "key=", key, #determine how to store key in project env
      "&limit=",  limit,
      "&offset=", offset,
      "&format=", format,
      "&min_lat=", min_lat,
      "&max_lat=", max_lat,
      "&min_lon=", min_lon,
      "&max_lon", max_lon
    )
    query
  }

#' Import sites from FoodFinder
#'
#' @param query A string used to call the FoodFinder API.
#'
#' @return A sf of FoodFinder sites, reformatted for use in the Community Sites Reactive.
#' @export
#'
#' @examples
ff_import <- function(query) {
  # input arg for site_types?
  food_finder_sites <- geojsonsf::geojson_sf(query)
  
  food_finder_sites <-
    food_finder_sites %>%  tidyr::extract(
      geometry,
      into = c('latitude', 'longitude'),
      '\\((.*),(.*)\\)',
      conv = T
    )
  
  food_finder_sites <-
    dplyr::select(as.data.frame(food_finder_sites), -geometry)
  
  food_finder_sites$site_type <-
    textclean::mgsub(
      food_finder_sites$filter_id,
      seq(1, 6),
      c(
        "School & Summer Meals",
        "Farmers Markets",
        "Food Pantry/Meal Site",
        "Grocery Stores",
        "Senior Food Resources",
        "SNAP & WIC Offices"
      )
    )
  
  food_finder_sites <-
    food_finder_sites %>% dplyr::filter(site_type == "Food Pantry/Meal Site") %>% dplyr::select(c(
      "name",
      "address",
      "city",
      "state",
      "zip",
      "latitude",
      "longitude",
      "site_type"
    ))
  
  food_finder_sites <-
    food_finder_sites  %>% dplyr::rename(
      "site_name" = "name",
      "site_address" = "address",
      "site_city" = "city",
      "site_state" = "state",
      "site_zip" = "zip"
    )
  
  food_finder_sites
}