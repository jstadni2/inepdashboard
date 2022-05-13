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

# census.R functions

#' Prepare Census subject table for use in Community Profile reactives
#'
#' @param census_data A df of ACS subject table. 
#' @param rename_cols A character vector of variable names that should be renamed.
#' @param rename_values A character vector of variable names that \code{rename_cols} should be renamed to.
#' @param geography A string representing the geography of the subject table ("counties", or "places").
#' @param pivot_cols_prefix A regular expression used to select variables to pivot and remove matching text from the start of each variable name.
#' @param pivot_col A string specifying the name of the column to create from the data stored in the column names selected from \code{pivot_cols_prefix}.
#' @param values_to A string specifying the name of the column to create from the data stored in column values.
#' @param ethnicity Logical, whether or not non-hispanic or latino ethnicity needs to be calculated.
#' @param ethnic_col A string specifying the name of the column for hispanic/latino origin (optional unless  \code{ethnicity == TRUE}).
#' @param total_col A string representing the renamed variable for the total value of the pivoted columns (optional unless \code{ethnicity == TRUE}).
#'
#' @return A dataframe of pivoted ACS subject table data prepared for use in Community Profile reactives.
#' @export
#'
#' @examples
clean_census_data <-
  function(census_data,
           rename_cols,
           rename_values,
           geography,
           pivot_cols_prefix,
           pivot_col,
           values_to,
           ethnicity = FALSE,
           ethnic_col,
           total_col) {
    
    out_df <- census_data
    
    out_df <-
      out_df %>% dplyr::rename_at(dplyr::all_of(rename_cols), ~ rename_values)
    
    out_df <-
      out_df %>% dplyr::rename(geographic_area_name = "NAME")
    
    if (geography == "counties") {
      
      out_df$geographic_area_name <-
        gsub(" County", "", out_df$geographic_area_name)
      out_df  <-
        out_df  %>% tidyr::separate("geographic_area_name", c("county", "state"), sep = ", ")
      
    } else {
      
      out_df$geographic_area_name <-
        textclean::mgsub(
          out_df$geographic_area_name,
          c(
            " city, Illinois",
            " CDP, Illinois",
            " village, Illinois",
            " town, Illinois"
          ),
          ""
        )
      
      out_df$geographic_area_name  <-
        gsub("De Pue", "DePue", out_df$geographic_area_name, fixed = TRUE) #still necessary?
      out_df <-
        out_df[out_df$GEOID != "1728950", ] #still necessary?
      
    }
    
    # Calculate ethnicity complement
    if (ethnicity == TRUE) {
      no_ethnic_col <- paste(pivot_cols_prefix, "no_", gsub(pivot_cols_prefix, "", ethnic_col), sep = "")
      out_df[[no_ethnic_col]] <- out_df[[total_col]] - out_df[[ethnic_col]]
    }
    
    out_df <- out_df %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with(pivot_cols_prefix),
        names_to = pivot_col,
        names_prefix = pivot_cols_prefix,
        values_to = values_to,
        values_drop_na = TRUE
      )
    
    out_df[[pivot_col]] <-
      gsub("_", " ", out_df[[pivot_col]]) %>% stringr::str_to_title()
    out_df
}
   
#' \code{rbind} dataframes for each poverty status
#'
#' @param total_pop_df Output of \code{clean_census_data()} for \code{total_population} fields.
#' @param below_poverty_df Output of \code{clean_census_data()} for \code{below_poverty_level} fields.
#' @param x A string for the name of the dataframe's categorical variable.
#'
#' @return The dataframe resulting from concatenating the input dataframes, with an additional categorical variable \code{poverty_status}.
#' @export
#'
#' @examples
rbind_poverty_status <- function(total_pop_df, below_poverty_df, x) {
  
  total_pop_df_c <- total_pop_df
  below_poverty_df_c <- below_poverty_df
  
  total_pop_df_c$poverty_status <- "Total Population"
  below_poverty_df_c$poverty_status <- "Below 100% Poverty Level"
  
  out_df <- rbind(total_pop_df_c, below_poverty_df_c)
  out_df[[x]] <- out_df[[x]] %>% stringr::str_to_title()
  out_df
}

#' Import American Community Survey subject table from Census API
#'
#' @param year Numeric value of Census year.
#' @param state Character value of the state used to subset the subject table data.
#' @param geography Character value for the geography used to group the data. (\code{"tract"}, \code{"place"}, or \code{"county"}).
#' @param var_ids Character string or vector of character strings of variable IDs.
#' @param acs_st_vars_lookup Data frame output of \code{tidycensus::load_variables(year, "acs5/subject")}, used to map \code{var_ids} to their labels.
#'
#' @return The data frame of the ACS subject table estimates in wide format.
#' @export
#'
#' @examples
get_acs_st <- function(year, state, geography, var_ids, acs_st_vars_lookup) {
  var_ids_df <-
    data.frame(
      name = var_ids,
      name_e = paste0(var_ids, "E")
    )
  
  var_labels <-
    dplyr::left_join(var_ids_df, acs_st_vars_lookup , by = "name")$label
  
 acs_st_df <-  suppressMessages(tidycensus::get_acs(
    geography = geography,
    variables = var_ids_df$name,
    state = state,
    year = year,
    output = "wide" # consider long format and refactor clean_census_data()
  )) %>%
    dplyr::select(-dplyr::ends_with("M"))
  
  acs_st_df <-
    acs_st_df %>%
    dplyr::rename_at(dplyr::vars(var_ids_df$name_e), ~ var_labels)
  acs_st_df
}
 
# map module functions

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

# community.R functions

#' Webscrape office sites from IL DHS website
#'
#' @param remDr Open CLASS remoteDriver from the RSelenium package.
#' @param office_type Character value for DHS Office type. Currently either \code{"WIC"} or \code{"FCRC"}.
#'
#' @return A data frame of sites for the specified office type.
#' @export
#'
#' @examples
scrape_dhs_sites <- function(remDr, office_type) {
  remDr$navigate("https://www.dhs.state.il.us/page.aspx?module=12&officetype=&county")
  
  # define value here for scope?
  # val <- ""

  if (office_type == "WIC") {
    val <- "#SearchOffice_OfficeTypeDropDownList > option:nth-child(20)"
  } else if (office_type == "FCRC") {
    val <- "#SearchOffice_OfficeTypeDropDownList > option:nth-child(10)"
  }
  
  remDr$findElement(using = "css selector", value = val)$clickElement()
  remDr$findElement(using = "css selector", value = "#SearchOffice_FindOfficesButton")$clickElement()
  
  Sys.sleep(5)
  
  search_results <- remDr$findElement(using = "css selector", "#OfficeList")
  html <- search_results$getElementAttribute('innerHTML')[[1]]
  
  # Closing the session or window causes errors on subsequent calls to remDr or scrape_dhs_sites()
  # remDr$close()
  # remDr$closeWindow()
  
  nodes <-
    rvest::html_nodes(x = rvest::read_html(html), css = "li")
  
  dhs_sites <- data.frame(
    site_name = nodes %>%
      rvest::html_element("h3") %>%
      rvest::html_text2(),
    full_address = nodes %>%
      rvest::html_element(css = ".OfficeAddress") %>%
      rvest::html_text2()
  )
  
  dhs_sites <-
    dhs_sites %>% tidyr::separate("full_address", c("address", "city_state_zip"), sep = "\n") %>%
    tidyr::separate("city_state_zip", c("site_city", "state_zip"), sep = ", ") %>%
    tidyr::separate("state_zip", c("site_state", "site_zip"), sep = " ")
  
  dhs_sites
}

#' Format sites for \code{community_sites}
#'
#' @param sites Data frame of sites to be included in \code{community_sites}.
#' @param rename_cols Character vector of variable names to rename.
#' @param site_type Character value for the type of site.
#'
#' @return A data frame that can be appended to \code{community_sites}.
#' @export
#'
#' @examples
clean_community_sites <- function(sites, rename_cols, site_type) {
  site_cols <- c("site_name",
                 "site_address",
                 "site_city",
                 "site_county",
                 "site_zip")
  
  cleaned_sites <-
    sites %>% dplyr::rename_at(dplyr::all_of(rename_cols),
                               ~ site_cols)
  
  cleaned_sites$site_state <- "IL"
  cleaned_sites$site_type <- site_type
  
  cleaned_sites <-
    cleaned_sites %>% dplyr::select(dplyr::starts_with("site_"))
  cleaned_sites
}