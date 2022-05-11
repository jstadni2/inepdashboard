# pears.R function tests

test_that("program_area_counts", {
  
  pa <-
    data.frame(
      program_id = seq(1, 5),
      name = paste0("Program Activity ", seq(1, 5)),
      program_area = c("EFNEP", "SNAP-Ed", "SNAP-Ed", "SNAP-Ed", "EFNEP"),
      site_id =  c(1, 2, 2, 3, 3)
    )

  # Using package-defined function
  
  sites_pa1 <-
    program_area_counts(pa,
                        c("program_id", "name", "program_area", "site_id"),
                        "program_activities")
  
  # Using manual operations
  
  sites_pa2 <-
    pa[pa$program_area %in% c("SNAP-Ed", "EFNEP") & 
         !grepl("TEST", pa$name), ]
  sites_pa2 <-
    dplyr::full_join(sites_pa2[sites_pa2$program_area == "SNAP-Ed", ] %>% dplyr::count(site_id, name = "snap_ed_program_activities"),
              sites_pa2[sites_pa2$program_area == "EFNEP", ] %>% dplyr::count(site_id, name = "efnep_program_activities"),
              by = "site_id")
  sites_pa2$program_activities <-
    rowSums(sites_pa2[, c("snap_ed_program_activities", "efnep_program_activities")],  na.rm = TRUE)
  
  expect_equal(sites_pa1, sites_pa2)
})

test_that("unique_child_sites", {
  ia <-
    data.frame(
      activity_id = seq(1, 5),
      title = paste0("Indirect Activity ", seq(1, 5)),
      program_area = c("EFNEP", "SNAP-Ed", "SNAP-Ed", "SNAP-Ed", "EFNEP")
    )
  
  ic <-
    data.frame(
      activity_id = rep(seq(1, 3), length.out = 8),
      channel_id = seq(1, 8),
      site_id = rep(c(seq(1, 5), NA), length.out = 8)
    )
  
  # Using manual operations
  
  ia_ic1 <-
    dplyr::left_join(ia, ic[c("activity_id", "channel_id", "site_id")], by = "activity_id") %>%
    dplyr::filter(!is.na(site_id)) %>%
    dplyr::distinct(activity_id, title, program_area, site_id)
  
  # Using package-defined function
  
  ia_ic2 <- unique_child_sites(ia, ic, activity_id, title)
  
  expect_equal(ia_ic1, ia_ic2)
})

test_that("program_bool", {
  site_programming <-
    data.frame(
      site_id = seq(1, 5),
      site_name = paste0("Site ", seq(1, 5)),
      snap_ed_program_activities = c(NA, 1, 3, NA, NA),
      efnep_program_activities = c(1, 3, NA, NA, 2),
      snap_ed_indirect_activities = c(NA, NA, 1, NA, 2),
      efnep_indirect_activities = c(1, 3, NA, NA, NA)
    )
  
  # Using manual operations
  
  site_programming1 <- site_programming
  
  cols <- colnames(site_programming1)
  program <- "snap_ed"
  program_cols <- cols[grepl(paste0("^", program), cols)]
  site_programming1[program] <-rowSums(site_programming1[, program_cols],  na.rm = TRUE)
  site_programming1[program] <- ifelse(site_programming1[program] > 0, "Yes", "No")
  
  cols <- colnames(site_programming1)
  program <- "efnep"
  program_cols <- cols[grepl(paste0("^", program), cols)]
  site_programming1[program] <-rowSums(site_programming1[, program_cols],  na.rm = TRUE)
  site_programming1[program] <- ifelse(site_programming1[program] > 0, "Yes", "No")
  
  # Using package-defined function
  
  site_programming2 <- program_bool(site_programming, "snap_ed")
  site_programming2 <- program_bool(site_programming2, "efnep")
  
  expect_equal(site_programming1, site_programming2)
})

# map module function tests

test_that ("ff_query", {
  # Using default parameters
  
  q1 <- ff_query(key = "1234")
  q2 <- "https://api-v2-prod-dot-foodfinder-183216.uc.r.appspot.com/partners/providers?key=1234&limit=10000&offset=1&format=1&min_lat=36.7335953714124&max_lat=42.816745766624&min_lon=-91.9198367832536&max_lon-87.3674917331893"
  expect_equal(q1, q2)
})

# census.R function tests

# Import test data

outputs_c <-
  c(
    "poverty_individuals_age_counties1",
    "poverty_individuals_age_counties2",
    "poverty_individuals_age_cities1",
    "poverty_individuals_age_cities2"
  )

prev_ts <-
  tail(unique(stringr::str_sub(list.files(
    testthat::test_path("testdata")
  ), 1, 19)), n = 1)

# Implement for loop here
poverty_individuals_age_counties1_prev <-
  arrow::read_feather(testthat::test_path(
    "testdata",
    paste0(prev_ts, "_", outputs_c[1], ".feather")
  ))
poverty_individuals_age_counties2_prev <-
  arrow::read_feather(testthat::test_path(
    "testdata",
    paste0(prev_ts, "_", outputs_c[2], ".feather")
  ))
poverty_individuals_age_cities1_prev <-
  arrow::read_feather(testthat::test_path(
    "testdata",
    paste0(prev_ts, "_", outputs_c[3], ".feather")
  ))
poverty_individuals_age_cities2_prev <-
  arrow::read_feather(testthat::test_path(
    "testdata",
    paste0(prev_ts, "_", outputs_c[4], ".feather")
  ))

test_that ("clean_census_data", {
  # Import poverty status by age data
  
  year <- 2020
  state <- "IL"
  
  s1701_var_ids <- c(
    "S1701_C01_001",
    "S1701_C01_041",
    "S1701_C01_003",
    "S1701_C01_004",
    "S1701_C01_007",
    "S1701_C01_008",
    "S1701_C01_010",
    "S1701_C02_003",
    "S1701_C02_004",
    "S1701_C02_007",
    "S1701_C02_008",
    "S1701_C02_010"
  )
  
  acs_st_vars_lookup <-
    tidycensus::load_variables(year, "acs5/subject", cache = TRUE)
  
  s1701_poverty_counties <-
    get_acs_st(year, state, geography = "county", s1701_var_ids, acs_st_vars_lookup)
  
  s1701_poverty_places <-
    get_acs_st(year, state, geography = "place", s1701_var_ids, acs_st_vars_lookup)
  
  # Call clean_census_data() on poverty status by age data
  
  cols <- c(
    "GEOID",
    "NAME",
    "Estimate!!Total!!Population for whom poverty status is determined",
    "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
    "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
    "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
    "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
    "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!65 years and over"
  )
  
  census_data <- s1701_poverty_counties %>% dplyr::select(dplyr::all_of(cols))
  
  rename_cols <- tail(cols, 6)
  
  rename_values <- c(
    "total_population",
    "total_population_age_under_5_years",
    "total_population_age_5_to_17_years",
    "total_population_age_18_to_34_years",
    "total_population_age_35_to_64_years",
    "total_population_age_65_years_and_over"
  )	
  
  pivot_col <- "age"
  pivot_cols_prefix <- "total_population_age_"
  values_to <- "individuals"
  
  poverty_individuals_age_counties1 <-
    clean_census_data(
      census_data,
      rename_cols,
      rename_values,
      geography = "counties",
      pivot_cols_prefix,
      pivot_col,
      values_to,
      ethnicity = FALSE
    )
  
  census_data <- s1701_poverty_places %>% dplyr::select(dplyr::all_of(cols))
  
  poverty_individuals_age_cities1 <-
    clean_census_data(
      census_data,
      rename_cols,
      rename_values,
      geography = "places",
      pivot_cols_prefix,
      pivot_col,
      values_to,
      ethnicity = FALSE
    )
  
  cols <- c(
    "GEOID",
    "NAME",
    "Estimate!!Total!!Population for whom poverty status is determined",
    "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
    "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
    "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
    "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
    "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!65 years and over"
  )
  
  census_data <- s1701_poverty_counties %>% dplyr::select(dplyr::all_of(cols))
  
  rename_cols <- tail(cols, 6)
  
  rename_values <- c(
    "total_population",
    "below_poverty_level_under_5_years",
    "below_poverty_level_5_to_17_years",
    "below_poverty_level_18_to_34_years",
    "below_poverty_level_35_to_64_years",
    "below_poverty_level_65_years_and_over"
  )	
  
  pivot_cols_prefix <- "below_poverty_level_"
  
  poverty_individuals_age_counties2 <-
    clean_census_data(
      census_data,
      rename_cols,
      rename_values,
      geography = "counties",
      pivot_cols_prefix,
      pivot_col,
      values_to,
      ethnicity = FALSE
    )
  
  census_data <- s1701_poverty_places %>% dplyr::select(dplyr::all_of(cols))
  
  poverty_individuals_age_cities2 <-
    clean_census_data(
      census_data,
      rename_cols,
      rename_values,
      geography = "places",
      pivot_cols_prefix,
      pivot_col,
      values_to,
      ethnicity = FALSE
    )

  # Uncomment following lines to save current implementation's outputs
  
  # out_dir <- testthat::test_path("testdata")
  # ts <- format(Sys.time(), "%Y.%m.%d.%H.%M.%S")
  
  # outputs <-
  #   list(
  #     poverty_individuals_age_counties1,
  #     poverty_individuals_age_counties2,
  #     poverty_individuals_age_cities1,
  #     poverty_individuals_age_cities2
  #   )
  
  # for (i in 1:length(outputs)) {
  #   arrow::write_feather(
  #     outputs[[i]],
  #     paste0(out_dir, "/", ts, "_", outputs_c[i], ".feather"),
  #   )
  # }
  
  # Import previous implementation's outputs
  
  # Reference for writing/reading files in tests:
  # https://stackoverflow.com/questions/32328802/where-should-i-put-data-for-automated-tests-with-testthat
  # https://blog.r-hub.io/2020/11/18/testthat-utility-belt/
  
  # Compare current outputs to previous outputs
  expect_equal(poverty_individuals_age_counties1, poverty_individuals_age_counties1_prev)
  expect_equal(poverty_individuals_age_counties2, poverty_individuals_age_counties2_prev)
  expect_equal(poverty_individuals_age_cities1, poverty_individuals_age_cities1_prev)
  expect_equal(poverty_individuals_age_cities2, poverty_individuals_age_cities2_prev)
})

test_that ("rbind_poverty_status", { 
  
  # Using manual operations
  
  total_pop_df_c <- poverty_individuals_age_counties1_prev
  below_poverty_df_c <- poverty_individuals_age_counties2_prev
  x <- "age"
  
  total_pop_df_c$poverty_status <- "Total Population"
  below_poverty_df_c$poverty_status <- "Below 100% Poverty Level"
  
  out_df <- rbind(total_pop_df_c, below_poverty_df_c)
  out_df[[x]] <- out_df[[x]] %>% stringr::str_to_title()
  
  # Using package-defined function
  
  poverty_individuals_age_counties <-
    rbind_poverty_status(poverty_individuals_age_counties1_prev,
                         poverty_individuals_age_counties2_prev,
                         "age")
  
  expect_equal(out_df, poverty_individuals_age_counties)
})

# Import most recent outputs from clean_census_data() tests

test_that ("get_acs_st", {
  year <- 2020
  state <- "IL"
  geography <- "tract"
  
  s1701_var_ids <- c(
    "S1701_C01_001",
    "S1701_C01_041",
    "S1701_C01_003",
    "S1701_C01_004",
    "S1701_C01_007",
    "S1701_C01_008",
    "S1701_C01_010",
    "S1701_C02_003",
    "S1701_C02_004",
    "S1701_C02_007",
    "S1701_C02_008",
    "S1701_C02_010",
    "S1701_C01_011",
    "S1701_C01_012",
    "S1701_C02_011",
    "S1701_C02_012",
    "S1701_C01_013",
    "S1701_C01_014",
    "S1701_C01_015",
    "S1701_C01_016",
    "S1701_C01_017",
    "S1701_C01_018",
    "S1701_C01_019",
    "S1701_C01_020",
    "S1701_C02_001",
    "S1701_C02_013",
    "S1701_C02_014",
    "S1701_C02_015",
    "S1701_C02_016",
    "S1701_C02_017",
    "S1701_C02_018",
    "S1701_C02_019",
    "S1701_C02_020"
  )
  
  acs_st_vars_lookup <-
    tidycensus::load_variables(year, "acs5/subject", cache = TRUE)
  
  # Using manual operations
  
  s1701_var_ids_df <-
    data.frame(
      name = s1701_var_ids,
      name_e = paste0(s1701_var_ids, "E")
    )
  
  s1701_var_labels <-
    dplyr::left_join(s1701_var_ids_df, acs_st_vars_lookup , by = "name")$label
  
  s1701_poverty_tracts1 <-  suppressMessages(tidycensus::get_acs(
    geography,
    variables = s1701_var_ids_df$name,
    state = state,
    year = year,
    output = "wide" 
  )) %>%
    dplyr::select(-dplyr::ends_with("M"))
  
  s1701_poverty_tracts1 <-
    s1701_poverty_tracts1 %>%
    dplyr::rename_at(dplyr::vars(s1701_var_ids_df$name_e), ~ s1701_var_labels)
  
  # Using package-defined function
  
  s1701_poverty_tracts2 <-get_acs_st(year, state, geography, s1701_var_ids, acs_st_vars_lookup)
  
  
  expect_equal(s1701_poverty_tracts1, s1701_poverty_tracts2)
})

# community_sites.R function tests

test_that ("scrape_dhs_sites", {
  # Check if Selenium container is running
  # If not, start Selenium container
  
  # Open Remote Driver
  
  remDr <-
    RSelenium::remoteDriver(remoteServerAddr = "192.168.1.5",
                            port = 4445L,
                            browserName = "chrome")
  remDr$open()
  
  # Using manual operations
  
  remDr$navigate("https://www.dhs.state.il.us/page.aspx?module=12&officetype=&county")
  
  # Select Office Type - WIC
  remDr$findElement(using = 'xpath', "/html/body/div[2]/div[2]/form/div[3]/div[2]/div[1]/select/option[20]")$clickElement()
  # Click "Find Offices"
  remDr$findElement(using = 'xpath', "//*[@id='SearchOffice_FindOfficesButton']")$clickElement()
  
  Sys.sleep(2)
  
  # search_results <- remDr$findElements("id", "SearchResults")
  search_results <- remDr$findElement(using = 'xpath', "//*[@id='OfficeList']")
  
  html <- search_results$getElementAttribute('innerHTML')[[1]]
  
  nodes <-
    rvest::html_nodes(x = rvest::read_html(html), css = "li")
  
  wic_sites1 <- data.frame(
    site_name = nodes %>%
      rvest::html_element("h3") %>%
      rvest::html_text2(),
    full_address = nodes %>%
      rvest::html_element(css = ".OfficeAddress") %>%
      rvest::html_text2()
  )
  
  wic_sites1 <-
    suppressWarnings(
      wic_sites1 %>% tidyr::separate("full_address", c("address", "city_state_zip"), sep = "\n") %>%
        tidyr::separate("city_state_zip", c("site_city", "state_zip"), sep = ", ") %>%
        tidyr::separate("state_zip", c("site_state", "site_zip"), sep = " ")
    )
  
  # Using package-defined function
  
  wic_sites2 <- suppressWarnings(scrape_dhs_sites(remDr, "WIC"))
  
  expect_equal(wic_sites1, wic_sites2)
})