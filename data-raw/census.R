## code to prepare `census` dataset goes here

year <- 2020
state <- "IL"

# Map data

acs_st_vars_lookup <-
  tidycensus::load_variables(year, "acs5/subject", cache = TRUE)

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

s1701_poverty_tracts <-
  get_acs_st(year, state, geography = "tract", s1701_var_ids, acs_st_vars_lookup)

s2_available <- !inherits(try(sf::sf_use_s2(TRUE), silent = TRUE), "try-error")

il_tracts_sf <- tigris::tracts(state = state, year = year)

## SNAP Eligible Individuals/% Tracts Layer

s1701_poverty_tracts <-
  s1701_poverty_tracts %>% dplyr::rename(
    total_population = "Estimate!!Total!!Population for whom poverty status is determined",
    individuals_income_below_185_percent_poverty_level = "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level",
  ) %>%
  tidyr::separate("NAME",
           c("census_tract", "county", "state"),
           sep = ", ")

s1701_poverty_tracts$county <- gsub(" County", "", s1701_poverty_tracts$county, fixed = TRUE)

snap_ed_eligibility_tracts <- s1701_poverty_tracts[,
                                      c(
                                        "GEOID",
                                        "census_tract",
                                        "county",
                                        "state",
                                        "total_population",
                                        "individuals_income_below_185_percent_poverty_level"
                                      )]

snap_ed_eligibility_tracts$census_tract <- gsub("Census Tract ", "", snap_ed_eligibility_tracts$census_tract, fixed = TRUE)

snap_ed_eligibility_tracts <-
  percent(
    snap_ed_eligibility_tracts,
    "snap_eligibility_percent",
    "individuals_income_below_185_percent_poverty_level",
    "total_population"
  )

# was named il_tracts_sf_merged, change in modules/app.R
snap_ed_eligibility_tracts_sf <- sf::merge(il_tracts_sf, snap_ed_eligibility_tracts, by = "GEOID")

# usethis::use_data(snap_ed_eligibility_tracts_sf, overwrite = TRUE)

# Community profile data

# SNAP Recipient Households by Race/Ethnicity

s2201_var_ids <-
  c(
    "S2201_C03_001",
    "S2201_C03_025",
    "S2201_C03_026",
    "S2201_C03_027",
    "S2201_C03_028",
    "S2201_C03_029",
    "S2201_C03_030",
    "S2201_C03_031",
    "S2201_C03_032"
  )

s2201_snap_counties <-
  get_acs_st(year, state, geography = "county", s2201_var_ids, acs_st_vars_lookup)

s2201_snap_places <-
  get_acs_st(year, state, geography = "place", s2201_var_ids, acs_st_vars_lookup)

rename_cols <- c(
  "Estimate!!Households receiving food stamps/SNAP!!Households",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!White alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Black or African American alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!American Indian and Alaska Native alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Asian alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Native Hawaiian and Other Pacific Islander alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Some other race alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Two or more races",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Hispanic or Latino origin (of any race)"
)

rename_values <- c(
  "total_snap_recipient_households",
  "snap_recipient_households_white",
  "snap_recipient_households_black_or_african_american",
  "snap_recipient_households_american_indian_and_alaska_native",
  "snap_recipient_households_asian",
  "snap_recipient_households_native_hawaiian_and_other_pacific_islander",
  "snap_recipient_households_some_other_race",
  "snap_recipient_households_two_or_more_races",
  "snap_recipient_households_hispanic_or_latino_origin"
)

pivot_col <- "demo"
pivot_cols_prefix <- "snap_recipient_households_"
values_to <- "snap_recipient_households"
ethnic_col <- "snap_recipient_households_hispanic_or_latino_origin"
total_col <- "total_snap_recipient_households"

snap_recipient_households_demo_counties <-
  clean_census_data(
    s2201_snap_counties,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

# usethis::use_data(snap_recipient_households_demo_counties, overwrite = TRUE)

snap_recipient_households_demo_cities <-
  clean_census_data(
    s2201_snap_places,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

# usethis::use_data(snap_recipient_households_demo_cities, overwrite = TRUE)

# LEP Households by Language

S1602_var_ids <-
  c(
    "S1602_C01_001",
    "S1602_C03_001",
    "S1602_C03_002",
    "S1602_C03_003",
    "S1602_C03_004",
    "S1602_C03_005"
  )

s1602_lep_counties <-
  get_acs_st(year, state, geography = "county", S1602_var_ids, acs_st_vars_lookup)

s1602_lep_places <-
  get_acs_st(year, state, geography = "place", S1602_var_ids, acs_st_vars_lookup)

rename_cols <- c(
  "Estimate!!Total!!All households",
  "Estimate!!Limited English-speaking households!!All households",
  "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Spanish",
  "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other Indo-European languages",
  "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Asian and Pacific Island languages",
  "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other languages"
)

rename_values <- c(
  "total_households",
  "total_lep_households",
  "lep_households_speaking_spanish",
  "lep_households_speaking_other_indo_european_languages",
  "lep_households_speaking_asian_and_pacific_island_languages",
  "lep_households_speaking_other_languages"
)	

pivot_col <- "language"
pivot_cols_prefix <- "lep_households_speaking_"
values_to <- "households"

lep_households_counties <-
  clean_census_data(
    s1602_lep_counties,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

# usethis::use_data(lep_households_counties, overwrite = TRUE)

lep_households_cities <-
  clean_census_data(
    s1602_lep_places,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

# usethis::use_data(lep_households_cities, overwrite = TRUE)

s1701_poverty_counties <-
  get_acs_st(year, state, geography = "county", s1701_var_ids, acs_st_vars_lookup)

s1701_poverty_places <-
  get_acs_st(year, state, geography = "place", s1701_var_ids, acs_st_vars_lookup)

# Poverty Status of Individuals by Age

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

census_data <- s1701_poverty_counties %>% dplyr::select(cols)

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

census_data <- s1701_poverty_places %>% dplyr::select(cols)

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

census_data <- s1701_poverty_counties %>% dplyr::select(cols)

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

census_data <- s1701_poverty_places %>% dplyr::select(cols)

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

poverty_individuals_age_counties <-
  rbind_poverty_status(poverty_individuals_age_counties1,
                      poverty_individuals_age_counties2,
                      "age")
poverty_individuals_age_counties$age <-
  textclean::mgsub(
    poverty_individuals_age_counties$age,
    unique(poverty_individuals_age_counties$age),
    c("Under 5", "5 To 17", "18 To 34", "35 To 64", "65 And Over")
  )

poverty_individuals_age_cities <-
  rbind_poverty_status(poverty_individuals_age_cities1,
                      poverty_individuals_age_cities2,
                      "age")
poverty_individuals_age_cities$age <-
  textclean::mgsub(
    poverty_individuals_age_cities$age,
    unique(poverty_individuals_age_cities$age),
    c("Under 5", "5 To 17", "18 To 34", "35 To 64", "65 And Over")
  )

# usethis::use_data(poverty_individuals_age_counties, overwrite = TRUE)
# usethis::use_data(poverty_individuals_age_cities, overwrite = TRUE)

# Poverty Status of Individuals by Sex

cols <- c(
  "GEOID",
  "NAME",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Male",
  "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Female"
)

census_data <- s1701_poverty_counties %>% dplyr::select(cols)

rename_cols <- tail(cols, 3)

rename_values <- c(
  "total_population",
  "total_population_sex_male",
  "total_population_sex_female"
)	

pivot_col <- "sex"
pivot_cols_prefix <- "total_population_sex_"
values_to <- "individuals"

poverty_individuals_sex_counties1 <-
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

census_data <- s1701_poverty_places %>% dplyr::select(cols)

poverty_individuals_sex_cities1 <-
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
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Male",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Female"
)

census_data <- s1701_poverty_counties %>% dplyr::select(cols)

rename_cols <- tail(cols, 3)

rename_values <- c(
  "total_population",
  "below_poverty_level_male",
  "below_poverty_level_female"
)	

pivot_cols_prefix <- "below_poverty_level_"

poverty_individuals_sex_counties2 <-
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

census_data <- s1701_poverty_places %>% dplyr::select(cols)

poverty_individuals_sex_cities2 <-
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

poverty_individuals_sex_counties <-
  rbind_poverty_status(poverty_individuals_sex_counties1,
                      poverty_individuals_sex_counties2,
                      "sex")
poverty_individuals_sex_cities <-
  rbind_poverty_status(poverty_individuals_sex_cities1,
                      poverty_individuals_sex_cities2,
                      "sex")

# usethis::use_data(poverty_individuals_sex_counties, overwrite = TRUE)
# usethis::use_data(poverty_individuals_sex_cities, overwrite = TRUE)

# Poverty Status of Individuals by Race/Ethnicity

cols <- c(
  "GEOID",
  "NAME",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)"
)

census_data <- s1701_poverty_counties %>% dplyr::select(cols)

rename_cols <- tail(cols, 9)

rename_values <- c(
  "total_population",
  "total_population_demo_white",
  "total_population_demo_black_or_african_american",
  "total_population_demo_american_indian_and_alaska_native",
  "total_population_demo_asian",
  "total_population_demo_native_hawaiian_and_other_pacific_islander",
  "total_population_demo_some_other_race",
  "total_population_demo_two_or_more_races",
  "total_population_demo_hispanic_or_latino_origin"
)	

pivot_col <- "demo"
pivot_cols_prefix <- "total_population_demo_"
values_to <- "individuals"
ethnic_col <- "total_population_demo_hispanic_or_latino_origin"
total_col <- "total_population"

poverty_individuals_demo_counties1 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

census_data <- s1701_poverty_places %>% dplyr::select(cols)

poverty_individuals_demo_cities1 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

cols <- c(
  "GEOID",
  "NAME",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)"
)

census_data <- s1701_poverty_counties %>% dplyr::select(cols)

rename_cols <- tail(cols, 10)

rename_values <- c(
  "total_population",
  "total_population_below_poverty_level",
  "below_poverty_level_white",
  "below_poverty_level_black_or_african_american",
  "below_poverty_level_american_indian_and_alaska_native",
  "below_poverty_level_asian",
  "below_poverty_level_native_hawaiian_and_other_pacific_islander",
  "below_poverty_level_some_other_race",
  "below_poverty_level_two_or_more_races",
  "below_poverty_level_hispanic_or_latino_origin"
)	

pivot_cols_prefix <- "below_poverty_level_"
ethnic_col <- "below_poverty_level_hispanic_or_latino_origin"
total_col <- "total_population_below_poverty_level"

poverty_individuals_demo_counties2 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

census_data <- s1701_poverty_places %>% dplyr::select(cols)

poverty_individuals_demo_cities2 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

poverty_individuals_demo_counties2 <-
  poverty_individuals_demo_counties2 %>% dplyr::select(-total_population_below_poverty_level)
poverty_individuals_demo_cities2 <-
  poverty_individuals_demo_cities2 %>% dplyr::select(-total_population_below_poverty_level)

poverty_individuals_demo_counties <-
  rbind_poverty_status(poverty_individuals_demo_counties1,
                      poverty_individuals_demo_counties2,
                      "demo")
poverty_individuals_demo_cities <-
  rbind_poverty_status(poverty_individuals_demo_cities1,
                      poverty_individuals_demo_cities2,
                      "demo")


# usethis::use_data(poverty_individuals_demo_counties, overwrite = TRUE)
# usethis::use_data(poverty_individuals_demo_cities, overwrite = TRUE)

# SNAP-Ed Eligibility

snap_ed_eligibility_counties <-
  snap_ed_eligibility_tracts[, c(
    "county",
    "state",
    "total_population",
    "individuals_income_below_185_percent_poverty_level"
  )] %>%
  dplyr::group_by(county, state) %>%
  dplyr::summarise(
    total_population = sum(total_population),
    individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)
  ) %>%
  dplyr::ungroup()

# calculate percent here instead of in reactive?

# usethis::use_data(snap_ed_eligibility_counties, overwrite = TRUE)

snap_ed_eligibility_cities <-
  unique(poverty_individuals_age_cities[, c("GEOID", "geographic_area_name", "total_population")]) %>%
  dplyr::left_join(s1701_poverty_places[, c(
    "GEOID",
    "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level"
  )], by = "GEOID") %>%
  dplyr::rename(individuals_income_below_185_percent_poverty_level = "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level")

# calculate percent here instead of in reactive?

# usethis::use_data(snap_ed_eligibility_cities, overwrite = TRUE)

# Places to Counties

il_places_sf <- tigris::places(state = state, year = year)

il_counties_sf <- tigris::counties(state = state, year = year)

places_counties_sf <- 
  sf::st_join(il_places_sf, il_counties_sf)

places_counties <- as.data.frame(places_counties_sf)[, c("NAME.x", "NAME.y")] # Include GEOIDs?

names(places_counties) <- c("place", "county")

# usethis::use_data(places_counties, overwrite = TRUE)
