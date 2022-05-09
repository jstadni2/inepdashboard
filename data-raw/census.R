## code to prepare `census` dataset goes here

year <- 2020
state <- "IL"

# Map data

acs_st_vars_lookup <-
  tidycensus::load_variables(year, "acs5/subject", cache = TRUE)

# Create wrapper function for following operations:

S1701_var_ids <-
  data.frame(
    name_e =
      c(
        "S1701_C01_001E",
        "S1701_C01_041E",
        "S1701_C01_003E",
        "S1701_C01_004E",
        "S1701_C01_007E",
        "S1701_C01_008E",
        "S1701_C01_010E",
        "S1701_C02_003E",
        "S1701_C02_004E",
        "S1701_C02_007E",
        "S1701_C02_008E",
        "S1701_C02_010E",
        "S1701_C01_011E",
        "S1701_C01_012E",
        "S1701_C02_011E",
        "S1701_C02_012E",
        "S1701_C01_013E",
        "S1701_C01_014E",
        "S1701_C01_015E",
        "S1701_C01_016E",
        "S1701_C01_017E",
        "S1701_C01_018E",
        "S1701_C01_019E",
        "S1701_C01_020E",
        "S1701_C02_001E",
        "S1701_C02_013E",
        "S1701_C02_014E",
        "S1701_C02_015E",
        "S1701_C02_016E",
        "S1701_C02_017E",
        "S1701_C02_018E",
        "S1701_C02_019E",
        "S1701_C02_020E"
      )
  )

S1701_var_ids$name <-
  stringr::str_sub(S1701_var_ids$name, 1, end = -2)

S1701_var_labels <-
  dplyr::left_join(S1701_var_ids, acs_st_vars_lookup , by = "name")$label

s1701_poverty_tracts <- tidycensus::get_acs(
  geography = "tract",
  variables = S1701_var_ids$name,
  state = state,
  year = year,
  output = "wide" # consider long format and refactor clean_census_data()
) %>%
  dplyr::select(-dplyr::ends_with("M"))

s1701_poverty_tracts <-
  s1701_poverty_tracts %>%
  dplyr::rename_at(dplyr::vars(S1701_var_ids$name_e), ~ S1701_var_labels)

# End wrapper here

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

s1701_poverty_counties <-
  data.table::fread(
    "data-raw/ACSST5Y2019.S1701_data_with_overlays_2021-11-08T111346.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!Population for whom poverty status is determined",
      "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!65 years and over",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!65 years and over",
      "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Male",
      "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Female",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Male",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Female",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)",
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
  )

s1701_poverty_counties2 <- tidycensus::get_acs(
  geography = "county",
  variables = S1701_var_ids$name,
  state = state,
  year = year,
  output = "wide" # consider long format and refactor clean_census_data()
) %>%
  dplyr::select(-dplyr::ends_with("M"))

S1701_Poverty_Places <-
  S1701_Poverty_Places %>% rename_at(vars(S1701_var_ids$name_e), ~S1701_var_labels)

s1701_poverty_places <-
  data.table::fread(
    "data-raw/ACSST5Y2019.S1701_data_with_overlays_2021-04-28T154515.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!Population for whom poverty status is determined",
      "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!65 years and over",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!65 years and over",
      "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Male",
      "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Female",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Male",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Female",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)",
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
  )

# SNAP Recipient Households by Race/Ethnicity

s2201_snap_counties <-
  data.table::fread(
    "data-raw/ACSST5Y2019.S2201_data_with_overlays_2021-12-16T152331.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
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
  )

s2201_snap_places <-
  data.table::fread(
    "data-raw/ACSST5Y2019.S2201_data_with_overlays_2021-05-04T143832.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
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
  )

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

# LEP Households by Language

s1602_lep_counties <-
  data.table::fread(
    "data-raw/ACSST5Y2019.S1602_data_with_overlays_2021-12-16T145907.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!All households",
      "Estimate!!Limited English-speaking households!!All households",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Spanish",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other Indo-European languages",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Asian and Pacific Island languages",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other languages"
    )
  )

s1602_lep_places <-
  data.table::fread(
    "data-raw/ACSST5Y2019.S1602_data_with_overlays_2021-05-05T160010.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!All households",
      "Estimate!!Limited English-speaking households!!All households",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Spanish",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other Indo-European languages",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Asian and Pacific Island languages",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other languages"
    )
  )

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

#Poverty Status of Individuals by Age

cols <- c(
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!65 years and over"
)

census_data <- s1701_poverty_counties[, ..cols]

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

census_data <- s1701_poverty_places[, ..cols]

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
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!65 years and over"
)

census_data <- s1701_poverty_counties[, ..cols]

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

census_data <- s1701_poverty_places[, ..cols]

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

#Poverty Status of Individuals by Sex

cols <- c(
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Male",
  "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Female"
)

census_data <- s1701_poverty_counties[, ..cols]

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

census_data <- s1701_poverty_places[, ..cols]

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
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Male",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Female"
)

census_data <- s1701_poverty_counties[, ..cols]

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

census_data <- s1701_poverty_places[, ..cols]

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

#Poverty Status of Individuals by Race/Ethnicity

cols <- c(
  "id",
  "Geographic Area Name",
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

census_data <- s1701_poverty_counties[, ..cols]

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

census_data <- s1701_poverty_places[, ..cols]

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
  "id",
  "Geographic Area Name",
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

census_data <- s1701_poverty_counties[, ..cols]

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

census_data <- s1701_poverty_places[, ..cols]

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
  poverty_individuals_demo_counties2 %>% select(-total_population_below_poverty_level)
poverty_individuals_demo_cities2 <-
  poverty_individuals_demo_cities2 %>% select(-total_population_below_poverty_level)

poverty_individuals_demo_counties <-
  rbind_poverty_status(poverty_individuals_demo_counties1,
                      poverty_individuals_demo_counties2,
                      "demo")
poverty_individuals_demo_cities <-
  rbind_poverty_status(poverty_individuals_demo_cities1,
                      poverty_individuals_demo_cities2,
                      "demo")


# usethis::use_data(census, overwrite = TRUE)
