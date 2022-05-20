## code to prepare `misc_profile_data` dataset goes here

# Adult Obesity

## CDC datasets
# cdc_datasets <- RSocrata::ls.socrata("https://chronicdata.cdc.gov")

## PLACES: Local Data for Better Health, County Data 2021 release (uses 2019 ACS)

adult_obesity_counties <-
  import_places(
    geography = "counties",
    year = "2021",
    state = "IL",
    measure = "Obesity among adults aged >=18 years",
    select_cols = c("locationname", "data_value"),
    rename_cols = c("county", "percent_adults_obesity")
  )

## PLACES: Local Data for Better Health, Place Data 2021 release

adult_obesity_cities  <-
  import_places(
    geography = "places",
    year = "2021",
    state = "IL",
    measure = "Obesity among adults aged >=18 years",
    select_cols = c("locationname", "data_value"),
    rename_cols = c("geographic_area_name", "percent_adults_obesity")
  )

## Behavioral Risk Factor Surveillance System (BRFSS) Prevalence Data (2011 to present)

adult_obesity_state <-
  import_brfss(
    year = "2020",
    state = "IL",
    question_id = "_BMI5CAT",
    response = "Obese (BMI 30.0 - 99.8)",
    break_out = "Overall",
    select_cols = c("locationabbr", "data_value"),
    rename_cols = c("state", "percent_adults_obesity")
  )

# Food Insecurity

food_insecurity <- data.table::fread("MMG2020_2018Data_ToShare.csv", skip = 1)

# Places to Counties

places_counties <- data.table::fread("IL Places-Counties.csv")

# usethis::use_data(misc_profile_data, overwrite = TRUE)
