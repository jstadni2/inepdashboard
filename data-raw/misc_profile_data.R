## code to prepare `misc_profile_data` dataset goes here

state <- "IL"

# Adult Obesity

## CDC datasets
# cdc_datasets <- RSocrata::ls.socrata("https://chronicdata.cdc.gov")

## PLACES: Local Data for Better Health, County Data 2021 release (uses 2019 ACS)

adult_obesity_counties <-
  import_places(
    geography = "counties",
    year = "2021",
    state = state,
    measure = "Obesity among adults aged >=18 years",
    select_cols = c("locationname", "data_value"),
    rename_cols = c("county", "percent_adults_obesity")
  )

## PLACES: Local Data for Better Health, Place Data 2021 release

adult_obesity_cities  <-
  import_places(
    geography = "places",
    year = "2021",
    state = state,
    measure = "Obesity among adults aged >=18 years",
    select_cols = c("locationname", "data_value"),
    rename_cols = c("geographic_area_name", "percent_adults_obesity")
  )

usethis::use_data(adult_obesity_cities, overwrite = TRUE)

## Behavioral Risk Factor Surveillance System (BRFSS) Prevalence Data (2011 to present)

adult_obesity_state <-
  import_brfss(
    year = "2020",
    state = state,
    question_id = "_BMI5CAT",
    response = "Obese (BMI 30.0 - 99.8)",
    break_out = "Overall",
    select_cols = c("locationabbr", "data_value"),
    rename_cols = c("state", "percent_adults_obesity")
  )

# Food Insecurity

# As of 05/20/2022, Map the Meal Gap data is not accessible via an API.
# Link to data info:
# https://www.feedingamerica.org/research/map-the-meal-gap/by-county
# Request data form:
# https://feedingamerica.az1.qualtrics.com/jfe/form/SV_5tJt5m9K62hRC6N
# Once submitted, download links for the data will be provided in a subsequent email
# The following code uses a download link (Map the Meal Gap Data.zip) provided on 03/23/22

# Extract this filepath programmatically

zip_dest <- "./data-raw/Map_the_Meal_Gap_Data.zip" # rename

download.file(
  url = "https://urldefense.com/v3/__https://feedingamerica.az1.qualtrics.com/CP/File.php?F=F_6tUW5yixHpzpdlQ__;!!DZ3fjg!u_3Ff9bYzttgzmu4Nt2n_iX15LNsFOPFIIm-ZjpcRCblz8e3p8A1nxIvbvdvbg-OuYo$",
  destfile = zip_dest,
  mode = "wb"
)

# Extract from zip file

mmg_file <-
  tail(unzip(zipfile = zip_dest,
             list = TRUE)$Name,
       n = 1)

mmg_year <- stringr::str_sub(mmg_file, start = -21, end = -18)

unzip(
  zipfile = zip_dest,
  files = mmg_file,
  exdir = "./data-raw/",
)

# Import most recent data set

mmg_filepath <- paste0("./data-raw/", mmg_file)

food_insecurity_counties <-
  import_food_insecurity(mmg_filepath, state, "2019", "County")


food_insecurity_state <-
  import_food_insecurity(mmg_filepath, state, "2019", "State")


# Food insecurity data not available by place/city

# Delete zip

file.remove(zip_dest)

# Join data sets by geography

misc_prof_data_counties <-
  dplyr::full_join(adult_obesity_counties, food_insecurity_counties, by = "county")

usethis::use_data(misc_prof_data_counties, overwrite = TRUE)

misc_prof_data_state <-
  dplyr::full_join(adult_obesity_state, food_insecurity_state, by = "state")

usethis::use_data(misc_prof_data_state, overwrite = TRUE)
