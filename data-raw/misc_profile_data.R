## code to prepare `misc_profile_data` dataset goes here

# document sources, provide links

adult_obesity <-
  data.table::fread(
    "2021 County Health Rankings Illinois Data - v1 - Ranked Measure Data.csv",
    skip = 1,
    select = c("FIPS",
               "State",
               "County",
               "% Adults with Obesity")
  )

# https://dev.socrata.com/foundry/chronicdata.cdc.gov/swc5-untb
# https://github.com/Chicago/RSocrata
# https://dev.socrata.com/docs/queries/

# CDC datasets
# cdc_datasets <- RSocrata::ls.socrata("https://chronicdata.cdc.gov")

# PLACES: Local Data for Better Health, County Data 2021 release (uses 2019 ACS)
places_county <- RSocrata::read.socrata(
  places_query(
    geography = "counties",
    year = "2019",
    state = "IL",
    measure = "Obesity among adults aged >=18 years",
    cols = c("locationname", "data_value")
  )
)

# PLACES: Local Data for Better Health, Place Data 2021 release
places_place <- RSocrata::read.socrata(
  places_query(
    geography = "places",
    year = "2019",
    state = "IL",
    measure = "Obesity among adults aged >=18 years",
    cols = c("locationname", "data_value")
  )
)


# https://chronicdata.cdc.gov/api/views/dttw-5yxu
# Behavioral Risk Factor Surveillance System (BRFSS) Prevalence Data (2011 to present)

brfss <- RSocrata::read.socrata(
  brfss_query(
    year = "2019",
    state = "IL",
    question_id = "_BMI5CAT",
    response = "Obese (BMI 30.0 - 99.8)",
    break_out = "Overall",
    cols = c("locationabbr", "data_value")
  )
)


# 2019

# Get email update for when new BRFSS is published

food_insecurity <- data.table::fread("MMG2020_2018Data_ToShare.csv", skip = 1)

places_counties <- data.table::fread("IL Places-Counties.csv")

# usethis::use_data(misc_profile_data, overwrite = TRUE)
