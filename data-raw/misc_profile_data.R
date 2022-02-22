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

food_insecurity <- data.table::fread("MMG2020_2018Data_ToShare.csv", skip = 1)

places_counties <- data.table::fread("IL Places-Counties.csv")

# usethis::use_data(misc_profile_data, overwrite = TRUE)
