## code to prepare `misc_profile_data` dataset goes here

# document sources, provide links

food_insecurity <- data.table::fread("MMG2020_2018Data_ToShare.csv", skip = 1)

places_counties <- data.table::fread("IL Places-Counties.csv")

# usethis::use_data(misc_profile_data, overwrite = TRUE)
