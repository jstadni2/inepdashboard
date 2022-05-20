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

# usethis::use_data(adult_obesity_countiesa, overwrite = TRUE)

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

# usethis::use_data(adult_obesity_cities, overwrite = TRUE)

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

# usethis::use_data(adult_obesity_cities, overwrite = TRUE)

# Food Insecurity

# As of 05/20/2022, Map the Meal Gap data is not accessible via an API.
# Link to data info:
# https://www.feedingamerica.org/research/map-the-meal-gap/by-county
# Request data form:
# https://feedingamerica.az1.qualtrics.com/jfe/form/SV_5tJt5m9K62hRC6N
# Once submitted, download links for the data will be provided in a subsequent email
# The following code uses a download link (Map the Meal Gap Data.zip) provided on 03/23/22

# Extract these filepaths programmatically

download.file(
  url = "https://urldefense.com/v3/__https://feedingamerica.az1.qualtrics.com/CP/File.php?F=F_6tUW5yixHpzpdlQ__;!!DZ3fjg!u_3Ff9bYzttgzmu4Nt2n_iX15LNsFOPFIIm-ZjpcRCblz8e3p8A1nxIvbvdvbg-OuYo$",
  destfile = "./data-raw/Map_the_Meal_Gap_Data.zip", # rename
  mode = "wb"
)

# Extract from zip file

mmg_file <-
  tail(unzip(zipfile = "./data-raw/Map_the_Meal_Gap_Data.zip",
             list = TRUE)$Name,
       n = 1)

mmg_year <- stringr::str_sub(mmg_file, start = -21, end = -18)

unzip(
  zipfile = "./data-raw/Map_the_Meal_Gap_Data.zip",
  files = mmg_file,
  exdir = "./data-raw/",
)

# Import most recent data set

mmg_filepath <- paste0("./data-raw/", mmg_file)

# Wrapper function for importing MMG data

food_insecurity_counties <-
  readxl::read_excel(mmg_filepath,
                     sheet = "2019 County")

food_insecurity_counties <-
  food_insecurity_counties[food_insecurity_counties$State == "IL", c(
    "County, State",
    "2019 Food Insecurity Rate",
    "# of Food Insecure Persons in 2019",
    "2019 Child food insecurity rate",
    "# of Food Insecure Children in 2019"
  )]

food_insecurity_counties <-
  readxl::read_excel("./data-raw/Map the Meal Gap Data/MMG2021_2019Data_ToShare.xlsx",
                     sheet = "2019 State")

food_insecurity <- data.table::fread("MMG2020_2018Data_ToShare.csv", skip = 1)

# Clean data

# food_insecurity <- food_insecurity %>% rename(county = "County, State",
#                                               food_insecurity_rate = "2019 Food Insecurity Rate",
#                                               food_insecure_persons = "# of Food Insecure Persons in 2019",
#                                               child_food_insecurity_rate = "2019 Child food insecurity rate",
#                                               food_insecure_children = "# of Food Insecure Children in 2019")
# 
# food_insecurity$county <- gsub(" County, Illinois", "", food_insecurity$county)

# usethis::use_data(food_insecurity, overwrite = TRUE)

# Delete zip






# Places to Counties

places_counties <- data.table::fread("IL Places-Counties.csv")

# usethis::use_data(misc_profile_data, overwrite = TRUE)
