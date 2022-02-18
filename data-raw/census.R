## code to prepare `census` dataset goes here

# Import Census data using tigris instead?

# Map data

s1701_poverty_tracts <-
  data.table::fread(
    "ACSST5Y2019.S1701_data_with_overlays_2021-03-11T131230.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!Population for whom poverty status is determined",
      "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level"
    )
  )

s2_available <- !inherits(try(sf::sf_use_s2(TRUE), silent = TRUE), "try-error")

il_tracts_sf <- sf::st_read("./cb_2019_17_tract_500k")

## SNAP Eligible Individuals/% Tracts Layer

s1701_poverty_tracts <-
  s1701_poverty_tracts %>% dplyr::rename(
    AFFGEOID = "id",
    total_population = "Estimate!!Total!!Population for whom poverty status is determined",
    individuals_income_below_185_percent_poverty_level = "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level",
  ) %>%
  tidyr::separate("Geographic Area Name",
           c("census_tract", "county", "state"),
           sep = ", ")

s1701_poverty_tracts$county <- gsub(" County", "", s1701_poverty_tracts$county, fixed = TRUE)

snap_ed_eligibility_tracts <- s1701_poverty_tracts[,
                                      c(
                                        "AFFGEOID",
                                        "census_tract",
                                        "county",
                                        "state",
                                        "total_population",
                                        "individuals_income_below_185_percent_poverty_level"
                                      )]

snap_ed_eligibility_tracts$census_tract <- gsub("Census Tract ", "", snap_ed_eligibility_tracts$census_tract, fixed = TRUE)

# utils function for calculating percent?
snap_ed_eligibility_tracts$snap_eligibility_percent <-
  round(
    100 * (
      snap_ed_eligibility_tracts$individuals_income_below_185_percent_poverty_level / snap_ed_eligibility_tracts$total_population
    )
  )

# was named il_tracts_sf_merged, change in modules/app.R
snap_ed_eligibility_tracts_sf <- sf::merge(il_tracts_sf, snap_ed_eligibility_tracts, by = "AFFGEOID") #missing two tracts?

# usethis::use_data(snap_ed_eligibility_tracts_sf, overwrite = TRUE)

# Community profile data

s1701_poverty_counties <-
  data.table::fread(
    "ACSST5Y2019.S1701_data_with_overlays_2021-11-08T111346.csv",
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

s1701_poverty_places <-
  data.table::fread(
    "ACSST5Y2019.S1701_data_with_overlays_2021-04-28T154515.csv",
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

s2201_snap_counties <-
  data.table::fread(
    "ACSST5Y2019.S2201_data_with_overlays_2021-12-16T152331.csv",
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
    "ACSST5Y2019.S2201_data_with_overlays_2021-05-04T143832.csv",
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

s1602_lep_counties <-
  data.table::fread(
    "ACSST5Y2019.S1602_data_with_overlays_2021-12-16T145907.csv",
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
    "ACSST5Y2019.S1602_data_with_overlays_2021-05-05T160010.csv",
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

adult_obesity <-
  data.table::fread(
    "2021 County Health Rankings Illinois Data - v1 - Ranked Measure Data.csv",
    skip = 1,
    select = c("FIPS",
               "State",
               "County",
               "% Adults with Obesity")
  )

# usethis::use_data(census, overwrite = TRUE)
