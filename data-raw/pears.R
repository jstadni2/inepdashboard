## code to prepare `pears` dataset goes here

aws.signature::use_credentials(profile = "pears")
pears_bucket <- "exports.pears.oeie.org"

prefix <- "uie/"
ts <- format(Sys.time(), "%Y/%m/%d/")

response <- aws.s3::get_bucket(
  bucket = pears_bucket,
  region = "us-east-1",
  prefix = paste0(prefix, ts),
  max = 100
)

# Better way to map this data?
# List of lists?
pears_modules <- tibble::tibble(
  module = c(
    "Sites",
    "Program Activities",
    "Indirect Activities",
    "Indirect Activities",
    "PSE Site Activities",
    "Coalitions",
    "Coalitions",
    "Partnerships"
  ),
  filename = c(
    "Site_Export.xlsx",
    "Program_Activities_Export.xlsx",
    "Indirect_Activity_Export.xlsx",
    "Indirect_Activity_Export.xlsx",
    "PSE_Site_Activity_Export.xlsx",
    "Coalition_Export.xlsx",
    "Coalition_Export.xlsx",
    "Partnership_Export.xlsx"
  ),
  sheet = c(
    "Site Data",
    "Program Activity Data",
    "Indirect Activity Data",
    "Intervention Channels",
    "PSE Data",
    "Coalition Data",
    "Members",
    "Partnership Data"
  )
)

# list of files
pears_imports <- list()

# move wrapper function
read_xlsx_sheet <- function(sheet = 1) {
  function(file) {
    readxl::read_xlsx(file, sheet = sheet)
  }
}

# Create function?
# ONLY PULLS DATA FOR CURRENT REPORT YEAR

for (f in response) {
  key <- f$Key
  filename <- stringr::str_sub(key, 16, -1)
  # Check if file is in desired list of files
  # If TRUE, create pears_imports entry for file
  if (filename %in% pears_modules$filename) {
    # Initialize list of data frames for file's sheets 
    sheets <- list()
    for (sheet in pears_modules[pears_modules$filename == filename, ]$sheet) {
      # Read S3 object without saving it locally
      # Add data frame to list of sheets
      sheets[[sheet]] <-
        aws.s3::s3read_using(FUN = read_xlsx_sheet(sheet),
                             bucket = pears_bucket,
                             object = key)
    }
    # Add list of sheet data frames to list of files
    pears_imports[[filename]] <- sheets
  }
}

# Use dfs list for all subsequent references to PEARS raw data

sites <- readxl::read_xlsx("data-raw/Sites_Export.xlsx", sheet = "Site Data") #update file

pa <- readxl::read_xlsx("data-raw/Program_Activities_Export.xlsx", sheet = "Program Activity Data") #update file

ia <- readxl::read_xlsx("data-raw/Indirect_Activities_Export.xlsx", sheet = "Indirect Activity Data") #update file
ia_ic <- readxl::read_xlsx("data-raw/Indirect_Activities_Export.xlsx", sheet = "Intervention Channels")

pse <- readxl::read_xlsx("data-raw/PSE_Site_Activities_Export.xlsx", sheet = "PSE Data") #update file

coa <- readxl::read_xlsx("data-raw/Coalitions_Export.xlsx", sheet = "Coalition Data") #update file
coa_members <- readxl::read_xlsx("data-raw/Coalitions_Export.xlsx", sheet = "Members")

part <- readxl::read_xlsx("data-raw/Partnerships_Export.xlsx", sheet = "Partnership Data") #update file

pa2 <- dplyr::rename(pa, "program_area" = "program_areas")

sites_pa <-
  program_area_counts(pa2,
                      c("program_id", "name", "program_area", "site_id"),
                      "program_activities")

ia_ic <-
  unique_child_sites(ia, ia_ic, activity_id, title) 
sites_ia <-
  program_area_counts(ia_ic,
                      c("activity_id", "title", "program_area", "site_id"),
                      "indirect_activities")

sites_pse <-
  program_area_counts(pse,
                      c("pse_id", "name", "program_area", "site_id"),
                      "pse_site_activities")

coa_members <-
  unique_child_sites(coa, coa_members, coalition_id, name) 
sites_coa <-
  program_area_counts(coa_members,
                      c("coalition_id", "name", "program_area", "site_id"),
                      "coalitions")

sites_part <-
  program_area_counts(
    part,
    c(
      "partnership_id",
      "partnership_name",
      "program_area",
      "site_id"
    ),
    "partnerships"
  )

site_programming_dfs <- list(sites[c("site_id",
                                     "site_name",
                                     "city__county",
                                     "latitude",
                                     "longitude")],
                             sites_pa, sites_ia, sites_pse, sites_coa, sites_part)

site_programming <-
  purrr::reduce(site_programming_dfs, dplyr::left_join, by = "site_id") %>%
  dplyr::filter(
    !is.na(program_activities) |
      !is.na(pse_site_activities) |
      !is.na(coalitions) | !is.na(partnerships)
  )

site_programming <- program_bool(site_programming, "snap_ed")
site_programming <- program_bool(site_programming, "efnep")

site_programming <-
  site_programming[, c(
    "site_id",
    "site_name",
    "city__county",
    "latitude",
    "longitude",
    "snap_ed",
    "efnep"# ,
    # head following fields for inep dashboard
    # "program_activities",
    # "indirect_activities",
    # "pse_site_activities",
    # "coalitions",
    # "partnerships"
  )]

# usethis::use_data(pears, overwrite = TRUE)
