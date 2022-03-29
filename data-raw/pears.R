## code to prepare `pears` dataset goes here

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
