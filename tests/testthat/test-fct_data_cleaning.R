# pears.R function tests

test_that("program_area_counts", {
  
  pa <-
    data.frame(
      program_id = seq(1, 5),
      name = paste0("Program Activity ", seq(1, 5)),
      program_area = c("EFNEP", "SNAP-Ed", "SNAP-Ed", "SNAP-Ed", "EFNEP"),
      site_id =  c(1, 2, 2, 3, 3)
    )

  # Using package-defined function
  
  sites_pa1 <-
    program_area_counts(pa,
                        c("program_id", "name", "program_area", "site_id"),
                        "program_activities")
  
  # Using manual operations
  
  sites_pa2 <-
    pa[pa$program_area %in% c("SNAP-Ed", "EFNEP") & 
         !grepl("TEST", pa$name), ]
  sites_pa2 <-
    dplyr::full_join(sites_pa2[sites_pa2$program_area == "SNAP-Ed", ] %>% dplyr::count(site_id, name = "snap_ed_program_activities"),
              sites_pa2[sites_pa2$program_area == "EFNEP", ] %>% dplyr::count(site_id, name = "efnep_program_activities"),
              by = "site_id")
  sites_pa2$program_activities <-
    rowSums(sites_pa2[, c("snap_ed_program_activities", "efnep_program_activities")],  na.rm = TRUE)
  
  expect_equal(sites_pa1, sites_pa2)
})

test_that("unique_child_sites", {
  ia <-
    data.frame(
      activity_id = seq(1, 5),
      title = paste0("Indirect Activity ", seq(1, 5)),
      program_area = c("EFNEP", "SNAP-Ed", "SNAP-Ed", "SNAP-Ed", "EFNEP")
    )
  
  ic <-
    data.frame(
      activity_id = rep(seq(1, 3), length.out = 8),
      channel_id = seq(1, 8),
      site_id = rep(c(seq(1, 5), NA), length.out = 8)
    )
  
  # Using manual operations
  
  ia_ic1 <-
    dplyr::left_join(ia, ic[c("activity_id", "channel_id", "site_id")], by = "activity_id") %>%
    dplyr::filter(!is.na(site_id)) %>%
    dplyr::distinct(activity_id, title, program_area, site_id)
  
  # Using package-defined function
  
  ia_ic2 <- unique_child_sites(ia, ic, activity_id, title)
  
  expect_equal(ia_ic1, ia_ic2)
})

test_that("program_bool", {
  site_programming <-
    data.frame(
      site_id = seq(1, 5),
      site_name = paste0("Site ", seq(1, 5)),
      snap_ed_program_activities = c(NA, 1, 3, NA, NA),
      efnep_program_activities = c(1, 3, NA, NA, 2),
      snap_ed_indirect_activities = c(NA, NA, 1, NA, 2),
      efnep_indirect_activities = c(1, 3, NA, NA, NA)
    )
  
  # Using manual operations
  
  site_programming1 <- site_programming
  
  cols <- colnames(site_programming1)
  program <- "snap_ed"
  program_cols <- cols[grepl(paste0("^", program), cols)]
  site_programming1[program] <-rowSums(site_programming1[, program_cols],  na.rm = TRUE)
  site_programming1[program] <- ifelse(site_programming1[program] > 0, "Yes", "No")
  
  cols <- colnames(site_programming1)
  program <- "efnep"
  program_cols <- cols[grepl(paste0("^", program), cols)]
  site_programming1[program] <-rowSums(site_programming1[, program_cols],  na.rm = TRUE)
  site_programming1[program] <- ifelse(site_programming1[program] > 0, "Yes", "No")
  
  # Using package-defined function
  
  site_programming2 <- program_bool(site_programming, "snap_ed")
  site_programming2 <- program_bool(site_programming2, "efnep")
  
  expect_equal(site_programming1, site_programming2)
})

# map module function tests

test_that ("ff_query", {
  # Using default parameters
  
  q1 <- ff_query(key = "1234")
  q2 <- "https://api-v2-prod-dot-foodfinder-183216.uc.r.appspot.com/partners/providers?key=1234&limit=10000&offset=1&format=1&min_lat=36.7335953714124&max_lat=42.816745766624&min_lon=-91.9198367832536&max_lon-87.3674917331893"
  expect_equal(q1, q2)
})

# test ff_import?

# census.R function tests

# test clean_census_data by comparing current output to previous outputs