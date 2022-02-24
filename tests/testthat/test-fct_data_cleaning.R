# pears.R function tests

test_that("program_area_counts", {
  
  program_id <- seq(1, 5)
  site_id <- c(1, 2, 2, 3, 3)
  name <- paste0("Program Activity ", program_id)
  program_area <- c("EFNEP", "SNAP-Ed", "SNAP-Ed", "SNAP-Ed", "EFNEP")
  pa <-  data.frame(program_id, name, program_area, site_id)

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
