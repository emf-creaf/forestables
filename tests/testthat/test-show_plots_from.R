# FIA -----------------------------------------------------------------------------------------

test_that("show_plots_from works for FIA", {

  # we skip this test on CRAN as we don't have access to the data there
  skip_on_cran()

  # test data
  inventory <- "FIA"
  folder <- Sys.getenv("fia_path")
  states <- c("OR", "CA", "WA")

  # tests
  expect_no_error(
    test_plots <- show_plots_from(inventory, folder, states = states)
  )
  expect_s3_class(test_plots, "sf")
  expect_named(test_plots, c("INVYR", "STATECD", "COUNTYCD", "PLOT", "geometry"))
  expect_true(all(unique(test_plots[["STATECD"]]) %in% c(6L, 41L, 53L)))
})
