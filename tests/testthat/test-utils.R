# Verbose messaging ---------------------------------------------------------------------------

test_that("verbose_msg works as intended", {

  # .verbose with message works
  expect_message(
    verbose_msg(message("foo"), .verbose = TRUE), "foo"
  )
  expect_no_message(
    verbose_msg(message("foo"), .verbose = FALSE)
  )

  # cli::* works
  expect_message(
    verbose_msg(cli::cli_inform(".verbose is TRUE"), .verbose = TRUE), ".verbose is TRUE"
  )
  expect_no_message(
    verbose_msg(cli::cli_inform(".verbose is FALSE"), .verbose = FALSE)
  )

  # expect errors if something is wrong
  expect_error(
    verbose_msg(message("foo"), .verbose = 4)
  )
  expect_error(
    verbose_msg(message("foo"), .verbose = "4")
  )
})

test_that(".read_inventory_data returns lazy_dt for fia", {
  test_file <- fs::path("/data/creaf/projects/emf/international_inventories/data/fia/FIA_DATAMART_MARCH_2023/OR_PLOT.csv")
  test_cmd <- glue::glue("grep -E ',INVYR,|,25,(84167|84167.0),' {test_file}")

  expect_s3_class(.read_inventory_data(test_file), "dtplyr_step_first")
  expect_s3_class(.read_inventory_data(test_cmd), "dtplyr_step_first")
})

test_that(".read_inventory_data returns lazy_dt for ffi", {

  test_file <- fs::path(Sys.getenv("ffi_path"), "PLACETTE.csv")
  test_cmd <- glue::glue('grep -E "CAMPAGNE|.*;900863;.*;10;" {test_file}')

  #ecologie table
  test_file <- fs::path(Sys.getenv("ffi_path"), "ECOLOGIE.csv")
  test_cmd <- glue::glue('grep -E "CAMPAGNE|.*;900863;" {test_file}')

  #flore arbre table
  test_file<- fs::path(Sys.getenv("ffi_path"), "FLORE.csv")
  test_cmd <- glue::glue('grep -E "CAMPAGNE|.*;900863;" {test_file}' )


  #
  expect_s3_class( .read_inventory_data(test_file, header = TRUE), "dtplyr_step_first")
  expect_s3_class( .read_inventory_data(test_cmd,header = TRUE), "dtplyr_step_first")
})
