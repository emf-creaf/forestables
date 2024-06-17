skip_on_cran()

## temp folder for inventories
inventories_path <- tempdir()

## ffi download
test_that("ffi download works as expected", {
  # assertions
  expect_error(download_inventory("ffi", inventories_path, .verbose = "TRUE"))
  expect_error(download_inventory("ffi", "/tururu/larara", .verbose = TRUE))
  expect_error(download_inventory("tururu", inventories_path, .verbose = TRUE))

  expect_true(download_inventory("ffi", inventories_path, .verbose = FALSE))
  # important files
  expect_true(
    fs::file_exists(fs::path(inventories_path, "metadonnees.csv")) &&
      fs::file_exists(fs::path(inventories_path, "PLACETTE.csv")) &&
      fs::file_exists(fs::path(inventories_path, "ARBRE.csv")) &&
      fs::file_exists(fs::path(inventories_path, "COUVERT.csv")) &&
      fs::file_exists(fs::path(inventories_path, "ECOLOGIE.csv")) &&
      fs::file_exists(fs::path(inventories_path, "espar-cdref13.csv"))
  )
})

## ifn download
test_that("ifn download works as expected", {
  # assertions
  expect_error(download_inventory("ifn", inventories_path, .verbose = "TRUE"))
  expect_error(download_inventory("ifn", "/tururu/larara", .verbose = TRUE))

  expect_true(
    suppressWarnings(download_inventory("ifn", inventories_path, .verbose = FALSE))
  )
  # important files (one of each version)
  expect_true(
    fs::file_exists(fs::path(inventories_path, "PIESMA08.DBF")) &&
      fs::file_exists(fs::path(inventories_path, "Ifn3p08.accdb")) &&
      fs::file_exists(fs::path(inventories_path, "Ifn4_Asturias.accdb"))
  )
})

## fia download
test_that("fia download works as expected", {
  # assertions
  expect_error(download_inventory("fia", inventories_path, .verbose = "TRUE"))
  expect_error(download_inventory("fia", "/tururu/larara", .verbose = TRUE))

  # too big to test :(
  # expect_true(download_inventory("fia", inventories_path, .verbose = FALSE))
})