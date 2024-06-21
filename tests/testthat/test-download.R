skip_on_cran()

## temp folder for inventories
inventories_path <- tempdir()

## ffi download
test_that("ffi download works as expected", {
  # assertions
  expect_error(download_inventory("FFI", inventories_path, .verbose = "TRUE"))
  expect_error(download_inventory("FFI", "/tururu/larara", .verbose = TRUE))
  expect_error(download_inventory("tururu", inventories_path, .verbose = TRUE))

  expect_true(download_inventory("FFI", inventories_path, .verbose = FALSE))
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
  expect_error(download_inventory("IFN", inventories_path, .verbose = "TRUE"))
  expect_error(download_inventory("IFN", "/tururu/larara", .verbose = TRUE))

  expect_true(
    suppressWarnings(download_inventory("IFN", inventories_path, .verbose = FALSE))
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
  expect_error(
    download_inventory("FIA", inventories_path, states = c("AK", "HI"), .verbose = "TRUE")
  )
  expect_error(
    download_inventory("FIA", "/tururu/larara", states = c("AK", "HI"), .verbose = TRUE)
  )
  expect_error(
    download_inventory("FIA", inventories_path, .verbose = TRUE)
  )

  expect_true(
    download_inventory("FIA", inventories_path, states = c("AK", "HI"), .verbose = FALSE)
  )
  # important files (one of each version)
  expect_true(
    fs::file_exists(fs::path(inventories_path, "HI_TREE.csv")) &&
      fs::file_exists(fs::path(inventories_path, "AK_TREE.csv")) &&
      fs::file_exists(fs::path(inventories_path, "HI_COND.csv")) &&
      fs::file_exists(fs::path(inventories_path, "AK_COND.csv"))
  )
})