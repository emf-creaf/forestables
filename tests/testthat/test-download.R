skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)

## ffi download
test_that("ffi download works as expected", {
  inventories_path <- Sys.getenv("ffi_path")
  # assertions
  expect_error(download_inventory("FFI", inventories_path, .verbose = "TRUE"))
  expect_error(download_inventory("FFI", "/tururu/larara", .verbose = TRUE))
  expect_error(download_inventory("tururu", inventories_path, .verbose = TRUE))

  # expect_true(download_inventory("FFI", inventories_path, .verbose = FALSE))
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
  inventories_path <- Sys.getenv("ifn_path")
  # assertions
  expect_error(download_inventory("IFN", inventories_path, .verbose = "TRUE"))
  expect_error(download_inventory("IFN", "/tururu/larara", .verbose = TRUE))

  # expect_true(
  #   suppressWarnings(download_inventory("IFN", inventories_path, .verbose = FALSE))
  # )
  # important files (one of each version)
  expect_true(
    fs::file_exists(fs::path(inventories_path, "PIESMA08.DBF")) &&
      fs::file_exists(fs::path(inventories_path, "Ifn3p08.accdb")) &&
      fs::file_exists(fs::path(inventories_path, "Ifn4_Extremadura.accdb"))
  )
  # multibyte characters IFN4
  expect_length(fs::dir_ls(inventories_path, regexp = "Ifn4_Catalu"), 1)
})

## fia download
test_that("fia download works as expected", {
  inventories_path <- Sys.getenv("fia_path")
  # assertions
  expect_error(
    download_inventory("FIA", inventories_path, states = c("OR", "HI"), .verbose = "TRUE")
  )
  expect_error(
    download_inventory("FIA", "/tururu/larara", states = c("OR", "HI"), .verbose = TRUE)
  )
  expect_error(
    download_inventory("FIA", inventories_path, .verbose = TRUE)
  )

  # expect_true(
  #   download_inventory("FIA", inventories_path, states = c("OR", "HI"), .verbose = FALSE)
  # )
  # important files (one of each version)
  expect_true(
    fs::file_exists(fs::path(inventories_path, "HI_TREE.csv")) &&
      fs::file_exists(fs::path(inventories_path, "OR_TREE.csv")) &&
      fs::file_exists(fs::path(inventories_path, "HI_COND.csv")) &&
      fs::file_exists(fs::path(inventories_path, "OR_COND.csv"))
  )
})