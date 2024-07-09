## Setup for testing
# This should be skipped in CRAN as we don't want to try to download anything there, but should be
# run in GitHub Actions, to test the package in the 3 platforms (Linux, Win, Mac).
skip_on_cran()
# Also, in main branch in github actions we dont want to run the expensive tests,
# as the pkgdown already will warn us if something is wrong, and the devel branch
# is always tested fully
skip_if(Sys.getenv("full_testing") == "true", message = "Full test skipped")

## Temporal folder setup
inventories_tmp <- tempdir()

## Download inventories
# FFI
download_inventory("FFI", inventories_tmp, .verbose = TRUE)
# IFN
suppressWarnings(download_inventory("IFN", inventories_tmp, .verbose = TRUE))
# FIA
download_inventory(
  "FIA", inventories_tmp, .verbose = TRUE,
  states = c("HI", "OR", "DE", "ND", "NE")
)

## Set the environment vars
old_ffi_path <- Sys.getenv("ffi_path")
old_ifn_path <- Sys.getenv("ifn_path")
old_fia_path <- Sys.getenv("fia_path")
Sys.setenv("ffi_path" = inventories_tmp)
Sys.setenv("ifn_path" = inventories_tmp)
Sys.setenv("fia_path" = inventories_tmp)

## Clean after
withr::defer(
  {
    Sys.setenv("ffi_path" = old_ffi_path)
    Sys.setenv("ifn_path" = old_ifn_path)
    Sys.setenv("fia_path" = old_fia_path)
    unlink(inventories_tmp)
  },
  teardown_env()
)