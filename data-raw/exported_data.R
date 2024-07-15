## output examples (external exported data)

# IFN (Barcelona)
future::plan(future::multisession)
ifn_output_example <- ifn_to_tibble(
  provinces = "08",
  versions = c("ifn2", "ifn3", "ifn4"),
  folder = Sys.getenv("ifn_path")
)

# FIA (Alaska)
fia_output_example <- fia_to_tibble(
  "AK",
  years = 2015:2018,
  folder = Sys.getenv("fia_path")
)

# FFI (Loire)
ffi_output_example <- ffi_to_tibble(
  "42",
  years = 2015:2018,
  folder = Sys.getenv("ffi_path")
)

usethis::use_data(
  ifn_output_example,
  fia_output_example,
  ffi_output_example,
  overwrite = TRUE, internal = FALSE
)
