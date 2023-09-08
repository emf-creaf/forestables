# test data -----------------------------------------------------------------------------------

test_plots <- list(
  "01" = 1404119,
  "10" = 900863,
  "11" = c(1436508, 1410492),
  "17" = 1416895,
  "19" = 1407238,
  "27" = c(960830, 923517),
  "2A" = 973917,
  "32" = 1425386,
  "34" = 977193,
  "35" = 1404830,
  "36" = 1438616,
  "39" = 1424577,
  "42" = c(1422347, 939340),
  "44" = 920801,
  "51" = 938482,
  "59" = 960829,
  "64" = 912307,
  "76" = 951430,
  "80" = c(1417044,1452529),
  "81" = c(1428398, 973950),
  "86" = c(957495,921133),
  "87" = c(975666,979897),
  "89" = 1433956,
  "91" = 1406115,
  "tururu" = 3555
)

test_year <- 2019
test_departments <- names(test_plots)
test_folder <- Sys.getenv("ffi_path")
test_input <- .build_ffi_input_with(
  test_departments, test_year, test_plots, test_folder,
  .verbose = FALSE
)
test_metadonnees <- readr::read_delim(file = fs::path(test_folder, "metadonnees.csv"), skip = 412) |>
  dplyr::rename(UNITE = "// Unité") |>
  dplyr::as_tibble()

test_espar_cdref <- .read_inventory_data(
  fs::path(test_folder, "espar-cdref13.csv"),
  colClasses = list(character = c( "// espar")),
  header = TRUE
) |>
  dplyr::as_tibble() |>
  dplyr::rename(
    ESPAR = "// espar",
    Libellé  = lib_espar
  ) |>
  #i need to change this because in the file csv it is recorded as "2" and in tree table as "02"
  dplyr::mutate(ESPAR = dplyr::case_when(
    ESPAR == "2" ~"02",
    ESPAR == "3" ~"03",
    ESPAR == "4" ~"04",
    ESPAR == "5" ~"05",
    ESPAR == "6" ~"06",
    ESPAR == "7" ~"07",
    ESPAR == "9" ~"09",
    TRUE ~ ESPAR
  )) |>
  dplyr::arrange(ESPAR)

test_idp_def_ref <- .read_inventory_data(
  fs::path(test_folder, "PLACETTE.csv"),
  select = c(
    "IDP",
    "DEP"
  ),
  colClasses = list(character = c( "IDP","DEP"))
) |>
  tibble::as_tibble() |>
  unique()

# table functions -----------------------------------------------------------------------------
test_that("ffi_plot_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "COUNTRY", "DEP", "DEP_NAME", "PLOT", "YEAR", "VISITE", "COORD_SYS", "XL",
    "XL_ORIGINAL", "YL", "YL_ORIGINAL", "EXPO", "EXPO_ORIGINAL", "PENT2",  "PENT2_ORIGINAL",
    "LIGN1", "LIGN2", "HERB"
  )

  # object
  expect_s3_class(
    test_res <- ffi_plot_table_process(
      test_input$plot_table[1],
      test_input$soils_table[1],
      test_input$plots[1],
      test_year,
      test_metadonnees
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$DEP), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1])
  expect_identical(unique(test_res$DEP) |> as.character(), test_input$department[1])

  # errors
  expect_warning(
    test_error <- ffi_plot_table_process(
      NA_character_,
      test_input$soils_table[1],
      test_input$plots[1],
      test_year,
      test_metadonnees
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
  expect_warning(
    test_error <- ffi_plot_table_process(
      test_input$plot_table[1],
      NA_character_,
      test_input$plots[1],
      test_year,
      test_metadonnees
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
})

test_that("ffi_tree_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "PLOT", "DEP", "YEAR", "TREE", "ESPAR", "SP_CODE",
    "SP_NAME", "STATUS", "VEGET5", "DIA", "HT", "DENSITY"
  )

  # object
  expect_s3_class(
    test_res <- ffi_tree_table_process(
      test_input$tree_table[1],
      test_input$plots[1],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$DEP), 1)

  expect_identical(unique(test_res$YEAR), test_year |> as.integer())
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.character())
  expect_identical(unique(test_res$DEP) |> as.character(), test_input$department[1])

  # errors
  expect_warning(
    test_error <- ffi_tree_table_process(
      NA_character_,
      test_input$plots[1],
      test_year,
      test_espar_cdref,
      test_idp_def_ref
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
})
