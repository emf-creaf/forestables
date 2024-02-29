skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
# test data -----------------------------------------------------------------------------------
general_plots <- ifn_plots_thesaurus |>
  dplyr::filter(
    PROVINCIA %in% c(
      "01", "02", "03", "04", "05", "06", "07", "10", "12", "13", "17",
      "23", "27", "30", "33", "31", "38", "40", "50", "49"
    ),
    ESTADILLO %in% (c(
      0, 5, 11, 14, 19, 23, 51, 61,
      78, 80, 90, 99, 105, 114, 135, 156,
      172, 190, 233, 261, 269, 283, 325, 328,
      377, 412, 419, 426, 444, 445, 479, 499,
      532, 537, 626, 629, 672, 679, 744, 761,
      783, 818, 1021, 1057, 1120, 1138, 1216, 1223,
      1460, 1463, 1483, 1518, 1557, 1728, 1839, 2003,
      2064, 2944, 3374, 1406115
    ) |> stringr::str_pad(width = 4, side = "left", pad = "0"))
  )

test_ifn2_plots <- general_plots |>
  dplyr::filter(
    class_ifn2 != "xx"
  ) |>
  dplyr::select(id_code, PROVINCIA) |>
  dplyr::group_by(PROVINCIA) |>
  dplyr::summarise(plots = list(id_code), .groups = "keep") |>
  dplyr::group_map(.f = \(province_plots, province_code) {
    tibble::deframe(province_plots) |>
      # list() |>
      purrr::set_names(province_code[[1]])
  }) |>
  purrr::flatten()
test_ifn2_plots$tururu <- c("tururu_0355_NN_A1_A1")
test_ifn2_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")
test_ifn2_provinces <- names(test_ifn2_plots)
test_ifn2_folder <- Sys.getenv("ifn_path")
test_ifn2_especies <- species_ifn_internal
test_ifn2_provinces_dictionary <- ifn_provinces_dictionary
test_ifn2_version <- "ifn2"
test_ifn2_input <- suppressWarnings(
  .build_ifn_input_with(
    test_ifn2_version,
    test_ifn2_provinces,
    test_ifn2_plots,
    test_ifn2_folder,
    .verbose = TRUE
  )
)

test_ifn3_plots <- general_plots |>
  dplyr::filter(
    class_ifn3 != "xx"
  ) |>
  dplyr::select(id_code, PROVINCIA) |>
  dplyr::group_by(PROVINCIA) |>
  dplyr::summarise(plots = list(id_code), .groups = "keep") |>
  dplyr::group_map(.f = \(province_plots, province_code) {
    tibble::deframe(province_plots) |>
      # list() |>
      purrr::set_names(province_code[[1]])
  }) |>
  purrr::flatten()
test_ifn3_plots$tururu <- c("tururu_0355_NN_A1_A1")
test_ifn3_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")
test_ifn3_provinces <- names(test_ifn3_plots)
test_ifn3_folder <- Sys.getenv("ifn_path")
test_ifn3_especies <- species_ifn_internal
test_ifn3_provinces_dictionary <- ifn_provinces_dictionary
test_ifn3_version <- "ifn3"
test_ifn3_input <- suppressWarnings(
  .build_ifn_input_with(
    test_ifn3_version,
    test_ifn3_provinces,
    test_ifn3_plots,
    test_ifn3_folder,
    .verbose = TRUE
  )
)

test_ifn4_plots <- general_plots |>
  dplyr::filter(
    class_ifn4 != "xx"
  ) |>
  dplyr::select(id_code, PROVINCIA) |>
  dplyr::group_by(PROVINCIA) |>
  dplyr::summarise(plots = list(id_code), .groups = "keep") |>
  dplyr::group_map(.f = \(province_plots, province_code) {
    tibble::deframe(province_plots) |>
      # list() |>
      purrr::set_names(province_code[[1]])
  }) |>
  purrr::flatten()
test_ifn4_plots$tururu <- c("tururu_0355_NN_A1_A1")
test_ifn4_plots$`91` <- c("91_6115_NN_A1_A1", "91_0000_NN_A1_A1")
test_ifn4_provinces <- names(test_ifn4_plots)
test_ifn4_folder <- Sys.getenv("ifn_path")
test_ifn4_especies <- species_ifn_internal
test_ifn4_provinces_dictionary <- ifn_provinces_dictionary
test_ifn4_version <- "ifn4"
test_ifn4_input <- suppressWarnings(
  .build_ifn_input_with(
    test_ifn4_version,
    test_ifn4_provinces,
    test_ifn4_plots,
    test_ifn4_folder,
    .verbose = TRUE
  )
)

# individual tables ---------------------------------------------------------------------------

test_that("ifn_tree_table_process for ifn2 works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "PLOT",
    "SP_CODE",
    "SP_NAME",
    "DIA",
    "HT",
    "DENSITY"
  )

  expect_s3_class(
    test_ifn2_res <- ifn_tree_table_process(
      test_ifn2_input$tree_table[3],
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn2_res) > 0)


  expect_length(unique(test_ifn2_res$ID_UNIQUE_PLOT), 1)
  expect_length(unique(test_ifn2_res$province_code), 1)


  expect_identical(
    unique(test_ifn2_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn2_input$plots[3] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn2_res$province_code) |> as.numeric(),
    test_ifn2_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn2_error <- ifn_tree_table_process(
      NA_character_,
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn2_error, "tbl")
  expect_true(nrow(test_ifn2_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn2_input$tree_table[nrow(test_ifn2_input) - 2],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input) - 2],
      test_ifn2_input$province[nrow(test_ifn2_input) - 2],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn2_input$tree_table[nrow(test_ifn2_input)],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input)],
      test_ifn2_input$province[nrow(test_ifn2_input)],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
})


test_that("ifn_tree_table_process for ifn3 works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "Clase",
    "Subclase",
    "PLOT",
    "SP_CODE",
    "SP_NAME",
    #tree number id in ifn4
    "nArbol",
    #CUALIDAD 6 = dead but providing functions
    "Calidad",
    "Forma",
    #check codes to understand origin and trace of individuals
    "OrdenIf2",
    "OrdenIf3",
    #diameter in cm
    "DIA",
    #height in m
    "HT",
    "DENSITY"
  )


  # object
  expect_s3_class(
    test_ifn3_res <- ifn_tree_table_process(
      test_ifn3_input$tree_table[3],
      test_ifn3_version,
      test_ifn3_input$plots[3],
      test_ifn3_input$province[3],
      test_ifn3_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn3_res) > 0)


  expect_length(unique(test_ifn3_res$ID_UNIQUE_PLOT), 1)
  expect_length(unique(test_ifn3_res$province_code), 1)


  expect_identical(
    unique(test_ifn3_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn3_input$plots[3] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn3_res$province_code) |> as.numeric(),
    test_ifn3_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn3_error <- ifn_tree_table_process(
      NA_character_,
      test_ifn3_version,
      test_ifn3_input$plots[3],
      test_ifn3_input$province[3],
      test_ifn3_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn3_error, "tbl")
  expect_true(nrow(test_ifn3_error) < 1)

  # error in province name, gives an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn3_input$tree_table[nrow(test_ifn3_input) - 2],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input) - 2],
      test_ifn3_input$province[nrow(test_ifn3_input) - 2],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn3_input$tree_table[nrow(test_ifn3_input)],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input)],
      test_ifn3_input$province[nrow(test_ifn3_input)],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)


})

test_that("ifn_tree_table_process for ifn4 works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "Clase",
    "Subclase",
    "PLOT",
    "SP_CODE",
    "SP_NAME",
    # #tree number id in ifn4
    "nArbol",
    #CUALIDAD 6 = dead but providing functions
    "Calidad",
    "Forma",
    #check codes to understand origin and trace of individuals
    "OrdenIf3",
    "OrdenIf4",
    #diameter in cm
    "DIA",
    #height in m
    "HT",
    "DENSITY"
  )


  # object
  expect_s3_class(
    test_ifn4_res <- ifn_tree_table_process(
      test_ifn4_input$tree_table[1],
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn4_res) > 0)


  expect_length(unique(test_ifn4_res$ID_UNIQUE_PLOT), 1)
  expect_length(unique(test_ifn4_res$province_code), 1)


  expect_identical(
    unique(test_ifn4_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn4_input$plots[1] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn4_res$province_code) |> as.numeric(),
    test_ifn4_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn4_error <- ifn_tree_table_process(
      NA_character_,
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn4_error, "tbl")
  expect_true(nrow(test_ifn4_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn4_input$tree_table[nrow(test_ifn4_input) - 2],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input) - 2],
      test_ifn4_input$province[nrow(test_ifn4_input) - 2],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_tree_table_process(
      test_ifn4_input$tree_table[nrow(test_ifn4_input)],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input)],
      test_ifn4_input$province[nrow(test_ifn4_input)],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)


})

test_that("ifn_shrub_table_process for ifn2 works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "PLOT",
    "SP_CODE",
    "SP_NAME",
    "HT",
    "COVER"
  )

  # object
  expect_s3_class(
    test_ifn2_res <- ifn_shrub_table_process(
      test_ifn2_input$shrub_table[1],
      test_ifn2_version,
      test_ifn2_input$plots[1],
      test_ifn2_input$province[1],
      test_ifn2_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn2_res) > 0)


  expect_length(unique(test_ifn2_res$PLOT), 1)
  expect_length(unique(test_ifn2_res$province_code), 1)


  expect_identical(
    unique(test_ifn2_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn2_input$plots[1]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn2_res$province_code) |> as.numeric(),
    test_ifn2_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn2_error <- ifn_shrub_table_process(
      NA_character_,
      test_ifn2_version,
      test_ifn2_input$plots[1],
      test_ifn2_input$province[1],
      test_ifn2_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn2_error, "tbl")
  expect_true(nrow(test_ifn2_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn2_input$shrub_table[nrow(test_ifn2_input) - 2],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input) - 2],
      test_ifn2_input$province[nrow(test_ifn2_input) - 2],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn2_input$shrub_table[nrow(test_ifn2_input)],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input)],
      test_ifn2_input$province[nrow(test_ifn2_input)],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
})
test_that("ifn_shrub_table_process for ifn3 works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "Clase",
    "Subclase",
    "PLOT",
    "SP_NAME",
    "SP_CODE",
    "HT",
    "COVER"
  )

  # object
  expect_s3_class(
    test_ifn3_res <- ifn_shrub_table_process(
      test_ifn3_input$shrub_table[1],
      test_ifn3_version,
      test_ifn3_input$plots[1],
      test_ifn3_input$province[1],
      test_ifn3_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn3_res) > 0)


  expect_length(unique(test_ifn3_res$PLOT), 1)
  expect_length(unique(test_ifn3_res$province_code), 1)


  expect_identical(
    unique(test_ifn3_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn3_input$plots[1]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn3_res$province_code) |> as.numeric(),
    test_ifn3_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn3_error <- ifn_shrub_table_process(
      NA_character_,
      test_ifn3_version,
      test_ifn3_input$plots[1],
      test_ifn3_input$province[1],
      test_ifn3_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn3_error, "tbl")
  expect_true(nrow(test_ifn3_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn3_input$shrub_table[nrow(test_ifn3_input) - 2],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input) - 2],
      test_ifn3_input$province[nrow(test_ifn3_input) - 2],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn3_input$shrub_table[nrow(test_ifn3_input)],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input)],
      test_ifn3_input$province[nrow(test_ifn3_input)],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
})
test_that("ifn_shrub_table_process for ifn4 works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "Clase",
    "Subclase",
    "PLOT",
    "SP_NAME",
    "SP_CODE",
    "HT",
    "COVER"
  )

  # object
  expect_s3_class(
    test_ifn4_res <- ifn_shrub_table_process(
      test_ifn4_input$shrub_table[1],
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn4_res) > 0)


  expect_length(unique(test_ifn4_res$PLOT), 1)
  expect_length(unique(test_ifn4_res$province_code), 1)


  expect_identical(
    unique(test_ifn4_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn4_input$plots[1] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn4_res$province_code) |> as.numeric(),
    test_ifn4_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn4_error <- ifn_shrub_table_process(
      NA_character_,
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn4_error, "tbl")
  expect_true(nrow(test_ifn4_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn4_input$shrub_table[nrow(test_ifn4_input) - 2],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input) - 2],
      test_ifn4_input$province[nrow(test_ifn4_input) - 2],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_shrub_table_process(
      test_ifn4_input$shrub_table[nrow(test_ifn4_input)],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input)],
      test_ifn4_input$province[nrow(test_ifn4_input)],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
})



test_that("ifn_regen_table_process for ifn2 works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "PLOT",
    "SP_CODE",
    "SP_NAME",
    "DBH",
    "Height",
    "N",
    "DENSITY"
  )

  # object
  expect_s3_class(
    test_ifn2_res <- ifn_regen_table_process(
      test_ifn2_input$regen_table[3],
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn2_res) > 0)


  expect_length(unique(test_ifn2_res$PLOT), 1)
  expect_length(unique(test_ifn2_res$province_code), 1)


  expect_identical(
    unique(test_ifn2_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn2_input$plots[3] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn2_res$province_code) |> as.numeric(),
    test_ifn2_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn2_error <- ifn_regen_table_process(
      NA_character_,
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn2_error, "tbl")
  expect_true(nrow(test_ifn2_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn2_input$regen_table[nrow(test_ifn2_input) - 2],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input) - 2],
      test_ifn2_input$province[nrow(test_ifn2_input) - 2],
      test_ifn2_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(
      ifn_regen_table_process(
        test_ifn2_input$regen_table[nrow(test_ifn2_input)],
        test_ifn2_version,
        test_ifn2_input$plots[nrow(test_ifn2_input)],
        test_ifn2_input$province[nrow(test_ifn2_input)],
        test_ifn2_especies
      )
    ),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
})
test_that("ifn_regen_table_process for ifn3 works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "PLOT",
    "Clase",
    "Subclase",
    "SP_CODE",
    "SP_NAME",
    "DBH",
    "Height",
    "N",
    "DENSITY"
  )

  # object
  expect_s3_class(
    test_ifn3_res <- ifn_regen_table_process(
      test_ifn3_input$regen_table[1],
      test_ifn3_version,
      test_ifn3_input$plots[1],
      test_ifn3_input$province[1],
      test_ifn3_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn3_res) > 0)


  expect_length(unique(test_ifn3_res$PLOT), 1)
  expect_length(unique(test_ifn3_res$province_code), 1)


  expect_identical(
    unique(test_ifn3_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn3_input$plots[1] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn3_res$province_code) |> as.numeric(),
    test_ifn3_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn3_error <- ifn_regen_table_process(
      NA_character_,
      test_ifn3_version,
      test_ifn3_input$plots[3],
      test_ifn3_input$province[3],
      test_ifn3_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn3_error, "tbl")
  expect_true(nrow(test_ifn3_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn3_input$regen_table[nrow(test_ifn3_input) - 2],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input) - 2],
      test_ifn3_input$province[nrow(test_ifn3_input) - 2],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn3_input$regen_table[nrow(test_ifn3_input)],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input)],
      test_ifn3_input$province[nrow(test_ifn3_input)],
      test_ifn3_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
})
test_that("ifn_regen_table_process for ifn4 works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "province_code",
    "PLOT",
    "Clase",
    "Subclase",
    "SP_CODE",
    "SP_NAME",
    "DBH",
    "Height",
    "N",
    "DENSITY"
  )

  # object
  expect_s3_class(
    test_ifn4_res <- ifn_regen_table_process(
      test_ifn4_input$regen_table[3],
      test_ifn4_version,
      test_ifn4_input$plots[3],
      test_ifn4_input$province[3],
      test_ifn4_especies
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn4_res) > 0)


  expect_length(unique(test_ifn4_res$PLOT), 1)
  expect_length(unique(test_ifn4_res$province_code), 1)


  expect_identical(
    unique(test_ifn4_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn4_input$plots[3]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn4_res$province_code) |> as.numeric(),
    test_ifn4_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn4_error <- ifn_regen_table_process(
      NA_character_,
      test_ifn4_version,
      test_ifn4_input$plots[1],
      test_ifn4_input$province[1],
      test_ifn4_especies
    ),
    "Some files"
  )
  expect_s3_class(test_ifn4_error, "tbl")
  expect_true(nrow(test_ifn4_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn4_input$regen_table[nrow(test_ifn4_input) - 2],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input) - 2],
      test_ifn4_input$province[nrow(test_ifn4_input) - 2],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_regen_table_process(
      test_ifn4_input$regen_table[nrow(test_ifn4_input)],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input)],
      test_ifn4_input$province[nrow(test_ifn4_input)],
      test_ifn4_especies
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
})


test_that("ifn_plot_table_process for ifn2  works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "COUNTRY",
    "province_code",
    "province_name_original",
    "ca_name_original",
    "PLOT",
    "YEAR",
    "version",
    "HOJA",
    "Huso",
    "COORDEX",
    "COORDEY",
    "COORD_SYS",
    "crs",
    "PENDIEN2",
    "SLOPE",
    "ELEV",
    "ASPECT"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn2_res <- ifn_plot_table_process(
      test_ifn2_input$plot_table[3],
      test_ifn2_input$coord_table[3],
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_provinces_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn2_res) > 0)

  expect_length(unique(test_ifn2_res$PLOT), 1)
  expect_length(unique(test_ifn2_res$province_code), 1)

  expect_identical(
    unique(test_ifn2_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn2_input$plots[3] |> as.character()
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn2_res$province_code) |> as.numeric(),
    test_ifn2_input$province[3] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn2_error <- ifn_plot_table_process(
      NA_character_,
      test_ifn2_input$coord_table[3],
      test_ifn2_version,
      test_ifn2_input$plots[3],
      test_ifn2_input$province[3],
      test_ifn2_provinces_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_ifn2_error, "tbl")
  expect_true(nrow(test_ifn2_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn2_input$plot_table[nrow(test_ifn2_input) - 2],
      test_ifn2_input$coord_table[nrow(test_ifn2_input) - 2],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input) - 2],
      test_ifn2_input$province[nrow(test_ifn2_input) - 2],
      test_ifn2_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn2_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn2_input$plot_table[nrow(test_ifn2_input)],
      test_ifn2_input$coord_table[nrow(test_ifn2_input)],
      test_ifn2_version,
      test_ifn2_input$plots[nrow(test_ifn2_input)],
      test_ifn2_input$province[nrow(test_ifn2_input)],
      test_ifn2_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn2_error) < 1)
})

test_that("ifn_plot_table_process for ifn3  works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "COUNTRY",
    "YEAR",
    "ca_name_original",
    "province_code",
    "province_name_original",
    "PLOT",
    "Cla",
    "Subclase",
    "version",
    "Tipo",
    "ASPECT",
    "SLOPE",
    "crs",
    "COORD_SYS",
    "COORDEX",
    "COORDEY",
    "HOJA",
    "Huso"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn3_res <- ifn_plot_table_process(
      test_ifn3_input$plot_table[6],
      test_ifn3_input$coord_table[6],
      test_ifn3_version,
      test_ifn3_input$plots[6],
      test_ifn3_input$province[6],
      test_ifn3_provinces_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn3_res) > 0)

  expect_length(unique(test_ifn3_res$PLOT), 1)
  expect_length(unique(test_ifn3_res$province_code), 1)

  expect_identical(
    unique(test_ifn3_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn3_input$plots[6]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn3_res$province_code) |> as.numeric(),
    test_ifn3_input$province[6] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn3_error <- ifn_plot_table_process(
      NA_character_,
      test_ifn3_input$coord_table[6],
      test_ifn3_version,
      test_ifn3_input$plots[6],
      test_ifn3_input$province[6],
      test_ifn3_provinces_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_ifn3_error, "tbl")
  expect_true(nrow(test_ifn3_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn3_input$plot_table[nrow(test_ifn3_input) - 2],
      test_ifn3_input$coord_table[nrow(test_ifn3_input) - 2],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input) - 2],
      test_ifn3_input$province[nrow(test_ifn3_input) - 2],
      test_ifn3_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn3_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn3_input$plot_table[nrow(test_ifn3_input)],
      test_ifn3_input$coord_table[nrow(test_ifn3_input)],
      test_ifn3_version,
      test_ifn3_input$plots[nrow(test_ifn3_input)],
      test_ifn3_input$province[nrow(test_ifn3_input)],
      test_ifn3_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn3_error) < 1)
})

test_that("ifn_plot_table_process for ifn4  works as intended", {
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "COUNTRY",
    "YEAR",
    "ca_name_original",
    "province_code",
    "province_name_original",
    "PLOT",
    "Cla",
    "Subclase",
    "version",
    "Tipo",
    "ASPECT",
    "SLOPE",
    "crs",
    "COORD_SYS",
    "COORDEX",
    "COORDEY",
    "HOJA",
    "Huso"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn4_res <- ifn_plot_table_process(
      test_ifn4_input$plot_table[3],
      test_ifn4_input$coord_table[3],
      test_ifn4_version,
      test_ifn4_input$plots[3],
      test_ifn4_input$province[3],
      test_ifn4_provinces_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_ifn4_res) > 0)

  expect_length(unique(test_ifn4_res$PLOT), 1)
  expect_length(unique(test_ifn4_res$province_code), 1)

  expect_identical(
    unique(test_ifn4_res$ID_UNIQUE_PLOT) |> as.character(),
    test_ifn4_input$plots[3]
  )
  #CHECK THIS AGAIN, BOTH SHOULD BE CHARACTER
  expect_identical(
    unique(test_ifn4_res$province_code) |> as.numeric(),
    test_ifn4_input$province[1] |> as.numeric()
  )

  # errors
  expect_warning(
    test_ifn4_error <- ifn_plot_table_process(
      NA_character_,
      test_ifn4_input$coord_table[6],
      test_ifn4_version,
      test_ifn4_input$plots[6],
      test_ifn4_input$province[6],
      test_ifn4_provinces_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_ifn4_error, "tbl")
  expect_true(nrow(test_ifn4_error) < 1)

  # error in department name, gives an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn4_input$plot_table[nrow(test_ifn4_input) - 2],
      test_ifn4_input$coord_table[nrow(test_ifn4_input) - 2],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input) - 2],
      test_ifn4_input$province[nrow(test_ifn4_input) - 2],
      test_ifn4_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
  # error in plot name, should return an empty tibble
  expect_s3_class(
    test_ifn4_error <- suppressWarnings(ifn_plot_table_process(
      test_ifn4_input$plot_table[nrow(test_ifn4_input)],
      test_ifn4_input$coord_table[nrow(test_ifn4_input)],
      test_ifn4_version,
      test_ifn4_input$plots[nrow(test_ifn4_input)],
      test_ifn4_input$province[nrow(test_ifn4_input)],
      test_ifn4_provinces_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_ifn4_error) < 1)
})

# tables process -----------------------------------------------------------------------------------

test_that("ifn_tables_process ifn2 works as intended", {

  ### TODO
  # - test what happens when some tables are NAs (not found when building the input)
  # -
  #

  # tests config
  test_ifn2_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # we have 1000 plots in ifn2_plots, so we do reduce a little, because it takes around
  # 4 minutes.
  test_ifn2_plots <- test_ifn2_plots |>
    purrr::map(\(x) {
      sample(x, 5, replace = TRUE) |> unique()
    })

  # tests data
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "COUNTRY",
    "YEAR",
    "ca_name_original",
    "province_name_original",
    "province_code",
    "PLOT",
    "version",
    "HOJA",
    "Huso",
    "COORD_SYS",
    "COORD1",
    "COORD2",
    "crs",
    "PENDIEN2",
    "SLOPE",
    "ELEV",
    "ASPECT",
    "tree",
    "understory",
    "regen"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn2_res <- suppressWarnings(ifn_tables_process(
      test_ifn2_provinces, test_ifn2_version, test_ifn2_plots, test_ifn2_folder,
      .parallel_options = test_ifn2_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn2_res, expected_names)
  expect_true(all(unique(test_ifn2_res$province_code) %in% names(test_ifn2_plots)))

  ### missing tables/plots
  # tururu and "91" province shouldn't appear
  # inexistent plots shouldn't also appear
  expect_false("tururu" %in% unique(test_ifn2_res$province_code))
  expect_identical(
    nrow(test_ifn2_res),
    ((test_ifn2_plots |> purrr::flatten() |> length()) - 3) |> as.integer()
  )

  ### missing random files
  # we test here what happens when some files are missing (ARBRE, ECOLOGIE...)
  # to do:



})

test_that("ifn_tables_process ifn3 works as intended", {

  ### TODO
  # - test what happens when some tables are NAs (not found when building the input)
  # -
  #

  # tests config
  test_ifn3_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # we have 1000 plots in ifn2_plots, so we do reduce a little, because it takes around
  # 4 minutes.
  test_ifn3_plots <- test_ifn3_plots |>
    purrr::map(\(x) {
      sample(x, 5, replace = TRUE) |> unique()
    })

  # tests data
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "COUNTRY",
    "YEAR",
    "ca_name_original",
    "province_name_original",
    "province_code",
    "PLOT",
    "Cla",
    "Subclase",
    "version",
    "Tipo",
    "HOJA",
    "Huso",
    "COORD_SYS",
    "COORD1",
    "COORD2",
    "crs",
    "SLOPE",
    "ASPECT",
    "tree",
    "understory",
    "regen"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn3_res <- suppressWarnings(ifn_tables_process(
      test_ifn3_provinces, test_ifn3_version, test_ifn3_plots, test_ifn3_folder,
      .parallel_options = test_ifn3_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn3_res, expected_names)
  expect_true(all(unique(test_ifn3_res$province_code) %in% names(test_ifn3_plots)))

  # ### missing tables/plots
  # # tururu state shouldn't appear
  # # inexistent plots (91-0) shouldn't
  # # be present, so 12 of 14 elements in filter list
  expect_false("tururu" %in% unique(test_ifn3_res$province_code))
  expect_identical(
    nrow(test_ifn3_res), as.integer((test_ifn3_plots |> purrr::flatten() |> length()) - 3)
  )

  # ### missing random files
  # # we test here what happens when some files are missing (ARBRE, ECOLOGIE...)
  # to do:



})

test_that("ifn_tables_process ifn4 works as intended", {

  ### TODO
  # - test what happens when some tables are NAs (not found when building the input)
  # -
  #

  # tests config
  test_ifn4_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # we have 1000 plots in ifn2_plots, so we do reduce a little, because it takes around
  # 4 minutes.
  test_ifn4_plots <- test_ifn4_plots |>
    purrr::map(\(x) {
      sample(x, 5, replace = TRUE) |> unique()
    })

  # tests data
  expected_names <- c(
    "ID_UNIQUE_PLOT",
    "COUNTRY",
    "YEAR",
    "ca_name_original",
    "province_name_original",
    "province_code",
    "PLOT",
    "Cla",
    "Subclase",
    "version",
    "Tipo",
    "HOJA",
    "Huso",
    "COORD_SYS",
    "COORD1",
    "COORD2",
    "crs",
    "SLOPE",
    "ASPECT",
    "tree",
    "understory",
    "regen"
    # "soils"
  )

  # object
  expect_s3_class(
    test_ifn4_res <- suppressWarnings(ifn_tables_process(
      test_ifn4_provinces, test_ifn4_version, test_ifn4_plots, test_ifn4_folder,
      .parallel_options = test_ifn4_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_ifn4_res, expected_names)
  expect_true(all(unique(test_ifn4_res$province_code) %in% names(test_ifn4_plots)))

  ### missing tables/plots
  # tururu state shouldn't appear, inexistent plots (91-0) shouldn't be present,
  # Also, "06" = c(2064,325), "07" = c(499) and "10" = c(3374) don't exist in IFN4
  expect_false("tururu" %in% unique(test_ifn4_res$province_code))
  expect_identical(
    nrow(test_ifn4_res),
    ((test_ifn4_plots |> purrr::flatten() |> length()) - 3) |> as.integer()
  )

  # ### missing random files
  # # we test here what happens when some files are missing (ARBRE, ECOLOGIE...)
  # to do:



})

# ifn_to_tibble -------------------------------------------------------------------------------

# test_that("ifn_to_tibble  ifn 2-3-4 works as intended", {
#
#   test_plots <- list(
#       "06" = c(2064,1138,325),
#       "07" = c(679,114,499),
#       "10" = c(3374,261),
#       "30" = c(78, 1223),
#       "31" = c(135,761,1518),
#       "33" = c(283),
#       "40" = c(412,1216,1728),
#       "49" = c(105,99,532),
#       "91" = c(1406115, 0),
#       "tururu" = 3555
# )
# #   test_plots <- list(
# #     "01" = c(19,80,1120),
# #     "02"= c(11,444,1839),
# #     "03"= c(626,1021,23),
# #     "04"= c(233,5,445),
# #     "05" = c(61, 14, 328),
# #     "06" = c(2064,1138,325),
# #     "07" = c(679,114,499),
# #     "10" = c(3374,261),
# #     "12" = c(156,1463,377),
# #     "13" = c(51, 419,783),
# #     "17" = c(2003,629,2944),
# #     "23" = c(269,1460,444),
# #     "26" = c(960,495,172),
# #     "27" = c(90, 190,537),
# #     "30" = c(78,1223,1057),
# #     "33" = c(818,283,1483),
# #     "31" = c(135,761,1518),
# #     "38" = c(672,426,1557),
# #     "40" = c(412,1216,1728),
# #     "50" = c(172, 479,744),
# #     "49" = c(105,99,532)
# #   )
# #
# #
#   test_provinces <- names(test_plots)
#
#
#   test_input_ifn2 <- suppressWarnings(
#     .build_ifn_input_with (
#    "ifn2",
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE
#     )
#   )
#
#
#   test_input_ifn3 <- suppressWarnings(
#     .build_ifn_input_with (
#     "ifn3",
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE
#     )
#   )
#
#   test_input_ifn4 <- suppressWarnings(
#   .build_ifn_input_with (
#     "ifn4",
#     test_provinces,
#     test_plots,
#     test_folder,
#     .verbose = TRUE
#   )
#   )
#
#   test_input <- rbind(test_input_ifn2,test_input_ifn3,test_input_ifn4)
#   test_version <- c("ifn2", "ifn3", "ifn4")
#
#
#   # tests config
#   test_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
#   future::plan(future::multisession, workers = 3)
#   withr::defer(future::plan(future::sequential))
#
#   # tests data
#   expected_names <- c(
#     "ID_UNIQUE_PLOT",
#     "COUNTRY",
#     "YEAR",
#     "ca_name_original",
#     "province_name_original",
#     "province_code",
#     "PLOT",
#     "version",
#     "HOJA",
#     "Huso",
#     "COORD_SYS",
#     "COORD1",
#     "COORD2",
#     "crs",
#     "PENDIEN2",
#     "SLOPE",
#     "ELEV",
#     "ASPECT",
#     "tree",
#     "understory",
#     "regen",
#     # "soils",
#     "Cla",
#     "Subclase",
#     "Tipo")
#
#
#   # object
#   expect_s3_class(
#     test_res <- suppressWarnings(ifn_to_tibble(
#       test_provinces, test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     )),
#     "tbl"
#   )
#
#   # data integrity
#   expect_named(test_res, expected_names)
#   expect_false("tururu" %in% unique(test_res$province_code))
#   expect_identical(nrow(test_res), 53L) # two plots dont exist, so 2x2=4 rows less
#   expect_true(all(unique(test_res$province_code) %in% names(test_plots)))
#   expect_true(all(unique(test_res$version) %in% test_version))
#
#   ### test all assertions done in ifn_to_tibble
#   # provinces
#   expect_error(
#     ifn_to_tibble(
#       1:7, test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "provinces must be a character vector with at least one province code"
#   )
#   expect_error(
#     ifn_to_tibble(
#       character(), test_version, test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "provinces must be a character vector with at least one province code"
#   )
#   # VERSION
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, numeric(), test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "version must be a character vector with at least one"
#   )
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, character(), test_plots, test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "version must be a character vector with at least one"
#   )
#   # folder
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, "nonexistantfolder",
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     ),
#     "Folder especified"
#   )
#   # filter list (TODO as testng interactive commands is tricky)
#   # parallel options
#   expect_error(
#     ifn_to_tibble(
#       test_provinces, test_version, test_plots, test_folder,
#       .parallel_options = list(scheduling = 2L, stdout = TRUE),
#       .verbose = FALSE
#     ),
#     ".parallel_options"
#   )
# # verbose
# expect_error(
#   ifn_to_tibble(
#     test_provinces, test_version, test_plots, test_folder,
#     .parallel_options = test_parallel_conf,
#     .verbose = "FALSE"
#   ),
#   ".verbose"
# )
# # ancillary data (tested just by providing an existing wrong folder)
# expect_error(
#   suppressWarnings(
#   ifn_to_tibble(
#     test_provinces, test_version, test_plots, ".",
#     .parallel_options = test_parallel_conf,
#     .verbose = FALSE
#   )
#   ),
#   "Ooops! Something went wrong, exiting..."
# )
#
# # what to expect if provinces or filter list are all wrong
#   expect_error(
#     suppressWarnings(ifn_to_tibble(
#       "tururu", test_version, list("tururu" = 0), test_folder,
#       .parallel_options = test_parallel_conf,
#       .verbose = FALSE
#     )
#     ),
#     "Ooops! Something went wrong, exiting..."
#   )
#
# })
