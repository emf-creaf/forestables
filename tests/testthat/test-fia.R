# test data -----------------------------------------------------------------------------------

# test data
test_plots <- list(
  "MN" = list(
    "137" = c(29396, 25064),
    "17" = 20005,
    "31" = 20421,
    "71" = 20210
  ),
  "CA" = list(
    "15" = c(53519, 63676),
    "105" = c(70128, 83043),
    "61" = 69600
  ),
  "AL" = list(
    "121" = 33,
    "73" = 20,
    "131" = 73,
    "1" = 27,
    "81" = 13
  ),
  "MO" = list(
    "113" = 20144,
    "119" = 20129,
    "225" = 20168,
    "221" = 20084,
    "88" = 20012
  ),
  "OH" = list(
    "41" = 3878,
    "167" = 2121,
    "25" = 5374,
    "103" = 4704,
    "53" = 3579
  ),
  "OR" = list(
    "59" = c(76413, 76413),
    "17" = 63905,
    "31" = 95724,
    "71" = 99371
  ),
  "tururu" = list(
    "1" = 2500
  )
)
test_year <- 2010L
test_states <- names(test_plots)
test_folder <- Sys.getenv("fia_path")
test_ref_species <- .read_inventory_data(fs::path(test_folder, "REF_SPECIES.csv")) |>
  dplyr::as_tibble()
test_ref_plant_dictionary <- .read_inventory_data(fs::path(test_folder, "REF_PLANT_DICTIONARY.csv")) |>
  dplyr::as_tibble()
test_input <-
  .build_fia_input_with(test_year, test_states, test_plots[-7], test_folder, .verbose = FALSE)

# table functions -----------------------------------------------------------------------------
test_that("fia_tree_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "TREE",
    "STATUS", "DIA", "HT", "SP_NAME", "SP_CODE", "DENSITY"
  )

  # object
  expect_s3_class(
    test_res <- fia_tree_table_process(
      test_input$tree_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      test_ref_species
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_tree_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      test_ref_species
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
})

test_that("fia_plot_table_process works as intended", {

  expected_names <- c(
    "YEAR", "ID_UNIQUE_PLOT", "COUNTRY", "STATECD", "STATEAB", "STATENM", "COUNTYCD", "PLOT",
    "P3PANEL", "RSCD", "DESIGNCD", "LAT", "LAT_ORIGINAL", "LON", "LON_ORIGINAL", "COORD_SYS",
    "ELEV", "ELEV_ORIGINAL", "ASPECT", "ASPECT_ORIGINAL", "SLOPE", "SLOPE_ORIGINAL"
  )

  # object
  expect_s3_class(
    test_res <- fia_plot_table_process(
      test_input$plot_table[1],
      test_input$survey_table[1],
      test_input$cond_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1])
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_plot_table_process(
      NA_character_,
      test_input$survey_table[1],
      test_input$cond_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
})

test_that("fia_p3_understory_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
    "SP_CODE", "SP_NAME", "HT", "COVER", "GROWTH_HABIT"
  )

  # shrub object
  expect_s3_class(
    test_res <- fia_p3_understory_table_process(
      test_input$p3_understory_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "Shrub",
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_p3_understory_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "Shrub",
      test_ref_plant_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # herb object
  expect_s3_class(
    test_res <- fia_p3_understory_table_process(
      test_input$p3_understory_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      c("Forb/herb", "Graminoids"),
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_p3_understory_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      c("Forb/herb", "Graminoids"),
      test_ref_plant_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)


  ### TODO
  # tests for the combination warning
  # find plots with p2 data to test
})

test_that("fia_p2_understory_table_process works as intended", {

  skip()

  expected_names <- c(
    "ID_UNIQUE_PLOT", "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP", "SPECIES_SYMBOL",
    "SP_NAME", "GROWTH_HABIT_CD", "HT", "COVER_PCT", "GROWTH_HABIT"
  )

  # shrub object
  expect_s3_class(
    test_res <- fia_p2_understory_table_process(
      test_input$p2_veg_subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "SH",
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_p2_understory_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "SH",
      test_ref_plant_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # herb object
  expect_s3_class(
    test_res <- fia_p2_understory_table_process(
      test_input$p2_veg_subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      c("FB", "GR"),
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_p2_understory_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      c("FB", "GR"),
      test_ref_plant_dictionary
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)


  ### TODO
  # tests for the combination warning
})

test_that("fia_seedling_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
    "SP_CODE", "SP_NAME", "N", "TPA_UNADJ", "DENSITY"
  )

  # object
  expect_s3_class(
    test_res <- fia_seedling_table_process(
      test_input$seedling_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      test_ref_species
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_seedling_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year,
      test_ref_species
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
})

test_that("fia_subplot_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
    "SLOPE", "ASPECT", "MACRCOND", "SUBPCOND", "MICRCOND"
  )

  # object
  expect_s3_class(
    test_res <- fia_subplot_table_process(
      test_input$subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_subplot_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
})

test_that("fia_soils_lab_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "PLOT", "COUNTYCD", "STATECD", "YEAR", "LAYER_TYPE",
    "FIELD_MOIST_WATER_CONTENT_PCT_MEAN", "TOTAL_WATER_CONTENT_PCT_MEAN",
    "RESIDUAL_WATER_CONTENT_PCT_MEAN", "BULK_DENSITY_MEAN", "COARSE_FRACTION_PCT_MEAN",
    "C_ORG_PCT_MEAN"
  )

  # object
  expect_s3_class(
    test_res <- fia_soils_lab_table_process(
      test_input$soils_lab_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_input$state[1],
      test_year
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1])
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_soils_lab_table_process(
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_input$state[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
})

test_that("fia_soils_loc_table_process works as intended", {

  expected_names <- c(
    "ID_UNIQUE_PLOT", "YEAR", "STATECD", "COUNTYCD", "PLOT", "FORFLTHK_MEAN",
    "FORFLTHK_MEAN_ORIGINAL", "LTRLRTHK_MEAN", "LTRLRTHK_MEAN_ORIGINAL", "TXTRLYR1",
    "TXTRLYR1_ORIGINAL", "TXTRLYR2", "TXTRLYR2_ORIGINAL", "DPTHSBSL_MEAN",
    "DPTHSBSL_MEAN_ORIGINAL", "ROCK_COVER_PCT_MEAN", "ROCK_COVER_PCT_MEAN_ORIGINAL"
  )

  # object
  expect_s3_class(
    test_res <- fia_soils_loc_table_process(
      test_input$soils_loc_table[1],
      test_input$veg_subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_input$state[1],
      test_year
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$YEAR), 1)
  expect_length(unique(test_res$PLOT), 1)
  expect_length(unique(test_res$COUNTYCD), 1)

  expect_identical(unique(test_res$YEAR), test_year)
  expect_identical(unique(test_res$PLOT), test_input$plots[1])
  expect_identical(unique(test_res$COUNTYCD) |> as.character(), test_input$county[1])

  # errors
  expect_warning(
    test_error <- fia_soils_loc_table_process(
      NA_character_,
      test_input$veg_subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_input$state[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)
})

# table process -------------------------------------------------------------------------------

test_that("fia_table_process works as intended", {

  ### TODO
  # - test what happens when some tables are NAs (not found when builoding the input)
  # -
  #

  # tests config
  test_parallel_conf <- furrr::furrr_options(scheduling = 2L, stdout = TRUE)
  future::plan(future::multisession, workers = 3)
  withr::defer(future::plan(future::sequential))

  # tests data
  expected_names <- c(
    "YEAR", "ID_UNIQUE_PLOT", "COUNTRY", "STATECD", "STATEAB", "STATENM", "COUNTYCD", "PLOT",
    "P3PANEL", "RSCD", "DESIGNCD", "LAT", "LAT_ORIGINAL", "LON", "LON_ORIGINAL", "COORD_SYS",
    "ELEV", "ELEV_ORIGINAL", "ASPECT", "ASPECT_ORIGINAL", "SLOPE", "SLOPE_ORIGINAL",
    "tree", "understory", "regen", "subplot", "soils"
  )

  # object
  expect_s3_class(
    test_res <- suppressWarnings(fia_tables_process(
      test_year, test_states, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names)
  expect_identical(nrow(test_res), 30L)
  expect_true(all(unique(test_res$STATEAB) %in% names(test_plots)))
  expect_true(all(
    unique(test_res$COUNTYCD) %in%
      (purrr::map_depth(test_plots, .depth = 1, names) |> purrr::flatten_chr() |> unique())
  ))

})
