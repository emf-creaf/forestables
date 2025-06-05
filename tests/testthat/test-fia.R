skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
skip_if(
  Sys.which("grep") == "",
  "grep not found in system commands"
)
# test data -----------------------------------------------------------------------------------
# we prepare the plots also with 3 unexistent plots (999999999)
test_plots <- list(
  "DE" = list("1" = c(454, 78, 999999999), "5" = c(345, 586, 163)),
  "HI" = list("1" = c(2630, 2757), "7" = c(1160, 1173, 999999999), "9" = 2014),
  "NE" = list("17" = 20277, "111" = 20556, "47" = 20203, "115" = 20210, "183" = 20211),
  "ND" = list("19" = 20566, "55" = 22311, "57" = 22301, "61" = 22221, "89" = 22241),
  "OR" = list("9" = 60747, "7" = 999999999),
  "tururu" = list("1" = 2500)
)
test_year <- 2010L
test_states <- names(test_plots)
test_folder <- Sys.getenv("fia_path")
test_ref_species <- .read_inventory_data(fs::path(test_folder, "REF_SPECIES.csv")) |>
  dplyr::as_tibble()
test_ref_plant_dictionary <-
  .read_inventory_data(fs::path(test_folder, "REF_PLANT_DICTIONARY.csv")) |>
  dplyr::as_tibble()
test_input <-
  .build_fia_input_with(test_year, test_states, test_plots[-6], test_folder, .verbose = FALSE)

# table functions -----------------------------------------------------------------------------
test_that("fia_plot_table_process works as intended", {

  expected_names <- c(
    "year", "id_unique_code", "country", "state_code", "state_ab", "state_name",
    "county_code", "plot", "p3panel", "p2veg_sampling_status_cd", "p2veg_sampling_level_detail_cd",
    "rscd", "design_code", "coordx",  "coordy",
    "coord_sys", "crs", "elev",  "aspect", "slope"
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

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$county_code), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), as.character(test_input$plots[1]))
  expect_identical(unique(test_res$county_code) |> as.character(), test_input$county[1])

  # errors
  # No tables
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

  expect_warning(
    test_error <- fia_plot_table_process(
      test_input$plot_table[1],
      NA_character_,
      test_input$cond_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  expect_warning(
    test_error <- fia_plot_table_process(
      test_input$plot_table[1],
      test_input$survey_table[1],
      NA_character_,
      test_input$plots[1],
      test_input$county[1],
      test_year
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_s3_class(
    test_no_plot_error <- suppressWarnings(fia_plot_table_process(
      test_input$plot_table[3],
      test_input$survey_table[3],
      test_input$cond_table[3],
      test_input$plots[3],
      test_input$county[3],
      test_year
    )),
    "tbl"
  )
  expect_true(nrow(test_no_plot_error) < 1L)
})

test_that("fia_tree_table_process works as intended", {

  expected_names <- c(
    "id_unique_code", "year", "state_code", "county_code", "plot", "tree_id",
    "status", "dbh", "height", "sp_name", "sp_code", "density_factor"
  )

  # object
  expect_s3_class(
    test_res <- fia_tree_table_process(
      test_input$tree_table[2],
      test_input$plots[2],
      test_input$county[2],
      test_year,
      test_ref_species
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$county_code), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), test_input$plots[2] |> as.integer())
  expect_identical(unique(test_res$county_code) |> as.character(), test_input$county[2])

  # errors
  expect_warning(
    test_error <- fia_tree_table_process(
      NA_character_,
      test_input$plots[2],
      test_input$county[2],
      test_year,
      test_ref_species
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_s3_class(
    test_no_plot_error <- suppressWarnings(fia_tree_table_process(
      test_input$tree_table[3],
      test_input$plots[3],
      test_input$county[3],
      test_year,
      test_ref_species
    )),
    "tbl"
  )
  expect_true(nrow(test_no_plot_error) < 1L)
})

test_that("fia_p3_understory_table_process works as intended", {

  expected_names <- c(
    "id_unique_code", "year", "state_code", "county_code", "plot", "subplot_id",
    "sp_name", "height", "cover", "growth_form", "sp_code"
  )

  # shrub object
  expect_s3_class(
    test_res <- fia_p3_understory_table_process(
      test_input$p3_understory_table[14],
      test_input$plots[14],
      test_input$county[14],
      test_year,
      "Shrub",
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$county_code), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), test_input$plots[14] |> as.integer())
  expect_identical(unique(test_res$county_code) |> as.character(), test_input$county[14])

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
      test_input$p3_understory_table[14],
      test_input$plots[14],
      test_input$county[14],
      test_year,
      c("Forb/herb", "Graminoids"),
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$county_code), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), test_input$plots[14] |> as.integer())
  expect_identical(unique(test_res$county_code) |> as.character(), test_input$county[14])

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

  # No plot
  expect_s3_class(
    test_no_plot_error <- suppressWarnings(fia_p3_understory_table_process(
      test_input$p3_understory_table[3],
      test_input$plots[3],
      test_input$county[3],
      test_year,
      "Shrub",
      test_ref_plant_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_no_plot_error) < 1L)

  ### TODO
  # tests for the combination warning
  # find plots with p2 data to test
})

test_that("fia_p2_understory_table_process works as intended", {

  # test data mods
  test_year <- 2019L
  test_input <-
    .build_fia_input_with(test_year, test_states, test_plots[-6], test_folder, .verbose = FALSE)

  expected_names <- c(
    "id_unique_code", "year", "state_code", "county_code", "plot", "subplot_id",
    "sp_name",  "height", "cover", "growth_form", "growth_form_code", "sp_code"
  )

  # shrub object
  expect_s3_class(
    test_res <- fia_p2_understory_table_process(
      test_input$p2_veg_subplot_table[8],
      test_input$plots[8],
      test_input$county[8],
      test_year,
      "SH",
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$county_code), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), test_input$plots[8])
  expect_identical(unique(test_res$county_code) |> as.character(), test_input$county[8])

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
      test_input$p2_veg_subplot_table[8],
      test_input$plots[8],
      test_input$county[8],
      test_year,
      c("FB", "GR"),
      test_ref_plant_dictionary
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$county_code), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), test_input$plots[8])
  expect_identical(unique(test_res$county_code) |> as.character(), test_input$county[8])

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

  # No plot
  expect_s3_class(
    test_no_plot_error <- suppressWarnings(fia_p2_understory_table_process(
      test_input$p2_veg_subplot_table[3],
      test_input$plots[3],
      test_input$county[3],
      test_year,
      "SH",
      test_ref_plant_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_no_plot_error) < 1L)


  ### TODO
  # tests for the combination warning
})

test_that("fia_understory_table_process works as intended", {
  # test data
  test_year <- 2019L
  test_input <-
    .build_fia_input_with(test_year, test_states, test_plots[-6], test_folder, .verbose = FALSE)
  expected_names_p2 <- c(
    "id_unique_code", "year", "state_code", "county_code", "plot", "subplot_id",
    "sp_name", "height", "cover", "growth_form", "growth_form_code", "sp_code"
  )
  expected_names_p3 <- c(
    "id_unique_code", "year", "state_code", "county_code", "plot", "subplot_id",
    "sp_name", "height", "cover", "growth_form", "sp_code"
  )
  # p2 shrubs
  expect_s3_class(
    test_res <- suppressWarnings(fia_understory_table_process(
      test_input$p3_understory_table[8],
      test_input$p2_veg_subplot_table[8],
      test_input$plots[8],
      test_input$county[8],
      test_year,
      "Shrub", "SH",
      test_ref_plant_dictionary
    )),
    "tbl"
  )
  # data integrity
  expect_true(nrow(test_res) > 0)
  expect_named(test_res, expected_names_p2, ignore.order = TRUE)
  expect_identical(test_res$year |> unique(), test_year)
  expect_identical(test_res$state_code |> unique(), 15L)
  expect_identical(test_res$county_code |> unique() |> as.character(), test_input$county[8])
  expect_identical(test_res$growth_form_code |> unique(), "SH")
  # We expect the same results in the general understory function than in the individual
  # ones
  expect_identical(
    test_res,
    fia_p2_understory_table_process(
      test_input$p2_veg_subplot_table[8],
      test_input$plots[8],
      test_input$county[8],
      test_year,
      "SH",
      test_ref_plant_dictionary
    )
  )

  # no p2 no p3 warnings
  expect_s3_class(
    test_empty <- suppressWarnings(fia_understory_table_process(
      test_input$p3_understory_table[1],
      test_input$p2_veg_subplot_table[1],
      test_input$plots[1],
      test_input$county[1],
      test_year,
      "Shrub", "SH",
      test_ref_plant_dictionary
    )),
    "tbl"
  )
  expect_true(nrow(test_empty) < 1)

  # p3 shrubs
  test_year <- 2010L
  test_input <-
    .build_fia_input_with(test_year, test_states, test_plots[-6], test_folder, .verbose = FALSE)
  expect_s3_class(
    test_res_p3 <- suppressWarnings(fia_understory_table_process(
      test_input$p3_understory_table[14],
      test_input$p2_veg_subplot_table[14],
      test_input$plots[14],
      test_input$county[14],
      2010,
      "Shrub", "SH",
      test_ref_plant_dictionary
    )),
    "tbl"
  )
  # data integrity
  expect_true(nrow(test_res_p3) > 1)
  expect_named(test_res_p3, expected_names_p3, ignore.order = TRUE)
  expect_identical(test_res_p3$year |> unique(), test_year)
  expect_identical(test_res_p3$state_code |> unique(), 31L)
  expect_identical(test_res_p3$county_code |> unique() |> as.character(), test_input$county[14])
  expect_true(stringr::str_detect(test_res_p3$growth_form |> unique(), "Shrub"))
  # We expect the same results in the general understory function than in the individual
  # ones
  expect_identical(
    test_res_p3,
    fia_p3_understory_table_process(
      test_input$p3_understory_table[14],
      test_input$plots[14],
      test_input$county[14],
      test_year,
      "Shrub",
      test_ref_plant_dictionary
    )
  )

  # p3 and p2
  test_year <- 2009L
  test_input <-
    .build_fia_input_with(test_year, test_states, test_plots[-6], test_folder, .verbose = FALSE)
  expect_s3_class(
    test_res_p3_p2 <- fia_understory_table_process(
      test_input$p3_understory_table[23],
      test_input$p2_veg_subplot_table[23],
      test_input$plots[23],
      test_input$county[23],
      test_year,
      "Shrub", "SH",
      test_ref_plant_dictionary
    ),
    "tbl"
  )
  # data integrity
  expect_true(nrow(test_res_p3_p2) > 1)
  # we expect the p2 names, as some of the data comes from p2
  expect_named(test_res_p3_p2, expected_names_p2, ignore.order = TRUE)

  expect_identical(test_res_p3_p2$year |> unique(), test_year)
  expect_identical(test_res_p3_p2$state_code |> unique(), 41L)
  expect_identical(test_res_p3_p2$county_code |> unique() |> as.character(), test_input$county[23])
})

test_that("fia_seedling_table_process works as intended", {

  expected_names <- c(
    "id_unique_code", "year", "state_code", "county_code", "plot", "subplot_id",
    "sp_code", "sp_name", "n", "treecount_calc", "density_factor", "height", "dbh"
  )

  test_year <- 2010L
  test_input <-
    .build_fia_input_with(test_year, test_states, test_plots[-6], test_folder, .verbose = FALSE)

  # object
  expect_s3_class(
    test_res <- fia_seedling_table_process(
      test_input$seedling_table[2],
      test_input$plots[2],
      test_input$county[2],
      test_year,
      test_ref_species
    ),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(nrow(test_res) > 0)

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$county_code), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), test_input$plots[2] |> as.integer())
  expect_identical(unique(test_res$county_code) |> as.character(), test_input$county[2])

  # errors
  expect_warning(
    test_error <- fia_seedling_table_process(
      NA_character_,
      test_input$plots[2],
      test_input$county[2],
      test_year,
      test_ref_species
    ),
    "Some files"
  )
  expect_s3_class(test_error, "tbl")
  expect_true(nrow(test_error) < 1)

  # No plot
  expect_s3_class(
    test_no_plot_error <- suppressWarnings(fia_seedling_table_process(
      test_input$seedling_table[3],
      test_input$plots[3],
      test_input$county[3],
      test_year,
      test_ref_species
    )),
    "tbl"
  )
  expect_true(nrow(test_no_plot_error) < 1L)
})

test_that("fia_subplot_table_process works as intended", {

  expected_names <- c(
    "id_unique_code", "year", "state_code", "county_code", "plot", "subplot_id",
    "slope_subplot", "aspect_subplot", "macro_cond", "subplot_cond", "micro_cond",
    "subplot_status", "psveg_subplot_status"
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

  expect_length(unique(test_res$year), 1)
  expect_length(unique(test_res$plot), 1)
  expect_length(unique(test_res$county_code), 1)

  expect_identical(unique(test_res$year), test_year)
  expect_identical(unique(test_res$plot), test_input$plots[1] |> as.integer())
  expect_identical(unique(test_res$county_code) |> as.character(), test_input$county[1])

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

  # No plot
  expect_s3_class(
    test_no_plot_error <- suppressWarnings(fia_subplot_table_process(
      test_input$subplot_table[3],
      test_input$plots[3],
      test_input$county[3],
      test_year
    )),
    "tbl"
  )
  expect_true(nrow(test_no_plot_error) < 1L)
})

# table process -------------------------------------------------------------------------------

test_that("fia_table_process works as intended", {

  ### TODO
  # - test new understory logic

  # tests config
  test_parallel_conf <- furrr::furrr_options(scheduling = 1L, stdout = TRUE)
  if (!Sys.info()["sysname"] %in% c("darwin", "Darwin", "DARWIN")) {
    future::plan(future::multisession, workers = 3)
    withr::defer(future::plan(future::sequential))
  }

  # tests data
  expected_names <- c(
    "year", "id_unique_code", "country", "state_code", "state_ab",
    "state_name", "county_code", "plot", "p3panel",
    "p2veg_sampling_status_cd", "p2veg_sampling_level_detail_cd",
    "rscd", "design_code", "coordx", "coordy", "coord_sys", "crs",
    "elev", "aspect", "slope", "tree", "understory", "regen", "subplot"
    # "soils"
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
  expect_true(all(unique(test_res$state_ab) %in% names(test_plots)))
  # this test is wrong:
  # expect_true(all(
  #   unique(test_res$county_code) %in%
  #     (purrr::map_depth(test_plots, .depth = 1, names) |> purrr::flatten_chr() |> unique())
  # ))

  ### missing tables/plots
  # tururu state shouldn't appear
  # inexistent plots (999999) shouldn't
  # be present, so 21 of 25 elements in filter list
  expect_false("tururu" %in% unique(test_res$state_ab))
  expect_identical(nrow(test_res), as.integer(length(purrr::flatten(purrr::flatten(test_plots))) - 4))

  ### missing random files
  # This is done with files in a folder for testing that lacks some files:
  # DE -> No TREE, we expect results, but all the tree tibbles for MN are empty tibbles
  # HI -> No COND, we expect no plots from CA as all plot info is missing
  # OR -> No SEEDLING, we expect results, but all the seedling tibbles for MN are empty tibbles
  # NE -> No SOIL (both), we expect results, but all the soil tibbles for AL are empty tibbles
  # NE -> No VEG_SUBplot, we expect_results, but understory should be empty tibbles for MO
  # ND -> No SUBplot, we expect_results, but subplot should be empty tibbles for OH
  # test_folder <- fs::path(Sys.getenv("fia_path"), "missing_files_test")
  fs::file_move(fs::path(test_folder, "DE_TREE.csv"), fs::path(test_folder, "_DE_TREE.csv"))
  withr::defer({
    if (fs::file_exists(fs::path(test_folder, "_DE_TREE.csv"))) {
      fs::file_move(fs::path(test_folder, "_DE_TREE.csv"), fs::path(test_folder, "DE_TREE.csv"))
    }
  })
  fs::file_move(fs::path(test_folder, "HI_COND.csv"), fs::path(test_folder, "_HI_COND.csv"))
  withr::defer({
    if (fs::file_exists(fs::path(test_folder, "_HI_COND.csv"))) {
      fs::file_move(fs::path(test_folder, "_HI_COND.csv"), fs::path(test_folder, "HI_COND.csv"))
    }
  })
  fs::file_move(fs::path(test_folder, "OR_SEEDLING.csv"), fs::path(test_folder, "_OR_SEEDLING.csv"))
  withr::defer({
    if (fs::file_exists(fs::path(test_folder, "_OR_SEEDLING.csv"))) {
      fs::file_move(fs::path(test_folder, "_OR_SEEDLING.csv"), fs::path(test_folder, "OR_SEEDLING.csv"))
    }
  })
  # fs::file_move(fs::path(test_folder, "NE_SOILS_LAB.csv"), fs::path(test_folder, "_NE_SOILS_LAB.csv"))
  # withr::defer({
  #   if (fs::file_exists(fs::path(test_folder, "_NE_SOILS_LAB.csv"))) {
  #     fs::file_move(fs::path(test_folder, "_NE_SOILS_LAB.csv"), fs::path(test_folder, "NE_SOILS_LAB.csv"))
  #   }
  # })
  # fs::file_move(fs::path(test_folder, "NE_SOILS_SAMPLE_LOC.csv"), fs::path(test_folder, "_NE_SOILS_SAMPLE_LOC.csv"))
  # withr::defer({
  #   if (fs::file_exists(fs::path(test_folder, "_NE_SOILS_SAMPLE_LOC.csv"))) {
  #     fs::file_move(fs::path(test_folder, "_NE_SOILS_SAMPLE_LOC.csv"), fs::path(test_folder, "NE_SOILS_SAMPLE_LOC.csv"))
  #   }
  # })
  fs::file_move(fs::path(test_folder, "NE_P2VEG_SUBPLOT_SPP.csv"), fs::path(test_folder, "_NE_P2VEG_SUBPLOT_SPP.csv"))
  withr::defer({
    if (fs::file_exists(fs::path(test_folder, "_NE_P2VEG_SUBPLOT_SPP.csv"))) {
      fs::file_move(fs::path(test_folder, "_NE_P2VEG_SUBPLOT_SPP.csv"), fs::path(test_folder, "NE_P2VEG_SUBPLOT_SPP.csv"))
    }
  })
  fs::file_move(fs::path(test_folder, "NE_VEG_SUBPLOT_SPP.csv"), fs::path(test_folder, "_NE_VEG_SUBPLOT_SPP.csv"))
  withr::defer({
    if (fs::file_exists(fs::path(test_folder, "_NE_VEG_SUBPLOT_SPP.csv"))) {
      fs::file_move(fs::path(test_folder, "_NE_VEG_SUBPLOT_SPP.csv"), fs::path(test_folder, "NE_VEG_SUBPLOT_SPP.csv"))
    }
  })
  fs::file_move(fs::path(test_folder, "ND_SUBPLOT.csv"), fs::path(test_folder, "_ND_SUBPLOT.csv"))
  withr::defer({
    if (fs::file_exists(fs::path(test_folder, "_ND_SUBPLOT.csv"))) {
      fs::file_move(fs::path(test_folder, "_ND_SUBPLOT.csv"), fs::path(test_folder, "ND_SUBPLOT.csv"))
    }
  })

  expect_s3_class(
    test_res_missing_files <- suppressWarnings(fia_tables_process(
      test_year, test_states, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )
  fs::file_move(fs::path(test_folder, "_DE_TREE.csv"), fs::path(test_folder, "DE_TREE.csv"))
  fs::file_move(fs::path(test_folder, "_HI_COND.csv"), fs::path(test_folder, "HI_COND.csv"))
  fs::file_move(fs::path(test_folder, "_OR_SEEDLING.csv"), fs::path(test_folder, "OR_SEEDLING.csv"))
  fs::file_move(fs::path(test_folder, "_NE_P2VEG_SUBPLOT_SPP.csv"), fs::path(test_folder, "NE_P2VEG_SUBPLOT_SPP.csv"))
  fs::file_move(fs::path(test_folder, "_NE_VEG_SUBPLOT_SPP.csv"), fs::path(test_folder, "NE_VEG_SUBPLOT_SPP.csv"))
  fs::file_move(fs::path(test_folder, "_ND_SUBPLOT.csv"), fs::path(test_folder, "ND_SUBPLOT.csv"))

  expect_true(
    (test_res_missing_files |>
       dplyr::filter(state_ab == "DE") |>
       dplyr::pull(tree) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )
  expect_false(all(c("tururu", "HI") %in% unique(test_res_missing_files$state_ab)))
  expect_true(
    (test_res_missing_files |>
       dplyr::filter(state_ab == "OR") |>
       dplyr::pull(regen) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )
  # expect_true(
  #   (test_res_missing_files |>
  #      dplyr::filter(state_ab == "AL") |>
  #      dplyr::pull(soils) |>
  #      purrr::list_rbind() |>
  #      dplyr::pull(soils_lab) |>
  #      purrr::list_rbind() |>
  #      nrow()) < 1
  # )

  # expect_true(
  #   (test_res_missing_files |>
  #      dplyr::filter(state_ab == "AL") |>
  #      dplyr::pull(soils) |>
  #      purrr::list_rbind() |>
  #      dplyr::pull(soils_loc) |>
  #      purrr::list_rbind() |>
  #      nrow()) < 1
  # )
  expect_true(
    (test_res_missing_files |>
       dplyr::filter(state_ab == "NE") |>
       dplyr::pull(understory) |>
       purrr::list_rbind() |>
       dplyr::pull(shrub) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )
  expect_true(
    (test_res_missing_files |>
       dplyr::filter(state_ab == "NE") |>
       dplyr::pull(understory) |>
       purrr::list_rbind() |>
       dplyr::pull(herbs) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )
  expect_true(
    (test_res_missing_files |>
       dplyr::filter(state_ab == "ND") |>
       dplyr::pull(subplot) |>
       purrr::list_rbind() |>
       nrow()) < 1
  )

})

# fia_to_tibble -------------------------------------------------------------------------------

test_that("fia_to_tibble works as intended", {

  # tests config
  test_parallel_conf <- furrr::furrr_options(scheduling = 1L, stdout = TRUE)
  if (!Sys.info()["sysname"] %in% c("darwin", "Darwin", "DARWIN")) {
    future::plan(future::multisession, workers = 3)
    withr::defer(future::plan(future::sequential))
  }

  # tests data
  expected_names <- c(
    "year", "id_unique_code", "country", "state_code", "state_ab",
    "state_name", "county_code", "plot", "p3panel",
    "p2veg_sampling_status_cd", "p2veg_sampling_level_detail_cd",
    "rscd", "design_code", "coordx", "coordy", "coord_sys", "crs",
    "elev", "aspect", "slope", "tree", "understory", "regen", "subplot"
    # "soils"
  )
  test_years <- c(2005, 2010)

  # object
  expect_s3_class(
    test_res <- suppressWarnings(fia_to_tibble(
      test_states, test_years, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "tbl"
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_false("tururu" %in% unique(test_res$state_ab))
  expect_identical(nrow(test_res), as.integer((2 * length(purrr::flatten(purrr::flatten(test_plots)))) - (2 * 4))) # four plots dont exist, so 4x2=8 rows less
  expect_true(all(unique(test_res$state_ab) %in% names(test_plots)))
  expect_true(all(
    unique(test_res$county_code) %in%
      (purrr::map_depth(test_plots, .depth = 1, names) |> purrr::flatten_chr() |> unique())
  ))
  expect_true(all(unique(test_res$year) %in% test_years))

  # tests for clean_empty and as_sf arguments
  sf_expected_names <- c(
    "year", "id_unique_code", "country", "state_code", "state_ab",
    "state_name", "county_code", "plot", "p3panel",
    "p2veg_sampling_status_cd", "p2veg_sampling_level_detail_cd",
    "rscd", "design_code", "coord_sys_orig", "crs", "elev", "aspect",
    "slope", "tree", "understory", "regen", "subplot",
    "geometry", "crs_orig"
    # "soils"
  )
  expect_s3_class(
    sf_res <- suppressWarnings(fia_to_tibble(
      test_states, test_years, test_plots, test_folder,
      as_sf = TRUE, clean_empty = c("tree", "shrub", "herbs", "regen"),
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "sf"
  )

  expect_named(sf_res, sf_expected_names, ignore.order = TRUE)
  expect_true(nrow(sf_res) < nrow(test_res))
  expect_false(any(purrr::map_lgl(sf_res$tree, rlang::is_empty)))
  expect_false(any(purrr::map_lgl(sf_res$understory, rlang::is_empty)))
  expect_false(any(purrr::map_lgl(sf_res$regen, rlang::is_empty)))
  expect_false(any(
    sf_res$understory |>
      purrr::map("shrub") |>
      purrr::map_lgl(.f = rlang::is_empty)
  ))
  expect_false(any(
    sf_res$understory |>
      purrr::map("herbs") |>
      purrr::map_lgl(.f = rlang::is_empty)
  ))

  ### test all assertions done in fia_to_tibble
  # states
  expect_error(
    fia_to_tibble(
      1:7, test_years, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "states must be a character vector with at least one"
  )
  expect_error(
    fia_to_tibble(
      character(), test_years, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "states must be a character vector with at least one"
  )
  # years
  expect_error(
    fia_to_tibble(
      test_states, as.character(test_years), test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "years must be a numeric vector with at least one"
  )
  expect_error(
    fia_to_tibble(
      test_states, numeric(), test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "years must be a numeric vector with at least one"
  )
  # folder
  expect_error(
    fia_to_tibble(
      test_states, test_years, test_plots, "nonexistentfolder",
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "Folder especified"
  )
  # filter list (TODO as testng interactive commands is tricky)
  # parallel options
  expect_error(
    fia_to_tibble(
      test_states, test_years, test_plots, test_folder,
      .parallel_options = list(scheduling = 1L, stdout = TRUE),
      .verbose = FALSE
    ),
    ".parallel_options"
  )
  # verbose
  expect_error(
    fia_to_tibble(
      test_states, test_years, test_plots, test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = "FALSE"
    ),
    ".verbose"
  )
  # ancillary data (tested just by providing an existing wrong folder)
  expect_error(
    fia_to_tibble(
      test_states, test_years, test_plots, ".",
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    ),
    "must be present"
  )

  # what to expect if states or filter list are all wrong
  expect_error(
    suppressWarnings(fia_to_tibble(
      test_states[7], test_years, test_plots[7], test_folder,
      .parallel_options = test_parallel_conf,
      .verbose = FALSE
    )),
    "Ooops!"
  )

})
