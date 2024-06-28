skip_if(
  any(c(Sys.getenv("fia_path"), Sys.getenv("ffi_path"), Sys.getenv("ifn_path")) == ""),
  "No testing data found skipping tests"
)
skip_if(
  Sys.which("grep") == "",
  "grep not found in system commands"
)
# build path and input ------------------------------------------------------------------------

test_that(".build_fia_input_with and .build_fia_file_path work as intended", {
  test_plots <- list(
    "DE" = list("1" = c(454, 148), "5" = c(345, 586, 163)),
    "HI" = list("1" = c(2630, 2757), "7" = c(1160, 1173), "9" = 2014),
    "NE" = list("17" = 20277, "31" = 21337, "47" = 20203, "115" = 20210, "183" = 20211),
    "ND" = list("19" = 20566, "55" = 22311, "57" = 22301, "61" = 22221, "89" = 22241),
    "OR" = list("9" = 60747),
    "tururu" = list("1" = 2500)
  )
  test_year <- 2010
  test_states <- names(test_plots)
  test_folder <- Sys.getenv("fia_path")
  expected_names <- c(
    "state", "county", "plots",
    "tree_table",
    "plot_table",
    "survey_table",
    "cond_table",
    "subplot_table",
    "p3_understory_table",
    "seedling_table",
    # "soils_loc_table",
    # "soils_lab_table",
    "veg_subplot_table",
    "p2_veg_subplot_table"
  )

  # warnings and messages
  expect_warning(
    test_res <-
      .build_fia_input_with(test_year, test_states, test_plots, test_folder, .verbose = TRUE),
    "file doesn't exists"
  )
  expect_message(
    .build_fia_input_with(test_year, test_states, test_plots[-6], test_folder, .verbose = TRUE),
    "Getting ready to retrieve"
  )
  expect_no_message(
    .build_fia_input_with(test_year, test_states, test_plots[-6], test_folder, .verbose = FALSE)
  )

  ## result tests
  # we expect a tibble
  expect_s3_class(test_res, "tbl")
  # with the correct names
  expect_named(test_res, expected_names)
  # and for 26 plots as per the filter list we create
  expect_true(nrow(test_res) == test_plots |> purrr::flatten() |> purrr::flatten() |> length())
  # and for the correct counties
  expect_identical(
    unique(test_res[["county"]]) |> sort(),
    purrr::map_depth(test_plots, 1, names) |> purrr::flatten_chr() |> unique() |> sort()
  )
  # and correct_plots
  expect_identical(
    unique(test_res[["plots"]]) |> sort(),
    purrr::map_depth(test_plots, 1, purrr::flatten_dbl) |>
      purrr::flatten_dbl() |>
      unique() |>
      sort()
  )

  # we can test here also if .build_fia_file_path works
  # .build_fia_file_path
  # a correct one
  expect_identical(
    test_res[["survey_table"]][1],
    fs::path(test_folder, paste0(names(test_plots)[1], "_SURVEY.csv")) |>
      as.character()
  )
  # a correct custom one
  expect_true(all(
    stringr::str_detect(as.character(test_res[["plot_table"]][1]), "grep"),
    stringr::str_detect(as.character(test_res[["plot_table"]][1]), "INVYR"),
    stringr::str_detect(as.character(test_res[["plot_table"]][1]), "454"),
    stringr::str_detect(as.character(test_res[["plot_table"]][1]), "PLOT.csv")
  ))
  # expect_identical(
  #   test_res[["plot_table"]][1],
  #   paste0(
  #     "grep -P \",INVYR,|,1,(454|454.0),\" ", fs::path(test_folder, paste0(names(test_plots)[1], "_PLOT.csv"))
  #   )
  # )
  # an incorrect one
  expect_identical(
    test_res[["plot_table"]][31], NA_character_
  )

  ## Test filter_list = NULL
  expect_s3_class(
    test_res_filter_list <- suppressWarnings(
      .build_fia_input_with(test_year, test_states, NULL, test_folder, .verbose = FALSE)
    ),
    "tbl"
  )
  expect_false("tururu" %in% (test_res_filter_list$state |> unique()))
  expect_true(all((test_res_filter_list$state |> unique()) %in% test_states))
  expect_length(test_res_filter_list$state |> unique(), length(test_states) - 1)
})

test_that(".get_plots_from_state works as intended", {
  test_folder <- Sys.getenv("fia_path")
  test_states <- c("HI", "RI", "ND", "tururu")

  # error
  expect_error(
    suppressWarnings(.get_plots_from_state(test_states[1], ".")),
    "folder doesn't contain"
  )

  ## results are ok
  # class
  expect_s3_class(test_res_ok <- .get_plots_from_state(test_states[1], test_folder), "sf")
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(test_res_ok, c("INVYR", "STATECD", "COUNTYCD", "PLOT", "STATEAB", "geometry"))
  # expect rows
  expect_true(nrow(test_res_ok) > 0)
  # expect values
  expect_identical(unique(test_res_ok$STATECD), 15L)
  expect_identical(unique(test_res_ok$STATEAB), "HI")
  expect_identical(unique(.get_plots_from_state(test_states[3], test_folder)$STATECD), 38L)
  expect_identical(unique(.get_plots_from_state(test_states[3], test_folder)$STATEAB), "ND")

  ## wrong state
  expect_error(
    suppressWarnings(.get_plots_from_state(test_states[4], test_folder)),
    "aborting"
  )
})


test_that("show_plots_from_fia works as intended", {
  test_folder <- Sys.getenv("fia_path")
  test_states <- c("HI", "DE", "ND")

  # error
  expect_error(
    suppressWarnings(show_plots_from_fia(".", test_states[1])),
    "No data found at"
  )

  ## results are ok
  # class
  expect_s3_class(test_res_ok <- show_plots_from_fia(test_folder, test_states), "sf")
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(test_res_ok, c("INVYR", "STATECD", "COUNTYCD", "PLOT", "STATEAB", "geometry"))
  # expect rows
  expect_true(
    nrow(test_res_ok) > 0
  )
  # we must have 3 states
  expect_identical(
    test_res_ok$STATEAB |> unique(), test_states
  )
})

test_that(".transform_plot_summary_fia works as intended", {
  test_folder <- Sys.getenv("fia_path")
  test_states <- c("HI", "DE", "ND")
  test_summary <- show_plots_from_fia(test_folder, test_states)
  test_years <- c(2010, 2019)

  # One state, one year
  # correct object
  expect_type(
    test_res_2010_hi <- .transform_plot_summary_fia(test_summary, test_years[1], test_states[1]),
    "list"
  )
  # correct names
  expect_named(test_res_2010_hi, "HI")
  # expect results
  expect_length(test_res_2010_hi, 1)
  expect_true(length(test_res_2010_hi[[1]]) > 1)
  # correct counties
  expect_named(
    test_res_2010_hi[["HI"]],
    test_summary |>
      dplyr::filter(STATEAB == "HI", INVYR == test_years[1]) |>
      dplyr::pull(COUNTYCD) |>
      unique() |>
      as.character(),
    ignore.order = TRUE
  )

  ## all states all years
  expect_type(
    test_res <- .transform_plot_summary_fia(test_summary, test_years, test_states),
    "list"
  )
  # correct names
  expect_named(test_res, test_states, ignore.order = TRUE)
  # expect results
  expect_length(test_res, 3)
  expect_true(length(test_res[[1]]) > 1)
  expect_true(length(test_res[[2]]) > 1)
  expect_true(length(test_res[[3]]) > 1)
  # correct counties
  expect_named(
    test_res[["HI"]],
    test_summary |>
      dplyr::filter(STATEAB %in% "HI", INVYR %in% test_years) |>
      dplyr::pull(COUNTYCD) |>
      unique() |>
      as.character(),
    ignore.order = TRUE
  )

  expect_named(
    test_res[["ND"]],
    test_summary |>
      dplyr::filter(STATEAB %in% "ND", INVYR %in% test_years) |>
      dplyr::pull(COUNTYCD) |>
      unique() |>
      as.character(),
    ignore.order = TRUE
  )

  ## error
  expect_error(
    .transform_plot_summary_fia(
      tibble::tibble(
        "INVYR" = vector(),
        "STATECD" = vector(),
        "COUNTYCD" = vector(),
        "PLOT" = vector(),
        "STATEAB" = vector(),
        "geometry" = vector()
      ),
      test_years[1], test_states[1]
    )
  )
})

test_that("create_filter_list_fia works as inteded", {
  # test data
  test_folder <- Sys.getenv("fia_path")
  test_states <- c("HI", "DE", "ND")
  test_summary <- show_plots_from_fia(test_folder, test_states) |>
    dplyr::filter(INVYR %in% c(2010, 2019))

  # errors
  # object error
  expect_error(create_filter_list_fia("test_summary"), "data.frame")
  expect_error(create_filter_list_fia(as.list(test_summary)), "data.frame")
  # names error
  expect_error(create_filter_list_fia(iris), "expected names")
  # rows error
  expect_error(create_filter_list_fia(test_summary |> dplyr::filter(INVYR == 1800)), "one row")

  # correct object
  expect_type(
    test_res <- create_filter_list_fia(test_summary),
    "list"
  )
  # correct names
  expect_named(test_res, test_states, ignore.order = TRUE)
  # expect results
  expect_length(test_res, 3)
  expect_true(length(test_res[[1]]) > 1)
  expect_true(length(test_res[[2]]) > 1)
  expect_true(length(test_res[[3]]) > 1)
})
