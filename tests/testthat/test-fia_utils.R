# build input ---------------------------------------------------------------------------------

test_that(".build_fia_input_with and .build_fia_file_path work as intended", {
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
  test_year <- 2010
  test_states <- names(test_plots)
  test_folder <- "/data/creaf/projects/emf/international_inventories/data/fia/FIA_DATAMART_MARCH_2023/"
  expected_names <- c(
    "state", "county", "plots",
    "tree_table",
    "plot_table",
    "survey_table",
    "cond_table",
    "subplot_table",
    "p3_understory_table",
    "seedling_table",
    "soils_loc_table",
    "soils_lab_table",
    "veg_subplot_table",
    "p2_veg_subplot_table"
  )

  # warnings and messages
  expect_warning(
    test_res <- .build_fia_input_with(test_year, test_states, test_plots, test_folder, .verbose = TRUE),
    "file doesn't exists"
  )
  expect_message(
    .build_fia_input_with(test_year, test_states, test_plots[-7], test_folder, .verbose = TRUE),
    "Getting ready to retrieve"
  )
  expect_no_message(
    .build_fia_input_with(test_year, test_states, test_plots[-7], test_folder, .verbose = FALSE)
  )

  ## result tests
  # we expect a tibble
  expect_s3_class(test_res, "tbl")
  # with the correct names
  expect_named(test_res, expected_names)
  # and for 31 plots as per the filter list we create
  expect_true(nrow(test_res) == 31L)
  # and for the correct counties
  expect_identical(
    unique(test_res[["county"]]) |> sort(),
    purrr::map_depth(test_plots, 1, names) |> purrr::flatten_chr() |> unique() |> sort()
  )
  # and correct_plots
  expect_identical(
    unique(test_res[["plots"]]) |> sort(),
    purrr::map_depth(test_plots, 1, purrr::flatten_dbl) |> purrr::flatten_dbl() |> unique() |> sort()
  )

  # we can test here also if .build_fia_file_path works
  # .build_fia_file_path
  # a correct one
  expect_identical(
    test_res[["plot_table"]][1],
    paste0("grep -E ',INVYR,|,137,(29396|29396.0),' ", test_folder, names(test_plots)[1], "_PLOT.csv")
  )
  # an incorrect one
  expect_identical(
    test_res[["plot_table"]][31], NA_character_
  )

  ## TODO test .build_fia_file_path for customized paths with grep
})
