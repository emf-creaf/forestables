testthat::skip()

test_plots <- list(
  "MN", "CA", "AL", "MO", "OH", "OR"
) |> purrr::set_names()

test_plots[["MN"]] <- list(
  "137" = c(29396, 25064),
  "17" = 20005,
  "31" = 20421,
  "71" = 20210
)

test_plots[["CA"]] <- list(
  "15" = c(53519, 63676),
  "105" = c(70128, 83043),
  "61" = 69600
)

test_plots[["AL"]] <- list(
  "121" = 33,
  "73" = 20,
  "131" = 73,
  "1" = 27,
  "81" = 13
)

test_plots[["MO"]] <- list(
  "113" = 20144,
  "119" = 20129,
  "225" = 20168,
  "221" = 20084,
  "88" = 20012
)

test_plots[["OH"]] <- list(
  "41" = 3878,
  "167" = 2121,
  "25" = 5374,
  "103" = 4704,
  "53" = 3579
)

test_plots[["OR"]] <- list(
  "59" = c(76413, 76413),
  "17" = 63905,
  "31" = 95724,
  "71" = 99371
)

year <- 2010
states <- names(test_plots)
filter_list <- test_plots
folder <- "../international_inventories/data/fia/FIA_DATAMART_MARCH_2023/"
.parallel_options <- furrr::furrr_options(scheduling = 1L, stdout = TRUE)
.verbose <- TRUE

# debug(esus:::fia_tables_process)
tictoc::tic()
foo <- esus:::fia_tables_process(year, states, filter_list, folder, .parallel_options, .verbose)
tictoc::toc()

test <- bench::mark(
  seq = {
    future::plan(future::sequential)
    seq_test <- esus:::fia_tables_process(year, states, filter_list, folder, .parallel_options, .verbose)
    future::plan(future::sequential)
    seq_test
  },
  multisession = {
    future::plan(future::multisession, workers = 3)
    ms_test <- esus:::fia_tables_process(year, states, filter_list, folder, .parallel_options, .verbose)
    future::plan(future::sequential)
    ms_test
  },
  min_time = Inf,
  iterations = 2,
  check = TRUE
)

test_fia <- bench::mark(
  # seq = {
  #   future::plan(future::sequential)
  #   seq_test_fia <- esus:::fia_to_tibble(
  #     years = c(2017,2018,2019,2020),
  #     states, filter_list, folder, .parallel_options = .parallel_options, .verbose = .verbose
  #   )
  #   future::plan(future::sequential)
  #   seq_test_fia
  # },
  multisession = {
    future::plan(future::multisession, workers = 6)
    ms_test_fia <- esus:::fia_to_tibble(
      years = c(2017,2018,2019,2020),
      states, filter_list, folder, .parallel_options = .parallel_options, .verbose = .verbose
    )
    future::plan(future::sequential)
    ms_test_fia
  },
  min_time = Inf,
  iterations = 2,
  check = FALSE
)


# parallel ------------------------------------------------------------------------------------

library(esus)
library(sf)

filter_list <- show_plots_from(
  "FIA",
  folder = "../international_inventories/data/fia/FIA_DATAMART_MARCH_2023",
  states = c("OR", "CA", "WA")
) |>
  dplyr::filter(INVYR %in% c(2005, 2015)) |>
  dplyr::group_by(STATECD, COUNTYCD, PLOT) |>
  dplyr::filter(length(INVYR) > 1) |>
  dplyr::group_by(INVYR, STATECD) |>
  dplyr::slice_sample(prop = 0.2) |>
  dplyr::group_by(STATECD, COUNTYCD, PLOT) |>
  dplyr::filter(length(INVYR) > 1) |>
  create_filter_list_fia()

future::plan(future.callr::callr, workers = 6)
res <- fia_to_tibble(
  years = c(2005, 2015),
  states = c("OR", "CA", "WA"),
  filter_list = filter_list,
  folder = "../international_inventories/data/fia/FIA_DATAMART_MARCH_2023/"
) |>
  dplyr::filter(purrr::map_lgl(tree, \(x) {!nrow(x) < 1})) |>
  dplyr::group_by(ID_UNIQUE_PLOT) |>
  dplyr::filter(length(YEAR) > 1) |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = sf::st_crs(4326))

test <- res |>
  tidyr::unnest(tree, names_sep = "_") |>
  dplyr::group_by(YEAR, ID_UNIQUE_PLOT) |>
  dplyr::summarise(DIA = mean(tree_DIA, na.rm = TRUE), HT = mean(tree_HT, na.rm = TRUE))

states_map <- maps::map("state", plot = FALSE, fill = TRUE) |>
  sf::st_as_sf() |>
  dplyr::filter(ID %in% c("california", "oregon", "washington"))

par(mfrow = c(1,2))
mapsf::mf_theme("darkula")
mapsf::mf_shadow(states_map)
mapsf::mf_map(
  x = states_map, type = "base", add = TRUE
)
mapsf::mf_map(
  x = test |> dplyr::filter(YEAR == 2005),
  var = c("DIA", "HT"),
  type = "prop_choro",
  alpha = 0.5
)
mapsf::mf_title("2005")
mapsf::mf_theme("darkula")
mapsf::mf_shadow(states_map)
mapsf::mf_map(
  x = states_map, type = "base", add = TRUE
)
mapsf::mf_map(
  x = test |> dplyr::filter(YEAR == 2015),
  var = c("DIA", "HT"),
  type = "prop_choro",
  alpha = 0.5
)
mapsf::mf_title("2015")
dev.off()


# tictoc::tic()
# future::plan(future.callr::callr, workers = 6)
# west_usa_study_data <- fia_to_tibble(
#     years = c(2005, 2015),
#     states = c("OR", "CA", "WA"),
#     filter_list = filter_list,
#     folder = "../international_inventories/data/fia/FIA_DATAMART_MARCH_2023/",
#     .parallel_options = furrr::furrr_options(scheduling = 2L, stdout = TRUE),
#     .verbose = TRUE
# )
# tictoc::toc()

bench_press_parallel <- bench::press(
  workers = c(3,6,12),
  scheduling = c(1L,2L),
  {
    future::plan(future.callr::callr, workers = workers)
    opt <- furrr::furrr_options(scheduling = scheduling, stdout = TRUE)
    bench::mark(
      iterations = 1,
      check = FALSE,
      fia_to_tibble(
        years = c(2005, 2015),
        states = c("OR", "CA", "WA"),
        filter_list = filter_list,
        folder = "../international_inventories/data/fia/FIA_DATAMART_MARCH_2023/",
        .parallel_options = opt,
        .verbose = TRUE
      )
    )
  }
)
