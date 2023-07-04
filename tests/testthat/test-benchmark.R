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
  dplyr::group_by(INVYR, STATECD, COUNTYCD) |>
  dplyr::slice_sample(prop = 0.1) |>
  dplyr::group_by(STATECD) |>
  dplyr::group_split() |>
  purrr::set_names("CA", "OR", "WA") |>
  purrr::imap(
    .f = \(state_data, state_name) {esus:::.transform_plot_summary(state_data, c(2005, 2015), state_name)}
  ) |>
  purrr::flatten()

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
