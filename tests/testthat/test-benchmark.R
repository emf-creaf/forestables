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


