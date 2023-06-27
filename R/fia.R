fia_to_tibble <- function(
  years,
  states,
  filter_list,
  folder,
  ...,
  .parallel_options = furrr::furrr_options(scheduling = 1L, stdout = FALSE),
  .verbose = TRUE
) {

  ## Assertions and checks ##
  # states
  assertthat::assert_that(
    is.character(states), length(states) > 0,
    msg = cli::cli_abort("years must be a character vector with at least one state code")
  )
  ## TODO
  # check all states are valid

  # years
  assertthat::assert_that(
    is.numeric(years), length(years) > 0,
    msg = cli::cli_abort("years must be a numeric vector with at least one year")
  )
  ## TODO
  # check years are valid

  # folder
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort( "Folder especified ({.path {folder}}) doesn't exists. Please create the folder first and populate it with the needed FIA csv files")
  )

  ## TODO
  # filter_list

  # parallel options
  assertthat::assert_that(
    inherits(.parallel_options, "furrr_options"),
    msg = cli::cli_abort(".parallel_options must come from {.code furrr::furrr_options}")
  )

  # verbose
  assertthat::assert_that(
    assertthat::is.flag(.verbose),
    msg = cli::cli_abort(".verbose must be logical (TRUE/FALSE)")
  )

  ## inform the user
  verbose_msg(
    cli::cli_inform(
      c("Start", "i" = "Processing {length(years)} year{?s}")
    ),
    .verbose
  )
  ## send the years in loop to process table function
  purrr::map(
    years,
    .f = \(year) {
      fia_tables_process(year, states, filter_list, folder, .parallel_options, .verbose, ...)
    },
    .progress = .verbose
  ) |>
    purrr::list_rbind()
}

fia_tables_process <- function(
  year, states, filter_list, folder, .parallel_options, .verbose, ...
) {

  ## TODO

}



# fia2sf_dt_furrr <- function(input_df, years, ref_species, ref_plant_dictionary) {
#
#   # Asertions and checks
#   if (!is.numeric(years)) {
#     cli::cli_abort("Oops")
#   }
#   # TODO other checks
#
#   # browser()
#
#   purrr::set_names(years) |>
#     purrr::imap(
#       .f = \(year, name) {
#         fia_tables_process_dt_furrr(year, input_df,  ref_species, ref_plant_dictionary) |>
#           dplyr::mutate(year = name)|>
#           dplyr::as_tibble()
#       },
#       .progress = TRUE
#     ) |>
#     purrr::list_rbind()
#
#
# }
