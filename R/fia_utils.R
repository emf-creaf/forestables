#' Function to read the FIA files
#'
#' Read FIA csv file
#'
#' This function uses internally \code{\link[data.table]{fread}} to read the csv files. This way
#' we can leverage the options of \code{fread} to execute \code{grep} system tool to prefilter the
#' rows and others.
#'
#' @param input character vector as provided by \code{\link{.build_fia_file_path}}. See there for
#'   details about the \code{grep} usage.
#' @params ... optional arguments for \code{\link[data.table]{fread}}. Most usually fo providing
#'   a list of columns to read with the \code{select} argument.
#'
#' @return A \code{\link[dtplyr]{lazy_dt}} object, with immutable set to TRUE (to avoid shaenningans
#'   with parallel)
#' @noRd
.read_fia_data <- function(input, ...) {

  # check if special input is provided
  if (stringr::str_detect(input, "^grep -E '")) {
    res <- data.table::fread(cmd = input, ...) |>
      # convert to tibble
      dtplyr::lazy_dt(immutable = TRUE)
    return(res)
  }

  # read the data
  res <- data.table::fread(file = input, ...) |>
    # convert to tibble
    dtplyr::lazy_dt(immutable = TRUE)

  return(res)
}

#' Build the input dataframe to interate by plots for the year
#'
#' Build the input dataframe
#'
#' This function takes the user input (year, states, plots and folder) and build the input to be
#' able to iterate by plots in a year. If no plots filter list is provided, this function uses
#' \code{\link{.get_plots_from_state}} and \code{\link{.trasnsform_plot_summary}} to create a
#' \code{filter_list} with all plots for each state for that year.
#'
#' @inheritParams fia_tables_process
#'
#' @return A data frame with state, county, plot and table file names
#'
#' @norRd
.build_fia_input_with <- function(
  year, states, filter_list, folder, .verbose
) {

  # first, if is null filter list, create it
  if (is.null(filter_list)) {
    filter_list <- purrr::map(
      states,
      .f = \(state) {
        .get_plots_from_state(state, folder) |>
          .transform_plot_summary(year, state)
      }
    ) |>
      purrr::flatten()
  }

  # inform the user about the amount of plots for this year
  verbose_msg(
    cli::cli_inform(c(
      "Getting ready to retrieve {.strong {filter_list |> purrr::flatten() |> purrr::flatten_dbl() |> length()}} plots for {.val {year}}"
    )), .verbose
  )

  purrr::imap(
    filter_list,
    .f = \(counties_list, state) {
      counties_list |>
        tibble::enframe() |>
        tidyr::unnest(cols = value) |>
        purrr::set_names(c("county", "plots")) |>
        dplyr::mutate(state = state) |>
        dplyr::select(state, county, plots)
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      tree_table = .build_fia_file_path(
        state, "tree", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE
      ),
      plot_table = .build_fia_file_path(
        state, "plot", folder,
        .county = county, .plot = plots, .year = year, .custom = FALSE
      ),
      survey_table = .build_fia_file_path(
        state, "survey", folder,
        .county = county, .plot = plots, .year = year, .custom = FALSE
      ),
      cond_table = .build_fia_file_path(
        state, "cond", folder,
        .county = county, .plot = plots, .year = year, .custom = FALSE
      ),
      subplot_table = .build_fia_file_path(
        state, "subplot", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE
      ),
      p3_understory_table = .build_fia_file_path(
        state, "p3_understory", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE
      ),
      seedling_table = .build_fia_file_path(
        state, "seedling", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE
      ),
      soils_loc_table = .build_fia_file_path(
        state, "soils_loc", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE
      ),
      soils_lab_table = .build_fia_file_path(
        state, "soils_lab", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE
      ),
      veg_subplot_table = .build_fia_file_path(
        state, "veg_subplot", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE
      ),
      p2_veg_subplot_table = .build_fia_file_path(
        state, "p2_veg_subplot", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE
      )
    )
}

#' Helper to read the PLOT.csv file from an state to retrieve the list of plots for that state
#' @noRd
.get_plots_from_state <- function(state, folder) {
  res <- .build_fia_file_path(state, "plot", folder) |>
    .read_fia_data(select = c("INVYR", "STATECD", "COUNTYCD", "PLOT", "LAT", "LON")) |>
    dplyr::as_tibble()

  epgs <- 4326
  if (res[["STATECD"]] |> unique() %in%  c(60,64,66,68,69,70)) {
    epgs <- 4269
  }

  res |>
    sf::st_as_sf(
      coords = c("LON", "LAT"),
      crs = sf::st_crs(epgs)
    )
}

#' Helper to transform the plot summary returned by \code{\link{.get_plots_from_state}} in a
#' filter_list object
#' @noRd
.transform_plot_summary <- function(plot_summary, years, state) {

  filter_list <- plot_summary |>
    dplyr::as_tibble() |>
    dplyr::filter(INVYR %in% years) |>
    dplyr::select(COUNTYCD, PLOT) |>
    dplyr::distinct() |>
    dplyr::group_by(COUNTYCD) |>
    dplyr::summarise(plots = list(PLOT)) |>
    tibble::deframe() |>
    list() |>
    purrr::set_names(state)

  return(filter_list)
}

#'
.build_fia_file_path <- function(
  state, type, folder = ".",
  .county = rep(NA, length(state)), .plot = rep(NA, length(state)), .year = NULL, .custom = FALSE
) {

  purrr::pmap_chr(
    .l = list(state, .county, .plot),
    .f = \(state, county, plot) {

      # file ending (beggining will be the state)
      ending <- switch(
        type,
        "tree" = "_TREE.csv",
        "plot" = "_PLOT.csv",
        "survey" = "_SURVEY.csv",
        "cond" = "_COND.csv",
        "subplot" = "_SUBPLOT.csv",
        "p3_understory" = "_VEG_SUBPLOT_SPP.csv",
        "seedling" = "_SEEDLING.csv",
        "soils_loc" = "_SOILS_SAMPLE_LOC.csv",
        "soils_lab" = "_SOILS_LAB.csv",
        "veg_subplot" = "_VEG_SUBPLOT.csv",
        "p2_veg_subplot" = "_P2VEG_SUBPLOT_SPP.csv"
      )

      # return path
      table_path <- fs::path(folder, glue::glue("{state}{ending}"))

      # check file exists
      if (!fs::file_exists(table_path)) {
        cli::cli_warn(c(
          "{.path {table_path}} file doesn't exists",
          "!" = "Please check if {.path {folder}} is the correct path",
          "i" = "Skipping {.path {table_path}}"
        ))
        return(NA_character_)
      }

      if (.custom) {
        if (type %in% c("tree", "p3_understory", "veg_subplot", "p2_veg_subplot", "seedling", "subplot"))
        customized_path <- glue::glue(
          "grep -E ',INVYR,|,{.year},.*,{county},({plot}|{plot}.0),' {table_path}"
        ) else {
          if (type %in% c("soils_lab", "soils_loc", "veg_subplot", "plot", "survey", "cond")) {
            customized_path <- glue::glue(
              "grep -E ',INVYR,|,{county},({plot}|{plot}.0),' {table_path}"
            )
          }
        }

        return(customized_path)
      }

     return(table_path)
    }
  )
}

#' @importFrom rlang `:=`
.extract_fia_metadata <- function(data_processed, vars, county, plot, year, .soil_mode = TRUE) {

  # ORIGINAL names
  vars_orig <- paste0(vars, "_ORIGINAL")

  data_processed <- dtplyr::lazy_dt(data_processed, immutable = TRUE)

  # we need the filtering vars in case they are missing (some tables dont have them)
  if (!all(c("PLOT", "COUNTYCD") %in% data_processed$vars)) {
    data_processed <- data_processed |>
      dplyr::mutate(
        PLOT = plot,
        COUNTYCD = county
      )
  }

  # loop among vars
  purrr::map2(
    .x = vars,
    .y = vars_orig,
    .f = \(var, var_orig) {

      filter_nas <- TRUE
      if (!.soil_mode) {
        filter_nas <- rlang::expr(!is.na(!!rlang::sym(var)))
      }

      # value at most recent year
      var_value <- data_processed |>
        dplyr::filter(
          PLOT == plot,
          COUNTYCD == county,
          !!filter_nas
        ) |>
        dplyr::filter(INVYR == max(INVYR, na.rm = TRUE)) |>
        dplyr::pull(var)

      # value at queried year
      var_orig_value <- data_processed |>
        dplyr::filter(
          PLOT == plot,
          COUNTYCD == county,
          INVYR == year
        ) |>
        dplyr::pull(var)

      # NA if data is not found
      if (length(var_orig_value) < 1) {
        var_orig_value <- NA
      }
      if (length(var_value) < 1) {
        var_value <- NA
      }

      # build the tibble
      dplyr::tibble(
        !!var := var_value,
        !!var_orig := var_orig_value
      )
    }
  ) |>
    purrr::list_cbind() |>
    data.table::as.data.table() |>
    dtplyr::lazy_dt(immutable = TRUE)
}
