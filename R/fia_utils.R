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
#' @param ... optional arguments for \code{\link[data.table]{fread}}. Most usually fo providing
#'   a list of columns to read with the \code{select} argument.
#'
#' @return A \code{\link[dtplyr]{lazy_dt}} object, with immutable set to TRUE (to avoid shenanigans
#'   with caching if used)
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
#' @noRd
.build_fia_input_with <- function(
  year, states, filter_list, folder, .verbose, .call = rlang::caller_env()
) {

  # first, if is null filter list, create it
  if (is.null(filter_list)) {
    filter_list <- purrr::map(
      states,
      .f = \(state) {
        .get_plots_from_state(state, folder, .call = .call) |>
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
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      ),
      plot_table = .build_fia_file_path(
        state, "plot", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      ),
      survey_table = .build_fia_file_path(
        state, "survey", folder,
        .county = county, .plot = plots, .year = year, .custom = FALSE, .call = .call
      ),
      cond_table = .build_fia_file_path(
        state, "cond", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      ),
      subplot_table = .build_fia_file_path(
        state, "subplot", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      ),
      p3_understory_table = .build_fia_file_path(
        state, "p3_understory", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      ),
      seedling_table = .build_fia_file_path(
        state, "seedling", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      ),
      soils_loc_table = .build_fia_file_path(
        state, "soils_loc", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      ),
      soils_lab_table = .build_fia_file_path(
        state, "soils_lab", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      ),
      veg_subplot_table = .build_fia_file_path(
        state, "veg_subplot", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      ),
      p2_veg_subplot_table = .build_fia_file_path(
        state, "p2_veg_subplot", folder,
        .county = county, .plot = plots, .year = year, .custom = TRUE, .call = .call
      )
    )
}

#' Helper to read the PLOT.csv file from an state to retrieve the list of plots for that state
#' @noRd
.get_plots_from_state <- function(state, folder, .call = rlang::caller_env()) {

  ## TODO Assertion to ensure PLOT.csv file exists, because .build_fia_file_path is fail
  ## resistant, returning always a result (NA_character) to allow its use in loops.
  ## .get_plots_from_state is only called from .build_fia_input_with or show_plots_from_fia,
  ## that can not check for file existence (this is done in the individual plot functions)

  plot_path <- .build_fia_file_path(state, "plot", folder)

  if (is.na(plot_path)) {
    cli::cli_abort(c(
      "{.path {folder}} folder doesn't contain a {.path {state}_PLOT.csv}, aborting."
    ), call = .call)
  }

  # If file exists, business as usual:
  plot_data <- plot_path |>
    .read_fia_data(select = c("INVYR", "STATECD", "COUNTYCD", "PLOT", "LAT", "LON")) |>
    # we need to weed out some plots that have all NAs in coordinates in some states
    # (i.e. CA or WA)
    dplyr::group_by(STATECD, COUNTYCD, PLOT) |>
    dplyr::filter(!all(is.na(LAT))) |>
    dplyr::arrange(INVYR) |>
    tidyr::fill(
      c(LAT, LON), .direction = "updown"
    ) |>
    dplyr::as_tibble()


  # For some states CRS is different so we use the correct crs to build the sf and transform
  # to 4326 to have all in the same coordinate system.
  if (plot_data[["STATECD"]] |> unique() %in%  c(60,64,66,68,69,70)) {
    epgs <- 4269
    res <- plot_data |>
      sf::st_as_sf(
        coords = c("LON", "LAT"),
        crs = sf::st_crs(epgs)
      ) |>
      sf::st_transform(crs = 4326)

    return(res)
  }

  # For most of the states, 4326 and return
  epgs <- 4326
  res <- plot_data |>
    sf::st_as_sf(
      coords = c("LON", "LAT"),
      crs = sf::st_crs(epgs)
    )
  return(res)
}

#' show plots from fia helper
#'
#' Iterate for states and retrieve all the plots
#'
#' @param folder Character, path to folder containing FIA csv files
#' @param states Character vector with two-letter code for states
#' @noRd
show_plots_from_fia <- function(folder, states, .call = rlang::caller_env()) {
  withCallingHandlers(
    purrr::map(
      states, .f = .get_plots_from_state, folder = folder, .call = .call
    ) |>
      purrr::list_rbind() |>
      sf::st_as_sf(),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
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

#' Create the path and system call for reading FIA csv's
#'
#' Create FIA csv file path with extra sugar
#'
#' This function builds the path to FIA table csv files based on the state and type of table.
#' Also, using the type, we add the system call to \code{grep} in those tables which it can
#' be used to avoid loading the whole table.
#'
#' @section \code{grep} system call:
#' \code{grep} system library allows to find patterns in text files. This can be used prior
#' to read the file to feed \code{fread} only with the rows we need. For this we build a
#' regular expression that matches the county and plot code, as well as year in the case of
#' some tables. This way we avoid loading the whole table and only the rows we need.
#' In this case, the regular expression used is:
#' \preformatted{
#' ',INVYR,|,{.year},.*,{county},({plot}|{plot}.0),'
#' }
#' \code{",INVYR,"} matches the first row in all tables, because all tables have the Inventory
#' year variable.
#' \code{"|"} means \code{OR} as in R code. This way we match the first row with the part before
#' "|", \emph{OR} the rows with the data as per the part after "|".
#' \code{,{.year},.*,{county},({plot}|{plot}.0)} part matches any row with the values for
#' year, county and plot in an specific order. First year between commas, after that an
#' unspecified number of characters (\code{".*"}), and county and plot together between
#' commas and separated by a comma.
#' \code{({plot}|{plot}.0)} indicates to match both plot code or plot code with a 0 decimal
#' because some states have this variable as a double value.
#'
#' @param state Character vector with two-letter code for states.
#' @param type Character, table type. One of "tree", "plot", "survey", "cond", "subplot",
#'   "p3_understory",  "seedling", "soils_loc", "soils_lab", "veg_subplot", "p2_veg_subplot".
#' @param folder Character, path to the folder with the FIA csv files.
#' @param .custom Logical indicating that a custom path, with \code{grep} must be created
#' @param .county,.plot, Vectors of the same length as \code{state}, with county and plot codes
#'   to build the \code{grep} command if \code{.custom} is \code{TRUE}.
#' @param .year Numeric value (length one) with the year to build the \code{grep} command
#'   if \code{.custom} is \code{TRUE}.
#'
#' @return Character vector with the paths (or custom command with path) to use with
#'   \code{\link{.read_fia_data}}.
#'
#' @noRd
.build_fia_file_path <- function(
  state, type, folder = ".",
  .county = rep(NA, length(state)), .plot = rep(NA, length(state)), .year = NULL, .custom = FALSE,
  .call = rlang::caller_env()
) {

  purrr::pmap_chr(
    .l = list(state, .county, .plot),
    .f = \(state, county, plot) {

      # file ending (beginning will be the state)
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
        ), call = .call)
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

#' Helper function to extract plot and soil metadata from from tables
#'
#' Extract year and most recent metadata for plot
#'
#' This function extracts the metadata for a plot in a year, creating two values for
#' each especified variable in \code{vars}. One with the value for the year selected
#' (\code{VAR_ORIG}) and another with the most recent value (\code{VAR}).
#' This function is intended exclusively to be called by the individual table process
#' functions that needs this functionality (plot and soil tables).
#'
#' @param data_processed Table data after reading and processing.
#' @param vars Character, names of variables to be extracted.
#' @param county,plot Numeric, codes for county or plot to be processed
#' @param year Numeric with the year to process
#' @param .soil_mode Logical. If \code{TRUE}, \code{.extract_fia_metadata} is run on soil mode,
#'   which means that no NAs are filtered before returning most recent data, to allow for
#'   different layers to be retrieved. If \code{FALSE}, then years with \code{NA} for \code{VAR}
#'   are removed prior to find the most recent value.
#'
#' @return A data frame with variables in \code{var} values for the desired year and for the
#'   most recent year with value.
#'
#' @importFrom rlang `:=`
#' @noRd
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
