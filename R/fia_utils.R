#' Build the input dataframe to iterate by plots for the year
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

    # create safe versions of .get_plots_from_state and .transform_plot_summary
    get_plots_safe <- purrr::safely(
      .get_plots_from_state,
      otherwise = tibble::tibble(
        "INVYR" = vector(),
        "STATECD" = vector(),
        "COUNTYCD" = vector(),
        "PLOT" = vector(),
        "STATEAB" = vector(),
        "geometry" = vector()
      )
    )
    transform_safe <- purrr::safely(
      .transform_plot_summary,
      otherwise = list()
    )

    filter_list <- purrr::map(
      states,
      .f = \(state) {
        res <- get_plots_safe(state, folder, .call = .call)[["result"]] |>
          transform_safe(year, state)
        res[["result"]]
      }
    ) |>
      purrr::flatten()
  }

  # inform the user about the amount of plots for this year
  verbose_msg(
    cli::cli_inform(c(
      "Getting ready to retrieve
      {.strong {filter_list |> purrr::flatten() |> purrr::flatten_dbl() |> length()}}
      plots for {.val {year}}"
    )), .verbose
  )

  purrr::imap(
    filter_list,
    .f = \(counties_list, state_fil) {
      counties_list |>
        tibble::enframe() |>
        tidyr::unnest(cols = "value") |>
        purrr::set_names(c("county", "plots")) |>
        dplyr::mutate(state = state_fil) |>
        dplyr::select("state", "county", "plots")
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      tree_table = .build_fia_file_path(
        .data$state, "tree", folder,
        .county = .data$county, .plot = .data$plots, .year = year, .custom = TRUE, .call = .call
      ),
      plot_table = .build_fia_file_path(
        .data$state, "plot", folder,
        .county = .data$county, .plot = .data$plots, .year = year, .custom = TRUE, .call = .call
      ),
      survey_table = .build_fia_file_path(
        .data$state, "survey", folder,
        .county = .data$county, .plot = .data$plots, .year = year, .custom = FALSE, .call = .call
      ),
      cond_table = .build_fia_file_path(
        .data$state, "cond", folder,
        .county = .data$county, .plot = .data$plots, .year = year, .custom = TRUE, .call = .call
      ),
      subplot_table = .build_fia_file_path(
        .data$state, "subplot", folder,
        .county = .data$county, .plot = .data$plots, .year = year, .custom = TRUE, .call = .call
      ),
      p3_understory_table = .build_fia_file_path(
        .data$state, "p3_understory", folder,
        .county = .data$county, .plot = .data$plots, .year = year, .custom = TRUE, .call = .call
      ),
      seedling_table = .build_fia_file_path(
        .data$state, "seedling", folder,
        .county = .data$county, .plot = .data$plots, .year = year, .custom = TRUE, .call = .call
      ),
      veg_subplot_table = .build_fia_file_path(
        .data$state, "veg_subplot", folder,
        .county = .data$county, .plot = .data$plots, .year = year, .custom = TRUE, .call = .call
      ),
      p2_veg_subplot_table = .build_fia_file_path(
        .data$state, "p2_veg_subplot", folder,
        .county = .data$county, .plot = .data$plots, .year = year, .custom = TRUE, .call = .call
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
    .read_inventory_data(
      select = c("INVYR", "STATECD", "COUNTYCD", "PLOT", "LAT", "LON")
    ) |>
    # we need to weed out some plots that have all NAs in coordinates in some states
    # (i.e. CA or WA)
    dplyr::group_by(.data$STATECD, .data$COUNTYCD, .data$PLOT) |>
    dplyr::filter(!all(is.na(.data$LAT))) |>
    dplyr::arrange(.data$INVYR) |>
    tidyr::fill(c("LAT", "LON"), .direction = "updown") |>
    dplyr::mutate(STATEAB = state) |>
    dplyr::as_tibble()


  # For some states CRS is different so we use the correct crs to build the sf and transform
  # to 4326 to have all in the same coordinate system.
  if (unique(plot_data[["STATECD"]]) %in%  c(60, 64, 66, 68, 69, 70)) {
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
      states, .f = .get_plots_from_state, folder = folder
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
.transform_plot_summary <- function(plot_summary, years, states) {

  filter_list <- plot_summary |>
    dplyr::as_tibble() |>
    dplyr::filter(.data$INVYR %in% years, .data$STATEAB %in% states) |>
    dplyr::select("STATEAB", "COUNTYCD", "PLOT") |>
    dplyr::distinct() |>
    dplyr::group_by(.data$STATEAB, .data$COUNTYCD) |>
    dplyr::summarise(plots = list(.data$PLOT)) |>
    dplyr::group_map(.f = \(state_plots, state_name) {
      tibble::deframe(state_plots) |>
        list() |>
        purrr::set_names(state_name[[1]])
    }) |>
    purrr::flatten()

  return(filter_list)
}

#' helper for translating numeric state codes to names
#' @noRd
.translate_fia_states <- function(states_numeric = NULL, states_abbr = NULL) {

  res <- NA

  if (is.null(states_abbr) && (!is.null(states_numeric))) {
    res <- fia_states_dictionary |>
      dplyr::filter(.data$VALUE %in% states_numeric) |>
      dplyr::pull(.data$ABBR) |>
      unique()
  }

  if ((!is.null(states_abbr)) && is.null(states_numeric)) {
    res <- fia_states_dictionary |>
      dplyr::filter(.data$ABBR %in% states_abbr) |>
      dplyr::pull(.data$VALUE) |>
      unique()
  }

  return(res)
}

#' Create the \code{filter_list} for FIA inventory
#'
create_filter_list_fia <- function(plots_info) {

  ## assertions
  # this process is independent from fia_to_tibble, and the user can modify plots_info to
  # filter plots and counties. So we can not assume plots_info is going to have the str we
  # need. So, we assert and inform the user if something is wrong

  ## TODO
  # assert class
  assertthat::assert_that(
    inherits(plots_info, c("tbl", "sf", "data.frame")),
    msg = cli::cli_abort(c(
      "{.arg plots_info} must be a data.frame or something coercible to one,
      as the result of {.code show_plots_from()}"
    ))
  )
  # assert col names
  assertthat::assert_that(
    all(names(plots_info) %in% c("INVYR", "STATECD", "COUNTYCD", "PLOT", "STATEAB", "geometry")),
    msg = cli::cli_abort(c(
      "{.arg plots_info} provided don't have the expected names",
      "i" = "Expected names are {.value {c('INVYR', 'STATECD', 'COUNTYCD', 'PLOT', 'geometry')}}"
    ))
  )
  # assert there is data
  assertthat::assert_that(
    nrow(plots_info) > 0,
    msg = cli::cli_abort(c(
      "{.arg plots_info} must have at least one row"
    ))
  )

  # loop around states
  plots_years <- plots_info[["INVYR"]] |>
    unique()
  states_names <- plots_info[["STATECD"]] |>
    unique() |>
    .translate_fia_states()

  res <- plots_info |>
    dplyr::group_by(.data$STATECD) |>
    dplyr::group_split() |>
    purrr::set_names(states_names) |>
    purrr::imap(
      .f = \(state_data, state_name) {
        .transform_plot_summary(state_data, plots_years, state_name)
      }
    ) |>
    purrr::flatten()

  return(res)
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
#'   \code{\link{.read_inventory_data}}.
#'
#' @noRd
.build_fia_file_path <- function(
  state, type, folder = ".",
  .county = rep(NA, length(state)),
  .plot = rep(NA, length(state)), .year = NULL, .custom = FALSE,
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
        if (type %in% c(
          "tree", "p3_understory", "veg_subplot", "p2_veg_subplot", "seedling", "subplot"
        )) {
          customized_path <- glue::glue(
            'grep -P ",INVYR,|,{.year},.*,{county},({plot}|{plot}.0)," {table_path}'
          )
        } else {
          if (type %in% c("soils_lab", "soils_loc", "veg_subplot", "plot", "survey", "cond")) {
            customized_path <- glue::glue(
              'grep -P ",INVYR,|,{county},({plot}|{plot}.0)," {table_path}'
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
        filter_nas <- rlang::expr(!is.na({{ var }}))
      }

      # value at most recent year
      var_value <- data_processed |>
        dplyr::filter(
          .data$PLOT == plot,
          .data$COUNTYCD == county,
          {{ filter_nas }}
        ) |>
        dplyr::filter(.data$INVYR == max(.data$INVYR, na.rm = TRUE)) |>
        dplyr::pull({{ var }})

      # value at queried year
      var_orig_value <- data_processed |>
        dplyr::filter(.data$PLOT == plot, .data$COUNTYCD == county, .data$INVYR == year) |>
        dplyr::pull({{ var }})

      # NA if data is not found
      if (length(var_orig_value) < 1) {
        var_orig_value <- NA
      }
      if (length(var_value) < 1) {
        var_value <- NA
      }

      # build the tibble
      dplyr::tibble(
        "{var}" := var_value,
        "{var_orig}" := var_orig_value
      )
    }
  ) |>
    purrr::list_cbind() |>
    data.table::as.data.table() |>
    dtplyr::lazy_dt(immutable = TRUE)
}
