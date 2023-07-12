#' Function to read the ffi files
#'
#' Read ffi csv file
#'
#' This function uses internally \code{\link[data.table]{fread}} to read the csv files. This way
#' we can leverage the options of \code{fread} to execute \code{grep} system tool to prefilter the
#' rows and others.
#'
#' @param input character vector as provided by \code{\link{.build_ffi_file_path}}. See there for
#'   details about the \code{grep} usage.
#' @param ... optional arguments for \code{\link[data.table]{fread}}. Most usually fo providing
#'   a list of columns to read with the \code{select} argument.
#'
#' @return A \code{\link[dtplyr]{lazy_dt}} object, with immutable set to TRUE (to avoid shaenningans
#'   with parallel)
#' @noRd
.read_ffi_data <- function(input, ...) {
  
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


#' Build the input dataframe to iterate by plots for the year
#'
#' Build the input dataframe
#'
#' This function takes the user input (year, dep, plots and folder) and build the input to be
#' able to iterate by plots in a year. If no plots filter list is provided, this function uses
#' \code{\link{.get_plots_from_state}} and \code{\link{.trasnsform_plot_summary}} to create a
#' \code{filter_list} with all plots for each state for that year.
#'
#' @inheritParams fia_tables_process
#'
#' @return A data frame with state, dep, plot and table file names
#'
#' @noRd
.build_ffi_input_with <- function(
    year,  filter_list, folder, .verbose
) {
  
  # # first, if is null filter list, create it
  # if (is.null(filter_list)) {
  #   blabalabalabañlb
  # }
  # 
  # # inform the user about the amount of plots for this year
  # verbose_msg(
  #   cli::cli_inform(c(
  #     "Getting ready to retrieve {.strong {filter_list |> purrr::flatten() |> purrr::flatten_dbl() |> length()}} plots for {.val {year}}"
  #   )), .verbose
  # )
  

  dep_list<-filter_list
  
      dep_list |>
        tibble::enframe() |>
        tidyr::unnest(cols = value) |>
        purrr::set_names(c("dep", "plots")) |>
        dplyr::select(dep, plots)|>
   
    dplyr::mutate(
      tree_table = paste0(folder,"ARBRE.CSV"),
      plot_table = paste0(folder,"PLACETTE.CSV"),
      shrub_table = paste0(folder,"FLORE.CSV"),
      soils_table =paste0(folder,"ECOLOGIE.CSV")
      
    )
}




#' Helper function to extract plot and soil metadata from from tables
#'
#' Extract year and most recent metadata for plot
#'
#' This function extracts the metadata for a plot in a year, creating two values for
#' each specified variable in \code{vars}. One with the value for the year selected
#' (\code{VAR_ORIG}) and another with the most recent value (\code{VAR}).
#' This function is intended exclusively to be called by the individual table process
#' functions that needs this functionality (plot and soil tables).
#'
#' @param data_processed Table data after reading and processing.
#' @param vars Character, names of variables to be extracted.
#' @param plot Numeric, codes for dep or plot to be processed
#' @param year Numeric with the year to process
#' @param .soil_mode Logical. If \code{TRUE}, \code{.extract_ffi_metadata} is run on soil mode,
#'   which means that no NAs are filtered before returning most recent data, to allow for
#'   different layers to be retrieved. If \code{FALSE}, then years with \code{NA} for \code{VAR}
#'   are removed prior to find the most recent value.
#'
#' @return A data frame with variables in \code{var} values for the desired year and for the
#'   most recent year with value.
#'
#' @importFrom rlang `:=`
#' @noRd
.extract_ffi_metadata <- function(data_processed, vars,  plot, year, .soil_mode = TRUE) {
  
  # ORIGINAL names
  vars_orig <- paste0(vars, "_ORIGINAL")
  
  data_processed <- dtplyr::lazy_dt(data_processed, immutable = TRUE)
  
  # we need the filtering vars in case they are missing (some tables dont have them)
  if (!("PLOT" %in% data_processed$vars)) {
    data_processed <- data_processed |>
      dplyr::mutate(
        PLOT = plot
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
          !!filter_nas
        ) |>
        dplyr::filter(YEAR == max(YEAR, na.rm = TRUE)) |>
        dplyr::pull(var)
      
      # value at queried year
      var_orig_value <- data_processed |>
        dplyr::filter(
          PLOT == plot,
          YEAR == year
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

