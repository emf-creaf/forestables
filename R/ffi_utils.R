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
    year,  filter_list, folder, .verbose, .call = rlang::caller_env()
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
  
# browser()
  dep_list <- filter_list
  
      dep_list |>
        tibble::enframe() |>
        tidyr::unnest(cols = value) |>
        purrr::set_names(c("dep", "plots")) |>
        dplyr::select(dep, plots) |>  
        # purrr::list_rbind() |>
        dplyr::mutate(
          plot_table = .build_ffi_file_path(
            dep, "plot", folder,
            .plot = plots, 
            .year = year, 
            .custom = TRUE,
            .call = .call
          ),
          tree_table = .build_ffi_file_path(
            dep, "tree", folder,
            .plot = plots, 
            .year = year, 
            .custom = TRUE,
            .call = .call
          ),
          shrub_table = .build_ffi_file_path(
            dep, "shrub", folder,
            .plot = plots, 
            .year = year, 
            .custom = TRUE,
            .call = .call
          ),
          soils_table = .build_ffi_file_path(
            dep, "soils", folder,
            .plot = plots, 
            .year = year, 
            .custom = TRUE,
            .call = .call
          )
          )

    
}



#' Create the path and system call for reading FFI csv's
#'
#' Create FFI csv file path with extra sugar
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


.build_ffi_file_path <- function(
    dep, type, folder = ".",
    .plot = rep(NA, length(dep)),
    .year = NULL,
    .custom = FALSE,
    .call = rlang::caller_env()
    ) 
{ 
  purrr::pmap_chr(
    .l = list(dep, .plot),
    .f = \(dep,  plot) 
    {
      
      # browser()
      # file ending (beginning will be the state)
      ending <- switch(
        type,
        "tree" = "ARBRE.csv",
        "plot" = "PLACETTE.csv",
        "shrub" = "FLORE.csv",
        "soils" = "ECOLOGIE.csv"
      )
      
      # return path
      table_path <- fs::path(folder, glue::glue("{ending}"))
      
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
        if (type %in% c("plot")) {
          customized_path <- glue::glue(
            "grep -E ';CAMPAGNE;|;{.year};.*;{plot};.*;{dep},' {table_path}"
          )
          } else {
            if (type %in% c("tree", "shrub", "soils")) {
              customized_path <- glue::glue(
                "grep -E ';CAMPAGNE;|;{.year};;{plot};' {table_path}"
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

