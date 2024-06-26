#' Util to control messaging
#'
#' @param msg Usually a \code{\link[cli]{cli_inform}} call
#'
#' @noRd
verbose_msg <- function(msg, .verbose) {

  # assertions
  assertthat::assert_that(
    assertthat::is.flag(.verbose),
    msg = cli::cli_abort(".verbose must be logical (TRUE/FALSE)")
  )

  # show message if needed
  if (.verbose) {
    msg
  }
}

#' Show plots for any inventory
#'
#' Show plots with minimal metadata from any inventory
#'
#' This function show the plots available in any inventory for the given administrative units.
#' Take into account that this can potentially show all plots in any inventory, so the object
#' returned can be memory heavy. This also will return all plots recorded, but it doesn't mean
#' that those plots have associated data (tree, understory...), just that they exist.
#'
#' @param inventory Character indicating the inventory. Allowed values are \code{"FIA"} for the
#'   USA forest inventory, \code{"FFI"} for the French \emph{Inventaire Forestier} and
#'   \code{"IFN"}, for the Spanish \emph{Inventario Forestal Nacional}.
#'
#' @param folder Character, path to the folder containing the \code{inventory} files.
#' @param ... Other arguments, depending on the \code{inventory}, see inventory sections.
#'
#' @section FIA:
#' FIA needs an extra argument, \code{states}, a character vector with the two-letter
#' code for the desired states.
#'
#' @section FFI:
#' FFI needs an extra argument, \code{departments}, a character vector with the desired
#' department codes.
#'
#' @section IFN:
#' IFN needs two extra arguments, \code{provinces}, a character vector with the numeric codes for
#' the provinces and \code{versions}, a character vector with the IFN versions to look at
#' (\code{"ifn2"}, \code{"ifn3"} or/and \code{"ifn4"}).
#'
#' @return A \code{\link[sf]{sf}} spatial object in which each row is a plot,. The metadata provided
#'   varies depending on the inventory, but usually includes the state (FIA) / department (FFI)/
#'   provincia (IFN) and year/date/IFN version
#'
#' @examples
#' \donttest{
#' \dontrun{
#' library(forestables)
#'
#' # FIA
#' show_plots_from("FIA", folder = "path/to/fia/data", states = "OR")
#' # FFI
#' show_plots_from("FFI", folder = "path/to/ffi/data", departments = "21")
#' # IFN
#' show_plots_from("IFN", folder = "path/to/ifn/data", provinces = "24", versions = "ifn4")
#' }
#' }
#'
#' @export
show_plots_from <- function(inventory = c("FIA", "FFI", "IFN"), folder = ".", ...) {

  # General Assertions
  # grep
  assertthat::assert_that(
    .sys_cmd_warning()
  )

  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort(c(
      "Folder especified ({.path {folder}}) doesn't exist.",
      "i" = "Please create the folder first and populate it with the needed {inventory} files"
    ))
  )

  # Switch to each inventory functions, but ensuring the inventory is one of the allowed ones.
  inventory <- match.arg(inventory)
  res <- switch(
    inventory,
    "FIA" = show_plots_from_fia(folder, ..., .call = rlang::caller_env(0)),
    "FFI" = show_plots_from_ffi(folder, ..., .call = rlang::caller_env(0)),
    "IFN" = show_plots_from_ifn(folder, ..., .call = rlang::caller_env(0))
  )

  return(res)
}

#' Create a compatible plots filter list object
#'
#' Create a compatible plots filter list from the result of \code{link{show_plots_from}}
#'
#' This function takes the result of \code{link{show_plots_from}}, or a compatible object and
#' creates a filter list ready to be use with \code{\link{ffi_to_tibble}},
#' \code{\link{fia_to_tibble}} or \code{\link{ifn_to_tibble}}. Internal heuristics determine the
#' inventory from the data supplied.
#'
#' @param plots_info Object resulted from \code{link{show_plots_from}}, or a compatible one
#'   (\emph{i.e.} the same object after some plot filtering).
#'
#' @return A list object compatible with the \code{filter_list} argument of
#'   \code{\link{ffi_to_tibble}}, \code{\link{fia_to_tibble}} or \code{\link{ifn_to_tibble}}
#'
#' @examples
#' \donttest{
#' \dontrun{
#' library(forestables)
#'
#' # FIA
#' show_plots_from("FIA", folder = ".", states = "OR") |>
#'   create_filter_list()
#' # FFI
#' show_plots_from("FFI", folder = ".", departments = "21") |>
#'   create_filter_list()
#' # IFN
#' show_plots_from("IFN", folder = ".", provinces = "24", version = "ifn4") |>
#'   create_filter_list()
#' }
#' }
#'
#' @export
create_filter_list <- function(plots_info) {

  inventory_function <- NULL
  if ("COUNTYCD" %in% names(plots_info)) {
    inventory_function <- create_filter_list_fia
  }
  if ("DEP" %in% names(plots_info)) {
    inventory_function <- create_filter_list_ffi
  }
  if ("province_code" %in% names(plots_info)) {
    inventory_function <- create_filter_list_ifn
  }

  res <- plots_info |>
    inventory_function()

  return(res)
}

#' Function to read inventory files
#'
#' Read inventory files
#'
#' This function dispatch the correct reading function depending on the inventory and the file
#' format. For FIA and FFI, uses \code{\link[data.table]{fread}} to read the csv files. This way
#' we can leverage the options of \code{fread} to execute \code{grep} system tool to prefilter the
#' rows and others. For IFN, it uses the custom \code{\link{.read_ifn_data}}.
#'
#' @param input character vector as provided by the .build_path functions for each inventory.
#' @param ... optional arguments for the reading function. Most usually fo providing
#'   a list of columns to read with the \code{select} argument.
#' @param .ifn logical value (default \code{FALSE}), indicating if the inventory read is the IFN.
#'   This is needed because the IFN is in DB formats, not csv formats and we need to use a custom
#'   function
#'
#' @return A \code{\link[dtplyr]{lazy_dt}} object, with immutable set to TRUE (to avoid shenanigans
#'   with caching if used)
#' @noRd
.read_inventory_data <- function(input, ..., .ifn = FALSE) {

  # check if we are reading IFN data
  if (isTRUE(.ifn)) {
    res <- .read_ifn_data(input, ...)
    return(dtplyr::lazy_dt(res, immutable = TRUE))
  }

  # check if special input is provided
  if (stringr::str_detect(input, "^grep")) {
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

#' Helper for system commands needed
#'
#' Check if a system command exists
#'
#' This function checks if the especified command exists in the SO and through an informative
#' warning if not.
#'
#' @param cmd Character with the system command to check. Default to grep, as is the most common
#'   check in the package.
#' @param warn_vector Character vector \emph{a la cli} for the warning to show. Default to grep
#'   command warning.
#'
#' @return \code{invisible(TRUE)} if command exists, \code{invisible(FALSE)} and a warning if it
#'   doesn't.
#'
#' @noRd
.sys_cmd_warning <- function(
  cmd = "grep",
  warn_vector = c(
    "x" = "{.emph {cmd}} system utility not found.",
    "i" = "In Windows, {.emph {cmd}} is part of the {.emph RTools} suite, that can be
    installed from the CRAN page: {.url https://cran.r-project.org/bin/windows/Rtools/}",
    "i" = "Mac and Linux users should have {.emph {cmd}} already installed, but if it is missing,
    please check your package manager to install it."
  )
) {
  grep_path <- Sys.which(cmd)
  if (stringr::str_length(grep_path) < 4) {
    cli::cli_warn(warn_vector)
    return(invisible(FALSE))
  }

  # This is some cli for
  # debugging cli::cli_inform(c("#####DEBUG#####", "v" = "{.emph {cmd}} found in {.envvar PATH}"))
  return(invisible(TRUE))
}

#' Cleaning empty results
#'
#' Cleaning inventory results to filter out empty data
#'
#' This functions remove plot rows with empty data in the desired nested columns.
#'
#' @param inventory_data Data from an inventory as obtained from \code{\link{ifn_to_tibble}},
#'   \code{\link{fia_to_tibble}} or \code{\link{ffi_to_tibble}}.
#' @param cols vector with column names to clean from empty results. Can be one or more of
#'   \code{"tree"}, \code{"shrubs"}, \code{"herbs"} and \code{"regen"}. If more than one, only
#'   plots with data in all columns selected will be retained. \code{"shrubs"} and \code{"herbs"}
#'   are inside \code{"understory"} column, and should be noted that IFN inventory never have
#'   \code{"herbs"} data, so cleaning by it will always return an empty tibble.
#'
#' @return A tibble the same as \code{inventory_data} with the empty data removed for the columns
#'   selected.
#'
#' @examples
#' \donttest{
#' \dontrun{
#' library(forestables)
#'
#' # FFI
#' ffi_to_tibble(
#'   departments = c("01"), years = c(2019),
#'   filter_list = list("01" = c(1404119)),
#'   folder = "path/to/ffi/data"
#' ) |>
#'   clean_empty(c("tree", "regen", "shrub", "herbs"))
#'
#' # FIA
#' fia_to_tibble(
#'   years = 2019, states = c("OR"),
#'   filter_list = list("OR" = list("59" = c(76413))),
#'   folder = "path/to/fia/data"
#' ) |>
#'   clean_empty(c("tree", "regen", "shrub", "herbs"))
#'
#' # IFN (never clean by "herbs", as is always empty)
#' ifn_to_tibble(
#'   provinces = c("24"), versions = c("ifn3"),
#'   filter_list = list("24" = c("24_0270_xx_A4_xx")),
#'   folder = "path/to/ifn/data"
#' ) |>
#'   clean_empty(c("tree", "regen", "shrub"))
#' }
#' }
#'
#' @export
clean_empty <- function(inventory_data, cols) {

  ## assertions
  # inventory_data has the necessary columns
  assertthat::assert_that(
    assertthat::has_name(inventory_data, "tree"),
    assertthat::has_name(inventory_data, "regen"),
    assertthat::has_name(inventory_data, "understory"),
    msg = cli::cli_abort(
      "{.arg inventory_data} must have columns for tree, regen and understory data"
    )
  )
  # cols
  assertthat::assert_that(
    is.null(cols) || all(cols %in% c("tree", "regen", "shrub", "herbs")),
    msg = cli::cli_abort(
      "{.arg cols} must be one or more of 'tree', 'regen', 'shrub' or 'herbs'"
    )
  )

  if (is.null(cols)) {
    return(inventory_data)
  }

  # we create a filter vector for each column (outer for main columns, inner for understory columns)
  # and combine them to know the rows (plots) that have data in all of them
  filter_vector <- cols |>
    purrr::map(
      .f = \(data_col) {
        # outer cols
        if (data_col %in% c("tree", "regen")) {
          inventory_data[[data_col]] |>
            purrr::map_lgl(.f = rlang::is_empty)
        } else {
          # inner understory cols
          inventory_data[["understory"]] |>
            purrr::map(data_col) |>
            purrr::flatten() |>
            purrr::map_lgl(.f = rlang::is_empty)
        }
      }
    ) |>
    # we have empty rows in each data, we need the opposite, non empty rows to filter by
    purrr::map(.f = `!`) |>
    # combine the filters with & to have the rows that have data in all columns
    purrr::reduce(.f = `&`)

  inventory_data |>
    dplyr::filter(filter_vector)

  # inventory_data |>
  #   dplyr::filter(
  #     dplyr::if_all(
  #       dplyr::contains(cols),
  #       ~ !purrr::map_lgl(.x, rlang::is_empty)
  #     )
  #   )
}

#' Convert inventory data to sf based on coords and crs present
#'
#' Use coords vars and crs to convert to sf
#'
#' @param inventory_data Data from an inventory as obtained from
#'   \code{\link{ifn_to_tibble}}, \code{\link{fia_to_tibble}} or
#'   \code{\link{ffi_to_tibble}}.
#'
#' @return An sf object with the same data as \code{inventory_data}
#'   and a new column with the original crs for traceability.
#'
#' @examples
#' \donttest{
#' \dontrun{
#' library(forestables)
#'
#' # FFI
#' ffi_to_tibble(
#'   departments = c("01"), years = c(2019),
#'   filter_list = list("01" = c(1404119)),
#'   folder = "path/to/ffi/data"
#' ) |>
#'   inventory_as_sf()
#'
#' # FIA
#' fia_to_tibble(
#'   years = 2019, states = c("OR"),
#'   filter_list = list("OR" = list("59" = c(76413))),
#'   folder = "path/to/fia/data"
#' ) |>
#'   inventory_as_sf()
#'
#' # IFN
#' ifn_to_tibble(
#'   provinces = c("24"), versions = c("ifn3"),
#'   filter_list = list("24" = c("24_0270_xx_A4_xx")),
#'   folder = "path/to/ifn/data"
#' ) |>
#'   inventory_as_sf()
#' }
#' }
#'
#' @export
inventory_as_sf <- function(inventory_data) {

  ## assertions
  # inventory_data has the necessary columns
  assertthat::assert_that(
    assertthat::has_name(inventory_data, "coordx"),
    assertthat::has_name(inventory_data, "coordy"),
    assertthat::has_name(inventory_data, "crs"),
    msg = cli::cli_abort(
      "{.arg inventory_data} must have columns for coordinates and crs"
    )
  )

  inventory_data |>
    dplyr::group_by(.data$crs) |>
    dplyr::group_modify(
      .f = \(crs_subset, crs) {
        sf::st_as_sf(crs_subset, coords = c("coordx", "coordy"), crs = unique(crs$crs)) |>
          sf::st_transform(crs = 4326) |>
          dplyr::mutate(crs_orig = crs$crs) |>
          dplyr::rename(coord_sys_orig = "coord_sys")
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(crs = 4326) |>
    sf::st_as_sf()
}

#' Reorder inventory output columns to uniformize them
#'
#' Reorder inventory output columns to uniformize them
#'
#' This function uniformizes de results of different inventories, ordering the variables to all
#' have the same order in the common columns. The order is:
#' * id_unique_code
#' * year
#' * plot
#' * coord vars (coordx, coordy, coord_sys, crs)
#' * topo vars (elev, aspect, slope)
#' * country
#' * _inventory unique vars_ (variables that are unique to that inventory)
#' * tree nested column
#' * understory nested column
#' * regen nested column
#'
#' @param inventory inventory nested data frame as obtained from `*_to_tibble` functions.
#'
#' @noRd
reorder_inventory_output <- function(inventory, inventory_cols = "metadata") {

  # switch
  relocated_vars <- switch(
    inventory_cols,
    metadata = c(
      # common vars
      "id_unique_code", "year", "plot",
      "coordx", "coordy", "coord_sys", "crs",
      "elev", "aspect", "slope",
      "country",
      # fia unique
      "state_code", "state_ab", "state_name", "county_code",
      "p3panel", "p2veg_sampling_status_cd", "p2veg_sampling_level_detail",
      "rscd", "design_code", "subplot",
      # ifn unique
      "version", "class", "subclass",
      "province_code", "province_name_original", "ca_name_original",
      "sheet_ntm", "huso", "slope_mean", "type",
      # ffi unique
      "dep", "dep_name", "visite",
      # nested cols
      "tree", "understory", "regen"
    ),
    tree = c(
      # common vars
      "tree_id", "sp_code", "sp_name", "status", "density_factor", "dbh", "height",
      # fia unique
      # none
      # ifn unique
      "cubing_form", "quality_wood",
      # ffi unique
      "height_last_recorded", "status5", "espar"
    )
  )

  inventory |>
    dplyr::relocate(dplyr::any_of(relocated_vars))
}