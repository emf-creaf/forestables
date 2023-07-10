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
#' This function show the plots available in any inventory. Take into account that this can
#' potentially show all plots in any inventory, so the object returned can be big.
#'
#' @param inventory Character indicating the inventory. Allowed values are \code{"FIA"} for the
#'   USA forest inventory, \code{"FFI"} for the French \emph{Inventaire Forestier} and
#'   \code{"IFN"}, for the Spanish \emph{Inventario Forestal Nacional}.
#'
#' @param folder Character, path to the folder containing the \code{inventory} files.
#' @param ... Other arguments, depending on the \code{inventory}, see details.
#'
#' @section FIA:
#' FIA inventory needs an extra argument, \code{states}, a character vector with the two-letter
#' code for the desired states.
#'
#' @section FFI:
#' TODO
#'
#' @section IFN:
#' TODO
#'
#' @return A \code{\link[sf]{sf}} spatial object in which each row is a plot,. The metadata provided
#'   varies depending on the inventory, but usually includes the state (FIA) / department (FFI)/
#'   provincia (IFN) and year/date/IFN version
#'
#' @examples
#' library(esus)
#'
#' show_plots_from("FIA", folder = ".", states = "OR")
#' # TODO
#'
#'
#' @family Inventory Utils
#' @export
show_plots_from <- function(inventory = c("FIA", "FFI", "IFN"), folder = ".", ...) {

  # General Assertions
  inventory <- match.arg(inventory)
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort(
      cli::cli_abort(c(
        "Folder especified ({.path {folder}}) doesn't exist.",
        "i" = "Please create the folder first and populate it with the needed {inventory} files"
      ))
    )
  )

  # Switch to each inventory functions
  show_plots_function <- switch(
    inventory,
    "FIA" = show_plots_from_fia,
    "FFI" = NULL,
    "IFN" = NULL
  )

  show_plots_function(folder, ...)
}

