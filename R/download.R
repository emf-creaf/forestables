#' Download, unzip and store FIA inventory data
#'
#' @inheritParams download_inventory
#'
#' @noRd
.download_fia <- function(destination, .verbose) {

  # downloading
  verbose_msg(
    cli::cli_inform(c(
      "i" = "Downloading FIA available data"
    )),
    .verbose = .verbose
  )

  # `while` control value
  tries <- 0L

  while (tries < 11L) {
    is_downloaded <- curl::multi_download(
      urls = "https://apps.fs.usda.gov/fia/datamart/CSV/CSV_FIADB_ENTIRE.zip",
      destfiles = fs::path(destination, "fia.zip"),
      resume = TRUE,
      progress = .verbose
    )

    if (is_downloaded$success == FALSE) {
      tries <- tries + 1L
    } else {
      tries <- 11
    }
  }

  if (is_downloaded$success == FALSE) {
    cli::cli_abort(c("x" = "Something went wrong during the download"))
  }

  # unzipping
  verbose_msg(
    cli::cli_inform(c(
      "i" = "Unzipping downloaded data in {.path {destination}}"
    )),
    .verbose = .verbose
  )

  extracted_files <- utils::unzip(
    zipfile = fs::path(destination, "fia.zip"),
    exdir = destination
  )

  failed_files <- extracted_files |>
    purrr::keep(.p = is.null) |>
    names() |>
    basename()

  if (length(failed_files) > 0) {
    cli::cli_warn(c(
      "x" = "The following files failed to be unzipped:",
      "{.file {failed_files}}"
    ))
  }

  verbose_msg(
    cli::cli_alert_success("Done!"),
    .verbose = .verbose
  )

  # end
  return(invisible(TRUE))
}

#' Download, unzip and store FFI inventory data
#'
#' @inheritParams download_inventory
#'
#' @noRd
.download_ffi <- function(destination, .verbose) {

  # downloading
  verbose_msg(
    cli::cli_inform(c(
      "i" = "Downloading FFI available data"
    )),
    .verbose = .verbose
  )

  # `while` control value
  tries <- 0L

  while (tries < 11L) {
    is_downloaded <- curl::multi_download(
      urls = "https://inventaire-forestier.ign.fr/dataifn/data/export_dataifn_2005_2022.zip",
      destfiles = fs::path(destination, "ffi.zip"),
      resume = TRUE,
      progress = .verbose
    )

    if (is_downloaded$success == FALSE) {
      tries <- tries + 1L
    } else {
      tries <- 11
    }
  }

  if (is_downloaded$success == FALSE) {
    cli::cli_abort(c("x" = "Something went wrong during the download"))
  }

  # unzipping
  verbose_msg(
    cli::cli_inform(c(
      "i" = "Unzipping downloaded data in {.path {destination}}"
    )),
    .verbose = .verbose
  )

  extracted_files <- utils::unzip(
    zipfile = fs::path(destination, "ffi.zip"),
    exdir = destination
  )

  failed_files <- extracted_files |>
    purrr::keep(.p = is.null) |>
    names() |>
    basename()

  if (length(failed_files) > 0) {
    cli::cli_warn(c(
      "x" = "The following files failed to be unzipped:",
      "{.file {failed_files}}"
    ))
  }

  verbose_msg(
    cli::cli_alert_success("Done!"),
    .verbose = .verbose
  )

  # end
  return(invisible(TRUE))
}

#' Download, unzip and store IFN inventory data
#'
#' @inheritParams download_inventory
#'
#' @noRd
.download_ifn <- function(destination, .verbose) {

  # downloading
  verbose_msg(
    cli::cli_inform(c(
      "i" = "Downloading IFN available data"
    )),
    .verbose = .verbose
  )

  # `while` control value
  tries <- 0L

  while (tries < 11L) {
    is_downloaded <- curl::multi_download(
      urls = glue::glue("https://www.miteco.gob.es{c(ifn2_links, ifn3_links, ifn4_links)}"),
      destfiles = fs::path(destination, basename(c(ifn2_links, ifn3_links, ifn4_links))),
      resume = TRUE,
      progress = .verbose
    )

    if (any(is_downloaded$success == FALSE)) {
      tries <- tries + 1L
    } else {
      tries <- 11
    }
  }

  if (any(is_downloaded$success == FALSE)) {
    cli::cli_abort(c("x" = "Something went wrong during the download"))
  }

  # unzipping
  verbose_msg(
    cli::cli_inform(c(
      "i" = "Unzipping downloaded data in {.path {destination}}"
    )),
    .verbose = .verbose
  )

  extracted_files <- fs::dir_ls(destination, regex = "zip$") |>
    purrr::map(
      .f = \(zip_file) {
        utils::unzip(
          zipfile = zip_file, exdir = destination
        )
      }
    )

  failed_files <- extracted_files |>
    purrr::keep(.p = is.null) |>
    names() |>
    basename()

  if (length(failed_files) > 0) {
    cli::cli_warn(c(
      "x" = "The following files failed to be downloaded:",
      "{.file {failed_files}}"
    ))
  }

  verbose_msg(
    cli::cli_alert_success("Done!"),
    .verbose = .verbose
  )

  # end
  return(invisible(TRUE))
}

#' Download the especified inventory
#'
#' Download and unzip the inventory data files at the desired destination
#'
#' This function tries to download the available files for the especified inventory
#' from the official repositories of the inventory. It can fail as can be connection
#' problems or files missing temporary (This usually happens for the version 4 of the IFN)
#'
#' @param inventory Character with the inventory abbreviation
#' @param destination Path to the inventory destination folder. This folder must exists.
#' @param .verbose Logical indicating if progress messages should be shown.
#'
#' @return Invisible TRUE if the download and unzip was succesful, an error otherwise.
#'
#' @examples
#' \donttest{
#' \dontrun{
#' library(forestables)
#' download_inventory(ffi, destination = tempdir())
#' }
#' }
#' @export
download_inventory <- function(
  inventory = c("fia", "ffi", "ifn"),
  destination = ".",
  .verbose = TRUE
) {
  # assertions
  inventory <- match.arg(inventory)
  assertthat::assert_that(
    fs::dir_exists(destination),
    msg = cli::cli_abort(c(
      "{.arg {destination}} path doesn't exists. Please create destination folder first"
    ))
  )
  assertthat::assert_that(
    assertthat::is.flag(.verbose),
    msg = cli::cli_abort(".verbose must be logical (TRUE/FALSE)")
  )

  # choose the helper function
  download_and_unzip <- switch(
    inventory,
    fia = .download_fia,
    ffi = .download_ffi,
    ifn = .download_ifn
  )

  # process
  download_and_unzip(destination, .verbose)
}