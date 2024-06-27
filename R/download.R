#' Download, unzip and store FIA inventory data
#'
#' @inheritParams download_inventory
#'
#' @noRd
.download_fia <- function(destination, states, .verbose) {

  ## assertions
  assertthat::assert_that(
    !is.null(states),
    msg = cli::cli_abort(c(
      "x" = "For downloading FIA data a {.arg states} argument must by provided",
      "i" = 'i.e. {.code states = c("AK", "AZ", "CA")}'
    ))
  )

  assertthat::assert_that(
    all(states %in% fia_states_dictionary$ABBR),
    msg = cli::cli_abort(c(
      "x" = "Invalid {.arg states} detected:",
      "i" = "{.arg {states[which(!states %in% fia_states_dictionary$ABBR)]}}"
    ))
  )
  
  # downloading
  verbose_msg(
    cli::cli_inform(c(
      "i" = "Downloading FIA available data"
    )),
    .verbose = .verbose
  )

  # links
  base_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/"
  needed_zips <- c(
    "_TREE.zip", "_PLOT.zip", "_SURVEY.zip", "_COND.zip", "_SUBPLOT.zip",
    "_VEG_SUBPLOT_SPP.zip", "_SEEDLING.zip", "_VEG_SUBPLOT.zip", "_P2VEG_SUBPLOT_SPP.zip"
  )
  file_urls <- purrr::map(states, .f = \(state) {
    paste0(base_url, state, needed_zips)
  }) |>
    purrr::flatten_chr()
  file_urls <- c(
    file_urls,
    paste0(base_url, c("REF_SPECIES.csv", "REF_PLANT_DICTIONARY.csv"))
  )

  # `while` control value
  tries <- 0L

  while (tries < 11L) {
    is_downloaded <- curl::multi_download(
      urls = file_urls,
      destfiles = fs::path(destination, basename(file_urls)),
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

  files_to_extract <-
    is_downloaded$destfile[which(stringr::str_detect(is_downloaded$type, "zip"))]

  extracted_files <- files_to_extract |>
    purrr::map(
      .f = \(zip_file) {
        suppressWarnings(utils::unzip(
          zipfile = zip_file, exdir = destination
        ))
      }
    )

  failed_files <-
    is_downloaded$destfile[which(!stringr::str_detect(is_downloaded$type, "zip|octet"))]

  if (length(failed_files) > 0) {
    cli::cli_warn(c(
      "x" = "The following files failed to be downloaded:",
      "{.file {basename(failed_files)}}"
    ))
    file.remove(failed_files)
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

  if (is.null(extracted_files)) {
    cli::cli_abort(c("x" = "Something went wrong unzipping the file"))
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

  files_to_extract <-
    is_downloaded$destfile[which((stringr::str_detect(is_downloaded$url, "zip$")))]
  
  unzip_method <- "unzip"
  if (Sys.info()["sysname"] %in% c("darwin", "Darwin", "DARWIN")) {
    unzip_method <- "internal"
  }

  extracted_files <- files_to_extract |>
    purrr::map(
      .f = \(zip_file) {

        suppressWarnings(utils::unzip(
          zipfile = zip_file, exdir = destination, unzip = unzip_method
        ))

        ## renaming problematic files
        # file_names_raw <-
        #   utils::unzip(zipfile = zip_file, list = TRUE, unzip = "unzip")[["Name"]]
        # file_names_fixed <-
        #   ## bad strings substitutions:
        #   # ñ
        #   stringr::str_replace_all(file_names_raw, stringr::fixed("\xa4"), "д") |>
        #   # ó
        #   stringr::str_replace_all(stringr::fixed("\xa2"), "в") #|>
        #   # í
        #   # stringr::str_replace_all(stringr::fixed("\x"), "б") |>
        #   # á
        #   # stringr::str_replace_all(stringr::fixed("\x"), "╡")
        
        # # only rename files if needed
        # if (!identical(file_names_raw, file_names_fixed)) {
        #   file.rename(
        #     file.path(destination, file_names_raw), file.path(destination, file_names_fixed)
        #   )
        # }
      }
    )

  failed_files <- is_downloaded$destfile[which((stringr::str_detect(is_downloaded$url, "404")))]

  if (length(failed_files) > 0) {
    cli::cli_warn(c(
      "x" = "The following files failed to be downloaded:",
      "{.file {basename(failed_files)}}"
    ))
    file.remove(failed_files)
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
#' @param states Character vector indicating the FIA states to download. Only used if FIA
#'   is selected.
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
  inventory = c("FIA", "FFI", "IFN"),
  destination = ".",
  states = NULL,
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

  # run the correct helper function
  res <- switch(
    inventory,
    FIA = .download_fia(destination, states, .verbose),
    FFI = .download_ffi(destination, .verbose),
    IFN = .download_ifn(destination, .verbose)
  )

  # return invisible TRUE if everything is ok
  return(invisible(res))
}