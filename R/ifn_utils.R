#' Read IFN data
#'
#' Read the different IFN db formats
#'
#' This functions checks the IFN file format (by checking the file extension) and
#' dispatch the corresponding function: \code{\link[foreign]{read.dbf}} for IFN2 and
#' \code{\link[Hmisc]{mdb.get}} for IFN3 and 4
#'
#' @param input file to read as it appears in the input data frame
#' @param ... extra arguments for the reading function
#'
#' @return A data.frame with the inventory table
#'
#' @noRd
.read_ifn_data <- function(input, ...) {

  # first, if ifn3 or ifn4 we have file name and table name in a character separated by "|" but if
  # we have ifn2 we have the name of the corresponding file.
  table_name <- NULL
  if (stringr::str_detect(input, "\\|")) {
    file_and_table <- stringr::str_split(input, "\\|", n = 2, simplify = TRUE)
    input <- file_and_table[1]
    table_name <- file_and_table[2]
  }

  # we need to check if dbf or accdb, and use the correct function to
  # simply read and return, the main function .read_inventory_data will take care of the rest
  file_ext <- fs::path_ext(input)

  res <- switch(
    file_ext,
    "DBF" = foreign::read.dbf(input),
    "accdb" = .read_accdb_data(input, table_name)
  )

  return(res)
}

.read_accdb_data <- function(input, table_name) {

  # first thing check the SO
  so <- .Platform$OS.type

  # unix or windows?
  if (so == "unix") {
    if (isFALSE(
      .sys_cmd_warning("mdb-tables", c(
        "x" = "{.emph mdbtools} system utility not found.",
        "i" = "{.emph mdbtools} is needed to read Spanish inventory (IFN) data versions 3 and 4.",
        "i" = "Please check your package manager (apt, brew, port...) to install it.",
        "i" = "More info at https://github.com/mdbtools/mdbtools"
      ))
    )) {
      cli::cli_abort("Aborting")
    }

    res <- Hmisc::mdb.get(input, tables = table_name)
  } else {
    res <- ODBC::odbcConnectAccess2007(input) |>
      RODBC::sqlFetch(table_name)
  }

  return(res)

}

.ifn4_prov_code_translator <- function(province) {
  ifn_provinces_dictionary |>
    dplyr::filter(province_code %in% province) |>
    dplyr::pull(ifn4_files_labels)
}

.build_ifn_file_path <- function(province, type, version, folder = ".", .call = rlang::caller_env()) {

  # Ok, so here we need to do some things. Depending on .version (the IFN version) we need to
  # provide different things.
  purrr::pmap_chr(
    .l = list(province, type, version),
    .f = \(province, type, version) {

      if (version == "ifn2") {
        file_name <- switch(
          type,
          "tree" = glue::glue("PIESMA{province}.DBF"),
          "shrub" = glue::glue("MATORR{province}.DBF"),
          "regen" = glue::glue("PIESME{province}.DBF"),
          "tiposp" = glue::glue("TIPOSP{province}.DBF"),
          "tiposx" = glue::glue("TIPOSX{province}.DBF")
        )

        # return path
        table_path <- fs::path(folder, file_name)
      }

      if (version == "ifn3") {
        file_name <- fs::path(folder, glue::glue("Ifn3p{province}.accdb"))
        table_name <- switch(
          type,
          "tree" = "PCMayores",
          "shrub" = "PCMatorral",
          "regen" = "PCRegenera",
          "plot" = "PCParcelas"
          # others needed
        )

        table_path <- glue::glue("{file_name}|{table_name}")
      }

      if (version == "ifn4") {

        # this is a little trickier. We don't always have provinces so, we need to translate the
        # province code to the correct ifn4 label
        province <- .ifn4_prov_code_translator(province)
        file_name <- fs::path(folder, glue::glue("Ifn4_{province}.accdb"))
        table_name <- switch(
          type,
          "tree" = "PCMayores",
          "shrub" = "PCMatorral",
          "regen" = "PCRegenera",
          "plot" = "PCParcelas"
          # others needed
        )

        table_path <- glue::glue("{file_name}|{table_name}")
      }

      return(table_path)
    }
  )
}