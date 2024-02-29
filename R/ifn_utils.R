.build_ifn_input_with <- function(
  version, provinces, filter_list, folder, .verbose, .call = rlang::caller_env()
) {

  # first, if is null filter list, create it
  if (is.null(filter_list)) {
    get_plots_safe <- purrr::safely(
      .get_plots_from_province,
      otherwise = tibble::tibble(
        "version" = vector(),
        "province_name_original" = vector(),
        "ID_UNIQUE_PLOT" = vector(),
        "crs" = vector(),
        "COORDEX" = vector(),
        "COORDEY" = vector(),
        "geometry" = vector()
      )
    )
    transform_safe <- purrr::safely(
      .transform_plot_summary_ifn,
      otherwise = list()
    )

    filter_list <- purrr::map(
      provinces,
      .f = \(province) {
        res <- get_plots_safe(province, folder, version, .call = .call)[["result"]] |>
          transform_safe(version, province)
        res[["result"]]
      }
    ) |>
      purrr::flatten()
  }

  # inform the user about the amount of plots for this year
  verbose_msg(
    cli::cli_inform(c(
      "Getting ready to retrieve
      {.strong {filter_list |> purrr::flatten() |> as.character() |> length()}}
      plots for {.val {version}}"
    )), .verbose
  )

  input_df <- filter_list |>
    tibble::enframe() |>
    tidyr::unnest(cols = value) |>
    purrr::set_names(c("province", "plots")) |>
    dplyr::mutate(
      plots = as.character(plots),
      version = as.character(version)
    ) |>
    dplyr::select(province, plots, version) |>
    dplyr::mutate(
      plot_table = .build_ifn_file_path(
        province,
        type = "plot",
        version,
        folder,
        .call = .call
      ),
      tree_table = .build_ifn_file_path(
        province,
        type = "tree",
        version,
        folder,
        .call = .call
      ),
      shrub_table = .build_ifn_file_path(
        province,
        type = "shrub",
        version,
        folder,
        .call = .call
      ),
      regen_table = .build_ifn_file_path(
        province,
        type = "regen",
        version,
        folder,
        .call = .call
      ),
      coord_table = ifelse(
        test = version %in% c("ifn3", "ifn4"),
        yes = .build_ifn_file_path(
          province,
          type = "coord",
          version,
          folder,
          .call = .call
        ),
        no = NA_character_
      )
    )

  return(input_df)
}

.get_plots_from_province <- function(province, folder, version, .call = rlang::caller_env()) {

  ## TODO Assertion to ensure province files exists, because .build_ifn_file_path is fail
  ## resistant, returning always a result (NA_character) to allow its use in loops.
  ## .get_plots_from_province is only called from .build_ifn_input_with or show_plots_from_ifn,
  ## that can not check for file existence (this is done in the individual plot functions)

  plot_path <- .build_ifn_file_path(province, "plot", version, folder, .call = .call)

  coord_path <- ifelse(
    test = version %in% c("ifn3", "ifn4"),
    yes = .build_ifn_file_path(province, type = "coord", version, folder, .call = .call),
    no = NA_character_
  )

  if (is.na(plot_path)) {
    cli::cli_abort(c(
      "{.path {folder}} folder doesn't contain the file corresponding to {.val {version}} version,
      aborting."
    ), call = .call)
  }

  plots_arg_value <- rlang::quo(.data$ID_UNIQUE_PLOT)
  # If file exists, business as usual. We use the general function (ifn_plot_table_process), because
  # it takes care of the version logic for us, DRY!!!
  # The only thing we need to take care of is the dancing coord ref systems. But for that is the crs
  # variable, so we group and transform to common crs
  res <- ifn_plot_table_process(
    plot_path, coord_path, version, plots_arg_value, province, ifn_provinces_dictionary
  ) |>
    dplyr::filter(!is.na(ID_UNIQUE_PLOT)) |>
    dplyr::select(
      "ID_UNIQUE_PLOT", "version", "province_code",
      "province_name_original", "PLOT", "crs", "COORDEX", "COORDEY"
    ) |>
    dplyr::group_by(crs) |>
    dplyr::group_modify(
      .f = \(crs_group, crs_code) {
        crs_group |>
          sf::st_as_sf(
            coords = c("COORDEX", "COORDEY"),
            crs = sf::st_crs(unique(crs_code[["crs"]]))
          ) |>
          sf::st_transform(crs = 4326)
      }
    ) |>
    sf::st_as_sf()

  return(res)
}

#' Helper to transform the plot summary returned by \code{\link{.get_plots_from_province}} in a
#' filter_list object
#' @noRd
.transform_plot_summary_ifn <- function(plot_summary, versions, provinces) {

  filter_list <- plot_summary |>
    dplyr::as_tibble() |>
    dplyr::filter(
      version %in% versions,
      province_code %in% provinces
    ) |>
    dplyr::select(province_code, ID_UNIQUE_PLOT) |>
    dplyr::distinct() |>
    dplyr::group_by(province_code) |>
    dplyr::summarise(plots = list(ID_UNIQUE_PLOT), .groups = "keep") |>
    dplyr::group_map(.f = \(province_plots, province_code) {
      tibble::deframe(province_plots) |>
        # list() |>
        purrr::set_names(province_code[[1]])
    }) |>
    purrr::flatten()

  return(filter_list)
}

#' show plots from ifn helper
#'
#' Iterate for states and retrieve all the plots
#'
#' @param folder Character, path to folder containing IFN csv files
#' @param provinces Character vector with two-number code for provinces
#' @noRd
show_plots_from_ifn <- function(folder, provinces, version, .call = rlang::caller_env()) {
  withCallingHandlers(
    {
      # safe version
      get_plots_safe <- purrr::safely(
        .get_plots_from_province,
        otherwise = NULL
      )

      res <- purrr::map(
        provinces,
        .f = \(prov) {
          get_plots_safe(prov, folder, version, .call = .call)$result
        }
      ) |>
        purrr::list_rbind()

      if (nrow(res) < 1) {
        cli::cli_abort(
          c("No data found at {.folder {folder}} for {.values {provinces}} province codes",
            "i" = "Please check if {.folder {folder}} is the correct folder for IFN data")
        )
      }

      res |>
        sf::st_as_sf()
    },
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
  )
}

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
.read_ifn_data <- function(input, colnames, ...) {

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
    "DBF" = foreign::read.dbf(input, as.is = TRUE) |>
      dplyr::select(dplyr::any_of(colnames)) |>
      dplyr::mutate(
        PROVINCIA = as.character(PROVINCIA),
        PROVINCIA = stringr::str_pad(PROVINCIA, width = 2, side = "left", pad = "0"),
        ESTADILLO = as.character(ESTADILLO)
      ) |>
      .ifn_unique_id_creator(...),
    "accdb" = .read_accdb_data(input, table_name) |>
      dplyr::select(dplyr::any_of(colnames)) |>
      dplyr::mutate(
        Estadillo = as.character(Estadillo),
        Subclase = .ifn_subclass_fixer(Subclase)
      ) |>
      .ifn_unique_id_creator(...)
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

    # in linux, space characters must be escaped
    if (stringr::str_detect(input, "(Ifn4_.* .*\\.accdb)$")) {
      input <- stringr::str_replace(input, "(Ifn4_.* .*\\.accdb)$", "'\\1'")
    }

    # read the table
    res <- Hmisc::mdb.get(input, tables = table_name)
  } else {
    res <- RODBC::odbcConnectAccess2007(input) |>
      RODBC::sqlFetch(table_name)
  }

  return(res)

}

.ifn4_prov_code_translator <- function(province) {
  res <- ifn_provinces_dictionary |>
    dplyr::filter(province_code %in% province) |>
    dplyr::pull(ifn4_files_labels)

  # In case province provided is not in the dictionary (because error, tururu tests)
  # then res is going to be character(0), so we convert to empty string
  if (length(res) < 1) {
    res <- ""
  }

  return(res)
}

.build_ifn_file_path <- function(
  province, type, version, folder = ".", .call = rlang::caller_env()
) {

  # Ok, so here we need to do some things. Depending on .version (the IFN version) we need to
  # provide different things.
  res <- purrr::pmap_chr(
    .l = list(province, type, version),
    .f = \(province, type, version) {

      if (version == "ifn2") {
        file_name <- switch(
          type,
          "plot" = fs::path(folder, glue::glue("DATEST{province}.DBF")),
          "tree" = fs::path(folder, glue::glue("PIESMA{province}.DBF")),
          "shrub" = fs::path(folder, glue::glue("MATORR{province}.DBF")),
          "regen" = fs::path(folder, glue::glue("PIESME{province}.DBF"))
        )

        # return path
        table_path <- file_name
      }

      if (version == "ifn3") {
        file_name <- fs::path(folder, glue::glue("Ifn3p{province}.accdb"))
        table_name <- switch(
          type,
          "tree" = "PCMayores",
          "shrub" = "PCMatorral",
          "regen" = "PCRegenera",
          "plot" = "PCParcelas",
          "coord" = "PCDatosMap"
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
          "plot" = "PCParcelas",
          "coord" = "PCDatosMap"
          # others needed
        )

        table_path <- glue::glue("{file_name}|{table_name}")
      }

      # check file exists
      if (!fs::file_exists(file_name)) {
        return(NA_character_)
      }
      return(table_path)
    }
  )

  if (sum(is.na(res)) > 0) {
    cli::cli_warn(c(
      "Files for provinces {.values {province[which(is.na(res))] |> unique()}} not found.",
      "Skipping plots {type} data from those provinces"
    ))
  }

  return(res)
}

.ifn_subclass_fixer <- function(subclasses) {
  # This helper fix errors with subclasses format
  subclasses |>
    # remove any ocurring extra whitespaces
    stringr::str_trim("both") |>
    # in IFN3, replace wrong 2C and 2E plots with 3C and 3E
    stringr::str_replace("^2C$", "3C") |>
    stringr::str_replace("^2E$", "3E") |>
    # in IFN3, replace wrong 5 subclass with 4
    stringr::str_replace("^5$", "4")
}

.ifn_unique_id_creator <- function(data, version, province, .dry = FALSE, .padding = TRUE) {

  if (isTRUE(.dry)) {
    if (isTRUE(.padding)) {
      # here to solve the estadillo padding. As dry only occurs in ifn3 and ifn4 coords tables,
      # due to the lack of Subclase in those, we can safely assume the var is called Estadillo
      data <- data |>
        dplyr::mutate(
          Estadillo = stringr::str_pad(Estadillo, width = 4, side = "left", pad = "0")
        )
    }


    return(data)
  }

  if (version == "ifn2") {
    res <- ifn_plots_thesaurus |>
      dplyr::filter(class_ifn2 == "NN") |>
      dplyr::select(id_code, PROVINCIA, ESTADILLO) |>
      dplyr::right_join(
        data |>
          dplyr::mutate(
            ESTADILLO = stringr::str_pad(ESTADILLO, width = 4, side = "left", pad = "0")
          ),
        by = c("PROVINCIA", "ESTADILLO")
      ) |>
      dplyr::rename(ID_UNIQUE_PLOT = id_code)
  } else {

    data_temp <- data |>
      dplyr::mutate(
        Estadillo = stringr::str_pad(Estadillo, width = 4, side = "left", pad = "0"),
        class = paste0(Cla, Subclase)
      )

    if (version == "ifn4") {
      data_temp <- data |>
        dplyr::filter(
          province == stringr::str_pad(Provincia, width = 2, side = "left", pad = "0")
        ) |>
        dplyr::mutate(
          Estadillo = stringr::str_pad(Estadillo, width = 4, side = "left", pad = "0"),
          class = paste0(Cla, Subclase)
        )
    }

    res <- ifn_plots_thesaurus |>
      dplyr::filter(PROVINCIA == province) |>
      dplyr::select(id_code, Estadillo = ESTADILLO, class = paste0("class_", version)) |>
      dplyr::filter(!is.na(class), class != "xx") |>
      dplyr::right_join(
        data_temp,
        by = c("Estadillo", "class")
      ) |>
      dplyr::rename(ID_UNIQUE_PLOT = id_code)
  }

  return(res)
}



#' obtaining crs for different coordinate systems IFN
#'
#' This function  get_crs reads var huso and coordinate system of plot table process the tree table
#' for one plot and one IFN
#'
#' @param tree_data file that contains the tree table for that plot
#' @param plot plot_id code
#' @param province province code
#' @param ref_tree_ifn data frame containing the species code reference table
#'
#' @noRd
#
#
