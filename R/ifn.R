#' Raw IFN data to tibble
#'
#' Transform raw IFN plot data into tidy data for easier use
#'
#' This function will take every year indicated and retrieve and transform the plot data for the
#' provinces, versions and plots provided. For that, IFN db files must reside in the folder
#' indicated in the \code{folder} argument.
#'
#' @param provinces A character vector with the two-number codes for the provinces.
#' @param versions A character vector with the ifn versions. Valid versions are \code{"ifn2"},
#'   \code{"ifn3"} and \code{"ifn4"}.
#' @param filter_list A list of provinces and plots to extract the data from.
#' @param folder The path to the folder containing the IFN db files, as character.
#' @param ... Not used at the moment
#' @param .parallel_options An object of class \code{furrr_options}. See
#'   \code{\link[furrr]{furrr_options}}.
#' @param .verbose Logical controlling if progress messages are shown.
#'
#' @section Filter list:
#'   If no \code{filter_list} argument is provided, \code{ifn_to_tibble} will attempt to process all
#'   plots for the provinces and ifn versions provided. This will result in sometimes
#'   thousands plots to be extracted, processed and returned, which in turn will cause a big use of
#'   memory (specially when running in parallel processes) and long times of calculation.
#'   Is better to provide a list of departments with the provinces and plots to look for to
#'   narrow the process. This \code{filter_list} should have the following structure:
#'   \preformatted{
#'    list(
#'    "01" = c("01_0644_NN_A1_A1"),
#'    "08" = c("08_1256_NN_A1_xx", "08_0056_xx_A4_xx"),
#'    "24" = c("24_0270_xx_A4_xx")
#'   )
#'   }
#'   \code{esus} package offers workflows to create this automatically, see
#'   \code{vignette("filtering_plots", pkg = "esus")} for more details.
#'
#' @section Parallel:
#'   Processing the plots from within an IFN version can be done in parallel (\code{esus} uses
#'   internally the \code{\link[furrr]{furrr}} package for this). This means that, if
#'   parallelization is active, several processes are launched to retrieve the plots data for that
#'   IFN version. This is repeated for all versions provided.
#'
#'   \code{.parallel_options} controls the finer details of how parallelization is performed (see
#'   \code{\link[furrr]{furrr_options}}). But no parallelization can occur without setting first
#'   a \code{\link[future]{plan}}. By default, the chosen plan is \code{\link[future]{sequential}},
#'   so no parellization is done. Changing the plan, i.e. to \code{\link[future]{multisession}} or
#'   to \code{\link[future.callr]{callr}}, will allow \code{ifn_to_tibble} to use parallelization
#'   when retrieving the data.
#'
#' @return A nested tibble. This tibble contains a row per plot/year combination, with the plot
#'   metadata included, as well as columns containing tibbles with tree, shrub, herbs and soil
#'   information. See \code{vignette("inventory_data_tibble", pkg = "esus")}
#'
#' @export
ifn_to_tibble <- function(
  provinces,
  versions,
  filter_list,
  folder,
  ...,
  .parallel_options = furrr::furrr_options(scheduling = 1L, stdout = TRUE),
  .verbose = TRUE
) {

  ## Assertions and checks ##
  # grep
  assertthat::assert_that(.sys_cmd_warning())

  # provinces
  assertthat::assert_that(
    is.character(provinces), length(provinces) > 0,
    msg = cli::cli_abort("provinces must be a character vector with at least one province code")
  )
  # provinces are valid
  valid_provinces <- esus:::ifn_plots_thesaurus |>
    dplyr::pull(PROVINCIA) |>
    unique() |>
    sort()
  invalid_provinces <- which(!provinces %in% valid_provinces)

  if (identical(length(provinces), length(invalid_provinces))) {
    cli::cli_abort(c(
      "x" = "Any of the provided {.arg provinces} ({.val {provinces}}) are valid. Aborting."
    ))
  } else {
    if (length(invalid_provinces) > 0) {
      cli::cli_warn(c(
        "!" = "Some {.arg provinces} are not valid: {.val {provinces[invalid_provinces]}}",
        "i" = "These will be skipped in the process"
      ))
    }
  }

  # versions
  assertthat::assert_that(
    is.character(versions), length(versions) > 0,
    msg = cli::cli_abort("versions must be a character vector with at least one")
  )
  assertthat::assert_that(
    all(versions %in% c("ifn2", "ifn3", "ifn4")),
    msg = cli::cli_abort('Only valid {.arg versions} are "ifn2", "ifn3" or "ifn4"')
  )

  # folder
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort(
      "Folder especified ({.path {folder}}) doesn't exists.
      Please create the folder first and populate it with the needed IFN csv files"
    )
  )

  # filter_list
  if (is.null(filter_list)) {
    if (interactive()) {
      cli::cli_inform(c(
        "You haven't specified any plots in the {.arg filter_list} argument.",
        "x" = "This will cause to retrieve {.strong ALL} plots  for the selected departments and years",
        "!" = "This will use a lot of memory and time, as hundred of thousands plots will potentially be evaluated",
        "TODO: add info about how to create the filter list",
        ""
      ))

      user_auth <- utils::menu(c("Yes", "No"), title = "Do you wish to continue anyway?")
      if (user_auth == 2L) {
        cli::cli_abort("Aborting per user request")
      }
    }
  }

  # parallel options
  assertthat::assert_that(
    inherits(.parallel_options, "furrr_options"),
    msg = cli::cli_abort(".parallel_options must come from {.code furrr::furrr_options}")
  )

  # verbose
  assertthat::assert_that(
    assertthat::is.flag(.verbose),
    msg = cli::cli_abort(".verbose must be logical (TRUE/FALSE)")
  )

  ## inform the user
  verbose_msg(
    cli::cli_inform(
      c("Start", "i" = "Processing {length(versions)} cicle{?s}")
    ),
    .verbose
  )

  # get the caller environment to propagate errors
  .call <- rlang::caller_env(0)
  # send the versions in loop to process table function
  purrr::map(
    versions,
    .f = \(version) {
      ifn_tables_process(
        provinces, version, filter_list, folder, .parallel_options, .verbose, .call, ...
      )
    },
    .progress = FALSE
  ) |>
    purrr::list_rbind()
}

#' Inner function to process all tables for one version
#'
#' Processing all tables for one version
#'
#' This function is intended to be called internally by \code{\link{ifn_to_tibble}} for each
#' version. This is implemented with furrr to allow parallelization of the plots data retrieval.
#'
#' @inherit ifn_to_tibble
#'
#' @param .call Caller environment (\code{\link[rlang]{caller_env}}) to allow informative errors
#'
#' @noRd
ifn_tables_process <- function(
  provinces, version, filter_list, folder,
  .parallel_options, .verbose, .call = rlang::caller_env(), ...
) {

  # Create input df for year
  input_df <- .build_ifn_input_with(version, provinces, filter_list, folder, .verbose, .call)

  # purrr::pmap(
  temp_res <- furrr::future_pmap(
    .progress = .verbose,
    .l = input_df,
    .f = \(
      province, plots, version, tree_table, plot_table, shrub_table, regen_table, coord_table
    ) {

      plot_info <- ifn_plot_table_process(
          plot_table, coord_table, version, plots, province, ifn_provinces_dictionary, .call
        )
      tree <- ifn_tree_table_process(
        tree_table, version, plots, province, species_ifn_internal, .call
      )
      shrub <- ifn_shrub_table_process(
        shrub_table, version, plots, province, species_ifn_internal, .call
      )
      regen <- ifn_regen_table_process(
        regen_table, version, plots, province, species_ifn_internal, .call
      )
      if (nrow(plot_info) < 1) {
        return(tibble::tibble())
      }

      # we put together all tables in a data frame
      understory <- plot_info |>
        dplyr::select("ID_UNIQUE_PLOT", "YEAR", "province_code", "PLOT") |>
        dplyr::mutate(shrub = list(shrub))

      plot_info |>
        dplyr::rename(COORD1 = "COORDEX", COORD2 = "COORDEY") |>
        dplyr::mutate(
          tree = list(tree),
          understory = list(understory),
          regen = list(regen)
        ) |>
        dplyr::select(
          dplyr::any_of(c(
            "ID_UNIQUE_PLOT", "COUNTRY", "YEAR", "ca_name_original", "province_name_original",
            "province_code", "PLOT", "Cla", "Subclase", "version", "Tipo", "HOJA", "Huso",
            "COORD_SYS", "COORD1", "COORD2", "crs", "PENDIEN2", "SLOPE", "ELEV", "ASPECT", "tree",
            "understory", "regen"
          ))
        )
    }
  ) |>
    purrr::list_rbind()

  # something went wrong (bad counties and plots, wrong filter list...)
  if (nrow(temp_res) < 1) {
    cli::cli_abort("Ooops! Something went wrong, exiting...", call = .call)
  }

  # filtering the missing plots. This is done based on the fact plot table functions returns NAs
  # for all vars, including coords, when the plot is not found
  temp_res |>
    dplyr::filter(!(is.na(.data$COORD1) & is.na(.data$COORD2)))

}

#' FIA data tables process
#'
#' Process to gather needed data from FIA csv tables
#'
#' These functions retrieve the data for a plot in one year.
#'
#' @param tree_data,shrub_data,regen_data,plot_data,coord_data Paths to the files with the corresponding data
#' @param version IFN version
#' @param plot Plot unique ID code
#' @param province Province two-number code
#' @param species_ifn_internal,ifn_provinces_dictionary species and provinces dictionary tables.
#'   These tables are included as internal data in the package
#' @param .call Caller environment (\code{\link[rlang]{caller_env}}) to allow informative errors
#'
#' @return A tibble with one or more rows (depending on the data retrieved) for each plot for that
#'  IFN version.
#'
#' @name ifn_tables_processing
#' @noRd
NULL

#' IFN data tables process
#' @describeIn ifn_tables_processing Process to gather needed data from tree table
#' @noRd
ifn_tree_table_process <- function(
  tree_data, version, plot, province, species_ifn_internal, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(tree_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}} "
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names we select the column names to be read

  if (version == "ifn2") {
    tree_filtered_data <- .read_inventory_data(
      tree_data,
      colnames = c(
        "PROVINCIA", "ESTADILLO", "ESPECIE", "NUMORDEN", "ARBOL", "DIAMETRO1", "DIAMETRO2", "ALTURA"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$ID_UNIQUE_PLOT == plot
      ) |>
      tibble::as_tibble()



    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(tree_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Tree data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}} "
      ), call = .call)
      return(dplyr::tibble())
    }

    tree <- tree_filtered_data |>
      # transformations and filters
      dplyr::rename(
        province_code = "PROVINCIA", PLOT = "ESTADILLO", SP_CODE = "ESPECIE",
        TREE = "ARBOL", Dn1 = "DIAMETRO1", Dn2 = "DIAMETRO2", HT = "ALTURA"
      ) |>
      dplyr::mutate(
        PLOT = as.character(.data$PLOT),
        province_code = as.character(.data$province_code),
        Dn1 = as.numeric(.data$Dn1),
        Dn2 = as.numeric(.data$Dn2),
        HT = as.numeric(stringr::str_replace(.data$HT, ",", ".")),
        SP_CODE = as.numeric(.data$SP_CODE),
        DIA = ((.data$Dn1 + .data$Dn2) / 2) * 0.1, # From mm to cm
        HT = .data$HT, # in meters
        DENSITY = dplyr::case_when(
          .data$DIA < 12.5 ~ 127.3239546,
          .data$DIA >= 12.5 & .data$DIA < 22.5 ~ 31.83098865,
          .data$DIA >= 22.5 & .data$DIA < 42.5 ~ 14.14710607,
          .data$DIA >= 42.5 ~ 5.092958185
        )
      )  |>
      # add species info ---> WHAT REFERENCE SHOULD I USEE???
      dplyr::left_join(
        y = species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::arrange(.data$SP_CODE) |>
      dplyr::select(
        "ID_UNIQUE_PLOT", "province_code", "PLOT", "SP_CODE", "SP_NAME",
        "DIA", # diameter in cm
        "HT", # height in m
        "DENSITY"
      )

    # Return tree
    return(tree)
  }

  if (version %in% c("ifn3", "ifn4")) {
    tree_filtered_data <-  .read_inventory_data(
      tree_data,
      colnames = c(
        "Provincia", "Estadillo", "Cla", "Subclase", "Especie", "nArbol", "OrdenIf3",
        "OrdenIf2", "OrdenIf4", "Dn1", "Dn2", "Ht", "Calidad", "Forma"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(.data$ID_UNIQUE_PLOT == plot) |>
      tibble::as_tibble()

    # IFN3 doesn't have Provincia, so we check
    if ("Provincia" %in% names(tree_filtered_data)) {
      tree_filtered_data <- tree_filtered_data |>
        dplyr::filter(.data$Provincia == as.integer(province))
    }

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(tree_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Tree data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}} "
      ), call = .call)
      return(dplyr::tibble())
    }

    tree <- tree_filtered_data |>
      dplyr::rename(PLOT = "Estadillo", HT = "Ht", Clase = "Cla") |>
      # units transformations
      dplyr::mutate(
        province_code = province,
        # Subclass fixes
        Subclase = .ifn_subclass_fixer(.data$Subclase),
        DIA = (.data$Dn1 + .data$Dn2) / 2,
        # MM TO CM
        DIA = .data$DIA * 0.1,
        SP_CODE = as.numeric(.data$Especie),
        # density represented by each tree considering  plot design (variable radious)
        DENSITY = dplyr::case_when(
          .data$DIA < 12.5 ~ 127.3239546,
          .data$DIA >= 12.5 & .data$DIA < 22.5 ~ 31.83098865,
          .data$DIA >= 22.5 & .data$DIA < 42.5 ~ 14.14710607,
          .data$DIA >= 42.5 ~ 5.092958185
        )
      ) |>
      # add species info
      dplyr::left_join(
        y =  species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::arrange(.data$SP_CODE) |>
      dplyr::select(
        dplyr::any_of(c(
          "ID_UNIQUE_PLOT", "province_code", "Clase", "Subclase", "PLOT", "SP_CODE", "SP_NAME",
          #tree number id in ifn4
          "nArbol",
          #CUALIDAD 6 = dead but providing functions
          "Calidad", "Forma",
          #check codes to understand origin and trace of individuals
          "OrdenIf2",
          "OrdenIf3",
          "OrdenIf4",
          #diameter in cm
          "DIA",
          #height in cm
          "HT", "DENSITY"
        ))
      )

    # Return tree
    return(tree)
  }
}

#' @describeIn ifn_tables_processing Process to gather needed data from shrub table
#' @noRd
ifn_shrub_table_process <- function(
  shrub_data, version, plot, province, species_ifn_internal, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(shrub_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping shrub data for plot {.var {plot}} "
    ), call = .call)

    return(dplyr::tibble())
  }

  if (version == "ifn2") {
    shrub_filtered_data <- .read_inventory_data(
      shrub_data,
      colnames = c("PROVINCIA", "ESTADILLO", "ESPECIE", "FRACCAB", "ALTUMED"),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(.data$ID_UNIQUE_PLOT == plot) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(shrub_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Shrub data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    #we add the id code
    shrub <- shrub_filtered_data |>
      dplyr::rename(PLOT = "ESTADILLO", COVER = "FRACCAB", HT = "ALTUMED") |>
      dplyr::mutate(
        PLOT = as.character(.data$PLOT),
        province_code = as.character(.data$PROVINCIA),
        HT = as.numeric(.data$HT),
        COVER = as.numeric(.data$COVER),
        HT = .data$HT * 10, # DM TO cm
        SP_CODE = as.numeric(.data$ESPECIE)
      ) |>
      # 3. ref_plant_dictionary
      # we join data from plant ref dictionary
      # some symbols apply for multiple species
      dplyr::left_join(
        y =  species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::arrange(.data$SP_CODE) |>
      dplyr::select("ID_UNIQUE_PLOT", "province_code", "PLOT", "SP_NAME", "SP_CODE", "HT", "COVER")
    # Return shrub
    return(shrub)
  }

  if (version %in% c("ifn3", "ifn4")) {
    shrub_filtered_data <- .read_inventory_data(
      shrub_data,
      colnames = c(
        "Provincia",
        "Estadillo",
        "Cla",
        "Subclase",
        "Especie",
        "Fcc",
        "Hm"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$ID_UNIQUE_PLOT == plot
      ) |>
      tibble::as_tibble()

    # IFN3 doesn't have Provincia, so we check
    if ("Provincia" %in% names(shrub_filtered_data)) {
      shrub_filtered_data <- shrub_filtered_data |>
        dplyr::filter(.data$Provincia == as.integer(province))
    }

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(shrub_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Shrub data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    shrub <- shrub_filtered_data |>
      dplyr::rename(PLOT = "Estadillo", HT = "Hm", Clase = "Cla") |>
      dplyr::mutate(
        province_code = province,
        # Subclass fixes
        Subclase = .ifn_subclass_fixer(.data$Subclase),
        COVER = .data$Fcc,
        # DM TO CM
        HT = .data$HT * 10,
        COVER = as.numeric(.data$COVER),
        SP_CODE = as.numeric(.data$Especie)
      ) |>
      # 3. ref_plant_dictionary
      # we join data from plant ref dictionary
      # some symbols apply for multiple species
      dplyr::left_join(
        y =  species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::select(
        "ID_UNIQUE_PLOT", "province_code", "Clase", "Subclase",
        "PLOT", "SP_NAME", "SP_CODE", "HT", "COVER"
      )

    return(shrub)
  }
}

#' @describeIn ifn_tables_processing Process to gather needed data from regen table
#' @noRd
ifn_regen_table_process <- function(
  regen_data, version, plot, province, species_ifn_internal, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(regen_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping regen data for plot {.var {plot}} "
    ), call = .call)

    return(dplyr::tibble())
  }

  if (version == "ifn2") {
    regen_filtered_data <- .read_inventory_data(
      regen_data,
      colnames = c("PROVINCIA", "ESTADILLO", "ESPECIE", "NUMERO", "ALTUMED", "REGENA"),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$ID_UNIQUE_PLOT == plot
      ) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(regen_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Regeneration data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    # we add the id code
    regeneration <- regen_filtered_data |>
      dplyr::mutate(
        PLOT = as.character(.data$ESTADILLO),
        province_code = as.character(.data$PROVINCIA),
        NUMERO = as.numeric(.data$NUMERO),
        Hm = as.numeric(.data$ALTUMED) * 10, # DM TO CM
        SP_CODE = as.numeric(.data$ESPECIE)
      ) |>
      dplyr::left_join(
        y = species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::arrange(.data$SP_CODE) |>
      dplyr::rename(Numero = "NUMERO", Regena = "REGENA") |>
      dplyr::slice(rep(dplyr::row_number(), each = 2)) |>
      dplyr::mutate(
        Regena = ifelse(dplyr::row_number() %% 2 != 0, NA, .data$Regena),
        Numero  = ifelse(dplyr::row_number() %% 2 == 0, NA, .data$Numero),
        Hm = ifelse(dplyr::row_number() %% 2 == 0, NA, .data$Hm),
        DBH = dplyr::case_when(.data$Numero > 0 ~ 5, .data$Regena > 0 ~ 1, TRUE ~ NA),
        N = .data$Numero * 127.3239546,
        N = dplyr::case_when(
          .data$Regena == 1 ~ 2.5 * 127.3239546,
          .data$Regena == 2 ~ 10 * 127.3239546,
          .data$Regena == 3 ~ 20 * 127.3239546,
          .data$N > 0 ~ .data$N,
          TRUE ~ NA
        ),
        Height = dplyr::case_when(
          .data$Hm > 0 ~ .data$Hm,
          .data$Regena > 0 ~ 100,
          TRUE ~ NA
        ),
        DENSITY = 127.3239546
      ) |>
      dplyr::select(
        "ID_UNIQUE_PLOT", "province_code", "PLOT", "SP_CODE",
        "SP_NAME", "DBH", "Height", "DENSITY", "N"
      ) |>
      dplyr::filter(complete.cases(.data$DBH, .data$Height, .data$N))

    # Return regen
    return(regeneration)
  }

  if (version %in% c("ifn3", "ifn4")) {
    regen_filtered_data <- .read_inventory_data(
      regen_data,
      colnames = c(
        "Provincia", "Estadillo", "Cla", "Subclase", "Especie", "CatDes",
        "Tipo", "Densidad", "NumPies", "Hm"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$ID_UNIQUE_PLOT == plot
      ) |>
      tibble::as_tibble()

    # IFN3 doesn't have Provincia, so we check
    if ("Provincia" %in% names(regen_filtered_data)) {
      regen_filtered_data <- regen_filtered_data |>
        dplyr::filter(.data$Provincia == as.integer(province))
    }


    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(regen_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Regeneration data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }


    # we add the id code
    regeneration <- regen_filtered_data |>
      dplyr::rename(PLOT = "Estadillo", Clase = "Cla") |>
      dplyr::mutate(
        province_code = province,
        Subclase = .ifn_subclass_fixer(.data$Subclase), # Subclass fixes
        Hm = .data$Hm * 10, # DM TO CM
        SP_CODE = as.numeric(.data$Especie)
      ) |>
      dplyr::left_join(
        y =  species_ifn_internal |>
          dplyr::select("SP_CODE", "SP_NAME"),
        by = "SP_CODE"
      ) |>
      dplyr::mutate(
        DBH = dplyr::case_when(
          .data$CatDes == 1 ~ 0.1,
          .data$CatDes == 2 ~ 0.5,
          .data$CatDes == 3 ~ 1.5,
          .data$CatDes == 4 ~ 5,
          TRUE ~ NA
        ),
        N = dplyr::case_when(
          .data$Densidad == 1 ~ 2.5 * 127.3239546,
          .data$Densidad == 2 ~ 10 * 127.3239546,
          .data$Densidad == 3 ~ 20 * 127.3239546,
          .data$CatDes == 4 ~ NumPies * 127.3239546,
          TRUE ~ NA
        ),
        Height = dplyr::case_when(
          .data$CatDes == 1 ~ 10,
          .data$CatDes == 2 ~ 80,
          .data$CatDes == 3 ~ 100,
          .data$CatDes == 4 ~ Hm,
          TRUE ~ NA
        ),
        DENSITY = 127.3239546
      ) |>
      dplyr::select(
        "ID_UNIQUE_PLOT", "province_code", "Clase", "Subclase", "PLOT",
        "SP_CODE", "SP_NAME", "DBH", "Height", "N", "DENSITY"
      )

    # Return regen
    return(regeneration)
  }
}

#' @describeIn ifn_tables_processing Process to gather needed data from plot and coords tables
#' @noRd
ifn_plot_table_process <- function(
  plot_data, coord_data, version,
  plot, province, ifn_provinces_dictionary, .call = rlang::caller_env()
) {

  # in some cases (get plots from provinces) we pass a quosure to the plot argument.
  # If not, get the estadillo padded. This is for the coord tables in ifn3 and ifn4
  if (!rlang::is_quosure(plot)) {
    plot_estadillo <- stringr::str_split_i(plot, "_", 2)
  } else {
    plot_estadillo <- rlang::quo(.data$Estadillo)
    if (version == "ifn2") {
      plot_estadillo <- rlang::quo(.data$ESTADILLO)
    }
  }

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(plot_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping regen data for plot {.var {plot}} "
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names
  if (version == "ifn2") {

    plot_filtered_data <- .read_inventory_data(
      plot_data,
      colnames = c(
        "PROVINCIA", "ESTADILLO", "HOJA", "ANO", "COORDEX", "COORDEY", "ALTITUD1", "ALTITUD2",
        "PENDIEN1", "PENDIEN2", "FRACCION1", "FRACCION2", "CLASUELO", "ESPESOR", "CLACOBER",
        "CUBIERTA", "ORIENTA1", "ORIENTA2", "MAXPEND1", "MAXPEND2"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$ID_UNIQUE_PLOT == !!plot,
        .data$PROVINCIA == !!province
      ) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(plot_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Plot data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    if (any(is.na(plot_filtered_data$COORDEX), is.na(plot_filtered_data$COORDEY))) {
      cli::cli_warn(c(
        "File {.file {plot_data}} has some errors in the coordinates
        (missing coordinates, bad format...).",
        "i" = "These records will be removed from the results"
      ), call = .call)
    }

    # fix bad formatted coords and filter missing coordinates
    # Ok, here is a little weird, but is what it is. Some IFN2 provinces have letters in the
    # coordinates, which mess with the process. The thing is that due to some
    # exporting/formatting/blackbox process numbers were converted to letters. B is 2, C is 3...
    # So we try to fix this
    plot_coord_fixed_data <- plot_filtered_data |>
      dplyr::filter(!is.na(.data$COORDEX), !is.na(.data$COORDEY)) |>
      dplyr:::mutate(
        COORDEX = stringr::str_replace_all(.data$COORDEX, "A", "1"),
        COORDEX = stringr::str_replace_all(.data$COORDEX, "B", "2"),
        COORDEX = stringr::str_replace_all(.data$COORDEX, "C", "3"),
        COORDEX = stringr::str_replace_all(.data$COORDEX, "D", "4"),
        COORDEX = stringr::str_replace_all(.data$COORDEX, "E", "5"),
        COORDEX = stringr::str_replace_all(.data$COORDEX, "F", "6"),
        COORDEX = stringr::str_replace_all(.data$COORDEX, "G", "7"),
        COORDEX = stringr::str_replace_all(.data$COORDEX, "H", "8"),
        COORDEX = stringr::str_replace_all(.data$COORDEX, "I", "9"),
        COORDEY = stringr::str_replace_all(.data$COORDEY, "A", "1"),
        COORDEY = stringr::str_replace_all(.data$COORDEY, "B", "2"),
        COORDEY = stringr::str_replace_all(.data$COORDEY, "C", "3"),
        COORDEY = stringr::str_replace_all(.data$COORDEY, "D", "4"),
        COORDEY = stringr::str_replace_all(.data$COORDEY, "E", "5"),
        COORDEY = stringr::str_replace_all(.data$COORDEY, "F", "6"),
        COORDEY = stringr::str_replace_all(.data$COORDEY, "G", "7"),
        COORDEY = stringr::str_replace_all(.data$COORDEY, "H", "8"),
        COORDEY = stringr::str_replace_all(.data$COORDEY, "I", "9")
      )

    # we add the id code
    info_plot <- plot_coord_fixed_data |>
      dplyr::rename(
        PLOT = "ESTADILLO", SLOPE = "MAXPEND2", ELEV = "ALTITUD2", YEAR = "ANO", ASPECT = "ORIENTA2"
      ) |>
      dplyr::mutate(
        COUNTRY = "ES",
        province_code = .data$PROVINCIA,
        ELEV = as.numeric(.data$ELEV) * 100,
        SLOPE = as.numeric(stringr::str_replace(.data$SLOPE, ",", ".")),
        SLOPE = dplyr::case_when(
          .data$SLOPE == 1 ~ 1.5,
          .data$SLOPE == 2 ~ 7.5,
          .data$SLOPE == 3 ~ 16,
          .data$SLOPE == 4 ~ 27,
          .data$SLOPE == 5 ~ 40
          ),
        ASPECT = as.numeric(.data$ASPECT),
        COORDEX = as.numeric(.data$COORDEX) * 1000,
        COORDEY = as.numeric(.data$COORDEY) * 1000,
        version = version,
        Huso = dplyr::case_when(
          .data$province_code %in% c("35", "38") ~ 28,
          .data$province_code %in% c(
            "01", "07", "08", "15", "17", "20", "25", "26", "27", "28",
            "30", "32", "33", "36", "39", "43", "48", "02", "03", "04", "05",
            "06", "09", "10", "11", "12", "13", "14", "16", "18", "19", "21",
            "22", "23", "24", "29", "31", "34", "37", "40", "41", "42", "44",
            "45", "46", "47", "49", "50"
          ) ~ 30
        ),
        COORD_SYS = dplyr::case_when(
          .data$province_code %in% c("35", "38") ~ "WGS84",
          .data$province_code %in% c(
            "01", "07", "08", "15", "17", "20", "25", "26", "27", "28",
            "30", "32", "33", "36", "39", "43", "48", "02", "03", "04", "05",
            "06", "09", "10", "11", "12", "13", "14", "16", "18", "19", "21",
            "22", "23", "24", "29", "31", "34", "37", "40", "41", "42", "44",
            "45", "46", "47", "49", "50"
          ) ~ "ED50"
        ),
        crs = dplyr::case_when(
          .data$Huso == 30 & .data$COORD_SYS == "ED50" ~ 23030,
          .data$Huso == 31 & .data$COORD_SYS == "ED50" ~ 4326,
          .data$Huso == 29 & .data$COORD_SYS == "ED50" ~ 23029,
          .data$Huso == 30 & .data$COORD_SYS == "ETRS89" ~ 25830,
          .data$Huso == 31 & .data$COORD_SYS == "ETRS89" ~ 25831,
          .data$Huso == 29 & .data$COORD_SYS == "ETRS89" ~ 25829,
          .data$Huso == 28 & .data$COORD_SYS == "ED50" ~ 23028,
          .data$Huso == 28 & .data$COORD_SYS == "WGS84" ~ 32628,
          TRUE ~ NA_integer_
        )
      ) |>
      dplyr::left_join(
        y = ifn_provinces_dictionary |>
          dplyr::select("province_code", "province_name_original", "ca_name_original"),
        by = "province_code"
      ) |>
      dplyr::select(dplyr::any_of(c(
        "ID_UNIQUE_PLOT", "COUNTRY", "ca_name_original", "province_name_original", "province_code",
        "PLOT", "YEAR", "version", "HOJA", "Huso", "COORDEX", "COORDEY", "COORD_SYS", "crs",
        "PENDIEN2", "SLOPE", "ELEV", "ASPECT" # "soils"
      )))

    return(info_plot)
  }

  if (version %in% c("ifn3", "ifn4")) {
    plot_filtered_data <- .read_inventory_data(
      plot_data,
      colnames = c(
        "Provincia", "Estadillo", "Cla", "Subclase", "Tipo", "Ano", "Rocosid", "MatOrg",
        "TipSuelo1", "TipSuelo2", "TipSuelo3", "Orienta1", "Orienta2", "MaxPend1", "MaxPend2"
      ),
      version = version,
      province = province,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$ID_UNIQUE_PLOT == !!plot,
        .data$Provincia == as.integer(province)
      ) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(plot_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Plot data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    plot_filtered_data <- plot_filtered_data |>
      dplyr::rename(YEAR = "Ano", PLOT = "Estadillo") |>
      dplyr::mutate(
        YEAR = as.character(.data$YEAR),
        Subclase = .ifn_subclass_fixer(.data$Subclase), # Subclass fixes
        version = version,
        province_code = as.character(province)
      )

    # we add the id code
    info_plot <- plot_filtered_data |>
      dplyr::rename(ASPECT = "Orienta1", SLOPE = "MaxPend1") |>
      dplyr::mutate(
        COUNTRY = "ES",
        # de grados centesimales a sexagesimales??
        ASPECT = as.numeric(.data$ASPECT) * 0.9,
        SLOPE = as.numeric(.data$SLOPE),
        SLOPE = dplyr::case_when(
          SLOPE <= 0.6 ~ 1.5,
          SLOPE > 0.6 & SLOPE <= 2.4 ~ 7.5 ,
          SLOPE > 2.4 & SLOPE <= 4 ~ 16 ,
          SLOPE > 4 & SLOPE <= 7 ~ 27 ,
          SLOPE > 7 ~ 40,
        ),
        COORD_SYS = dplyr::case_when(
          .data$version == "ifn4" & .data$province_code %in% c(
            "01", "07", "08", "15", "17", "20", "25", "26", "27", "28",
            "30", "32", "33", "36", "39", "43", "48"
          )  ~ "ED50",
          .data$version == "ifn4" & .data$province_code %in% c("35", "38") ~ "WGS84",
          .data$version == "ifn4" & .data$province_code %in% c(
            "02", "03", "04", "05", "06", "09", "10", "11", "12", "13", "14", "16", "18",
            "19", "21", "22", "23", "24", "29", "31", "34", "37", "40", "41", "42", "44",
            "45", "46", "47", "49", "50"
          ) ~ "ETRS89",
          .data$version == "ifn3" & .data$province_code %in% c(
            "01", "07", "08", "15", "17", "20", "25", "26", "27", "28", "30", "32", "33",
            "36", "39", "43", "48", "02", "03", "04", "05", "06", "09", "10", "11", "12",
            "13", "14", "16", "18", "19", "21", "22", "23", "24", "29", "31", "34", "37",
            "40", "41", "42", "44", "45", "46", "47", "49", "50", "35", "38"
          ) ~ "ED50"
        )
      ) |>
      dplyr::left_join(
        y = ifn_provinces_dictionary |>
          dplyr::select("province_code", "province_name_original", "ca_name_original"),
        by = "province_code"
      ) |>
      # selection of final variables
      dplyr::select(
        "ID_UNIQUE_PLOT", "COUNTRY", "ca_name_original", "province_code", "province_name_original",
        "PLOT", "Cla", "Subclase", "COORD_SYS", "YEAR", "version", "Tipo", "ASPECT", "SLOPE"
      )

    files_validation <- assertthat::validate_that(!any(is.na(c(coord_data))))

    coords_filtered_data <- .read_inventory_data(
      coord_data,
      colnames = c(
        "Provincia", "Estadillo", "Clase", "Cla", "Subclase", "Hoja50", "CoorX", "CoorY", "Huso"
      ),
      version = version,
      province = province,
      .dry = TRUE,
      .ifn = TRUE
    ) |>
      dplyr::filter(
        .data$Estadillo == !!plot_estadillo,
        .data$Provincia == as.integer(province)
      ) |>
      tibble::as_tibble()

    # We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(coords_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Coordinates data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ), call = .call)
      return(dplyr::tibble())
    }

    # check for bad formatted or missing coords to inform the user
    if (any(is.na(coords_filtered_data$CoorX), is.na(coords_filtered_data$CoorY))) {
      cli::cli_warn(c(
        "File {.file {plot_data}} has some errors in the coordinates
        (missing coordinates, bad format...).",
        "i" = "These records will be removed from the results"
      ), call = .call)
    }

    # remove bad formatted or missing coordinates
    coords_fixed_data <- coords_filtered_data |>
      dplyr::filter(!is.na(.data$CoorX), !is.na(.data$CoorY))

    coords_data <- coords_fixed_data |>
      dplyr::rename(
        PLOT = "Estadillo", COORDEX = "CoorX", COORDEY = "CoorY", HOJA = "Hoja50",
        Cla = dplyr::any_of(c("Clase", "Cla"))
      ) |>
      dplyr::mutate(
        HOJA = as.character(.data$HOJA),
        Subclase = .ifn_subclass_fixer(.data$Subclase), # Subclass fixes
        province_code = province,
        province_code = as.character(.data$province_code),
        version = version,
        Huso = ifelse("Huso" %in% names(coords_fixed_data), coords_fixed_data$Huso, NA)
      )

    ## BUG_: coord data now doesn't have unique id, as it has no subclass in table. We need to
    ## join these two tables and we use plot and province code. There is only one
    ## set of coordinates by PLOT (estadillo), so any plots with the same PLOT but
    ## different classes have the same coordinates.
    info_plot <- info_plot |>
      dplyr::left_join(
        y = coords_data |>
          dplyr::select(
            dplyr::any_of(c("PLOT", "province_code", "COORDEX", "COORDEY", "HOJA", "Huso"))
          ) |>
          dplyr::distinct(),
        by = c("province_code", "PLOT")
      ) |>
      # sometimes, plots present in data are not present in coords, so weç
      # remove them
      dplyr::filter(!is.na(.data$COORDEX)) |>
      dplyr::mutate(
        crs = dplyr::case_when(
          is.na(.data$Huso) & .data$COORD_SYS == "ED50" ~ 23030, # For now, we need to solve this: Issue #6
          .data$Huso == 30 & .data$COORD_SYS == "ED50" ~ 23030,
          .data$Huso == 31 & .data$COORD_SYS == "ED50" ~ 4326,
          .data$Huso == 29 & .data$COORD_SYS == "ED50" ~ 23029,
          .data$Huso == 30 & .data$COORD_SYS == "ETRS89" ~ 25830,
          .data$Huso == 31 & .data$COORD_SYS == "ETRS89" ~ 25831,
          .data$Huso == 29 & .data$COORD_SYS == "ETRS89" ~ 25829,
          .data$Huso == 28 & .data$COORD_SYS == "ED50" ~ 23028,
          .data$Huso == 28 & .data$COORD_SYS == "WGS84" ~ 32628,
          TRUE ~ NA_integer_
        )
      ) |>
      dplyr::select(
        dplyr::any_of(c(
          "ID_UNIQUE_PLOT", "COUNTRY", "YEAR", "ca_name_original", "province_code",
          "province_name_original", "PLOT", "Cla", "Subclase", "version", "Tipo", "ASPECT", "SLOPE",
          "crs", "COORD_SYS", "COORDEX", "COORDEY", "HOJA", "Huso"
        ))
      )

    return(info_plot)
  }

  # Return plot with soil
  return(info_plot)
}
