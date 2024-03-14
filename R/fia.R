#' Raw FIA data to tibble
#'
#' Transform raw FIA plot data into tidy data for easier use
#'
#' This function will take every year specified and will retrieve and transform the plot data
#' for the states and plots provided. For that, csv files from FIA must reside in the folder
#' indicated in the \code{folder} argument.
#'
#' @param years A numeric vector with the years to extract de data from.
#' @param states A character vector with the two letters code for the states to extract the data
#'   from.
#' @param filter_list A nested list of states, counties and plots to extract the data from.
#'   If left \code{NULL} all plots for the state for all years will be extracted, which can use a
#'   big amount of memory. See details.
#' @param folder The path to the folder containing the FIA csv files, as character.
#' @param ... Not used at the moment
#' @param .parallel_options An object of class \code{furrr_options}. See
#'   \code{\link[furrr]{furrr_options}}.
#' @param .verbose Logical controlling if progress messages are shown.
#'
#' @section Filter list:
#'   If no \code{filter_list} argument is provided, \code{fia_to_tibble} will attempt to process all
#'   plots for the states and years provided. This will result in sometimes hundred of thousands
#'   plots to be extracted, processed and returned, which in turn will cause a big use of
#'   memory (specially when running in parallel processes) and long times of calculation.
#'   Is better to provide a list of states with the counties and plots to look for to narrow
#'   the process. This \code{filter_list} should have the following structure:
#'   \preformatted{
#'   list(
#'     "MN" = list("137" = c(29396, 25064), "71" = c(20210)),
#'     "OR" = list("59" = c(76413)),
#'     "CA" = list("105" = c(70128, 83043))
#'   )
#'   }
#'   \code{esus} package offers workflows to create this automatically, see
#'   \code{vignette("filtering_plots", pkg = "esus")} for more details.
#'
#' @section Parallel:
#'   Processing the plots from within a year can be done in parallel (\code{esus} uses internally
#'   the \code{\link[furrr]{furrr}} package for this). This means that, if parallelization is
#'   active, several processes are launched to retrieve the plots data for that year. This is
#'   repeated for all years provided.
#'
#'   \code{.parallel_options} controls the finer details of how parallelization is performed (see
#'   \code{\link[furrr]{furrr_options}}). But no parallelization can occur without setting first
#'   a \code{\link[future]{plan}}. By default, the chosen plan is \code{\link[future]{sequential}},
#'   so no parellization is done. Changing the plan, i.e. to \code{\link[future]{multisession}} or
#'   to \code{\link[future.callr]{callr}}, will allow \code{fia_to_tibble} to use parallelization
#'   when retrieving the data.
#'
#' @return A nested tibble. This tibble contains a row per plot/year combination, with the plot
#'   metadata included, as well as columns containing tibbles with tree, shrub, herbs and soil
#'   information. See \code{vignette("inventory_data_tibble", pkg = "esus")}
#'
#' @export
fia_to_tibble <- function(
  years,
  states,
  filter_list = NULL,
  folder,
  ...,
  .parallel_options = furrr::furrr_options(scheduling = 1L, stdout = TRUE),
  .verbose = TRUE
) {

  ## Assertions and checks ##
  # grep
  assertthat::assert_that(
    .sys_cmd_warning()
  )

  # states
  assertthat::assert_that(
    is.character(states), length(states) > 0,
    msg = cli::cli_abort("states must be a character vector with at least one state code")
  )
  ## TODO
  # check all states are valid (create a dictionary of states)

  # years
  assertthat::assert_that(
    is.numeric(years), length(years) > 0,
    msg = cli::cli_abort("years must be a numeric vector with at least one year")
  )
  ## TODO
  # check years are valid (create a dictionary of years)

  # folder
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort(
      "Folder especified ({.path {folder}}) doesn't exists.
      Please create the folder first and populate it with the needed FIA csv files"
    )
  )

  # filter_list
  if (is.null(filter_list)) {
    if (interactive()) {
      cli::cli_inform(c(
        "You haven't specified any counties/plots in the {.arg filter_list} argument.",
        "x" = "This will cause to retrieve {.strong ALL} plots for {.strong ALL} counties for the
        selected states and years",
        "!" = "This will use a lot of memory and time, as hundred of thousands plots will
        potentially be evaluated",
        "TODO: add info about how to create the filter list",
        ""
      ))

      user_auth <- utils::menu(c("Yes", "No"), title = "Do you wish to continue anyway?")
      if (user_auth == 2L) {
        cli::cli_abort("Aborting per user request")
      }
    }
  }
  ## TODO
  # Check counties and plots??

  ## TODO
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

  # ancillary data needed
  # ref species
  assertthat::assert_that(
    fs::file_exists(fs::path(folder, "REF_SPECIES.csv")),
    msg = cli::cli_abort(
      "{.file REF_SPECIES.csv} must be present at {.path {folder}} to be able to continue"
    )
  )
  # plant dict
  assertthat::assert_that(
    fs::file_exists(fs::path(folder, "REF_PLANT_DICTIONARY.csv")),
    msg = cli::cli_abort(
      "{.file REF_PLANT_DICTIONARY.csv} file must be present at {.path {folder}} to be able
      to continue"
    )
  )

  ## inform the user
  verbose_msg(
    cli::cli_inform(
      c("Start", "i" = "Processing {length(years)} year{?s}")
    ),
    .verbose
  )

  # get the caller environment to propagate errors
  .call <- rlang::caller_env(0)
  ## send the years in loop to process table function
  purrr::map(
    years,
    .f = \(year) {
      fia_tables_process(
        year, states, filter_list, folder,
        .parallel_options, .verbose, .call = .call,
        ...
      )
    },
    .progress = FALSE
  ) |>
    purrr::list_rbind()
}


#' Inner function to process all tables for one year
#'
#' Processing all tables for one year
#'
#' This function is intended to be called internally by \code{\link{fia_to_tibble}} for each
#' year. This is implemented with furrr to allow parallelization of the plots data retrieval.
#'
#' @inherit fia_to_tibble
#'
#' @param .call Caller environment (\code{\link[rlang]{caller_env}}) to allow informative errors
#'
#' @noRd
fia_tables_process <- function(
  year, states, filter_list, folder, .parallel_options, .verbose, .call = rlang::caller_env(), ...
) {

  # Create input df for year. We need to remove the NAs due to missing files
  input_df <- .build_fia_input_with(year, states, filter_list, folder, .verbose, .call) |>
    # filter file name NAs due to missing files (bad states or bad paths)
    # (missing plots are filtered at the end of the process, not ideal but ok)
    dplyr::filter(!is.na(.data$plot_table))

  # Get needed ancillary data
  ref_species <- .read_inventory_data(fs::path(folder, "REF_SPECIES.csv")) |> dplyr::as_tibble()
  ref_plant_dictionary <- .read_inventory_data(fs::path(folder, "REF_PLANT_DICTIONARY.csv")) |>
    dplyr::as_tibble()

  # purrr::pmap(
  temp_res <- furrr::future_pmap(
    .progress = .verbose,
    .l = input_df,
    .f = \(
      state,
      county,
      plots,
      tree_table_file,
      plot_table_file,
      survey_table_file,
      cond_table_file,
      p3_understory_table_file,
      seedling_table_file,
      subplot_table_file,
      veg_subplot_table_file,
      p2_veg_subplot_table_file
    ) {

      # plot info
      plot_info <- fia_plot_table_process(
        plot_table_file, survey_table_file, cond_table_file, plots, county, year, .call
      )

      redundant_vars <- c(
        "YEAR", "ID_UNIQUE_PLOT", "COUNTRY", "STATECD", "STATEAB", "STATENM",
        "COUNTYCD", "PLOT", "P3PANEL", "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
        "RSCD", "DESIGNCD", "COORD1", "COORD1_ORIGINAL", "COORD2", "COORD2_ORIGINAL",
        "COORD_SYS", "ELEV", "ELEV_ORIGINAL", "ASPECT", "ASPECT_ORIGINAL", "SLOPE", "SLOPE_ORIGINAL"
      )

      # if there is no info for the plot (missing files) there is no need to continue,
      # so we check and return an empty tibble which is ignored after the loop when rbinding
      # the results
      if (nrow(plot_info) < 1) {
        return(tibble::tibble())
      }

      state <- plot_info[["STATECD"]]

      # tree data
      tree <- fia_tree_table_process(tree_table_file, plots, county, year, ref_species, .call) |>
        dplyr::select(!dplyr::any_of(redundant_vars))

      # understory
      shrub <- fia_understory_table_process(
        p3_understory_table_file, p2_veg_subplot_table_file,
        plots, county, year,
        growth_habit_p3 = "Shrub", growth_habit_p2 = "SH",
        ref_plant_dictionary,
        .call
      ) |>
        dplyr::select(!dplyr::any_of(redundant_vars))
      herbs <- fia_understory_table_process(
        p3_understory_table_file, p2_veg_subplot_table_file,
        plots, county, year,
        growth_habit_p3 = c("Forb/herb", "Graminoids"), growth_habit_p2 = c("FB", "GR"),
        ref_plant_dictionary,
        .call
      ) |>
        dplyr::select(!dplyr::any_of(redundant_vars))

      # seedlings
      regen <-
        fia_seedling_table_process(seedling_table_file, plots, county, year, ref_species, .call) |>
        dplyr::select(!dplyr::any_of(redundant_vars))

      # subplot
      subplot <-
        fia_subplot_table_process(subplot_table_file, plots, county, year, .call) |>
        dplyr::select(!dplyr::any_of(redundant_vars))

      # we group in a data frame understory info
      understory <- dplyr::tibble(
        # PLOT = plots,
        # COUNTYCD = county,
        # YEAR = year,
        # STATECD = state,
        # ID_UNIQUE_PLOT = paste("US", .data$STATECD, .data$COUNTYCD, .data$PLOT, sep = "_"),
        shrub = list(shrub),
        herbs = list(herbs)
      ) |>
        dplyr::select("shrub", "herbs")

      # finally we put together all tables in a data frame and return it
      plot_info |>
        dplyr::mutate(
          tree = list(tree),
          understory = list(understory),
          regen = list(regen),
          subplot = list(subplot)
        )
    }
  ) |>
    purrr::list_rbind()

  # something went wrong (bad counties and plots, wrong filter list...)
  if (nrow(temp_res) < 1) {
    cli::cli_abort("Ooops! Something went wrong, exiting...", call = .call)
  }

  # return the res, but filtering first
  temp_res |>
    # filtering the missing plots. This is done based on the fact plot table functions returns NAs
    # for all vars, including coords, when the plot is not found
    dplyr::filter(!(
      is.na(.data$COORD1) & is.na(.data$COORD2_ORIGINAL) &
        is.na(.data$COORD1) & is.na(.data$COORD1_ORIGINAL)
    ))
}

#' FIA data tables process
#'
#' Process to gather needed data from FIA csv tables
#'
#' These functions retrieve the data for a plot in one year.
#'
#' @param plot_data,survey_data,cond_data,tree_data,understory_data,understory_p2,seedling_data,subplot_data,soils_lab,soils_loc Paths to the files with the corresponding data
#' @param plot Numeric, plot code
#' @param county COUNTYCD code
#' @param year Numeric, year to extract
#' @param ref_species,ref_plant_dictionary ref species and ref plant dictionary tables. These tables
#'   are automatically read in \code{\link{fia_tables_process}} based on the folder provided.
#' @param growth_habit Character, growth habit value to filter data (to distinguish between herbs
#'   and shrubs).
#' @param .call Caller environment (\code{\link[rlang]{caller_env}}) to allow informative errors
#'
#' @return A tibble with one or more rows (depending on the data retrieved) for each plot for that
#'   year.
#'
#' @name fia_tables_processing
#' @noRd
NULL

#' FIA data tables process
#' @describeIn fia_tables_processing Process to gather needed data from plot, survey and cond tables
#' @noRd
fia_plot_table_process <- function(
  plot_data, survey_data, cond_data, plot, county, year, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(plot_data, survey_data, cond_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping plot info for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  ## Data gathering
  # survey table
  data_survey <- .read_inventory_data(
    survey_data,
    select = c("INVYR", "STATECD", "STATEAB", "STATENM", "RSCD", "ANN_INVENTORY")
  ) |>
    # we arrange by year to lately get the last record
    dplyr::arrange(desc(.data$INVYR)) |>
    #there might be more than 1 record
    dplyr::distinct() |>
    .extract_fia_metadata(
      c("RSCD", "STATECD", "STATEAB", "STATENM"), county, plot, year, .soil_mode = FALSE
    ) |>
    dplyr::mutate(PLOT  = plot, INVYR  = year, COUNTYCD = county)

  # plot table
  data_plot_raw <- .read_inventory_data(
    plot_data,
    select = c(
      "INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", "LAT", "LON",
      "ELEV", "PLOT_STATUS_CD", "SAMP_METHOD_CD", "SUBP_EXAMINE_CD",
      "P3PANEL", "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD", "DESIGNCD"
    )
  )

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data_plot_raw) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no plot data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county
      {.var {county}}"
    ), call = .call)
    return(dplyr::tibble())
  }

  data_plot <- data_plot_raw |>
    # we arrange by year to catch last record lately
    dplyr::arrange(desc(.data$INVYR)) |>
    dplyr::mutate(
      ELEV = .data$ELEV * 0.3048, # elev in feet to meters
      ID_UNIQUE_PLOT = paste("US", .data$STATECD, .data$COUNTYCD, .data$PLOT, sep = "_"),
      COORD_SYS =  dplyr::if_else(.data$STATECD %in% c(60, 64, 66, 68, 69, 70), "WGS84", "NAD83")
    ) |>
    dplyr::select(
      "ID_UNIQUE_PLOT", "PLOT", "INVYR", "STATECD", "COUNTYCD", "P3PANEL",
      "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD", "ELEV",
      # LON & LAT, NAD 83 datum ;   EXCEPTIONS depending on  RSCD
      "LON", "LAT", "COORD_SYS",
      # DESIGN : can change between years:
      "DESIGNCD"
    ) |>
    data.table::as.data.table() |>
    .extract_fia_metadata(
      c(
        "LAT", "LON", "ELEV", "P3PANEL", "P2VEG_SAMPLING_STATUS_CD",
        "P2VEG_SAMPLING_LEVEL_DETAIL_CD", "DESIGNCD", "COORD_SYS"
      ),
      county, plot, year, .soil_mode = FALSE
    ) |>
    dplyr::mutate(
      crs = dplyr::case_when(
        .data$COORD_SYS == "WGS84" ~ 4326,
        .data$COORD_SYS == "NAD83" ~ 4269),
      PLOT  = plot,
      INVYR  = year,
      COUNTYCD   = county,
    )

  # CONDITION TABLE
  data_cond <- .read_inventory_data(
    cond_data,
    select = c(
      "INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT",
      "SLOPE", "ASPECT", "CONDID", "COND_STATUS_CD", "SOIL_ROOTING_DEPTH_PNW"
    )
  ) |>
    dplyr::filter(
      # FOREST LAND
      # Accessible forest land - Land within the population of interest that can be
      # occupied safely and has at least 10 percent canopy cover by live tally trees of any
      # size or has had at least 10 percent canopy cover of live tally species in the past,
      # based on the presence of stumps, snags, or other evidence
      # the time of the plot establishment,
      # the condition class at plot center (the center of subplot 1) is usually designated as
      # condition class 1.

      # condid 1 to use this as proxy
      .data$CONDID == 1
    ) |>
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", .data$STATECD, .data$COUNTYCD, .data$PLOT, sep = "_")
    ) |>
    dplyr::group_by(.data$ID_UNIQUE_PLOT, .data$INVYR) |>
    dplyr::select("ID_UNIQUE_PLOT", "PLOT", "COUNTYCD", "SLOPE", "ASPECT", "INVYR") |>
    dplyr::distinct() |>
    data.table::as.data.table() |>
    .extract_fia_metadata(c("SLOPE", "ASPECT"), county, plot, year, .soil_mode = FALSE) |>
    dplyr::mutate(PLOT = plot, INVYR = year, COUNTYCD = county)

  # we extract the vars we need and return the object
  data_survey |>
    dplyr::left_join(data_plot, by = c("PLOT", "INVYR", "COUNTYCD")) |>
    dplyr::left_join(data_cond, by = c("PLOT", "INVYR", "COUNTYCD")) |>
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", .data$STATECD, .data$COUNTYCD, .data$PLOT, sep = "_"),
      COUNTRY = "US"
    ) |>
    dplyr::select(
      "INVYR", "ID_UNIQUE_PLOT", "COUNTRY", "STATECD", "STATEAB", "STATENM",
      "COUNTYCD", "PLOT", "P3PANEL", "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
      "RSCD", "DESIGNCD", "LON", "LON_ORIGINAL", "LAT", "LAT_ORIGINAL",
      "COORD_SYS", "crs", "ELEV", "ELEV_ORIGINAL", "ASPECT", "ASPECT_ORIGINAL", "SLOPE", "SLOPE_ORIGINAL"
    ) |>
    dplyr::rename(
      YEAR = "INVYR",
      COORD1 = "LON",
      COORD2 = "LAT",
      COORD1_ORIGINAL = "LON_ORIGINAL",
      COORD2_ORIGINAL = "LAT_ORIGINAL"
    ) |>
    dplyr::as_tibble()
}

#' @describeIn fia_tables_processing Process to gather needed data from tree table
#' @noRd
fia_tree_table_process <- function(
  tree_data, plot, county, year, ref_species, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(tree_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names we select the column names to be read
  filtered_data <- .read_inventory_data(
    tree_data,
    select = c(
      "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP", "TREE", "STATUSCD", "CONDID",
      "SPCD", "SPGRPCD", "DIA", "DIAHTCD", "HT", "TPA_UNADJ"
    )
  ) |>
    dplyr::filter(.data$PLOT == plot, .data$INVYR == year, .data$COUNTYCD == county)

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no tree data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county
      {.var {county}}"
    ), call = .call)
    return(dplyr::tibble())
  }


  # 3. join with ref_species??

  tree <- filtered_data |>
    # units transformations
    dplyr::mutate(
      # unique inner code
      ID_UNIQUE_PLOT = paste("US", .data$STATECD, .data$COUNTYCD, .data$PLOT, sep = "_"),
      DIA = .data$DIA * 2.54, # INCHES TO CM
      HT = .data$HT * 0.3048, # FEET TO M
      DENSITY = .data$TPA_UNADJ / 0.4046856422 # acre to ha
    ) |>
    # add species info
    dplyr::left_join(
      y = ref_species |>
        dplyr::select("SPCD", "GENUS", "SPECIES", "SPECIES_SYMBOL"),
      by = "SPCD"
    ) |>
    dplyr::mutate(SP_NAME = (paste(.data$GENUS, .data$SPECIES, sep = " "))) |>
    dplyr::arrange(.data$SP_NAME) |>
    dplyr::select(
      "ID_UNIQUE_PLOT", "INVYR", "STATECD", "COUNTYCD", "PLOT",
      "TREE", "STATUSCD", "DIA", "HT", "SP_NAME", "SPCD", "DENSITY"
    ) |>
    dplyr::rename(YEAR = "INVYR", STATUS = "STATUSCD", SP_CODE = "SPCD") |>
    dplyr::as_tibble()

  # Return tree
  return(tree)
}

#' @describeIn fia_tables_processing Process to guess which understory data is available and launch
#'   the corresponding function.
#' @noRd
fia_understory_table_process <- function(
  understory_data, understory_p2,
  plot, county, year, growth_habit_p3, growth_habit_p2, ref_plant_dictionary,
  .call = rlang::caller_env()
) {

  # Final logic to decide between p3 and p2
  #   - p3 is always preferred
  #   - p2 is taken if no p3 is found
  #   - if not p3 or p2 is found for plot/year combination, return empty tibble
  #   - if p3 and p2 are found, complete info with species in p2 not present in p3

  # read the p3 and p2 data to check nrows
  p3_info <- suppressWarnings(fia_p3_understory_table_process(
    understory_data, plot, county, year, growth_habit_p3, ref_plant_dictionary
  ))
  p2_info <- suppressWarnings(fia_p2_understory_table_process(
    understory_p2, plot, county, year, growth_habit_p2, ref_plant_dictionary
  ))
  p3_rows <- nrow(p3_info)
  p2_rows <- nrow(p2_info)

  # check rows and return the corresponding data
  if (p3_rows < 1) {
    if (p2_rows > 0) {
      return(p2_info)
    } else {
      cli::cli_warn(c(
        "No understory data found",
        "i" = "Skipping understory data for plot {.var {plot}} at county {.var {county}} for
        {.var {year}}"
      ), call = .call)
      return(tibble::tibble())
    }
  } else {
    if (p2_rows > 0) {
      # join p3 and those species of p2 not present in p3
      understory_info <- p2_info |>
        dplyr::filter(!.data$SP_NAME %in% unique(p3_info$SP_NAME)) |>
        dplyr::bind_rows(p3_info)
      return(understory_info)
    }
  }

  return(p3_info)
}


#' @describeIn fia_tables_processing Process to gather needed data from veg subplot spp table
#' @noRd
fia_p3_understory_table_process <- function(
  understory_data, plot, county, year, growth_habit, ref_plant_dictionary,
  .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(understory_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping understory p3 data for plot {.var {plot}} at county {.var {county}} for
      {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names

  filtered_data <- .read_inventory_data(
    understory_data,
    select = c(
      "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP", "VEG_FLDSPCD",
      # REF_SPECIES TABLE CODE
      "VEG_SPCD",
      "SP_CANOPY_COVER_TOTAL",
      #  0-6 feet 1mfeet- 0,3048m (1.8m)
      "SP_CANOPY_COVER_LAYER_1_2",
      # 6 to 16 feet (4.9 m)
      "SP_CANOPY_COVER_LAYER_3",
      # above 16 feet
      "SP_CANOPY_COVER_LAYER_4"
    )
  ) |>
    dplyr::rename(
      "SPECIES_SYMBOL" = "VEG_SPCD",
    ) |>
    dplyr::filter(.data$PLOT == plot, .data$INVYR == year, .data$COUNTYCD == county)

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no p3 understory data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county
      {.var {county}}"
    ), call = .call)
    return(dplyr::tibble())
  }

  # we add the id code
  understory_filtered_data <- filtered_data |>
    # we group by species to calculate means (height, cover)
    dplyr::group_by(.data$SPECIES_SYMBOL) |>
    # here we calculate an averaged height by species, for that we select the height that has
    # the maximum percentage cover and we assign as a height the middle value of the interval of
    # that layer in meters layer 1-2  = 0- 1,8288meters, layer 3 from 1,8288meters to 4,8768
    # and layer 4 more than  4,8768m
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", .data$STATECD, .data$COUNTYCD, .data$PLOT, sep = "_"),
      COVER_PCT = .data$SP_CANOPY_COVER_TOTAL,
      # HT in cm
      HT = dplyr::case_when(
        (which.max(c(
          max(.data$SP_CANOPY_COVER_LAYER_1_2), max(.data$SP_CANOPY_COVER_LAYER_3),
          max(.data$SP_CANOPY_COVER_LAYER_4)
        ))) == 1 ~ 91,
        (which.max(c(
          max(.data$SP_CANOPY_COVER_LAYER_1_2), max(.data$SP_CANOPY_COVER_LAYER_3),
          max(.data$SP_CANOPY_COVER_LAYER_4)
        ))) == 2 ~ 340,
        # for third layer this is the minimum height not the averaged  ! :)
        (which.max(c(
          max(.data$SP_CANOPY_COVER_LAYER_1_2), max(.data$SP_CANOPY_COVER_LAYER_3),
          max(.data$SP_CANOPY_COVER_LAYER_4)
        ))) == 3 ~ 500
      )
    ) |>
    # 3. ref_plant_dictionary
    # we join data from plant ref dictionary
    # some symbols apply for multiple species
    dplyr::left_join(
      y = ref_plant_dictionary |>
        dplyr::select(
          dplyr::all_of(c(
            # there are  species with the same code
            "SPECIES_SYMBOL" = "SYMBOL",
            "FAMILY",
            "GENUS",
            "SPECIES",
            "CATEGORY",
            # we will use this variable to discriminate between functional/form group
            "GROWTH_HABIT",
            "DURATION"
          ))
        ),
      by = "SPECIES_SYMBOL"
    ) |>
    dplyr::filter(.data$GROWTH_HABIT %in% growth_habit)

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(understory_filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no p3 understory data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county
      {.var {county}}"
    ), call = .call)
    return(dplyr::tibble())
  }

  # we add latin name and select variables
  understory <- understory_filtered_data |>
    dplyr::mutate(SP_NAME = paste(.data$GENUS, .data$SPECIES, sep = " ")) |>
    dplyr::arrange(.data$SPECIES_SYMBOL, .data$SUBP) |>
    dplyr::select(
      "ID_UNIQUE_PLOT", "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
      "SPECIES_SYMBOL", "SP_NAME", "HT", "COVER_PCT", "GROWTH_HABIT"
    ) |>
    dplyr::rename(YEAR = "INVYR", SP_CODE = "SPECIES_SYMBOL", COVER = "COVER_PCT") |>
    dplyr::distinct() |>
    dplyr::as_tibble()

  # Return understory
  return(understory)
}

#' @describeIn fia_tables_processing Process to gather needed data from p2 veg subplot spp table
#' @noRd
fia_p2_understory_table_process <- function(
  understory_p2, plot, county, year, growth_habit, ref_plant_dictionary, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(understory_p2)))
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping understory p2 data for plot {.var {plot}} at county {.var {county}} for
      {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names
  filtered_data <- .read_inventory_data(
    understory_p2,
    select = c(
      "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP", "CONDID",
      "VEG_FLDSPCD", "VEG_SPCD", "GROWTH_HABIT_CD",
      # VERTICAL LAYER 1, 2, 3 OR 4
      "LAYER",
      "COVER_PCT"
    )
  ) |>
    dplyr::rename(
      "SPECIES_SYMBOL" = "VEG_SPCD",
    ) |>
    dplyr::filter(.data$PLOT == plot, .data$INVYR == year, .data$COUNTYCD == county)

  # We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no p2 understory data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county
      {.var {county}}"
    ), call = .call)
    return(dplyr::tibble())
  }

  # we add the id code
  understory_p2_filtered_data <- filtered_data |>
    # we group by species to calculate means (height, cover)
    dplyr::group_by(.data$SPECIES_SYMBOL) |>
    dplyr::filter(.data$GROWTH_HABIT_CD %in% growth_habit) |>
    # we calculate mean cover an height from layer
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", .data$STATECD, .data$COUNTYCD, .data$PLOT, sep = "_"),
      # HT in cm
      HT = dplyr::case_when(
        .data$LAYER == 1 ~  30,
        .data$LAYER == 2 ~ 122,
        .data$LAYER == 3 ~ 335,
        # for 4TH layer this is the minimum height not the averaged  !
        .data$LAYER == 4 ~ 500
      )
    ) |>
    # we join data from plant ref dictionary one symbol can apply for various species
    dplyr::left_join(
      y = ref_plant_dictionary |>
        dplyr::select(
          dplyr::all_of(c(
            # there are species with the same code
            "SPECIES_SYMBOL" = "SYMBOL",
            "FAMILY",
            "GENUS",
            "SPECIES",
            "CATEGORY",
            # we DONT use this variable here because filter is already done
            "GROWTH_HABIT",
            "DURATION"
          ))
        ),
      by = "SPECIES_SYMBOL"
    )

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(understory_p2_filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no p2 understory data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county
      {.var {county}}"
    ), call = .call)
    return(dplyr::tibble())
  }

  # we add latin name and select variables
  understory_p2 <- understory_p2_filtered_data |>
    # we add latin name
    dplyr::mutate(
      SP_NAME = paste(.data$GENUS, .data$SPECIES, sep = " ")
    ) |>
    dplyr::arrange(.data$SPECIES_SYMBOL, .data$SUBP) |>
    # we select final variables
    dplyr::select(
      "ID_UNIQUE_PLOT", "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
      "SPECIES_SYMBOL", "SP_NAME", "GROWTH_HABIT_CD", "HT", "COVER_PCT", "GROWTH_HABIT"
    ) |>
    dplyr::rename(YEAR = "INVYR", SP_CODE = "SPECIES_SYMBOL", COVER = "COVER_PCT") |>
    # We have repeated rows after the selection because we summarised shrubs species. We remove with
    # distinct
    dplyr::distinct() |>
    dplyr::as_tibble()


  # Return understory_p2
  return(understory_p2)
}

#' @describeIn fia_tables_processing Process to gather needed data from seedling table
#' @noRd
fia_seedling_table_process <- function(
  seedling_data, plot, county, year, ref_species, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(seedling_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping seedling data for plot {.var {plot}} at county {.var {county}} for
      {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names
  filtered_data <- .read_inventory_data(
    seedling_data,
    select = c(
      "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP", "CONDID", "SPCD",
      "SPGRPCD", "TREECOUNT", "TREECOUNT_CALC", "TPA_UNADJ", "TOTAGE"
    )
  ) |>
    dplyr::filter(.data$PLOT == plot, .data$INVYR == year, .data$COUNTYCD == county)

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no seedling data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county
      {.var {county}}"
    ), call = .call)
    return(dplyr::tibble())
  }

  # we add the id code
  seedling <- filtered_data |>
    # we filter by species to calculate means (height, cover)
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", .data$STATECD, .data$COUNTYCD, .data$PLOT, sep = "_"),
      #conversion from acre to ha
      TPA_UNADJ = .data$TPA_UNADJ / 0.4046856422
    ) |>
    # join with ref_species
    dplyr::left_join(
      y = ref_species |>
        dplyr::select("SPCD", "GENUS", "SPECIES", "SPECIES_SYMBOL"),
      by = "SPCD"
    ) |>
    dplyr::mutate(
      SP_NAME = paste(.data$GENUS, .data$SPECIES, sep = " "),
      #LESS THAN 6 INCH FOR CONIFER AND 12 FOR HARDWOOD MINIMUM default = 6 inch
      Height = 15,
      #LESS THAN 1 INCH = 2.54 CM default ? revisar ifn
      DBH = 2.54,
      #calculate density represented by tree
      N = .data$TPA_UNADJ * .data$TREECOUNT_CALC
    ) |>
    # we arrange by species and tpa
    dplyr::arrange(.data$SPCD, .data$SUBP) |>
    #selection of final variables
    dplyr::select(
      "ID_UNIQUE_PLOT", "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP", "SPCD",
      "SP_NAME", "TREECOUNT_CALC", "TPA_UNADJ", "N", "Height", "DBH"
    ) |>
    dplyr::rename(YEAR = "INVYR", SP_CODE = "SPCD", DENSITY = "TPA_UNADJ") |>
    # # We have repeated rows after the selection because we summarised shrubs species.
    # We remove with distinct
    dplyr::distinct() |>
    dplyr::as_tibble()

  # Return seedlings
  return(seedling)
}

#' @describeIn fia_tables_processing Process to gather needed data from subplot table
#' @noRd
fia_subplot_table_process <- function(
  subplot_data, plot, county, year, .call = rlang::caller_env()
) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(!any(is.na(c(subplot_data))))

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping subplot data for plot {.var {plot}} at county {.var {county}} for
      {.var {year}}"
    ), call = .call)

    return(dplyr::tibble())
  }

  # 2. col names
  filtered_data <- .read_inventory_data(
    subplot_data,
    select = c(
      "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP", "SUBP_STATUS_CD", "MACRCOND",
      "SUBPCOND", "MICRCOND", "SLOPE", "ASPECT", "P2VEG_SUBP_STATUS_CD"
    )
  ) |>
    dplyr::filter(.data$PLOT == plot, .data$INVYR == year, .data$COUNTYCD == county)

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(data.table::as.data.table(filtered_data)) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no subplot data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county
      {.var {county}}"
    ), call = .call)
    return(dplyr::tibble())
  }

  # we add the id code
  subplot <- filtered_data |>
    # we add id code
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", .data$STATECD, .data$COUNTYCD, .data$PLOT, sep = "_"),
    ) |>
    dplyr::select(
      "ID_UNIQUE_PLOT", "INVYR", "STATECD", "COUNTYCD",
      "PLOT", "SUBP", "SLOPE", "ASPECT", "MACRCOND",
      # Condition number for the condition at the center of the subplot.
      "SUBPCOND", "MICRCOND"
    ) |>
    dplyr::rename(YEAR = "INVYR") |>
    # We have repeated rows after the selection because we summarised shrubs species. We remove with
    # distinct
    dplyr::distinct() |>
    dplyr::as_tibble()

  # Return shrub
  return(subplot)
}
