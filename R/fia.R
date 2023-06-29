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
  # states
  assertthat::assert_that(
    is.character(states), length(states) > 0,
    msg = cli::cli_abort("years must be a character vector with at least one state code")
  )
  ## TODO
  # check all states are valid

  # years
  assertthat::assert_that(
    is.numeric(years), length(years) > 0,
    msg = cli::cli_abort("years must be a numeric vector with at least one year")
  )
  ## TODO
  # check years are valid

  # folder
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort( "Folder especified ({.path {folder}}) doesn't exists. Please create the folder first and populate it with the needed FIA csv files")
  )

  # filter_list
  if (is.null(filter_list)) {
    if (interactive()) {
      cli::cli_inform(c(
        "You haven't specified any counties/plots in the {.arg filter_list} argument.",
        "x" = "This will cause to retrieve {.strong ALL} plots for {.strong ALL} counties for the selected states and years",
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
  ## TODO
  # Check counties and plots??

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
    msg = cli::cli_abort("{.file REF_SPECIES.csv} must be present at {.path {folder}} to be able to continue")
  )
  # plant dict
  assertthat::assert_that(
    fs::file_exists(fs::path(folder, "REF_PLANT_DICTIONARY.csv")),
    msg = cli::cli_abort("{.file REF_PLANT_DICTIONARY.csv} file must be present at {.path {folder}} to be able to continue")
  )

  ## inform the user
  verbose_msg(
    cli::cli_inform(
      c("Start", "i" = "Processing {length(years)} year{?s}")
    ),
    .verbose
  )
  ## send the years in loop to process table function
  purrr::map(
    years,
    .f = \(year) {
      fia_tables_process(year, states, filter_list, folder, .parallel_options, .verbose, ...)
    },
    .progress = FALSE
  ) |>
    purrr::list_rbind()
}

fia_tables_process <- function(
  year, states, filter_list, folder, .parallel_options, .verbose, ...
) {

  # debug
  # browser()

  # Create input df for year
  input_df <- .build_fia_input_with(year, states, filter_list, folder, .verbose)

  # Get needed ancillary data
  ref_species <- .read_fia_data(fs::path(folder, "REF_SPECIES.csv")) |> dplyr::as_tibble()
  ref_plant_dictionary <- .read_fia_data(fs::path(folder, "REF_PLANT_DICTIONARY.csv")) |> dplyr::as_tibble()

  furrr::future_pmap(
  # purrr::pmap(
    .progress = .verbose,
    .l = input_df,
    .f = \(
      state, county, plots, tree_table_file, plot_table_file, survey_table_file, cond_table_file,
      p3_understory_table_file, seedling_table_file, subplot_table_file, soils_loc_table_file,
      soils_lab_table_file, veg_subplot_table_file, p2_veg_subplot_table_file
    ) {
      # debug
      # browser()

      # plot info
      plot_info <- fia_plot_table_process(
        plot_table_file, survey_table_file, cond_table_file, plots, county, year
      )

      P3PANEL <- plot_info[["P3PANEL"]]
      state <- plot_info[["STATECD"]]

      # tree data
      tree <- fia_tree_table_process(tree_table_file, plots, county, year, ref_species)

      # understory
      if (!is.na(P3PANEL)) {
        shrub <- fia_p3_understory_table_process(
          p3_understory_table_file, plots, county, year,
          growth_habit = "Shrub", ref_plant_dictionary
        )
        herbs <- fia_p3_understory_table_process(
          p3_understory_table_file, plots, county, year,
          growth_habit = c("Forb/herb", "Graminoids"), ref_plant_dictionary
        )
      } else {
        shrub <- fia_p2_understory_table_process(
          p2_veg_subplot_table_file, plots, county, year,
          growth_habit = "SH", ref_plant_dictionary
        )
        herbs <- fia_p2_understory_table_process(
          p2_veg_subplot_table_file, plots, county, year,
          growth_habit = c("FB", "GR"), ref_plant_dictionary
        )
      }

      # seedlings
      regen <- fia_seedling_table_process(seedling_table_file, plots, county, year, ref_species)

      # subplot
      subplot <- fia_subplot_table_process(subplot_table_file, plots, county, year)

      # soil
      soils_lab <- fia_soils_lab_table_process( soils_lab_table_file, plots, county, state, year)
      soils_loc <- fia_soils_loc_table_process(soils_loc_table_file, veg_subplot_table_file, plots, county, state, year)

      # we group in a data frame soil info
      soils <- dplyr::tibble(
        PLOT = plots,
        COUNTYCD = county,
        YEAR = year,
        STATECD =  state,
        ID_UNIQUE_PLOT = paste("US", STATECD, COUNTYCD, PLOT, sep = "_"),
        soils_lab = list(soils_lab),
        soils_loc = list(soils_loc)
      ) |>
        dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          STATECD,
          COUNTYCD,
          YEAR,
          soils_lab,
          soils_loc
        )

      #we group in a data frame understory info
      understory <- dplyr::tibble(
        PLOT = plots,
        COUNTYCD = county,
        YEAR = year,
        STATECD =  state,
        ID_UNIQUE_PLOT=paste("US",STATECD,COUNTYCD,PLOT,sep="_"),
        shrub = list(shrub),
        herbs = list(herbs)
      ) |>
        dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          COUNTYCD,
          YEAR,
          STATECD,
          shrub,
          herbs
        )

      # finally we put together all tables in a data frame and return it
      plot_info |>
        dplyr::mutate(
          tree = list(tree),
          understory = list(understory),
          regen = list(regen),
          subplot = list(subplot),
          soils = list(soils)
        )
    }
  ) |>
    purrr::list_rbind()
}

#' @importFrom dplyr desc
fia_plot_table_process <- function(plot_data, survey_data, cond_data, plot,county, year) {

  ## Debug
  # browser()

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(c(plot_data, survey_data, cond_data) == "")
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping plot info for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ))

    return(dplyr::tibble())
  }

  ## Data gathering
  # survey table
  data_survey <- .read_fia_data(
    survey_data,
    select = c("INVYR","STATECD","STATEAB","STATENM", "RSCD", "ANN_INVENTORY")
  ) |>
    # we arrange by year to lately get the last record
    dplyr::arrange(desc(INVYR)) |>
    #there might be more than 1 record
    dplyr::distinct() |>
    # data.table::as.data.table() |>
    .extract_fia_metadata(c("RSCD", "STATECD","STATEAB", "STATENM"), county, plot, year, .soil_mode = FALSE) |>
    dplyr::mutate(
      PLOT  = plot,
      INVYR  = year,
      COUNTYCD   = county,
    )

  # plot table
  data_plot <- .read_fia_data(
    plot_data,
    select = c("INVYR",
               "STATECD",
               "UNITCD",
               "COUNTYCD",
               "PLOT",
               "LAT",
               "LON",
               "ELEV",
               "PLOT_STATUS_CD",
               "SAMP_METHOD_CD",
               "SUBP_EXAMINE_CD",
               "P3PANEL",
               "DESIGNCD"
    )
  ) |>
    #we arrange by year to catch last record lately
    dplyr::arrange(desc(INVYR)) |>
    dplyr::mutate(
      #elev in feet to meters
      ELEV = ELEV*0.3048,
      ID_UNIQUE_PLOT = paste("US", STATECD, COUNTYCD, PLOT, sep="_"),
      COORD_SYS =  dplyr::if_else(STATECD %in% c(60,64,66,68,69,70), "WGS84", "NAD83")
    ) |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      PLOT,
      INVYR,
      STATECD,
      COUNTYCD,
      P3PANEL,
      ELEV,
      #LON, NAD 83 datum ;   EXCEPTIONS depending on  RSCD:
      LON,
      # LAT, NAD 83 datum    exceptions based on  RSCD
      LAT,
      COORD_SYS,
      # DESIGN : can change between years:
      DESIGNCD
    ) |>
    data.table::as.data.table() |>
    .extract_fia_metadata(c("LAT", "LON", "ELEV","P3PANEL","DESIGNCD","COORD_SYS"), county, plot, year, .soil_mode = FALSE) |>
    dplyr::mutate(
      PLOT  = plot,
      INVYR  = year,
      COUNTYCD   = county,
    )

  # CONDITION TABLE
  data_cond <- .read_fia_data(
    cond_data,
    select = c("INVYR",
               "STATECD",
               "UNITCD",
               "COUNTYCD",
               "PLOT",
               "SLOPE",
               "ASPECT",
               "CONDID",
               "COND_STATUS_CD",
               "SOIL_ROOTING_DEPTH_PNW")
  ) |>
    dplyr::filter(
      #   #FOREST LAND
      #   #Accessible forest land - Land within the population of interest that can be
      #   # occupied safely and has at least 10 percent canopy cover by live tally trees of any
      #   # size or has had at least 10 percent canopy cover of live tally species in the past,
      #   # based on the presence of stumps, snags, or other evidence
      #   #the time of the plot establishment,
      #   #the condition class at plot center (the center of subplot 1) is usually designated as condition class 1.

      #   #condid 1 to use this as proxy
      CONDID == 1

      #   #cond status 1= accesible forest land
      #   # COND_STATUS_CD == 1
    ) |>
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", STATECD, COUNTYCD, PLOT, sep="_")

      # SOIL_ROOTING_DEPTH_PNW = dplyr::case_when(
      #   SOIL_ROOTING_DEPTH_PNW = 1 ~ 10*25.4,
      #   SOIL_ROOTING_DEPTH_PNW = 2 ~ 20*25.4,
      # )
      # SOIL_ROOTING_DEPTH_PNW Code Description 1 < 20 inches (508mm) ; 2 >20 inches
      # Z95 = case_when(SOIL_ROOTING_DEPTH_PNW>0 ~ SOIL_ROOTING_DEPTH_PNW,
      #                 TRUE ~ 0),
      # Z50 = 0.5*Z95

    ) |>
    dplyr::group_by(
      ID_UNIQUE_PLOT,INVYR
    ) |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      PLOT,
      COUNTYCD,
      SLOPE,
      ASPECT,
      INVYR,
      # CONDID
      # Z95,
      #Z50
    ) |>
    dplyr::distinct() |>
    data.table::as.data.table() |>
    .extract_fia_metadata(c("SLOPE", "ASPECT"), county, plot, year, .soil_mode = FALSE) |>
    dplyr::mutate(
      PLOT = plot,
      INVYR = year,
      COUNTYCD = county,
    )

  # we extract the vars we need and return the object
  dplyr::left_join(data_survey, data_plot, by = c("PLOT", "INVYR", "COUNTYCD")) |>
    dplyr::left_join(data_cond, by = c("PLOT", "INVYR", "COUNTYCD")) |>
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", STATECD, COUNTYCD, PLOT, sep="_"),
      COUNTRY = "US") |>
    dplyr::select(
      INVYR,
      ID_UNIQUE_PLOT,
      COUNTRY,
      STATECD,
      STATEAB,
      STATENM,
      COUNTYCD,
      PLOT,
      P3PANEL,
      RSCD,
      DESIGNCD,
      LAT,
      LAT_ORIGINAL,
      LON,
      LON_ORIGINAL,
      COORD_SYS,
      ELEV,
      ELEV_ORIGINAL,
      ASPECT,
      ASPECT_ORIGINAL,
      SLOPE,
      SLOPE_ORIGINAL
    ) |>
    dplyr::rename(
      YEAR = INVYR

    ) |>
    dplyr::as_tibble()
}

fia_tree_table_process <- function(tree_data, plot, county, year, ref_species) {

  ## Debug
  # browser()

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(c(tree_data) == "")
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ))

    return(dplyr::tibble())
  }

  # 2. col names we select the column names to be read
  filtered_data <- .read_fia_data(
    tree_data,
    select = c(
      "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP", "TREE", "STATUSCD", "CONDID",
      "SPCD", "SPGRPCD", "DIA", "DIAHTCD", "HT", "TPA_UNADJ"
    )
  ) |>
    dplyr::filter(
      PLOT == plot,
      INVYR == year,
      COUNTYCD == county
    ) |>
    dplyr::as_tibble()

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no tree data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }


  # 3. join with ref_species??

  tree <- filtered_data |>
    # units transformations
    dplyr::mutate(
      # unique inner code
      ID_UNIQUE_PLOT=paste("US",STATECD,COUNTYCD,PLOT,sep="_"),
      # INCHES TO CM
      DIA = DIA*2.54,
      # FEET TO M
      HT = HT*0.3048,
      # acre to ha
      DENSITY = TPA_UNADJ/0.4046856422,

    ) |>
    # add species info
    dplyr::left_join(
      y = ref_species |>
        dplyr::select(
          SPCD,
          GENUS,
          SPECIES,
          SPECIES_SYMBOL),
      by = "SPCD"
    ) |>
    dplyr::mutate(
      SP_NAME = (paste(GENUS,SPECIES,sep=" "))
    ) |>

    dplyr::arrange(SP_NAME) |>

    dplyr::select(
      ID_UNIQUE_PLOT,
      INVYR,
      STATECD,
      COUNTYCD,
      PLOT,
      TREE,
      STATUSCD,
      DIA,
      HT,
      SP_NAME,
      SPCD,
      DENSITY
    )|>

    dplyr::rename(
      YEAR = INVYR,
      STATUS = STATUSCD,
      SP_CODE = SPCD
    ) |>
    dplyr::as_tibble()

  # Return tree
  return(tree)
}

fia_p3_understory_table_process <- function(understory_data, plot, county, year, growth_habit, ref_plant_dictionary) {

  ## Debug
  # browser()

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(c(understory_data) == "")
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping understory p3 data for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ))

    return(dplyr::tibble())
  }

  # 2. col names

  filtered_data <- .read_fia_data(
    understory_data,
    select = c(
      "INVYR",
      "STATECD",
      "COUNTYCD",
      "PLOT",
      "SUBP",
      "VEG_FLDSPCD",
      # REF_SPECIES TABLE CODE
      "VEG_SPCD",
      "SP_CANOPY_COVER_TOTAL",
      #  0-6 feet 1mfeet- 0,3048m (1.8m)
      "SP_CANOPY_COVER_LAYER_1_2",
      # 6 to 16 feet (4.9 m)
      "SP_CANOPY_COVER_LAYER_3",
      #above 16 feet
      "SP_CANOPY_COVER_LAYER_4"
    )
  ) |>
    dplyr::rename(
      "SPECIES_SYMBOL" = "VEG_SPCD",
    ) |>
    dplyr::filter(
      PLOT == plot,
      INVYR == year,
      COUNTYCD == county
    ) |>
    data.table::as.data.table()

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no p3 understory data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }

  #we add the id code
  understory_filtered_data <- dtplyr::lazy_dt(filtered_data, immutable = TRUE) |>
    dplyr::mutate(ID_UNIQUE_PLOT = paste("US",STATECD,COUNTYCD,PLOT,sep="_")) |>
    #we group by species to calculate means (height, cover)
    dplyr::group_by(SPECIES_SYMBOL) |>


    # here we calculate an averaged height by species, for that we select the height that has
    # the maximum percentage cover and we assign as a height the midle value of the interval of that layer
    # in meters layer 1-2  = 0- 1,8288meters,
    # layer 3 from 1,8288meters to 4,8768
    # and layer 4 more than  4,8768m
    dplyr::mutate(
      COVER_PCT = SP_CANOPY_COVER_TOTAL,
      # MEAN_COV = mean(SP_CANOPY_COVER_TOTAL,na.rm = TRUE),
      HT = dplyr::case_when(
        (which.max(c(max(SP_CANOPY_COVER_LAYER_1_2),max(SP_CANOPY_COVER_LAYER_3), max(SP_CANOPY_COVER_LAYER_4))))==1 ~ 0.91,
        (which.max(c(max(SP_CANOPY_COVER_LAYER_1_2),max(SP_CANOPY_COVER_LAYER_3), max(SP_CANOPY_COVER_LAYER_4))))==2 ~ 3.4,
        # for third layer this is the minimum height not the averaged  ! :)
        (which.max(c(max(SP_CANOPY_COVER_LAYER_1_2),max(SP_CANOPY_COVER_LAYER_3), max(SP_CANOPY_COVER_LAYER_4))))==3 ~ 5
      )
    ) |>
    # 3. ref_plant_dictionary
    #we join data from plant ref dictionary
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
            #we will use this variable to discriminate between functional/form group
            "GROWTH_HABIT",
            "DURATION"
          ))),
      by = "SPECIES_SYMBOL") |>
    dplyr::filter(GROWTH_HABIT %in% growth_habit) |>
    data.table::as.data.table()


  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(understory_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no p3 understory data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }

  #we add latin name and select variables
  understory <- dtplyr::lazy_dt(understory_filtered_data, immutable = TRUE) |>
    dplyr::mutate(
      SP_NAME = paste(GENUS, SPECIES, sep=" ")
    ) |>
    dplyr::arrange(SPECIES_SYMBOL,SUBP)|>
    dplyr::select(
      ID_UNIQUE_PLOT,
      INVYR,
      STATECD,
      COUNTYCD,
      PLOT,
      SUBP,
      # SP_CANOPY_COVER_TOTAL,
      # SP_CANOPY_COVER_LAYER_1_2,
      # SP_CANOPY_COVER_LAYER_3,
      # SP_CANOPY_COVER_LAYER_4,
      # GENUS,
      # FAMILY,
      SPECIES_SYMBOL,
      SP_NAME,
      HT,
      COVER_PCT,
      GROWTH_HABIT
    ) |>
    dplyr::rename(
      YEAR = INVYR,
      SP_CODE = SPECIES_SYMBOL,
      COVER = COVER_PCT
    ) |>
    dplyr::distinct() |>
    dplyr::as_tibble()


  # Return understory
  return(understory)
}

fia_p2_understory_table_process <- function(understory_p2, plot, county, year, growth_habit, ref_plant_dictionary) {

  ## Debug
  # browser()

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(c(understory_p2) == "")
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping understory p3 data for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ))

    return(dplyr::tibble())
  }

  # 2. col names
  filtered_data <- .read_fia_data(
    understory_p2,
    select = c(
      "INVYR",
      "STATECD",
      "COUNTYCD",
      "PLOT",
      "SUBP",
      "CONDID",
      "VEG_FLDSPCD",
      "VEG_SPCD",
      "GROWTH_HABIT_CD",
      #VERTICAL LAYER 1, 2 3 OR 4
      "LAYER",
      "COVER_PCT"
    )
  ) |>
    dplyr::rename(
      "SPECIES_SYMBOL" = "VEG_SPCD",
    ) |>
    dplyr::filter(
      PLOT == plot,
      INVYR == year,
      COUNTYCD == county
    ) |>
    data.table::as.data.table()

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no p3 understory data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }

  #we add the id code
  understory_p2_filtered_data <- dtplyr::lazy_dt(filtered_data, immutable = TRUE) |>
    dplyr::mutate(ID_UNIQUE_PLOT = paste("US",STATECD,COUNTYCD,PLOT,sep="_")) |>
    #we group by species to calculate means (height, cover)
    dplyr::group_by(SPECIES_SYMBOL) |>
    dplyr::filter(
      #this table comes with a categorization of species in growth habits
      # SHRUBS
      #GROWTH_HABIT_CD == "SH",
      #SEDLING AND SAPLINGS
      # GROWTH_HABIT_CD == "SD",
      # #SEDLINGS AND SAPLINGS 2 (SURVEY.RSCD = 26, 27))
      # GROWTH_HABIT_CD == "SP",
      #FORBS
      # GROWTH_HABIT_CD == "FB",
      #GRAMINOIDS
      #GROWTH_HABIT_CD == "GR"

      GROWTH_HABIT_CD %in% growth_habit
    ) |>
    #we calculate mean cover an height from layer
    dplyr::mutate(
      COVER_PCT,
      # MEAN_COV = mean(COVER_PCT, na.rm = TRUE),
      HT = dplyr::case_when(
        LAYER == 1 ~  0.3048,
        LAYER == 2 ~ 1.2192,
        LAYER == 3 ~ 3.3528,
        # for 4TH layer this is the minimum height not the averaged  !
        LAYER == 4 ~ 5
      )
    ) |>
    #we join data from plant ref dictionary
    # one symbol can apply for various species
    dplyr::left_join(
      y = ref_plant_dictionary |>
        dplyr::select(
          dplyr::all_of(c(
            # there are  species with the same code
            "SPECIES_SYMBOL" = "SYMBOL",
            #SPECIES_NAME_author="Scientific Name with Author",
            "FAMILY",
            "GENUS",
            "SPECIES",
            "CATEGORY",
            #we DONT use this variable here because filter is already done
            "GROWTH_HABIT",
            "DURATION"
          ))),
      by = "SPECIES_SYMBOL") |>
    data.table::as.data.table()

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(understory_p2_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no p3 understory data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }

  #we add latin name and select variables
  understory_p2 <- dtplyr::lazy_dt(understory_p2_filtered_data, immutable = TRUE) |>

    #we add latin name
    dplyr::mutate(
      SP_NAME = paste(GENUS, SPECIES, sep=" ")
    ) |>
    dplyr::arrange(SPECIES_SYMBOL,SUBP) |>
    #we select final variables
    dplyr::select(
      ID_UNIQUE_PLOT,
      INVYR,
      STATECD,
      COUNTYCD,
      PLOT,
      SUBP,
      # GENUS,
      # FAMILY,
      SPECIES_SYMBOL,
      SP_NAME,
      GROWTH_HABIT_CD,
      HT,
      COVER_PCT,
      GROWTH_HABIT
    ) |>
    dplyr::rename(
      YEAR=INVYR,
      SP_CODE = SPECIES_SYMBOL,
      COVER = COVER_PCT
    ) |>
    # We have repeated rows after the selection because we summarised shrubs species. We remove with
    # distinct
    dplyr::distinct() |>
    dplyr::as_tibble()


  # Return understory_p2
  return(understory_p2)
}

fia_seedling_table_process <- function(seedling_data, plot, county, year, ref_species) {


  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(c(seedling_data) == "")
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping seedling data for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ))

    return(dplyr::tibble())
  }
  # 2. col names


  filtered_data <- .read_fia_data(
    seedling_data,
    select = c(
      "INVYR",
      "STATECD",
      "COUNTYCD",
      "PLOT",
      "SUBP",
      "CONDID",
      "SPCD",
      "SPGRPCD",
      "TREECOUNT",
      "TREECOUNT_CALC",
      "TPA_UNADJ",
      "TOTAGE"
    )
  ) |>
    dplyr::filter(
      PLOT == plot,
      INVYR == year,
      COUNTYCD == county
    ) |>
    data.table::as.data.table()

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no seedling data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }


  #we add the id code
  seedling <- dtplyr::lazy_dt(filtered_data, immutable = TRUE) |>
    #we filter by species to calculate means (height, cover)
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", STATECD, COUNTYCD, PLOT, sep = "_"),
      #conversion from acre to ha
      TPA_UNADJ = TPA_UNADJ / 0.4046856422) |>
    # join with ref_species
    dplyr::left_join(
      y = ref_species |>
        dplyr::select(
          SPCD,
          GENUS,
          SPECIES,
          SPECIES_SYMBOL
        ),
      by = "SPCD"
    ) |>
    dplyr::mutate(
      SP_NAME = paste(GENUS, SPECIES, sep = " ")
    ) |>
    # we arrange by species and tpa
    dplyr::arrange(SPCD,TPA_UNADJ) |>
    dplyr::mutate(
      #calculate density represented by tree
      DENSITY = TPA_UNADJ * TREECOUNT_CALC
      #ADD COLUMN FOR MEAN HEIGHT
      # HM = NA
    ) |>
    dplyr::arrange(SPCD, SUBP)|>
    #selection of final variables
    dplyr::select(
      ID_UNIQUE_PLOT,
      INVYR,
      STATECD,
      COUNTYCD,
      PLOT,
      SUBP,
      SPCD,
      SP_NAME,
      TREECOUNT_CALC,
      TPA_UNADJ,
      # HM,
      DENSITY
    ) |>
    dplyr::rename(
      YEAR = INVYR,
      SP_CODE = SPCD,
      N = TREECOUNT_CALC
    ) |>
    # # We have repeated rows after the selection because we summarised shrubs species. We remove with
    # distinct
    dplyr::distinct() |>
    dplyr::as_tibble()

  # Return seedlings
  return(seedling)
}

fia_subplot_table_process <- function(subplot_data, plot, county, year) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(c(subplot_data) == "")
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping subplot data for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ))

    return(dplyr::tibble())
  }
  # 2. col names

  filtered_data <- .read_fia_data(
    subplot_data,
    select = c(
      "INVYR",
      "STATECD",
      "COUNTYCD",
      "PLOT",
      "SUBP",
      "SUBP_STATUS_CD",
      "MACRCOND",
      "SUBPCOND",
      "MICRCOND",
      "SLOPE",
      "ASPECT",
      "P2VEG_SUBP_STATUS_CD"
    )
  ) |>
    dplyr::filter(
      PLOT == plot,
      INVYR == year,
      COUNTYCD == county
    ) |>
    dplyr::as_tibble()

  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no subplot data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }

  #we add the id code
  subplot <- filtered_data |>
    #we add id code
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US",STATECD,COUNTYCD,PLOT,sep="_"),
    ) |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      INVYR,
      STATECD,
      COUNTYCD,
      PLOT,
      SUBP,
      SLOPE,
      ASPECT,
      MACRCOND,
      #Condition number for the condition at the center of the subplot.
      SUBPCOND,
      MICRCOND
    ) |>
    dplyr::rename(YEAR = INVYR) |>
    # We have repeated rows after the selection because we summarised shrubs species. We remove with
    # distinct
    dplyr::distinct() |>
    dplyr::as_tibble()

  # Return shrub
  return(subplot)
}

fia_soils_lab_table_process <- function(soils_lab, plot, county, state, year) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(c(soils_lab) == "")
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping plot info for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ))

    return(dplyr::tibble())
  }

  #soils lab
  data_soils_lab <- .read_fia_data(
    soils_lab,
    select = c(
      "INVYR",
      "STATECD",
      "COUNTYCD",
      "PLOT",
      #SMPLNNBR is equals to subplot number (SUBP)
      "SMPLNNBR",
      "LAYER_TYPE",
      "SAMPLE_DATE",
      #in percent
      "FIELD_MOIST_WATER_CONTENT_PCT",
      "RESIDUAL_WATER_CONTENT_PCT",
      "TOTAL_WATER_CONTENT_PCT",
      "BULK_DENSITY",
      "COARSE_FRACTION_PCT",
      "C_ORG_PCT"
    )
  ) |>
    data.table::as.data.table()

  #we check if we are getting data (= file is not empty) before continuing

  if (nrow(data_soils_lab) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no soil lab data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }

  #we modify object and add variables
  soils <- dtplyr::lazy_dt(data_soils_lab, immutable = TRUE) |>
    # we group by plot, year and layer
    dplyr::group_by(
      STATECD, COUNTYCD,
      PLOT,
      INVYR,
      LAYER_TYPE
    ) |>
    # We average data by plot (collected at subplot level) and by LAYER
    dplyr::summarise(
      ID_UNIQUE_PLOT = unique(paste("US",STATECD,COUNTYCD,PLOT,sep="_")),
      # #subplot
      # SUBP = SMPLNNBR,
      BULK_DENSITY_MEAN = round(mean(BULK_DENSITY, na.rm = TRUE),3),
      C_ORG_PCT_MEAN = mean(C_ORG_PCT, na.rm = TRUE),
      COARSE_FRACTION_PCT_MEAN = mean(COARSE_FRACTION_PCT,na.rm = TRUE),
      FIELD_MOIST_WATER_CONTENT_PCT_MEAN = mean(FIELD_MOIST_WATER_CONTENT_PCT,na.rm = TRUE),
      RESIDUAL_WATER_CONTENT_PCT_MEAN = mean(RESIDUAL_WATER_CONTENT_PCT, na.rm = TRUE),
      TOTAL_WATER_CONTENT_PCT_MEAN = mean(TOTAL_WATER_CONTENT_PCT, na.rm = TRUE)
    ) |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      PLOT,
      COUNTYCD,
      STATECD,
      INVYR,
      LAYER_TYPE,
      FIELD_MOIST_WATER_CONTENT_PCT_MEAN,
      TOTAL_WATER_CONTENT_PCT_MEAN,
      RESIDUAL_WATER_CONTENT_PCT_MEAN,
      BULK_DENSITY_MEAN,
      COARSE_FRACTION_PCT_MEAN,
      C_ORG_PCT_MEAN
    ) |>
    # we arange by year
    # dplyr::arrange(desc(INVYR)) |>
    # we oly want one value per plot/ layer
    dplyr::distinct() |>
    data.table::as.data.table() |>
    .extract_fia_metadata(
      # data_soils_lab,
      c("LAYER_TYPE",
        "FIELD_MOIST_WATER_CONTENT_PCT_MEAN",
        "RESIDUAL_WATER_CONTENT_PCT_MEAN",
        "TOTAL_WATER_CONTENT_PCT_MEAN",
        "BULK_DENSITY_MEAN",
        "COARSE_FRACTION_PCT_MEAN",
        "C_ORG_PCT_MEAN",
        "STATECD"
      ), county, plot, year
    ) |>
    #we add variables
    dplyr::mutate(
      PLOT = plot,
      INVYR = year,
      COUNTYCD = county,
      STATECD = state,
      ID_UNIQUE_PLOT = paste("US", STATECD, COUNTYCD, PLOT, sep="_")
    ) |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      PLOT,
      COUNTYCD,
      STATECD,
      INVYR,
      #SUBP,
      LAYER_TYPE,
      FIELD_MOIST_WATER_CONTENT_PCT_MEAN,
      TOTAL_WATER_CONTENT_PCT_MEAN,
      RESIDUAL_WATER_CONTENT_PCT_MEAN,
      BULK_DENSITY_MEAN,
      COARSE_FRACTION_PCT_MEAN,
      C_ORG_PCT_MEAN
    ) |>
    dplyr::rename(
      YEAR = INVYR
    ) |>
    dplyr::as_tibble()

  #warning

  if (all(is.na(soils[["LAYER_TYPE"]]))) {
    cli::cli_warn(c(
      "There is no soil lab data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))

    soils <- dplyr::tibble()
  }

  return(soils)
}

fia_soils_loc_table_process <- function(soils_loc, veg_subplot, plot, county, state, year) {

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(c(soils_loc, veg_subplot) == "")
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping plot info for plot {.var {plot}} at county {.var {county}} for {.var {year}}"
    ))

    return(dplyr::tibble())
  }


  #veg_subplot
  data_veg_subplot <- .read_fia_data(
    veg_subplot,
    select = c(
      "INVYR",
      "STATECD",
      "COUNTYCD",
      "PLOT",
      "SUBP",
      # "VEG_SUBP_STATUS_CD",
      "ROCK_COVER_PCT"
    )) |>
    data.table::as.data.table()

  #we check it contains data
  if (nrow(data_veg_subplot) < 1) {
    cli::cli_warn(c(
      "There is no soil loc data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }

  data_veg_subplot <- dtplyr::lazy_dt(data_veg_subplot, immutable = TRUE) |>
    #we group by plot and year to obtain mean for rock cover
    dplyr::group_by(
      STATECD, COUNTYCD,
      PLOT,
      INVYR
    ) |>
    dplyr::summarise(
      ID_UNIQUE_PLOT = unique(paste("US", STATECD, COUNTYCD, PLOT, sep = "_")),
      ROCK_COVER_PCT_MEAN = round(mean(ROCK_COVER_PCT, na.rm = TRUE), 3)
    ) |>
    dplyr::select(
      PLOT,
      COUNTYCD,
      STATECD,
      INVYR,
      # SUBP,
      # VEG_SUBP_STATUS_CD,
      ROCK_COVER_PCT_MEAN
    ) |>
    dplyr::arrange(desc(INVYR))|>
    dplyr::distinct() |>
    data.table::as.data.table() |>
    .extract_fia_metadata(
      c(
        # "VEG_SUBP_STATUS_CD",
        "ROCK_COVER_PCT_MEAN"
      ), county, plot, year
    ) |>
    dplyr::mutate(
      PLOT  = plot,
      INVYR  = year,
      COUNTYCD = county,
      STATECD = state
    )

  #soils_loc
  data_soils_loc <- .read_fia_data(
    soils_loc,
    select = c(
      "INVYR",
      "STATECD",
      "COUNTYCD",
      "PLOT",
      # "SMPLNNBR",
      #check if condid is needed
      "CONDID",
      "FORFLTHK",
      "LTRLRTHK",
      "TXTRLYR1",
      "TXTRLYR2",
      #DEPTH TO A RESTRICTIVE LAYER
      "DPTHSBSL"
    )) |>
    data.table::as.data.table()

  if (nrow(data_soils_loc) < 1) {
    cli::cli_warn(c(
      "There is no soil loc data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    return(dplyr::tibble())
  }

  data_soils_loc <- dtplyr::lazy_dt(data_soils_loc, immutable = TRUE) |>
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US",STATECD,COUNTYCD,PLOT,sep="_"),
      # SUBP = SMPLNNBR,
      #inches to cm
      DPTHSBSL = 2.54*DPTHSBSL,
      LTRLRTHK = 2.54*LTRLRTHK,
      FORFLTHK = 2.54*FORFLTHK,
      # for tidyr::fill to work when all values are NAs
      TXTRLYR1 = as.numeric(TXTRLYR1),
      TXTRLYR2 = as.numeric(TXTRLYR2)
    ) |>
    dplyr::group_by(
      PLOT, INVYR
    ) |>
    #we fill all data of same plot and year with same value
    tidyr::fill(TXTRLYR1, TXTRLYR2, .direction = "downup") |>
    #we generate means
    dplyr::mutate(
      text1_temp = dplyr::case_when(
        TXTRLYR1 == 0 ~ "organic",
        TXTRLYR1 == 1 ~ "loamy",
        TXTRLYR1 == 2 ~ "clayey",
        TXTRLYR1 == 3 ~ "sandy",
        TXTRLYR1 == 4 ~ "coarse_sand",
        TXTRLYR1 == 9 ~ "NA"
      ),
      text2_temp = dplyr::case_when(
        TXTRLYR2 == 0 ~ "organic",
        TXTRLYR2 == 1 ~ "loamy",
        TXTRLYR2 == 2 ~ "clayey",
        TXTRLYR2 == 3 ~ "sandy",
        TXTRLYR2 == 4 ~ "coarse_sand",
        TXTRLYR1 == 9 ~ "NA"
      ),
      DPTHSBSL_MEAN = mean(DPTHSBSL,na.rm = TRUE),
      LTRLRTHK_MEAN = mean(LTRLRTHK,na.rm = TRUE),
      FORFLTHK_MEAN = mean(FORFLTHK,na.rm = TRUE)
    ) |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      INVYR,
      STATECD,
      COUNTYCD,
      PLOT,
      # SUBP,
      #check if condid is needed
      # CONDID,
      FORFLTHK_MEAN,
      # FORFLTHK,
      LTRLRTHK_MEAN,
      # LTRLRTHK,
      # TXTRLYR1,
      # TXTRLYR2,
      text1_temp,
      text2_temp,
      #DEPTH TO A RESTRICTIVE LAYER
      DPTHSBSL_MEAN,
      # DPTHSBSL
    ) |>
    dplyr::rename(
      TXTRLYR1 = text1_temp,
      TXTRLYR2 = text2_temp,
    ) |>
    dplyr::arrange(desc(INVYR)
    ) |>
    dplyr::distinct() |>
    data.table::as.data.table() |>
    .extract_fia_metadata(c(
      "FORFLTHK_MEAN",
      "LTRLRTHK_MEAN",
      "TXTRLYR1",
      "TXTRLYR2",
      #DEPTH TO A RESTRICTIVE LAYER
      "DPTHSBSL_MEAN",
      "STATECD"
      ), county, plot, year
    ) |>
    dplyr::mutate(
      PLOT  = plot,
      INVYR  = year,
      COUNTYCD = county,
    )

  # combining data
  soils_loc_combined <- dplyr::left_join(
    data_soils_loc, data_veg_subplot, by = c("PLOT", "INVYR", "COUNTYCD", "STATECD")
  ) |>
    # we add id cariable
    dplyr::mutate(
      ID_UNIQUE_PLOT = paste("US", STATECD, COUNTYCD, PLOT, sep="_")
    ) |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      INVYR,
      STATECD,
      COUNTYCD,
      PLOT,
      # SUBP,
      # SUBP_ORIGINAL,
      FORFLTHK_MEAN,
      FORFLTHK_MEAN_ORIGINAL,
      LTRLRTHK_MEAN,
      LTRLRTHK_MEAN_ORIGINAL,
      TXTRLYR1,
      TXTRLYR1_ORIGINAL,
      TXTRLYR2,
      TXTRLYR2_ORIGINAL,
      DPTHSBSL_MEAN,
      DPTHSBSL_MEAN_ORIGINAL,
      ROCK_COVER_PCT_MEAN,
      ROCK_COVER_PCT_MEAN_ORIGINAL
    ) |>
    dplyr::as_tibble()

  #we check that we have data

  if (all(is.na(c(soils_loc_combined[["TXTRLYR1"]], soils_loc_combined[["TXTRLYR2"]], soils_loc_combined[["DPTHSBSL_MEAN"]])))) {
    cli::cli_warn(c(
      "There is no soil loc data for that combination of plot, county and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} at county {.var {county}}"
    ))
    soils_loc_combined<-dplyr::tibble()
  }

  return(soils_loc_combined)
}
