#' Raw FFI data to tibble
#'
#' Transform raw FFI plot data into tidy data for use in models
#'
#' This function will take every year indicated and retrieve and transform the plot data for the departments and
#' plots provided. For that, csv files from FFI must reside in the folder indicated in the
#' \code{folder} argument.
#'
#' @param years A numeric vector with the years to extract de data from.
#' @param department A character vector with the two letters code for the departments to extract the data from.
#' @param filter_list A list of counties and plots to extract the data from. If \code{NULL} all
#'   plots for the department for all years will be extracted, which can use a big amount of memory. See
#'   details.
#' @param folder The path to the folder containing the FFI csv files, as character.
#' @param ... Not used at the moment
#' @param .parallel_options An object of class \code{furrr_options}. See
#'   \code{\link[furrr]{furrr_options}}.
#' @param .verbose Logical controlling if progress messages are shown.
#'
#' @section Filter list:
#'   If no \code{filter_list} argument is provided, \code{ffi_to_tibble} will attempt to process all
#'   plots for the departments and years provided. This will result in sometimes hundred of thousands
#'   plots to be extracted, processed and returned, will in turn will cause a big use of memory and
#'   long times of calculation (specially when parallelising). Is better to provide a list of departments
#'   with the counties and plots to look after to narrow the process. This \code{filter_list} should
#'   have the following structure:
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
#'   Processing the plots from within a year can be done in parallel (\code{esus} uses internally the
#'   \code{\link[furrr]{furrr}} package for this). This means that, if parallelization is active,
#'   several processes are launched to retrieve the plots data for that year. This is repeated for
#'   all years provided.
#'
#'   \code{.parallel_options} controls the finer details of how parallelization is performed (see
#'   \code{\link[furrr]{furrr_options}}). But no parallelization can occur without setting first
#'   a \code{\link[future]{plan}}. By default, the chosen plan is \code{\link[future]{sequential}},
#'   so no parellization is done. Changing the plan, i.e. to \code{\link[future]{multisession}} or
#'   to \code{\link[future.callr]{callr}}, will allow \code{ffi_to_tibble} to use parallelization
#'   when retrieving the data.
#'
#' @return A nested tibble. This tibble contains a row per plot/year combination, with the plot
#'   metadata included, as well as columns containing tibbles with tree, shrub, herbs and soil
#'   information. See \code{vignette("inventory_data_tibble", pkg = "esus")}
#'
#' @export
ffi_to_tibble <- function(
    years,
    deps,
    filter_list = NULL,
    folder,
    ...,
    .parallel_options = furrr::furrr_options(scheduling = 1L, stdout = TRUE),
    .verbose = TRUE
) {
  
  ## Assertions and checks ##
  # departments
  assertthat::assert_that(
    is.character(deps), length(deps) > 0,
    msg = cli::cli_abort("years must be a character vector with at least one department code")
  )
  ## TODO
  # check all departments are valid
  
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
  ## TODO
  # Check counties and plots??
  
  ## TODO
  # Check ancillary data is present!!
  
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
  # espar cdref
  assertthat::assert_that(
    fs::file_exists(fs::path(folder, "espar_cdref13.xlsx")),
    msg = cli::cli_abort("{.file espar_cdref13.xlsx} must be present at {.path {folder}} to be able to continue")
  )
  # def metadonnes --> this file is edited from source!!
  assertthat::assert_that(
    fs::file_exists(fs::path(folder, "metadonnees.xlsx")),
    msg = cli::cli_abort("{.file metadonnees.xlsx} file must be present at {.path {folder}} to be able to continue")
  )
  

  
  #  growth_form_species_france (try db)
  assertthat::assert_that(
    fs::file_exists(fs::path(folder, "growth_form_species_france.RData")),
    msg = cli::cli_abort("{.file growth_form_species_france.RData} file must be present at {.path {folder}} to be able to continue")
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
      ffi_tables_process(year, deps, filter_list, folder, .parallel_options, .verbose, ...)
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
#' @describeIn ffi_to_tibble Process one year
#'
ffi_tables_process <- function(
    year, filter_list, folder, .parallel_options, .verbose, ...
) {
  
  # debug
 
    # browser()

  
  # Create input df for year
  
  input_df <- .build_ffi_input_with(year,  filter_list, folder, .verbose)
  
  # Get needed ancillary data (changed for excel)
  

  espar_cdref13 <- .read_ffi_data(fs::path(folder, "espar-cdref13.CSV")) |>
    dplyr::as_tibble() |>
    dplyr::rename(
      ESPAR = "// espar",
      Libellé  = lib_espar                        
    ) |>
    dplyr::arrange(ESPAR)
  
  metadonnees <- readr::read_delim(file = fs::path(folder, "metadonnees.csv"), skip = 412) |>
    dplyr::as_tibble() |>
    dplyr::rename(
      UNITE = "// Unité"
    )
  
  cd_ref <-  metadonnees |>
    dplyr::filter(
      UNITE == "CDREF13"
          ) |>
    dplyr::mutate(
      lib_cdref =  stringr::str_remove_all(Libellé, "\\s*\\(.*?\\)")
      ) |>

   dplyr::rename(
     cd_ref = Code  
    )
    
    
  


  idp_dep_ref <- readr::read_delim(
                          paste0(folder, "PLACETTE.csv"), 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE
                          ) |>
    tibble::tibble() |>
    dplyr::select(
      IDP,
      DEP
    ) |>

    unique()
  
  
  #revise  path!!!!
 load(paste0(folder,"/growth_form_lignified_france.RData"))
  
  
  # furrr::future_pmap(

     purrr::pmap(

    .progress = .verbose,
    .l = input_df,
      
    .f = \(dep,
           plots,
           tree_table,
           plot_table,
           shrub_table,
           soils_table
    ) {
      
      # browser()
      
      plot_info <- ffi_plot_table_process(plot_table, soils_table, plots, year, metadonnees)
      
      
      
      tree <- ffi_tree_table_process(tree_table, plots, year,espar_cdref13, idp_dep_ref)
      
       shrub <- ffi_shrub_table_process(shrub_table, plots, year, cd_ref, growth_form_lignified_france, idp_dep_ref)

       soil <- ffi_soil_table_process(soils_table, plots, year, metadonnees, idp_dep_ref)
      # 
      
      #we select herbs
      herbs <- plot_info |>
        dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          DEP,
          VISITE,
          YEAR,
          HERB
        )
      # 
      # 
      # #we create understory with herbs and shrub

      understory <- plot_info|>

        dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          DEP,
          VISITE,
          LIGN1,
          LIGN2,
          YEAR
        ) |>

        dplyr::mutate(
          shrub = list(shrub),
          herbs = list(herbs)
        )
      # 
      # 
      
      # we put together all tables in a data frame
      

      plot_info <- plot_info |>

        dplyr::mutate(
          crs = 2154,
          tree = list(tree),
           understory = list(understory),
           soil = list(soil)

          ) |>

        dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          DEP,
          DEP_NAME,
          COUNTRY,
          VISITE,
          YEAR,
          XL,
          XL_ORIGINAL,
          YL,
          YL_ORIGINAL,
          crs,
          EXPO,
          EXPO_ORIGINAL,
          PENT2,
          PENT2_ORIGINAL,
          COORD_SYS,
          tree,
          understory,
          soil
        ) |>
        
        # #HARMONIZATION OF NAMES
        
        dplyr::rename(
          ASPECT = EXPO,
          ASPECT_ORIGINAL = EXPO_ORIGINAL,
          SLOPE = PENT2,
          SLOPE_ORIGINAL = PENT2_ORIGINAL,
          soils = soil
        )
      
    }

   ) |>

     purrr::list_rbind()
}



#' Data tables process
#'
#' Process to gather needed data from FFI csv tables
#'
#' These functions retrieve the data for a plot in one year.
#'
#' @param plot_data,tree_data,shrub_data,soil_data Paths to the files with the corresponding data
#' @param plot Numeric, plot code
#' @param dep department code
#' @param year Numeric, year to extract
#' @param espar_cdref13,metadonnees,growth_form_lignified_france tables. These tables
#'   are automatically read in \code{\link{fia_tables_process}} based on the folder provided.
#' @param growth_habit Character, growth habit value to filter data (to distinguish between herbs
#'   and shrubs)
#'
#' @return A tibble with one or more rows (depending on the data retrieved) for each plot for that
#'   year.
#'
#' @importFrom dplyr desc
#' @name tables_processing
NULL

#' @describeIn tables_processing Process to gather needed data from plot, survey and cond tables
ffi_plot_table_process <- function(plot_data, soil_data, plot, year, metadonnees) {
  
  ## Debug
   # browser()
  
  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(plot_data, soil_data)))
    # !any(c(plot_data, survey_data) == NA_character_)
  )
  
  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping plot info for plot {.var {plot}}  for {.var {year}}"
    ))
    
    return(dplyr::tibble())
  }
 
  
  plot_processed <- .read_ffi_data(
    plot_data,
    select = c(
    "CAMPAGNE",
    "VISITE",
    "IDP",
    "XL",
    "YL",
    "DEP"
    # #CSA : Couverture du sol
    # "CSA",
    # #TCAT10 : Taux de couvert total des arbres
    #   "TCAT10",
    #   #UTA1 et UTA2 : Première et deuxième utilisation du sol
    #   "UTA1",
    #   "UTA2"
    )
  ) |>
    #join with metadonnes
    dplyr::left_join(
      y = metadonnees |>
        dplyr::filter(
          UNITE == "DP"
        ) |>

        dplyr::select(
          DEP = Code,
          DEP_NAME = Libellé),
      
      by = "DEP"
    ) |>
    
    #transformations 
    dplyr::mutate(
      ID_UNIQUE_PLOT = (paste("FR", DEP, IDP, sep = "_")),
      COORD_SYS = "LAMBERT"
    ) |>

    dplyr::select(
      ID_UNIQUE_PLOT,
      IDP,
      DEP,
      DEP_NAME,
      VISITE,
      CAMPAGNE,
      XL,
      YL,
      COORD_SYS,
    ) |>
    dplyr::arrange(desc(CAMPAGNE)) |>

    dplyr::rename(
      PLOT = IDP,
      YEAR = CAMPAGNE
      

    ) |> data.table::as.data.table() |>
    .extract_ffi_metadata(
      c("ID_UNIQUE_PLOT","PLOT","DEP","DEP_NAME","VISITE","COORD_SYS","XL","YL"),
      plot, 
      year, 
      .soil_mode = FALSE) |>
    dplyr::mutate(
      PLOT  = plot,
      YEAR  = year
    ) |> dplyr::as_tibble()

  
    
    
  
  
  eco_filtered_data <- .read_ffi_data(
    soil_data,
    select = c(

      "CAMPAGNE",
      "IDP",
      "EXPO",
      "PENT2",
      "LIGN1",
      "LIGN2",
      "HERB"
    )
  ) |>

    
    
    #transformations 
    dplyr::rename(
      PLOT = IDP,
      YEAR = CAMPAGNE) |>
    dplyr::mutate(
      #CONVERSION TO SEXAGESIMAL
      EXPO = 0.9 * EXPO) |>
    dplyr::select(
      PLOT,
      YEAR,
      EXPO,
      PENT2, 
      LIGN1,
      LIGN2,
      HERB
    ) |>
    dplyr::arrange(desc(YEAR)) |>
    data.table::as.data.table() |>
    .extract_ffi_metadata(
      c("EXPO", "PENT2", "LIGN1", "LIGN2", "HERB"),
      plot,
      year,
      .soil_mode = FALSE
    ) |>
    dplyr::mutate(
      PLOT  = plot,
      YEAR  = year,
      

    ) |>

    dplyr::as_tibble()
  

  plot_info <- dplyr::left_join( 

    # dplyr::bind_cols(
    plot_processed,
    eco_filtered_data,
    by = c("PLOT")

  ) |>
    dplyr::mutate(
      PLOT  = plot,
      YEAR  = year
    ) |>
    dplyr::mutate(
      ID_UNIQUE_PLOT = (paste("FR", DEP, PLOT, sep = "_")),
      COUNTRY = "FR") |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      COUNTRY,
      DEP,
      DEP_NAME,
      PLOT,
      YEAR,
      VISITE,
      COORD_SYS,
      XL,
      XL_ORIGINAL,
      YL,
      YL_ORIGINAL,
      EXPO,
      EXPO_ORIGINAL,
      PENT2, 
      PENT2_ORIGINAL,
      LIGN1,
      LIGN2,
      HERB

   ) |>
    dplyr::as_tibble()
  
  
  return(plot_info)
  

}



#' @describeIn tables_processing Process to gather needed data from tree table

ffi_tree_table_process <- function(tree_data, plot,  year, espar_cdref13, idp_dep_ref) {

  
  ## Debug
  # browser()
  
  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(tree_data)))
    # !any(c(tree_data) == NA_character_)
  )
  
  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}}  for {.var {year}}"
    ))
    
    return(dplyr::tibble())
  }
  
  
  tree_filtered_data <- .read_ffi_data(
    tree_data,
    select = c(
  
      "CAMPAGNE",
      "IDP",
      "A",
      "W",
      "ESPAR",
      "VEGET", 
      "VEGET5",
      "C13",
      "HTOT",
      "IR5",
      "TIGE",
      "TETARD",
      "QUALITE",
      "CIBLE"

    )) |>

    
    # we  filter the data for plot/year and status (alive)
    
    dplyr::filter(
      IDP == plot,
      CAMPAGNE == year
      
    ) |>

    dplyr::as_tibble()
  
  
  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(tree_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "There is no tree data for that combination of plot and year",
      "i" = "Returning empty tibble for plot {.var {plot}} in year {.var {year}} "
    ))
    return(dplyr::tibble())
  }
  
  

  tree_plot_data <- tree_filtered_data |>

    
    # we  filter the data for plot/
    dplyr::filter(
      IDP == plot)
  
  # transformations and filters
  

  tree <- tree_plot_data |>

    
    dplyr::mutate(
      DENSITY = W,
      C13 = as.numeric(C13),
      
      #transformation to diameter
      DIA = (C13/pi)*100,
      YEAR = CAMPAGNE,
      #ID_UNIQUE_PLOT= (paste("FR", IDP, sep="_"))
      
=
    ) |>
    
    #join with espar_cdref
    dplyr::left_join(
      y = espar_cdref13 |>
        dplyr::select(
          ESPAR,
          cd_ref,
          lib_cdref
        ),
      by = "ESPAR"
    ) |>
    
    # #join with espar
    # dplyr::left_join(
    #   y = espar_ref|>
    #     dplyr::select(
    #       ESPAR,
    #       LIBELLE
    #     ),
    #   by = "ESPAR"
    # ) |>
    
    dplyr::left_join(
      y = idp_dep_ref,
      by = "IDP"

    ) |>
    
    dplyr::mutate(
      ID_UNIQUE_PLOT = (paste("FR",DEP,IDP, sep = "_"))
    ) |>

    
    dplyr::rename(
      PLOT = IDP,
     # FR_SP_NAME = LIBELLE,
      SP_NAME = lib_cdref, 
      SP_CODE = cd_ref,
      TREE = A, 
      HT = HTOT,
      STATUS = VEGET

    ) |>

    
    #selection of final variables 
    
    dplyr::select( 
      ID_UNIQUE_PLOT, 
      PLOT,
      DEP,
      YEAR,
      TREE,
      ESPAR,
      #FR_SP_NAME,
      SP_CODE,
      SP_NAME,
      STATUS,
      VEGET5,
      #LIB, 
      DIA,
      HT,
      DENSITY,
      # IR5,
      # TIGE,
      # TETARD,
      # QUALITE,
      # CIBLE
      
    ) |>

    
    
    #homogeneization
    
    
    
    #añadir condiciones en funcion de si es na o no ??
    dplyr::group_by(ID_UNIQUE_PLOT) |>

    dplyr::arrange(ID_UNIQUE_PLOT,TREE) |>
    tidyr::fill(c(ESPAR,SP_CODE,SP_NAME)) |>
    
    # we  filter the data for year
    dplyr::filter(
      YEAR == year) |>
    dplyr::as_tibble()
  
  
  return(tree)
  
}


#' @describeIn tables_processing 
#' Process to gather needed data from xxxxx

ffi_shrub_table_process <- function(shrub_data, plot, year,cd_ref, growth_form_lignified_france, idp_dep_ref) {

  
  # Debug
  # browser()
  
  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(shrub_data))
    # !any(c(understory_data) == NA_character_)
  )
  
  
  
  # 2. col names
  
  shrub_filtered_data <- .read_ffi_data(
    shrub_data,
    select = c(
      "CAMPAGNE",
      "IDP",
      "CD_REF",
      "ABOND"
    )

  ) |>

    
  
  # we  filtering the data for plot/year and status (alive)
  
  dplyr::filter(
    IDP == plot,
    CAMPAGNE == year
    
  ) |>
    dplyr::as_tibble()
  
  
  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  
  if (nrow(shrub_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "File {.file {shrub_data}} has no data for that combination of plot, dep and year", 
      "i" = "Returning NULL for plot {.var {plot}} in year {.var {year}} "
    ))
    return(dplyr::tibble())
  }
  
  
  # transformations and filters
  

  shrub <- shrub_filtered_data |>
    dplyr::mutate(
      YEAR = CAMPAGNE,
      # ID_UNIQUE_PLOT= (paste("FR", IDP, sep="_")),
      cd_ref = as.character(CD_REF),
      #conversion to percentage
      ABOND = dplyr::case_when(
        # présence faible	Taux de recouvrement de l'espèce inférieur à 5 % et présence faible.				
        ABOND == 1 ~ 5,
        #présence nette	Taux de recouvrement de l'espèce inférieur à 25 % mais présence nette.		
        ABOND == 	2	~ 12.5,
        #Taux de recouvrement de l'espèce compris entre 25 et 50 %  		
        ABOND	== 3 ~	37.5, 
        #Taux de recouvrement de l'espèce compris entre 25 et 50 %  
        #Taux de recouvrement de l'espèce compris entre 50% et 75 %.
        ABOND	== 4 ~	62.5,
        #	Taux de recouvrement de l'espèce supérieur à 75%.	
        ABOND	== 5 ~	87.5			

      )
      ) |>
    
    dplyr::left_join(
      y = cd_ref |>
        dplyr::select(
          cd_ref,
          lib_cdref
        ), 
      by = "cd_ref"
    ) |>
    
    dplyr::left_join(
      y = idp_dep_ref,
      by = "IDP"
    ) |>
    
    dplyr::mutate(
      ID_UNIQUE_PLOT = (paste("FR", DEP, IDP, sep = "_"))
      # Hm = NA
    ) |>
    dplyr::rename(
      PLOT = IDP,
      SP_NAME = lib_cdref,
      SP_CODE = cd_ref,
      COVER = ABOND
    ) |>
    
    # dplyr::rename(
    #   full_name = SP_NAME
    # )|>
    # dplyr::mutate(genus = stringr::str_extract(full_name, "\\b\\w+\\b"),
    #               species =  stringr::str_extract(full_name, "(?<=\\s)\\w+"),
    #               species = dplyr::if_else(stringr::str_detect(species, "^\\("), "", species),
    #               SP_NAME = ifelse(is.na(species) | species == "", genus, paste(genus, species, sep = " ")))|>
    
    #selection of final variables 
    
    dplyr::select( 
      ID_UNIQUE_PLOT, 
      PLOT,
      DEP,
      YEAR,
      SP_CODE,
      SP_NAME,
      COVER,
      #Hm

    ) |>
    dplyr::as_tibble()
  
  #this is new
  #to eliminate herbs i do a join with a database from try
  
  growth_form_lignified_france <- growth_form_lignified_france |>
    dplyr::select(
      AccSpeciesName,  
      GrowthForm   
    ) |>
    dplyr::mutate(SP_NAME = AccSpeciesName)
  
  shrub_no_herbs <- shrub |>
    dplyr::left_join(
      y = growth_form_lignified_france,
      by = "SP_NAME"
    ) |>
    dplyr::filter(
      (grepl("tree|shrub", GrowthForm))) |>
    dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          DEP,    
          YEAR, 
          SP_CODE, 
          SP_NAME,
          COVER,
          GrowthForm
        ) |>
    dplyr::as_tibble()
  
  
  return(shrub_no_herbs)
  
  
  
}


ffi_soil_table_process <- function(soil_data, plot, year, metadonnees,idp_dep_ref){

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(soil_data)))
    # !any(c(soil_data) == NA_character_)
  )
  
  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping plot info for plot {.var {plot}}  for {.var {year}}"
    ))
    
    return(dplyr::tibble())
  } 
  
    soil_filtered_data <- .read_ffi_data(
    soil_data,
    select = c(
    "CAMPAGNE",
    "IDP",
    "DATEECO",
    "TSOL",
    # in dm
    #horizon superieur
    "PROF1",
    #horizon inferieur
    "PROF2",
    "TEXT1", 
    "TEXT2",
    "ROCHE",
    
    # dixiemes 
    "AFFROC",
    "CAI40",
    "CAILLOUX",
    "AFPLA"
    

  )) |>
    
    # we  filter the data for plot/year 
    
    dplyr::filter(
      IDP == plot,
      CAMPAGNE == year
      
    ) |>
    dplyr::as_tibble()
  
  ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  
  if (nrow(soil_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "File {.file {soil_data}} has no data for that combination of plot, dep and year", 
      "i" = "Returning NULL for plot {.var {plot}} in year {.var {year}} "
    ))
    return(dplyr::tibble())
  }
  
  

  soil_meta <- metadonnees |>
    dplyr::filter(
      UNITE  %in% c("TSOL", "TEXT1", "ROCHED0"))
  
  
  

  soil <- soil_filtered_data |>
    dplyr::mutate(
      YEAR = CAMPAGNE,
      TEXT1 = as.character(TEXT1),
      ROCHE = as.character(ROCHE),
      TSOL = as.character(TSOL),
      #dm to cm
      PROF1 = PROF1*10,
      PROF2 = PROF2*10

    ) |>
    
    dplyr::left_join(
      soil_meta |>
        dplyr::filter(UNITE == "TEXT1") |>
        dplyr::rename(TEXT1 = Code,
                      TEXT1_DES = Libellé) |>
        dplyr::select(TEXT1,TEXT1_DES),
      by = "TEXT1") |>
    
    dplyr::left_join(
      soil_meta |>
        dplyr::filter(UNITE == "TEXT1") |>
        dplyr::rename(TEXT2 = Code,
                      TEXT2_DES = Libellé) |>
        dplyr::select(TEXT2,TEXT2_DES),
      
      by = "TEXT2") |>
    
    dplyr::left_join(

      soil_meta |>
        dplyr::filter(UNITE == "ROCHED0") |>
        dplyr::rename(ROCHE = Code,
                      ROCHE_DES = Libellé) |>
        dplyr::select(ROCHE,ROCHE_DES),
      
      by = "ROCHE") |>
    
    dplyr::left_join(
      soil_meta |>
        dplyr::filter(UNITE == "TSOL") |>
        dplyr::rename(TSOL = Code,
                      TSOL_DES = Libellé) |>
        dplyr::select(TSOL,TSOL_DES),
      
      by = "TSOL") |>
    
    dplyr::left_join(
      y = idp_dep_ref,
      by = "IDP"
    ) |>
    
    dplyr::mutate(
      ID_UNIQUE_PLOT = (paste("FR", DEP, IDP, sep = "_"))) |>

    dplyr::rename(
      PLOT = IDP
    )
  
  soil_field <- list(
    soil |> dplyr::select(
      ID_UNIQUE_PLOT, 
      PLOT,
      DEP,
      YEAR,
      DATEECO,
      TSOL, 
      TSOL_DES,
      ROCHE,
      ROCHE_DES,
      TEXT1,
      TEXT1_DES,
      TEXT2,
      TEXT2_DES,
      PROF1,
      PROF2,
      CAI40,
      CAILLOUX,
      AFFROC,
      AFPLA
    ))
  

  soil <- soil |>
    dplyr::mutate(
      soil_field = soil_field
    ) |>
    dplyr::select(
      ID_UNIQUE_PLOT, 
      PLOT,
      DEP,
      YEAR,
      DATEECO,
      soil_field

    ) |>
    dplyr::as_tibble()
  
  return(soil)
  
  
  
  
}

