#' Raw ifn data to tibble
#'
#' Transform raw IFN plot data into tidy data for use in models
#'
#' This function will take every year indicated and retrieve and transform the plot data for the departments and
#' plots provided. For that, csv files from IFN must reside in the folder indicated in the
#' \code{folder} argument.
#'
#' @param provinces A character vector with the code for the departments.
#' @param versions A character vector with the ifn versions.
#' @param filter_list A list of provinces and plots to extract the data from.
#' @param folder The path to the folder containing the IFN csv files, as character.
#' @param ... Not used at the moment
#' @param .parallel_options An object of class \code{furrr_options}. See
#'   \code{\link[furrr]{furrr_options}}.
#' @param .verbose Logical controlling if progress messages are shown.
#'
#' @section Filter list:
#'   If no \code{filter_list} argument is provided, \code{ifn_to_tibble} will attempt to process all
#'   plots for the provinces and ifn versions provided. This will result in sometimes hundred of thousands
#'   plots to be extracted, processed and returned, will in turn will cause a big use of memory and
#'   long times of calculation (specially when parallelising). Is better to provide a list of departments
#'   with the counties and plots to look after to narrow the process. This \code{filter_list} should
#'   have the following structure:
#'   \preformatted{
#'    list(
#'    "01" = 1404119,
#'    "10" = 900863,
#'    "11" = c(1436508, 1410492))
#'
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

   # browser()
  ## Assertions and checks ##
  # grep
  assertthat::assert_that(
    .sys_cmd_warning()
  )

  # departments
  assertthat::assert_that(
    is.character(provinces), length(provinces) > 0,
    msg = cli::cli_abort("provinces must be a character vector with at least one province code")
  )
  ## TODO
  # check all provinces are valid



  # versions
  assertthat::assert_that(
    is.character(versions), length(versions) > 0,
    msg = cli::cli_abort("versions must be a character vector with at least one")
  )

  # folder
  assertthat::assert_that(
    fs::dir_exists(folder),
    msg = cli::cli_abort( "Folder especified ({.path {folder}}) doesn't exists. Please create the folder first and populate it with the needed IFN csv files")
  )


  # # filter_list
  # if (is.null(filter_list)) {
  #   if (interactive()) {
  #     cli::cli_inform(c(
  #       "You haven't specified any plots in the {.arg filter_list} argument.",
  #       "x" = "This will cause to retrieve {.strong ALL} plots  for the selected departments and years",
  #       "!" = "This will use a lot of memory and time, as hundred of thousands plots will potentially be evaluated",
  #       "TODO: add info about how to create the filter list",
  #       ""
  #     ))
  #
  #     user_auth <- utils::menu(c("Yes", "No"), title = "Do you wish to continue anyway?")
  #     if (user_auth == 2L) {
  #       cli::cli_abort("Aborting per user request")
  #     }
  #   }
  # }
  ## TODO
  # Check provinces and plots??

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



  ## inform the user
  verbose_msg(
    cli::cli_inform(
      c("Start", "i" = "Processing {length(versions)} cicle{?s}")
    ),
    .verbose
  )

  ## send the versions in loop to process table function
  purrr::map(
    versions,
    .f = \(version) {
      ifn_tables_process(
        provinces, version, filter_list, folder, .parallel_options, .verbose, ...)
    },
    .progress = FALSE
  ) |>
    purrr::list_rbind()
}


#' Inner function to process all tables for one year
#'
#' Processing all tables for one year
#'
#' This function is intended to be called internally by \code{\link{ifn_to_tibble}} for each
#' year. This is implemented with furrr to allow parallelization of the plots data retrieval.
#'
#' @describeIn ifn_to_tibble
#'
ifn_tables_process <- function(
  provinces, version, filter_list, folder,
  .parallel_options, .verbose, ...
) {

    # debug
       # browser()

    # Create input df for year

    input_df <- .build_ifn_input_with(version, provinces, filter_list, folder, .verbose)

    # temp_res <- furrr::future_pmap(
     temp_res <- purrr::pmap(
      .progress = .verbose,
      .l = input_df,
      .f = \(province, plots, version, tree_table, plot_table, shrub_table, regen_table, coord_table) {

      # browser()
      #
      plot_info <- ifn_plot_table_process(plot_table, coord_table, version, plots, province, ifn_provinces_dictionary)


      tree <- ifn_tree_table_process(tree_table, version, plots, province, ESPECIES)


      shrub <- ifn_shrub_table_process(shrub_table, version, plots, province, ESPECIES)


      regen <- ifn_regen_table_process(regen_table, version, plots, province,ESPECIES)


      if (nrow(plot_info) < 1) {
        return(tibble::tibble())
      }

      # we put together all tables in a data frame


      understory <- plot_info |>
        dplyr::select(
          ID_UNIQUE_PLOT,
          YEAR,
          province_code,
          PLOT)|>

        dplyr::mutate(
          shrub = list(shrub))


      plot_info |>
        dplyr::rename(
            COORD1 = COORDEX,
            COORD2 = COORDEY
        ) |>
        dplyr::mutate(
          tree = list(tree),
          understory = list(understory),
          regen = list(regen)
        ) |>

        dplyr::select(
          dplyr::any_of(c(
          "ID_UNIQUE_PLOT",
          "COUNTRY",
          "YEAR",
          "ca_name_original",
          "province_name_original",
          "province_code",
          "PLOT",
          "Cla",
          "Subclase",
          "version",
          "Tipo",
          "HOJA",
          "Huso",
          "COORD_SYS",
          "COORD1",
          "COORD2",
          "crs",
          "PENDIEN2",
          "SLOPE",
          "ELEV",
          "ASPECT",
          "tree",
          "understory",
          "regen"
          # "soils"
          ))

        )

    }
  ) |>
    purrr::list_rbind()


    # something went wrong (bad counties and plots, wrong filter list...)
    if (nrow(temp_res) < 1) {
      cli::cli_abort("Ooops! Something went wrong, exiting...")
    }

    temp_res |>
      # filtering the missing plots. This is done based on the fact plot table functions returns NAs
      # for all vars, including coords, when the plot is not found
      dplyr::filter(!(is.na(COORD1)  & is.na(COORD2) ))
  }




#' IFN 2 tree table process
#'
#' Processing tree table for IFN
#'
#' This function  ifn_tree_table_process reads and process the tree table for one plot and one IFN
#'
#' @param tree_data file that contains the tree table for that plot
#' @param plot plot_id code
#' @param province province code
#' @param ref_tree_ifn data frame containing the species code reference table
#'
#' @noRd
#'

ifn_tree_table_process <- function(tree_data, version, plot, province, ESPECIES) {

  # browser()
  plot <- stringr::str_split_i(plot, "_", 2)

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(tree_data)))
    # !any(c(tree_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping tree data for plot {.var {plot}} "
    ))

    return(dplyr::tibble())
  }

  # 2. col names we select the column names to be read

   # browser()

  if (version == "ifn2"){

  tree_filtered_data <-  .read_inventory_data(
    tree_data,
    colnames = c(
      "PROVINCIA",
      "ESTADILLO",
      "ESPECIE",
      "NUMORDEN",
      "ARBOL",
      "DIAMETRO1",
      "DIAMETRO2",
      "ALTURA"
    ),
    version = version,
    province = province,
    .ifn = TRUE
  ) |>
    dplyr::filter(
      ESTADILLO == plot
    ) |>
    tibble::as_tibble()



  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(tree_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Tree data missing for plot {.var {plot}}",
      "i" = "Returning empty tibble for plot {.var {plot}} "
    ))
    return(dplyr::tibble())
  }
   # browser()

  tree <- tree_filtered_data |>
    # transformations and filters
    dplyr::rename(
      province_code = PROVINCIA,
      PLOT = ESTADILLO,
      SP_CODE = ESPECIE,
      TREE = ARBOL,
      Dn1 = DIAMETRO1,
      Dn2 = DIAMETRO2,
      HT = ALTURA) |>
    dplyr::mutate(
      PLOT = as.character(PLOT),
      province_code = as.character(province_code),
      Dn1 = as.numeric(Dn1),
      Dn2 = as.numeric(Dn2),
      HT = as.numeric(stringr::str_replace(HT, ",", ".")),
      SP_CODE = as.numeric(SP_CODE),
      # ID_UNIQUE_PLOT = paste("ES",province_code,PLOT,sep="_"),
      # From mm to cm
      DIA = ((Dn1 + Dn2)/2)*0.1,
      # in meters
      HT = HT ,
      DENSITY = dplyr::case_when(
        DIA < 12.5 ~ 127.3239546,
        DIA >= 12.5 & DIA < 22.5 ~ 31.83098865,
        DIA >= 22.5 & DIA < 42.5 ~ 14.14710607,
        DIA >= 42.5 ~ 5.092958185
      ))  |>

    # add species info ---> WHAT REFERENCE SHOULD I USEE???
    dplyr::left_join(
      y = ESPECIES |>
        dplyr::select(
          SP_CODE ,
          SP_NAME ),
      by = "SP_CODE"
    ) |>
    dplyr::arrange(SP_CODE) |>

    dplyr::select(
        ID_UNIQUE_PLOT,
        province_code,
        PLOT,
        SP_CODE,
        SP_NAME,
        #diameter in cm
        DIA,
        #height in m
        HT,
        DENSITY

    )




  # Return tree
  return(tree)
  }

  if (version %in% c("ifn3", "ifn4")){

    # browser()

    tree_filtered_data <-  .read_inventory_data(
      tree_data,
      colnames = c(
        "Provincia",
        "Estadillo",
        "Cla",
        "Subclase",
        "Especie",
        "nArbol",
        "OrdenIf3",
        "OrdenIf2",
        "OrdenIf4",
        "Dn1",
        "Dn2",
        "Ht",
        "Calidad",
        "Forma"
      ),
      version = version,
      province = province,
    .ifn = TRUE
    ) |>
      dplyr::filter(
        Estadillo == plot
      ) |>
      tibble::as_tibble()

    # IFN3 doesn't have Provincia, so we check
    if ("Provincia" %in% names(tree_filtered_data)) {
      tree_filtered_data <- tree_filtered_data |>
        dplyr::filter(Provincia == as.integer(province))
    }

    # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(tree_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Tree data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}} "
      ))
      return(dplyr::tibble())
    }
    # browser()




    tree <- tree_filtered_data |>
      dplyr::rename(
        PLOT = Estadillo,
        HT = Ht,
        Clase = Cla,
      ) |>
      # units transformations
      dplyr::mutate(
        province_code =  province,

        # Subclass fixes
        Subclase = .ifn_subclass_fixer(Subclase),

        # unique inner code
        # ID_UNIQUE_PLOT = paste("ES",province_code, PLOT, sep="_"),
        DIA = (Dn1 + Dn2)/2,

        # MM TO CM
        DIA = DIA * 0.1,
        SP_CODE = as.numeric(Especie),

        # density represented by each tree considering  plot design (variable radious)
        DENSITY = dplyr::case_when(
          DIA < 12.5 ~ 127.3239546,
          DIA >= 12.5 & DIA < 22.5 ~ 31.83098865,
          DIA >= 22.5 & DIA < 42.5 ~ 14.14710607,
          DIA >= 42.5 ~ 5.092958185
        )) |>


      #different for ifn3 and 4 ???

      # add species info

      dplyr::left_join(
        y =  ESPECIES |>
          dplyr::select(
            SP_CODE ,
            SP_NAME ),
        by = "SP_CODE"
      ) |>


      dplyr::arrange(SP_CODE) |>

      dplyr::select(
        dplyr::any_of(c(
          "ID_UNIQUE_PLOT",
          "province_code",
          "Clase",
          "Subclase",
          "PLOT",
          "SP_CODE",
          "SP_NAME",
          #tree number id in ifn4
          "nArbol",
          #CUALIDAD 6 = dead but providing functions
          "Calidad",
          "Forma",
          #check codes to understand origin and trace of individuals
          "OrdenIf2",
          "OrdenIf3",
          "OrdenIf4",
          #diameter in cm
          "DIA",
          #height in cm
          "HT",
          "DENSITY"
        ))
      )

    # Return tree
    return(tree)

  }
}




ifn_shrub_table_process <- function(shrub_data, version, plot, province, ESPECIES) {


  # browser()
  plot <- stringr::str_split_i(plot, "_", 2)

  # Assertions (things we need) and checks/validations
  #
  # 1. file


  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(shrub_data)))
    # !any(c(shrub_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping shrub data for plot {.var {plot}} "
    ))

    return(dplyr::tibble())
  }

  if (version == "ifn2"){




  # 2. col names
    # browser()

  shrub_filtered_data <- .read_inventory_data(
      shrub_data,
      colnames = c(
        "PROVINCIA",
        "ESTADILLO",
        "ESPECIE",
        "FRACCAB",
        "ALTUMED"
      ),
      version = version,
      province = province,
    .ifn = TRUE
    ) |>
    dplyr::filter(
      ESTADILLO == plot
    ) |>
    tibble::as_tibble()

  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(shrub_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Shrub data missing for plot {.var {plot}}",
      "i" = "Returning empty tibble for plot {.var {plot}}  "
    ))
    return(dplyr::tibble())
  }


  #we add the id code
  shrub <- shrub_filtered_data |>

    dplyr::rename(
      PLOT = ESTADILLO,
      COVER = FRACCAB,
      HT = ALTUMED
    ) |>

    dplyr::mutate(
      PLOT = as.character(PLOT),
      province_code = as.character(PROVINCIA),
      HT = as.numeric(HT),
      COVER =as.numeric(COVER),
      #DM TO cm
      HT = HT * 10 ,
      SP_CODE = as.numeric(ESPECIE),
      # ID_UNIQUE_PLOT = paste("ES",province_code,PLOT,sep = "_")

    ) |>


    # 3. ref_plant_dictionary
    #we join data from plant ref dictionary
    # some symbols apply for multiple species


    dplyr::left_join(
      y =  ESPECIES |>
        dplyr::select(
          SP_CODE ,
          SP_NAME ),
      by = "SP_CODE"
    ) |>
    dplyr::arrange(SP_CODE) |>

    dplyr::select(
      ID_UNIQUE_PLOT,
      province_code,
      PLOT,
      SP_NAME,
      SP_CODE,
      HT,
      COVER
    )


  #
  # Return shrub
  return(shrub)
  }

  if (version %in% c("ifn3", "ifn4")){


    shrub_filtered_data <- .read_inventory_data(
      shrub_data,
      colnames =c(
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
        Estadillo == plot
      ) |>
      tibble::as_tibble()

    # IFN3 doesn't have Provincia, so we check
    if ("Provincia" %in% names(shrub_filtered_data)) {
      shrub_filtered_data <- shrub_filtered_data |>
        dplyr::filter(Provincia == as.integer(province))
    }

    # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(shrub_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Shrub data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ))
      return(dplyr::tibble())
    }

    shrub <- shrub_filtered_data |>

      dplyr::rename(
        PLOT = Estadillo,
        HT = Hm,
        Clase = Cla
      ) |>
      dplyr::mutate(
        province_code = province,

        # Subclass fixes
        Subclase = .ifn_subclass_fixer(Subclase),

        COVER = Fcc,
        #DM TO CM
        HT = HT * 10 ,
        COVER = as.numeric(COVER),
        # ID_UNIQUE_PLOT = paste("ES",province_code, PLOT, sep="_"),
        SP_CODE = as.numeric(Especie)

      ) |>


      # 3. ref_plant_dictionary
      #we join data from plant ref dictionary
      # some symbols apply for multiple species

      dplyr::left_join(
        y =  ESPECIES |>
          dplyr::select(
            SP_CODE ,
            SP_NAME ),
        by = "SP_CODE"
      ) |>

      dplyr::select(
        ID_UNIQUE_PLOT,
        province_code,
        Clase,
        Subclase,
        PLOT,
        SP_NAME,
        SP_CODE,
        HT,
        COVER
      )
    return(shrub)

  }
}



ifn_regen_table_process <- function(regen_data, version, plot, province, ESPECIES) {
# browser()

  plot <- stringr::str_split_i(plot, "_", 2)
  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(regen_data)))
    # !any(c(regen_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping regen data for plot {.var {plot}} "
    ))

    return(dplyr::tibble())
  }

  if (version == "ifn2"){

  regen_filtered_data <- .read_inventory_data(
    regen_data,
    colnames = c(
      "PROVINCIA",
      "ESTADILLO",
      "ESPECIE",
      "NUMERO",
      "ALTUMED",
      "REGENA"
    ),
    version = version,
    province = province,
    .ifn = TRUE
  ) |>
    dplyr::filter(
      ESTADILLO == plot
    ) |>
    tibble::as_tibble()


  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(regen_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Regeneration data missing for plot {.var {plot}}",
      "i" = "Returning empty tibble for plot {.var {plot}}  "
    ))
    return(dplyr::tibble())
  }



  #we add the id code
  regeneration <- regen_filtered_data |>

    dplyr::mutate(
      PLOT = as.character(ESTADILLO),
      province_code = as.character(PROVINCIA),
      NUMERO = as.numeric(NUMERO),

      #DM TO CM
      Hm = as.numeric(ALTUMED) * 10,
      # ID_UNIQUE_PLOT = paste("ES",province_code,PLOT,sep = "_"),
      SP_CODE = as.numeric(ESPECIE)
      ) |>

    # add species info ---> WHAT REFERENCE SHOULD I USEE???
    dplyr::left_join(
      y = ESPECIES |>

        dplyr::select(
          SP_CODE,
          SP_NAME ),
      by = "SP_CODE"
    ) |>
    dplyr::arrange(SP_CODE) |>


    dplyr::rename(
      Numero = NUMERO,
      Regena = REGENA
    ) |>
    dplyr::slice(rep(dplyr::row_number(), each = 2))|>
    # dplyr:: mutate(
    #   Regena = as.character(Regena)
    # )|>

    dplyr::mutate(

      Regena = ifelse(dplyr::row_number() %% 2 != 0, NA, Regena),
      Numero  = ifelse(dplyr::row_number() %% 2 == 0, NA, Numero ),
      Hm = ifelse(dplyr::row_number() %% 2 == 0, NA, Hm ),
      DBH = dplyr::case_when( Numero > 0 ~ 5,
                              #default
                              Regena > 0 ~ 1,
                              TRUE ~ NA ),
      N = Numero *127.3239546,
      N = dplyr::case_when( Regena == 1 ~ 2.5 *127.3239546,
                            Regena == 2 ~ 10 *127.3239546,
                            Regena == 3 ~ 20*127.3239546 ,
                            N>0 ~ N,
                            TRUE ~ NA ),
      Height = dplyr::case_when( Hm > 0 ~ Hm,
                                 #default
                                 Regena > 0 ~ 100 ,
                                 TRUE ~ NA
                                 ),
      DENSITY = 127.3239546
      # Z50 = NA ,
      # Z95 = NA,
      # OrdenIf2 = NA
      ) |>
    dplyr::select(
      ID_UNIQUE_PLOT,
      province_code,
      PLOT,
      #codigo
      SP_CODE,
      #nombre en latin
      SP_NAME,
      # NUMERO,
      # Hm,
      # REGENA
      DBH,
      Height,
      # OrdenIf2,
      DENSITY,
      N,
      # Z95,
      # Z50
    ) |>
    dplyr::filter(complete.cases(DBH, Height, N)
    )


  # Return regen
  return(regeneration)

  }


  if (version %in% c("ifn3", "ifn4")){

  regen_filtered_data <- .read_inventory_data(
  regen_data,
  colnames = c(
    "Provincia",
    "Estadillo",
    "Cla",
    "Subclase",
    "Especie",
    "CatDes",
    "Tipo",
    "Densidad",
    "NumPies",
    "Hm"
  ),
  version = version,
  province = province,
    .ifn = TRUE
  ) |>
    dplyr::filter(
      Estadillo == plot
    ) |>
    tibble::as_tibble()

  # IFN3 doesn't have Provincia, so we check
  if ("Provincia" %in% names(regen_filtered_data)) {
    regen_filtered_data <- regen_filtered_data |>
      dplyr::filter(Provincia == as.integer(province))
  }


  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(regen_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Regeneration data missing for plot {.var {plot}}",
      "i" = "Returning empty tibble for plot {.var {plot}}  "
    ))
    return(dplyr::tibble())
  }


  #we add the id code
  regeneration<- regen_filtered_data |>

    dplyr::rename(
      PLOT = Estadillo,
      Clase = Cla
    ) |>

    dplyr::mutate(
      province_code = province,

      # Subclass fixes
      Subclase = .ifn_subclass_fixer(Subclase),

      #DM TO CM
      Hm = Hm * 10,
      # ID_UNIQUE_PLOT = paste("ES",province, PLOT, sep = "_"),
      SP_CODE = as.numeric(Especie)
      ) |>

    dplyr::left_join(
      y =  ESPECIES |>
        dplyr::select(
          SP_CODE ,
          SP_NAME ),
      by = "SP_CODE"
    ) |>


    dplyr::mutate(
      DBH = dplyr::case_when(
        CatDes == 1 ~ 0.1,
        CatDes == 2 ~ 0.5,
        CatDes == 3 ~ 1.5,
        CatDes == 4 ~ 5,
        TRUE ~ NA
      ),

      N = dplyr::case_when(
        CatDes == 1 ~ 2.5*127.3239546,
        CatDes == 2 ~ 10*127.3239546,
        CatDes == 3 ~ 20*127.3239546,
        CatDes == 4 ~ NumPies*127.3239546,
        TRUE ~ NA),

      Height = dplyr::case_when(
        CatDes == 1 ~ 10,
        CatDes == 2 ~ 80,
        CatDes == 3 ~ 100,
        CatDes == 4 ~ Hm ,
        TRUE ~ NA),
      DENSITY = 127.3239546
      # nArbol = NA,
      # OrdenIf2 = NA,
      # OrdenIf3 = NA,
      # OrdenIf4 = NA,
      # Z50 = NA ,
      # Z95 = NA


    )|>
    dplyr::select(
      ID_UNIQUE_PLOT,
      province_code,
      Clase,
      Subclase,
      PLOT,
      SP_CODE,
      SP_NAME,
      DBH,
      Height,
      # nArbol,
      # OrdenIf2,
      # OrdenIf3,
      # OrdenIf4,
      N,
      DENSITY
      # Z95,
      # Z50
      )



  # Return regen
  return(regeneration)



  }
}



ifn_plot_table_process <- function(plot_data, coord_data, version, plot, province, ifn_provinces_dictionary){

  # browser()
  # in some cases (get plots from provinces) we pass a quosure to the plot argument.
  # If not, get the estadillo padded.
  if (!rlang::is_quosure(plot)) {
    plot <- stringr::str_split_i(plot, "_", 2)
  }

  # Assertions  and checks/validations
  files_validation <- assertthat::validate_that(
    !any(is.na(c(plot_data)))
    # !any(c(plot_data) == NA_character_)
  )

  # If any file is missing abort and return an empty tibble??
  if (is.character(files_validation)) {
    cli::cli_warn(c(
      "Some files can't be found",
      "i" = "Skipping regen data for plot {.var {plot}} "
    ))

    return(dplyr::tibble())
  }

  # 2. col names
  # browser()
  if (version == "ifn2"){


  plot_filtered_data <- .read_inventory_data(
      plot_data,
      colnames = c(
        "PROVINCIA",
        "ESTADILLO",
        "HOJA",
        "ANO",
        "COORDEX",
        "COORDEY",
        "ALTITUD1",
        "ALTITUD2",
        "PENDIEN1",
        "PENDIEN2",
        "FRACCION1",
        "FRACCION2",
        "CLASUELO",
        "ESPESOR",
        "CLACOBER",
        "CUBIERTA",
        "ORIENTA1",
        "ORIENTA2",
        "MAXPEND1",
        "MAXPEND2"
      ),
      version = version,
      province = province,
    .ifn = TRUE
    ) |>
    dplyr::filter(
      ESTADILLO == !!plot,
      PROVINCIA == !!province
    ) |>
    tibble::as_tibble()



  # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
  if (nrow(plot_filtered_data) < 1) {
    # warn the user
    cli::cli_warn(c(
      "Plot data missing for plot {.var {plot}}",
      "i" = "Returning empty tibble for plot {.var {plot}}  "
    ))
    return(dplyr::tibble())
  }

  # check for bad formatted or missing coords to inform the user
  if (any(
    is.na(plot_filtered_data$COORDEX), is.na(plot_filtered_data$COORDEY),
    grepl("[A-Za-z]", plot_filtered_data$COORDEX), grepl("[A-Za-z]", plot_filtered_data$COORDEY)
  )) {
    cli::cli_warn(c(
      "File {.file {plot_data}} has some errors in the coordinates (missing coordinates, bad format...).",
      "i" = "These records will be removed from the results"
    ))
  }

  # remove bad formatted or missing coordinates
  plot_coord_fixed_data <- plot_filtered_data |>
    dplyr:::mutate(
      COORDEX = ifelse(grepl("[A-Za-z]", COORDEX), NA, COORDEX),
      COORDEY = ifelse(grepl("[A-Za-z]", COORDEY), NA, COORDEY)
    ) |>
    dplyr::filter(!is.na(COORDEX), !is.na(COORDEY))

  #we add the id code
  info_plot <- plot_coord_fixed_data |>

    dplyr::rename(
      PLOT = ESTADILLO,
      SLOPE = MAXPEND2,
      ELEV = ALTITUD2,
      YEAR = ANO,
      ASPECT = ORIENTA2) |>

    dplyr::mutate(
      # YEAR = as.character(YEAR),
      # HOJA = as.character(HOJA),
      COUNTRY = "ES",
      province_code = PROVINCIA,
      # ID_UNIQUE_PLOT = paste("ES", province_code,PLOT,sep = "_"),
      # ALTITUD1 = as.numeric(ALTITUD1),
      ELEV = as.numeric(ELEV)*100,
      SLOPE = as.numeric(stringr::str_replace(SLOPE, ",", ".")),
      ASPECT = as.numeric(ASPECT),
      # ALTITUD1 = ALTITUD1*100,
      # ELEV = ELEV*100,
      COORDEX = as.numeric(COORDEX) * 1000,
      COORDEY = as.numeric(COORDEY) * 1000,
      # COORDEX = 1000 * COORDEX,
      # COORDEY = 1000 * COORDEY,
      version = version,
      Huso = dplyr::case_when(
        province_code %in% c("35", "38") ~ 28,
        province_code %in% c("01", "07", "08", "15","17","20", "25", "26","27","28","30","32","33","36","39","43","48","02","03","04","05","06","09","10","11","12","13","14","16","18","19","21","22","23","24","29","31",
                                "34","37","40","41","42","44","45","46","47","49","50") ~ 30
      ),
      COORD_SYS = dplyr::case_when(
        province_code %in% c("35", "38") ~ "WGS84",
        province_code %in% c("01", "07", "08", "15","17","20", "25", "26","27","28","30","32","33","36","39","43","48","02","03","04","05","06","09","10","11","12","13","14","16","18","19","21","22","23","24","29","31",
                             "34","37","40","41","42","44","45","46","47","49","50") ~ "ED50"),
      crs = dplyr::case_when(
        Huso == 30 & COORD_SYS == "ED50" ~ 23030,
        Huso == 31 & COORD_SYS == "ED50" ~ 4326,
        Huso == 29 & COORD_SYS == "ED50" ~ 23029,
        Huso == 30 & COORD_SYS == "ETRS89" ~ 25830,
        Huso == 31 & COORD_SYS == "ETRS89" ~ 25831,
        Huso == 29 & COORD_SYS == "ETRS89" ~ 25829,
        Huso == 28 & COORD_SYS == "ED50" ~ 23028,
        Huso == 28 & COORD_SYS == "WGS84" ~ 32628,
        TRUE ~ NA_integer_
      )
    ) |>

    dplyr::left_join(
      y = ifn_provinces_dictionary |>
        dplyr::select(
          province_code = province_code,
          province_name_original = province_name_original,
          ca_name_original = ca_name_original
        ),
      by = "province_code") |>

    dplyr::select(dplyr::any_of(c(

      "ID_UNIQUE_PLOT",
      "COUNTRY",
      "ca_name_original",
      "province_name_original",
      "province_code",
      "PLOT",
      "YEAR",
      "version",
      "HOJA",
      "Huso",
      "COORDEX",
      "COORDEY",
      "COORD_SYS",
      "crs",
      "PENDIEN2",
      "SLOPE",
      "ELEV",
      "ASPECT"
      # "soils"
    ))
    )


#
#   soil_info <- info_plot |>
#
#     dplyr::select(
#       ID_UNIQUE_PLOT,
#       province_code,
#       PLOT,
#       YEAR) |>
#     dplyr::mutate(
#       soil_field = list(info_plot |>
#                           dplyr::select(
#                             ID_UNIQUE_PLOT,
#                             province_code,
#                             PLOT,
#                             CLASUELO,
#                             ESPESOR,
#                             CLACOBER,
#                             CUBIERTA))
#     ) |>
#
#     dplyr::select(
#       ID_UNIQUE_PLOT,
#       province_code,
#       PLOT,
#       YEAR,
#       soil_field
#
#     ) |>
#     tibble::tibble()




  # info_plot <- info_plot |>
  #   dplyr::mutate(
  #     # soils = list(soil_info),
  #
  #     #linea provisional
  #     # crs = get_crs(info_plot$Huso, info_plot$COORD_SYS)) |>
  #
  #     crs = dplyr::case_when(
  #       Huso == 30 & COORD_SYS == "ED50" ~ 23030,
  #       Huso == 31 & COORD_SYS == "ED50" ~ 4326,
  #       Huso == 29 & COORD_SYS == "ED50" ~ 23029,
  #       Huso == 30 & COORD_SYS == "ETRS89" ~ 25830,
  #       Huso == 31 & COORD_SYS == "ETRS89" ~ 25831,
  #       Huso == 29 & COORD_SYS == "ETRS89" ~ 25829,
  #       Huso == 28 & COORD_SYS == "ED50" ~ 23028,
  #       Huso == 28 & COORD_SYS == "WGS84" ~ 32628,
  #       TRUE ~ NA_integer_
  #     )
  #     ) |>
  #
  #   dplyr::select(any_of(c(
  #
  #     "ID_UNIQUE_PLOT",
  #     "COUNTRY",
  #     "ca_name_original",
  #     "province_name_original",
  #     "province_code",
  #     "PLOT",
  #     "YEAR",
  #     "version",
  #     "HOJA",
  #     "Huso",
  #     "COORDEX",
  #     "COORDEY",
  #     "COORD_SYS",
  #     "crs",
  #     "PENDIEN2",
  #     "SLOPE",
  #     "ELEV",
  #     "ASPECT"
  #     # "soils"
  #   ))

  return(info_plot)
  }

  if (version %in% c("ifn3", "ifn4")){


    plot_filtered_data <- .read_inventory_data(
      plot_data,
      colnames = c(
        "Provincia",
        "Estadillo",
        "Cla",
        "Subclase",
        "Tipo",
        "Ano",
        "Rocosid",
        "MatOrg",
        "TipSuelo1",
        "TipSuelo2",
        "TipSuelo3",
        "Orienta1",
        "Orienta2",
        "MaxPend1",
        "MaxPend2"

      ),
      version = version,
      province = province,
    .ifn = TRUE
    ) |>
      dplyr::filter(
        Estadillo == !!plot,
        Provincia == as.integer(province)
      ) |>
      tibble::as_tibble()



    # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
    if (nrow(plot_filtered_data) < 1) {
      # warn the user
      cli::cli_warn(c(
        "Plot data missing for plot {.var {plot}}",
        "i" = "Returning empty tibble for plot {.var {plot}}  "
      ))
      return(dplyr::tibble())
    }

    plot_filtered_data <- plot_filtered_data |>
      dplyr::rename(
        YEAR = Ano,
        PLOT = Estadillo
      ) |>
      dplyr::mutate(
        YEAR = as.character(YEAR),

        # Subclass fixes
        Subclase = .ifn_subclass_fixer(Subclase),


        version = version,
        province_code = province,
        # ID_UNIQUE_PLOT = paste("ES",province_code, PLOT, sep = "_"),
        province_code = as.character(province_code)
        )

    # soil <- plot_filtered_data |>
    #
    #   dplyr::mutate(
    #     # province_code = province,
    #     # province_code = as.numeric(province_code),
    #     # PLOT = Estadillo,
    #     # YEAR = Ano,
    #     # ID_UNIQUE_PLOT = paste("ES",province_code, PLOT, sep = "_"),
    #       soil_field = list(
    #         plot_filtered_data |>
    #         # dplyr::mutate(
    #         #   PLOT = Estadillo,
    #         #   province_code = province,
    #         #   province_code = as.numeric(province_code),
    #         #   ID_UNIQUE_PLOT = paste("ES",province_code, PLOT, sep="_"))|>
    #           dplyr::select(
    #             ID_UNIQUE_PLOT,
    #              province_code,
    #              Clase,
    #              Subclase,
    #              PLOT,
    #              YEAR,
    #              Rocosid,
    #              MatOrg,
    #              TipSuelo1,
    #              TipSuelo2,
    #              TipSuelo3
    #             ))) |>
    #
    #   #selection of final variables
    #
    #   dplyr::select(
    #     ID_UNIQUE_PLOT,
    #     province_code,
    #     version,
    #     Clase,
    #     Subclase,
    #     PLOT,
    #     YEAR,
    #     soil_field
    #   )

    #we add the id code
    info_plot <- plot_filtered_data |>
      dplyr::rename(
        ASPECT = Orienta1,
        SLOPE = MaxPend1
      ) |>
      dplyr::mutate(
        # PLOT = Estadillo,
        # YEAR = Ano,
        COUNTRY = "ES",
        # province_code = as.numeric(province),
        # ID_UNIQUE_PLOT = paste("ES",province_code, PLOT, sep="_"),
        #de grados centesimales a sexagesimales??
        ASPECT = as.numeric(ASPECT)* 0.9,
        SLOPE = as.numeric(SLOPE),
        # soils = list(soil),
        COORD_SYS = dplyr::case_when(
          version == "ifn4" & province_code %in% c("01", "07", "08", "15","17","20", "25", "26","27","28","30","32","33","36","39","43","48")  ~ "ED50",
          version == "ifn4" & province_code %in% c("35", "38") ~ "WGS84",
          version == "ifn4" & province_code %in% c("02","03","04","05","06","09","10","11","12","13","14","16","18","19","21","22","23","24","29","31","34","37","40","41","42","44","45","46","47","49","50") ~ "ETRS89",
          version == "ifn3" & province_code %in% c("01", "07", "08", "15","17","20", "25", "26","27","28","30","32","33","36","39","43","48","02","03","04","05","06","09","10","11","12","13","14","16","18","19","21","22","23","24","29","31","34","37","40","41","42","44","45","46","47","49","50", "35", "38") ~ "ED50")
      ) |>

      dplyr::left_join(
        y = ifn_provinces_dictionary |>
          dplyr::select(
            province_code = province_code,
            province_name_original = province_name_original ,
            ca_name_original
          ),
        by = "province_code"
        ) |>

      #selection of final variables

      dplyr::select(

        ID_UNIQUE_PLOT,
        COUNTRY,
        ca_name_original,
        province_code,
        province_name_original,
        PLOT,
        Cla,
        Subclase,
        COORD_SYS,
        YEAR,
        version,
        Tipo,
        ASPECT,
        SLOPE
        # soils

      )




  files_validation <- assertthat::validate_that(
    !any(is.na(c(coord_data)))
    # !any(c(coord_data) == NA_character_)
    )

  # browser()

  coords_filtered_data <- .read_inventory_data(
    coord_data,
    colnames = c(
      "Provincia",
      "Estadillo",
      "Clase",
      "Subclase",
      "Hoja50",
      "CoorX",
      "CoorY",
      "Huso"
    ),
    version = version,
    province = province,
    .dry = TRUE,
    .ifn = TRUE
   ) |>
     dplyr::filter(
       Estadillo == !!plot,
       Provincia == as.integer(province)
     ) |>
     tibble::as_tibble()




   # ## We check before continuing, because if the filter is too restrictive maybe we dont have rows
   if (nrow(coords_filtered_data) < 1) {
     # warn the user
     cli::cli_warn(c(
       "Coordinates data missing for plot {.var {plot}}",
       "i" = "Returning empty tibble for plot {.var {plot}}  "
     ))
     return(dplyr::tibble())
   }

   # check for bad formatted or missing coords to inform the user
   if (any(is.na(coords_filtered_data$CoorX), is.na(coords_filtered_data$CoorY))) {
     cli::cli_warn(c(
       "File {.file {plot_data}} has some errors in the coordinates (missing coordinates, bad format...).",
       "i" = "These records will be removed from the results"
     ))
   }

   # remove bad formatted or missing coordinates
   coords_fixed_data <- coords_filtered_data |>
     dplyr::filter(!is.na(CoorX), !is.na(CoorY))

   coords_data <- coords_fixed_data |>
      dplyr::rename(
        PLOT = Estadillo,
        COORDEX = CoorX,
        COORDEY = CoorY,
        HOJA = Hoja50,
        Cla = Clase
        ) |>
      dplyr::mutate(
        HOJA = as.character(HOJA),

        # Subclass fixes
        Subclase = .ifn_subclass_fixer(Subclase),



        province_code = province,
        province_code = as.character(province_code),
        version = version,
        # ID_UNIQUE_PLOT = paste("ES",province_code, PLOT, sep = "_"),
        Huso = ifelse("Huso" %in% names(coords_fixed_data), coords_fixed_data$Huso, NA)
      )

   ## BUG_: coord data now doesn't have unique id, as it has no subclass in table. We need to
   ## join these two tables, how??
   # browser()

   info_plot <- info_plot |>
     dplyr::left_join(
           y = coords_data |>
             dplyr::select(dplyr::any_of(c(
               # "ID_UNIQUE_PLOT" ,
               "Cla",
               "COORDEX",
               "COORDEY",
               "HOJA",
               "Huso"
             ))
             )
         ) |>
         dplyr::mutate(

       crs = dplyr::case_when(
         is.na(Huso) & COORD_SYS == "ED50" ~ 23030, # For now, we need to solve this: Issue #6
         Huso == 30 & COORD_SYS == "ED50" ~ 23030,
         Huso == 31 & COORD_SYS == "ED50" ~ 4326,
         Huso == 29 & COORD_SYS == "ED50" ~ 23029,
         Huso == 30 & COORD_SYS == "ETRS89" ~ 25830,
         Huso == 31 & COORD_SYS == "ETRS89" ~ 25831,
         Huso == 29 & COORD_SYS == "ETRS89" ~ 25829,
         Huso == 28 & COORD_SYS == "ED50" ~ 23028,
         Huso == 28 & COORD_SYS == "WGS84" ~ 32628,
         TRUE ~ NA_integer_)
         ) |>

     dplyr::select(
       dplyr::any_of(c(
         "ID_UNIQUE_PLOT",
         "COUNTRY",
         "YEAR",
         "ca_name_original",
         "province_code",
         "province_name_original",
         "PLOT",
         "Cla",
         "Subclase",
         "version",
         "Tipo",
         "ASPECT",
         "SLOPE",
         "crs",
         "COORD_SYS",
         "COORDEX",
         "COORDEY",
         "HOJA",
         "Huso"
         # "soils"
       ))
     )

       return(info_plot)

  }

#
#   # Return plot with soil
  return(info_plot)
}


