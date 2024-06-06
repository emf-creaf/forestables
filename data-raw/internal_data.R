
# FIA states dict -----------------------------------------------------------------------------

fia_states_dictionary <- FIESTA::ref_statecd

# IFN provinces dictionary --------------------------------------------------------------------

ifn_provinces_dictionary <- readxl::read_xls(
  "data-raw/090471228013528a_tcm30-278472.xls", sheet = "NUTS",
  skip = 1
) |>
  dplyr::select(
    province_code = `CÓDIGO PROVINCIA INE`,
    province_name_original = `NOMBRE PROVINCIA`,
    ca_name_original = `NOMBRE COMUNIDAD AUTÓNOMA`
  ) |>
  tidyr::fill("ca_name_original", .direction = "down") |>
  dplyr::mutate(
    # add zeros to province code
    province_code = as.character(province_code) |> stringr::str_pad(2, "left", "0"),
    # for some reason castilla y león aparece mal en el archivo original
    ca_name_original = dplyr::if_else(
      ca_name_original == "Datos espaciales (IFNXX_MCA)", "Castilla y León", ca_name_original
    ),
    # ifn4 labels, as they mixed provinces and ccaa
    ifn4_files_labels = dplyr::case_when(
      ca_name_original == "Principado de Asturias" ~ "Asturias",
      ca_name_original == "Islas Baleares" ~ "Baleares",
      ca_name_original == "Canarias" ~ "Canarias",
      ca_name_original == "Cataluña" ~ "Cataluña",
      ca_name_original == "Extremadura" ~ "Extremadura",
      ca_name_original == "Región de Murcia" ~ "Murcia",
      ca_name_original == "Comunidad Foral de Navarra" ~ "Navarra",
      ca_name_original == "Pais Vasco" ~ "País Vasco",
      ca_name_original == "La Rioja" ~ "`La Rioja`",
      .default = province_name_original
    ),
    # converting special characters
    ifn4_files_labels = stringr::str_replace(ifn4_files_labels, "ñ", "д"),
    ifn4_files_labels = stringr::str_replace(ifn4_files_labels, "í", "б"),
    ifn4_files_labels = stringr::str_replace(ifn4_files_labels, "ó", "в"),
    ifn4_files_labels = stringr::str_replace(ifn4_files_labels, "Á", "╡")
    # TODO, convert to NA_chr the missing provinces in the IFN4
    # ifn4_files_labels = dplyr::if_else(
    #   ifn4_files_labels %in% c()
    # )
  )

# FFI growth habit data -----------------------------------------------------------------------

# Usaremos GIFT con su paquete de R para acceder a los datos de growth form
cd_ref_edit <- readr::read_delim(
  file = fs::path(Sys.getenv("ffi_path"), "metadonnees.csv"),
  skip = 331
) |>
  dplyr::as_tibble() |>
  dplyr::rename(
    UNITE = "// Unité"
  ) |>
  dplyr::filter(
    UNITE == "CDREF13"
  ) |>
  dplyr::mutate(
    lib_cdref =  stringr::str_remove_all(Libellé, "\\s*\\(.*?\\)")
  ) |>
  dplyr::rename(
    cd_ref = Code
  ) |>
  # dplyr::select(
  #   cd_ref,
  #   lib_cdref
  # ) |>
  dplyr::rename(
    full_name = lib_cdref
  ) |>

  #Add genus and species
  dplyr::mutate(
    genus = stringr::str_extract(full_name, "\\b\\w+\\b"),
    species = stringr::str_extract(full_name, "(?<=\\s)\\w+"),
    species = dplyr::if_else(stringr::str_detect(species, "^\\("), "", species),
    SP_NAME = ifelse(is.na(species) | species == "", genus, paste(genus, species, sep = " "))
  ) |>
  dplyr::arrange(full_name) |>
  dplyr::group_by(genus)

#FINAL VECTOR
fr_species_cdref <- cd_ref_edit$SP_NAME

# growth form from GIFT
trait_meta <- GIFT::GIFT_traits_meta()
gf_trait_id <- trait_meta[which(trait_meta$Trait2 == "Growth_form_1"), ][["Lvl3"]]
growth_form <- GIFT::GIFT_traits(
  trait_IDs = c(gf_trait_id), agreement = 0.66,
  bias_ref = FALSE, bias_deriv = FALSE
)

growth_form_lignified_france <- growth_form |>
  dplyr::filter(work_species %in% fr_species_cdref) |>
  dplyr::select(work_species, work_author, trait_value_1.2.1) |>
  dplyr::distinct() |>
  dplyr::rename(
    GrowthForm = trait_value_1.2.1,
    AccSpeciesName = work_species
  )

#ifn data

# IFN species data and provinces ------------------------------------------------------------------

shrub_codes_ifn4 <- readr::read_delim(
  "data-raw/shrub_codes_ifn4.csv", delim = ";",
  escape_double = FALSE, trim_ws = TRUE
) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(IFNCODE)
  ) |>
  dplyr::rename(SP_NAME = IFNNAME) |>
  dplyr::select(SP_NAME, SP_CODE)

tree_codes_ifn4 <-  readr::read_delim(
  "data-raw/tree_codes_ifn4.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(IFNCODE)
  ) |>
  dplyr::rename(SP_NAME = IFNNAME) |>
  dplyr::select(SP_NAME, SP_CODE)


species_codes_ifn23 <- readr::read_delim(
  "data-raw/SpeciesCodesIFN23.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(IFNCODE)
  ) |>
  dplyr::rename(SP_NAME = IFNNAME) |>
  dplyr::select(SP_NAME, SP_CODE)

species_ifn_internal <- shrub_codes_ifn4 |>
  dplyr::full_join(tree_codes_ifn4, by = c("SP_NAME", "SP_CODE")) |>
  dplyr::full_join(species_codes_ifn23, by = c("SP_NAME", "SP_CODE")) |>
  dplyr::mutate(
    SP_CODE = as.numeric(SP_CODE)
  ) |>
  dplyr::distinct() |>
  dplyr::group_by(SP_CODE) |>
  dplyr::summarise(SP_NAME = dplyr::first(SP_NAME))


# ifn_plots_thesaurus -------------------------------------------------------------------------

source("data-raw/IFN_plots_thesaurus.R") # this creates ifn_plots_thesaurus object

# use internal data ---------------------------------------------------------------------------

usethis::use_data(
  fia_states_dictionary,
  fr_species_cdref,
  growth_form_lignified_france,
  ifn_provinces_dictionary,
  species_ifn_internal,
  ifn_plots_thesaurus,
  overwrite = TRUE, internal = TRUE
)

## output examples (external exported data)

# IFN (León)
future::plan(future::multisession)
ifn_output_example <- ifn_to_tibble(
  provinces = "24",
  versions = c("ifn2", "ifn3", "ifn4"),
  folder = Sys.getenv("ifn_path")
)

# FIA (Alaska)
fia_output_example <- fia_to_tibble(
  "AK",
  years = 2015:2018,
  folder = Sys.getenv("fia_path")
)

# FFI (Loire)
ffi_output_example <- ffi_to_tibble(
  "42",
  years = 2015:2018,
  folder = Sys.getenv("ffi_path")
)

usethis::use_data(
  ifn_output_example,
  fia_output_example,
  ffi_output_example,
  overwrite = TRUE, internal = FALSE
)