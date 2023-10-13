## FIA states Dict
fia_states_dictionary <- FIESTA::ref_statecd

# FFI growth habit data
# Usaremos GIFT con su paquete de R para acceder a los datos de growth form
cd_ref_edit <- readr::read_delim(
  file = fs::path(Sys.getenv("ffi_path"), "metadonnees.csv"),
  skip = 412
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
    SP_NAME = ifelse(is.na(species) | species == "", genus, paste(genus, species, sep = " "))) |>
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


# growth_form_lignified_france <- readr::read_delim("data-raw/27398.txt",
#                        delim = "\t", escape_double = FALSE,
#                        trim_ws = TRUE) |>
#   tibble::tibble() |>
#   dplyr::filter(DataName == "Plant growth form simple consolidated from GIFT") |>
#   dplyr::rename(
#   GrowthForm = OrigValueStr
# ) |>
#   dplyr::filter(
#     AccSpeciesName %in% fr_species_cdref
#   ) |>
#   dplyr::filter(grepl("shrub|tree", GrowthForm)) |>
#   dplyr::mutate(Genus = stringr::word(AccSpeciesName, 1, sep = " ")) |>
#   dplyr::arrange(GrowthForm,AccSpeciesName) |>
#   dplyr::group_by(GrowthForm) |>
#   dplyr::select(
#     AccSpeciesName,
#     TraitID,
#     DatasetID,
#     GrowthForm,
#     Genus) |>
#   unique()

# use internal data
usethis::use_data(
  fia_states_dictionary,
  growth_form_lignified_france,
  overwrite = TRUE, internal = TRUE
)
