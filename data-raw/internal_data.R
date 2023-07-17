## FIA states Dict
fia_states_dictionary <- FIESTA::ref_statecd

# FFI growth habit data
# partimos de foo.txt que  me lo he bajado de TRY con species = bar y country = FRA

fr_species_cdref <-
  
  
  
  
  cd_ref_edit <- readr::read_delim(file = fs::path(folder, "metadonnees.csv"), skip = 412) |>
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


  

growth_form_lignified_france <- readr::read_delim("data-raw/27398.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE) |> 
  tibble::tibble() |> 
  dplyr::filter(DataName == "Plant growth form simple consolidated from GIFT") |>
  dplyr::rename(
  GrowthForm = OrigValueStr
) |>
  dplyr::filter(
    AccSpeciesName %in% fr_species_cdref
  ) |>
  dplyr::filter(grepl("shrub|tree", GrowthForm)) |>
  dplyr::mutate(Genus = stringr::word(AccSpeciesName, 1, sep = " ")) |>
  dplyr::arrange(GrowthForm,AccSpeciesName) |>
  dplyr::group_by(GrowthForm) |>
  dplyr::select(
    AccSpeciesName,
    TraitID, 
    DatasetID,
    GrowthForm,
    Genus) |>
  unique()


# use internal data
usethis::use_data(
  fia_states_dictionary,
  growth_form_lignified_france,
  overwrite = TRUE, internal = TRUE
)
