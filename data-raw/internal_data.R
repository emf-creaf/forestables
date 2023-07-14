## FIA states Dict
fia_states_dictionary <- FIESTA::ref_statecd

# FFI growth habit data
# partimos de foo.txt que  me lo he bajado de TRY con species = bar y country = FRA

fr_species_cdref <-
#   
#   cd_ref<- readxl::read_excel("data/CD_REF.xlsx")|>
#   tibble::tibble()
# 
# 
# cd_ref_edit<-cd_ref|>
#   dplyr::select(cd_ref, lib_cdref)|>
#   dplyr::rename( 
#     full_name = lib_cdref
#   )|>
#   
#   #Add genus and species
#   dplyr::mutate(
#     genus = str_extract(full_name, "\\b\\w+\\b"),
#     species = str_extract(full_name, "(?<=\\s)\\w+"),
#     species = if_else(str_detect(species, "^\\("), "", species),
#     SP_NAME = ifelse(is.na(species) | species == "", genus, paste(genus, species, sep = " ")))|>
#   arrange(full_name)|>
#   
#   group_by(genus)
# 
# #FINAL VECTOR
# fr_species_cdref<- cd_ref_edit$SP_NAME


  

TRY_3400 <- readr::read_delim("C:/Users/a.tovar/Documents/esus/data-raw/27398.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)|> tibble::tibble() |> 
  dplyr::filter(DataName == "Plant growth form simple consolidated from GIFT")|>
  dplyr::rename(
  GrowthForm = OrigValueStr
) |>
  dplyr::filter(
    AccSpeciesName %in% fr_species_cdref
  ) |>
  dplyr::filter(grepl("shrub|tree", GrowthForm)) |>
  dplyr::mutate(Genus = word(AccSpeciesName, 1, sep = " ")) |>
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
  ffi_growth_habit,
  overwrite = TRUE, internal = TRUE
)
