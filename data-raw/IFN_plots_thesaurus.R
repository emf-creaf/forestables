
# libraries -----------------------------------------------------------------------------------
library(stringr)
library(purrr)
library(tibble)
library(dplyr)

# data & paths --------------------------------------------------------------------------------

ifn_path <- Sys.getenv("ifn_path")

province_codes <- c(1:50) |>
  as.character() |>
  stringr::str_pad(width = 2, side = "left", pad = "0")

plot_paths_ifn2 <- province_codes |>
  purrr::map_chr(
    .f = \(province) {
      esus:::.build_ifn_file_path(province, "plot", "ifn2", ifn_path)
    }
  )
province_codes_ifn2 <- province_codes[which(!is.na(plot_paths_ifn2))]
plot_paths_ifn2 <- plot_paths_ifn2 |>
  purrr::discard(.p = \(path) {is.na(path)}) |>
  purrr::set_names(province_codes_ifn2)

plot_paths_ifn3 <- province_codes |>
  purrr::map_chr(
    .f = \(province) {
      esus:::.build_ifn_file_path(province, "plot", "ifn3", ifn_path)
    }
  )
province_codes_ifn3 <- province_codes[which(!is.na(plot_paths_ifn3))]
plot_paths_ifn3 <- plot_paths_ifn3 |>
  purrr::discard(.p = \(path) {is.na(path)}) |>
  purrr::set_names(province_codes_ifn3)

plot_paths_ifn4 <- province_codes |>
  purrr::map_chr(
    .f = \(province) {
      esus:::.build_ifn_file_path(province, "plot", "ifn4", ifn_path)
    }
  )
province_codes_ifn4 <- province_codes[which(!is.na(plot_paths_ifn4))]
plot_paths_ifn4 <- plot_paths_ifn4 |>
  purrr::discard(.p = \(path) {is.na(path)}) |>
  purrr::set_names(province_codes_ifn4)

# ifn2 ----------------------------------------------------------------------------------------

plots_ifn2 <- plot_paths_ifn2 |>
  purrr::map(
    .f = \(path) {
      esus:::.read_inventory_data(
        path,
        colnames = c("PROVINCIA", "ESTADILLO"),
        .ifn = TRUE
      ) |>
        tibble::as_tibble() |>
        dplyr::mutate(
          ESTADILLO = stringr::str_pad(ESTADILLO, width = 4, side = "left", pad = "0")
        )
    }
  ) |>
  purrr::list_rbind() |>
  dplyr::distinct()

# ifn3 ----------------------------------------------------------------------------------------

plots_ifn3 <- plot_paths_ifn3 |>
  purrr::imap(
    .f = \(path, province) {
      esus:::.read_inventory_data(
        path,
        colnames = c("Estadillo", "Cla", "Subclase"),
        .ifn = TRUE
      ) |>
        tibble::as_tibble() |>
        dplyr::mutate(
          PROVINCIA = province,
          ESTADILLO = stringr::str_pad(Estadillo, width = 4, side = "left", pad = "0"),
          Subclase = esus:::.ifn_subclass_fixer(Subclase)
        ) |>
        dplyr::select(PROVINCIA, ESTADILLO, Cla, Subclase)
    }
  ) |>
  purrr::list_rbind() |>
  dplyr::distinct()

# ifn4 ----------------------------------------------------------------------------------------

plots_ifn4 <- plot_paths_ifn4 |>
  purrr::imap(
    .f = \(path, province) {
      esus:::.read_inventory_data(
        path,
        colnames = c("Provincia", "Estadillo", "Cla", "Subclase"),
        .ifn = TRUE
      ) |>
        tibble::as_tibble() |>
        dplyr::filter(Provincia == as.integer(province)) |>
        dplyr::mutate(
          PROVINCIA = province,
          ESTADILLO = stringr::str_pad(Estadillo, width = 4, side = "left", pad = "0")
        ) |>
        dplyr::select(PROVINCIA, ESTADILLO, Cla, Subclase)
    }
  ) |>
  purrr::list_rbind() |>
  dplyr::distinct()

# Creating the dictionary -----------------------------------------------------------------------------
# The process is as follows, each IFN version is broken in parts:
#   - IFN2:
#       + ifn2_only: Plots only in IFN2
#       + ifn2_with_3: Plots in both IFN2 and IFN3, but not in IFN4
#       + ifn2_with_3_with_4: Plots in all IFN
#       + (There is no ifn2_with_4 because is not possible)
#
#   - IFN3:
#       + ifn3_only: Plots only in IFN3
#       + ifn3_with_2: Plots in both, IFN3 and IFN2, but not IFN4
#       + ifn3_with_4: Plots in both IFN3 and IFN4, but not in IFN2
#       + ifn3_with_2_with_4: Plots in all IFN
#
#   - IFN4:
#       + ifn4_only: Plots only in IFN4
#       + ifn4_with_3: Plots in both IFN4 and IFN3, but not in IFN2
#       + ifn4_with_3_with_2: Plots in all IFN
#       + (There is no ifn4_with_2 because is not possible)
#
# Checks are as follows:
#
#    - ifn2_with_3 and ifn3_with_2 should be the same
#    - ifn2_with_3_with_4, ifn3_with_2_with_4 and ifn4_with_3_with_2 should be the same
#    - ifn3_with_4 and ifn4_with_3 should be the same
#
#    - the bind rows of ifn2* should be the same as plots_ifn2
#    - the bind rows of ifn3* should be the same as plots_ifn3
#    - the bind rows of ifn4* should be the same as plots_ifn4
#
#    - others
#
# Unique ID
#
# Unique id is created by concatenation of province code (with two numbers, so leading 0 for the
# first nine), estadillo (plot) number, ifn versions, i.e.:
#
# "080001_234"
# "080002_2"
#
# # same base code, but different versions, indicating that is not the same plot in ifn2 than in 3
# # and 4:
# "080003_2"
# "080003_34"
#
# Dictionary creation:
#
#   - Take the unique codes, add the needed variables (coords??, Clase/Subclase in IFN3 if any,
#     Clase/Subclase in IFN4 if any...)
#
#   - Return a tibble as package data with the dict, ready to use for creating the unique ID in
#     the tables functions.



## IFN3 in both
ifn3_both <- plots_ifn3 |>
  dplyr::filter(Cla == "A", Subclase %in% c("1", "3C")) |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}{ESTADILLO}_23"))

## Only in IFN3 but estadillo number in IFN2 also
ifn3_only_conflict <- plots_ifn3 |>
  dplyr::filter(Cla == "A", Subclase %in% c("3E", "4", "4C", "6C")) |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}{ESTADILLO}_3"))

## Only in IFN3 no estadillo number in IFN2
ifn3_only_no_conflict <- plots_ifn3 |>
  dplyr::filter(Cla == "N") |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}{ESTADILLO}_3"))

## Satellites
ifn3_satellites <- plots_ifn3 |>
  dplyr::filter(Cla == "R") |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}{ESTADILLO}_3_R"))

# ## Testing time!!
# # Rows sum must be equal
# sum(nrow(ifn3_both), nrow(ifn3_only_conflict), nrow(ifn3_only_no_conflict), nrow(ifn3_satellites)) ==
#   nrow(plots_ifn3)
#
# # Bad formatted subclasses in IFN3
# plots_ifn3$Subclase |> unique()

## Now let's create the id code for IFN2 based on the ones in IFN3
ifn2_codes_temp <- ifn3_both |>
  ## individual fixes
  dplyr::mutate(
    ESTADILLO = dplyr::case_when(
      ESTADILLO == "0228" & PROVINCIA == "04" & Cla == "A" & Subclase == "3C" ~ "0229",
      .default = ESTADILLO
    )
  ) |>
  ## rest of the process
  dplyr::select(-Cla, -Subclase) |>
  # right join the ifn2 plots (right join to get all ifn2 plots, with id_code NA for those not in
  # IFN3)
  dplyr::right_join(plots_ifn2) |>
  # if id_code is NA, then they are only present in 2
  dplyr::mutate(id_code = dplyr::if_else(is.na(id_code), glue::glue("{PROVINCIA}{ESTADILLO}_2"), id_code))

ifn2_only <- ifn2_codes_temp |>
  dplyr::filter(stringr::str_detect(id_code, "_2$"))
ifn2_with_others_temp <- ifn2_codes_temp |>
  dplyr::filter(stringr::str_detect(id_code, "_23$"))
