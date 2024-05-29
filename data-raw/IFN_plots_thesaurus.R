
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
      forestables:::.build_ifn_file_path(province, "plot", "ifn2", ifn_path)
    }
  )
province_codes_ifn2 <- province_codes[which(!is.na(plot_paths_ifn2))]
plot_paths_ifn2 <- plot_paths_ifn2 |>
  purrr::discard(
    .p = \(path) {
      is.na(path)
    }
  ) |>
  purrr::set_names(province_codes_ifn2)

plot_paths_ifn3 <- province_codes |>
  purrr::map_chr(
    .f = \(province) {
      forestables:::.build_ifn_file_path(province, "plot", "ifn3", ifn_path)
    }
  )
province_codes_ifn3 <- province_codes[which(!is.na(plot_paths_ifn3))]
plot_paths_ifn3 <- plot_paths_ifn3 |>
  purrr::discard(
    .p = \(path) {
      is.na(path)
    }
  ) |>
  purrr::set_names(province_codes_ifn3)

plot_paths_ifn4 <- province_codes |>
  purrr::map_chr(
    .f = \(province) {
      forestables:::.build_ifn_file_path(province, "plot", "ifn4", ifn_path)
    }
  )
province_codes_ifn4 <- province_codes[which(!is.na(plot_paths_ifn4))]
plot_paths_ifn4 <- plot_paths_ifn4 |>
  purrr::discard(
    .p = \(path) {
      is.na(path)
    }
  ) |>
  purrr::set_names(province_codes_ifn4)

# ifn2 ----------------------------------------------------------------------------------------

plots_ifn2 <- plot_paths_ifn2 |>
  purrr::map(
    .f = \(path) {
      forestables:::.read_inventory_data(
        path,
        colnames = c("PROVINCIA", "ESTADILLO"),
        .dry = TRUE, .padding = FALSE,
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
      forestables:::.read_inventory_data(
        path,
        colnames = c("Estadillo", "Cla", "Subclase"),
        .dry = TRUE, .padding = FALSE,
        .ifn = TRUE
      ) |>
        tibble::as_tibble() |>
        dplyr::mutate(
          PROVINCIA = province,
          ESTADILLO = stringr::str_pad(Estadillo, width = 4, side = "left", pad = "0"),
          Subclase = forestables:::.ifn_subclass_fixer(Subclase)
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
      forestables:::.read_inventory_data(
        path,
        colnames = c("Provincia", "Estadillo", "Cla", "Subclase"),
        .dry = TRUE, .padding = FALSE,
        .ifn = TRUE
      ) |>
        tibble::as_tibble() |>
        dplyr::filter(Provincia == as.integer(province)) |>
        dplyr::mutate(
          PROVINCIA = province,
          ESTADILLO = stringr::str_pad(Estadillo, width = 4, side = "left", pad = "0"),
          Subclase = forestables:::.ifn_subclass_fixer(Subclase)
        ) |>
        dplyr::select(PROVINCIA, ESTADILLO, Cla, Subclase)
    }
  ) |>
  purrr::list_rbind() |>
  dplyr::distinct()

# Creating the dictionary -------------------------------------------------------------------------
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
# Unique ids are created by concatenation of province code (2 char), estadillo code (4 char),
# class and subclass ifn2, class and subclass ifn3 and class and subclass ifn4. Missing ifn
# classes and subclasses are filled with xx. All parts are separated from each other with
# underscores. This results in these examples:
#
# "08_0001_NN_A1_A1"
# "08_0001_NN_A1_xx"
# "08_0001_xx_NN_A1"
# "08_0001_xx_R1_11"
# ...
#
# Dictionary creation:
#
#   - Take the unique codes, add the needed variables (coords??, Clase/Subclase in IFN3 if any,
#     Clase/Subclase in IFN4 if any...)
#
#   - Return a tibble as package data with the dict, ready to use for creating the unique ID in
#     the tables functions.



## IFN3 in both, IFN2 and 3
ifn3_both <- plots_ifn3 |>
  dplyr::filter(Cla == "A", Subclase %in% c("1", "3C")) |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}_{ESTADILLO}_NN_{Cla}{Subclase}_"))

## Only in IFN3 but estadillo number in IFN2 also
ifn3_only_conflict <- plots_ifn3 |>
  dplyr::filter(Cla == "A", Subclase %in% c("3E", "4", "4C", "6C")) |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}_{ESTADILLO}_xx_{Cla}{Subclase}_"))

## Only in IFN3 no estadillo number in IFN2
ifn3_only_no_conflict <- plots_ifn3 |>
  dplyr::filter(Cla == "N") |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}_{ESTADILLO}_xx_{Cla}{Subclase}_"))

## Satellites
ifn3_satellites <- plots_ifn3 |>
  dplyr::filter(Cla == "R") |>
  dplyr::mutate(
    Subclase = stringr::str_remove(Subclase, "R$"),
    id_code = glue::glue("{PROVINCIA}_{ESTADILLO}_xx_{Cla}{Subclase}_")
  )

# Testing time!!
# Rows sum must be equal
# sum(
#   nrow(ifn3_both), nrow(ifn3_only_conflict),
#   nrow(ifn3_only_no_conflict), nrow(ifn3_satellites)
# ) == nrow(plots_ifn3)
#
# Bad formatted subclasses in IFN3
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
  dplyr::mutate(
    id_code = dplyr::if_else(
      is.na(id_code), glue::glue("{PROVINCIA}_{ESTADILLO}_NN_xx_xx"), id_code
    )
  )

ifn2_only <- ifn2_codes_temp |>
  dplyr::filter(stringr::str_detect(id_code, "_xx_xx$"))
ifn2_with_3 <- ifn2_codes_temp |>
  dplyr::filter(stringr::str_detect(id_code, "_$"))

# testing
# nrow(ifn2_only)+nrow(ifn2_with_3)==nrow(ifn2_codes_temp)
# plot 25_1955 esta en ifn3 como A1 pero no aparece en ifn2??? otras como esta???
# nrow(ifn2_with_3)==nrow(ifn3_both)
# anti_join(ifn3_both, ifn2_with_3, by = c("PROVINCIA", "ESTADILLO"))

## There are 2001 plots in IFN3 that says (A1, A3C) that exists in IFN2, but they
## don't exists in IFN2, we need to separate them, and recode them
ifn3_only_missing_ifn2 <- anti_join(
  ifn3_both, ifn2_with_3, by = c("PROVINCIA", "ESTADILLO")
) |>
  dplyr::mutate(
    id_code = stringr::str_replace(id_code, "_NN_", "_xx_")
  )

ifn3_with_2 <- anti_join(
  ifn3_both, ifn3_only_missing_ifn2, by = c("PROVINCIA", "ESTADILLO")
)

## IFN4, both 3 and 4
ifn4_both <- plots_ifn4 |>
  dplyr::filter(Cla == "A", Subclase %in% c("1")) |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}_{ESTADILLO}_??_??_{Cla}{Subclase}"))

ifn4_with_3_with_2 <- ifn4_both |>
  dplyr::inner_join(
    ifn3_with_2 |>
      dplyr::filter(Subclase != "3C"),
    by = c("PROVINCIA", "ESTADILLO")
  ) |>
  dplyr::mutate(id_code = glue::glue("{id_code.y}A1")) |>
  dplyr::select(-id_code.x, -id_code.y, -Cla.y, -Subclase.y) |>
  dplyr::rename(Cla = Cla.x, Subclase = Subclase.x)

ifn3_with_4_with_2 <- ifn4_both |>
  dplyr::inner_join(
    ifn3_with_2 |>
      dplyr::filter(Subclase != "3C"),
    by = c("PROVINCIA", "ESTADILLO")
  ) |>
  dplyr::mutate(id_code = glue::glue("{id_code.y}A1")) |>
  dplyr::select(-id_code.x, -id_code.y, -Cla.x, -Subclase.x) |>
  dplyr::rename(Cla = Cla.y, Subclase = Subclase.y)

ifn3_with_2 <- anti_join(
  ifn3_with_2, ifn3_with_4_with_2,
  by = c("PROVINCIA", "ESTADILLO", "Cla", "Subclase")
) |>
  dplyr::mutate(id_code = glue::glue("{id_code}xx"))

ifn2_with_3_with_4 <- ifn4_with_3_with_2 |>
  dplyr::select(-Cla, -Subclase)


ifn4_only_missing_ifn3 <- ifn4_both |>
  # remove the ones in the three inventories
  dplyr::anti_join(ifn4_with_3_with_2, by = c("PROVINCIA", "ESTADILLO")) |>
  # add the ifn3
  # here we have to add the ifn3 A3E, but not the ifn3 A3C
  dplyr::left_join(
    purrr::list_rbind(
      list(
        ifn3_only_conflict,
        ifn3_only_no_conflict,
        ifn3_only_missing_ifn2 |> dplyr::filter(Subclase != "3C")
      )
    ),
    by = c("PROVINCIA", "ESTADILLO")
  ) |>
  filter(is.na(Cla.y)) |>
  dplyr::select(-dplyr::ends_with(".y")) |>
  dplyr::rename(Cla = Cla.x, Subclase = Subclase.x, id_code = id_code.x) |>
  dplyr::mutate(id_code = stringr::str_replace_all(id_code, "_\\?\\?", "_xx"))

ifn4_with_3 <- ifn4_both |>
  # remove the ones in the three inventories
  dplyr::anti_join(ifn4_with_3_with_2, by = c("PROVINCIA", "ESTADILLO")) |>
  dplyr::anti_join(ifn4_only_missing_ifn3, by = c("PROVINCIA", "ESTADILLO")) |>
  # add the ifn3
  # here we have to add the ifn3 A3E, but not the ifn3 A3C
  dplyr::left_join(
    purrr::list_rbind(
      list(
        ifn3_only_conflict,
        ifn3_only_no_conflict,
        ifn3_only_missing_ifn2 |> dplyr::filter(Subclase != "3C")
      )
    ),
    by = c("PROVINCIA", "ESTADILLO")
  ) |>
  dplyr::mutate(id_code = glue::glue("{id_code.y}{Cla.x}{Subclase.x}")) |>
  dplyr::select(-id_code.x, -id_code.y, -Cla.y, -Subclase.y) |>
  dplyr::rename(Cla = Cla.x, Subclase = Subclase.x)

ifn3_with_4 <- ifn4_both |>
  # remove the ones in the three inventories
  dplyr::anti_join(ifn4_with_3_with_2, by = c("PROVINCIA", "ESTADILLO")) |>
  dplyr::anti_join(ifn4_only_missing_ifn3, by = c("PROVINCIA", "ESTADILLO")) |>
  # add the ifn3
  # here we have to add the ifn3 A3E, but not the ifn3 A3C
  dplyr::left_join(
    purrr::list_rbind(
      list(
        ifn3_only_conflict,
        ifn3_only_no_conflict,
        ifn3_only_missing_ifn2 |> dplyr::filter(Subclase != "3C")
      )
    ),
    by = c("PROVINCIA", "ESTADILLO")
  ) |>
  dplyr::mutate(id_code = glue::glue("{id_code.y}{Cla.x}{Subclase.x}")) |>
  dplyr::select(-id_code.x, -id_code.y, -Cla.x, -Subclase.x) |>
  dplyr::rename(Cla = Cla.y, Subclase = Subclase.y)

# testing
# nrow(ifn4_with_3_with_2)+nrow(ifn4_with_3)==nrow(ifn4_both)
# nrow(ifn3_with_4_with_2)+nrow(ifn3_with_4)==nrow(ifn4_both)


ifn4_sat_rev <- plots_ifn4 |>
  dplyr::filter(Cla %in% c("1", "2", "3", "4")) |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}_{ESTADILLO}_??_??_{Cla}{Subclase}"))

ifn3_satellites <- ifn3_satellites |>
  # dplyr::select(-Cla) |>
  dplyr::left_join(
    ifn4_sat_rev |>
      dplyr::filter(Subclase == "1"),
    by = c("PROVINCIA", "ESTADILLO", "Subclase" = "Cla")
  ) |>
  dplyr::mutate(
    id_code = dplyr::if_else(
      is.na(id_code.y), glue::glue("{id_code.x}xx"), glue::glue("{id_code.x}{Subclase}1")
    ),
    Cla.y = dplyr::if_else(is.na(id_code.y), NA_character_, glue::glue("{Subclase}"))
  ) |>
  dplyr::select(-id_code.x, -id_code.y)

ifn3_only_satellites <- ifn3_satellites |>
  dplyr::filter(is.na(Cla.y)) |>
  dplyr::select(-dplyr::ends_with(".y"))
ifn3_with_4_satellites <- ifn3_satellites |>
  dplyr::filter(!is.na(Cla.y)) |>
  dplyr::select(-dplyr::ends_with(".y"))
ifn4_with_3_satellites <- ifn3_satellites |>
  dplyr::filter(!is.na(Cla.y)) |>
  dplyr::select(-Cla, -Subclase) |>
  dplyr::rename(Cla = Cla.y, Subclase = Subclase.y)

# nrow(ifn3_only_satellites) + nrow(ifn3_with_4_satellites) == nrow(ifn3_satellites)

ifn4_only_satellites <- ifn4_sat_rev |>
  dplyr::filter(Subclase != "1") |>
  dplyr::mutate(id_code = stringr::str_replace_all(id_code, "_\\?\\?", "_xx"))

ifn4_with_3_satellites <- ifn4_sat_rev |>
  dplyr::inner_join(ifn4_with_3_satellites, by = c("PROVINCIA", "ESTADILLO", "Cla", "Subclase")) |>
  dplyr::mutate(id_code = id_code.y) |>
  dplyr::select(-id_code.x, -dplyr::ends_with(".y"))

# nrow(ifn4_only_satellites) + nrow(ifn4_with_3_satellites) == nrow(ifn4_sat_rev)

## Only in IFN4 but estadillo number in IFN2 also
ifn4_only_conflict <- plots_ifn4 |>
  dplyr::filter(Cla == "A", Subclase %in% c("4", "4C", "6C")) |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}_{ESTADILLO}_xx_xx_{Cla}{Subclase}"))

## Only in IFN4 no estadillo number in IFN2
ifn4_only_no_conflict <- plots_ifn4 |>
  dplyr::filter(Cla == "N") |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}_{ESTADILLO}_xx_xx_{Cla}{Subclase}"))

## Satellites
ifn4_only_support <- plots_ifn4 |>
  dplyr::filter(Cla == "R") |>
  dplyr::mutate(id_code = glue::glue("{PROVINCIA}_{ESTADILLO}_xx_xx_{Cla}{Subclase}"))

ifn4_only <- list(
  ifn4_only_satellites, ifn4_only_conflict, ifn4_only_no_conflict, ifn4_only_support
) |>
  purrr::list_rbind()

ifn3_only <- list(
  ifn3_only_conflict, ifn3_only_missing_ifn2, ifn3_only_no_conflict, ifn3_only_satellites
) |>
  purrr::list_rbind() |>
  dplyr::anti_join(ifn3_with_4, by = c("PROVINCIA", "ESTADILLO", "Cla", "Subclase")) |>
  dplyr::mutate(id_code = glue::glue("{id_code}xx"))

# ## Testing time!!
#
# nrow(ifn4_with_3) + nrow(ifn4_with_3_with_2) + nrow(ifn4_only) +
#   nrow(ifn4_with_3_satellites) == nrow(plots_ifn4)

ifn4_plots_processed <- list(
  ifn4_with_3,
  ifn4_with_3_with_2,
  ifn4_only, ifn4_only_missing_ifn3,
  ifn4_with_3_satellites
) |>
  purrr::list_rbind() |>
  # dplyr::mutate(ifn4_class = glue::glue("{Cla}{Subclase}")) |>
  dplyr::select(id_code, PROVINCIA, ESTADILLO)

# nrow(ifn4_plots_processed) == nrow(plots_ifn4)
# sum(is.na(ifn4_plots_processed$id_code)) < 1
# sum(stringr::str_detect(ifn4_plots_processed$id_code, "\\?")) < 1

ifn3_plots_processed <- list(
  ifn3_only,
  ifn3_with_2, ifn3_with_4_satellites,
  ifn3_with_4, ifn3_with_4_with_2
) |>
  purrr::list_rbind() |>
  dplyr::arrange(PROVINCIA, ESTADILLO) |>
  # dplyr::mutate(ifn3_class = glue::glue("{Cla}{Subclase}")) |>
  dplyr::select(id_code, PROVINCIA, ESTADILLO)

# nrow(ifn3_plots_processed) == nrow(plots_ifn3)
# sum(is.na(ifn3_plots_processed$id_code)) < 1
# sum(stringr::str_detect(ifn3_plots_processed$id_code, "\\?")) < 1

ifn2_with_3 <- ifn2_with_3 |>
  dplyr::anti_join(ifn2_with_3_with_4, by = c("PROVINCIA", "ESTADILLO")) |>
  dplyr::mutate(id_code = glue::glue("{id_code}xx"))


ifn2_plots_processed <- list(
  ifn2_only, ifn2_with_3, ifn2_with_3_with_4
) |>
  purrr::list_rbind() |>
  dplyr::arrange(PROVINCIA, ESTADILLO) |>
  # dplyr::mutate(ifn2_class = glue::glue("{NA_character_}")) |>
  dplyr::select(id_code, PROVINCIA, ESTADILLO)

# nrow(ifn2_plots_processed) == nrow(plots_ifn2)
# sum(is.na(ifn2_plots_processed$id_code)) < 1
# sum(stringr::str_detect(ifn2_plots_processed$id_code, "\\?")) < 1


# Thesaurus object ----------------------------------------------------------------------------

ifn_plots_thesaurus <- ifn2_plots_processed |>
  dplyr::full_join(ifn3_plots_processed) |>
  dplyr::full_join(ifn4_plots_processed) |>
  dplyr::arrange(PROVINCIA, ESTADILLO) |>
  dplyr::mutate(
    class_ifn2 = stringr::str_split_i(id_code, "_", 3),
    class_ifn3 = stringr::str_split_i(id_code, "_", 4),
    class_ifn4 = stringr::str_split_i(id_code, "_", 5)
  ) |>
  ### CLEANING
  # Asturias 9999 plot
  dplyr::filter(ESTADILLO != "9999") |>
  # Due to the process some plots has "xxxx" in class_ifn4, substitute with xx
  dplyr::mutate(
    id_code = stringr::str_replace(id_code, "xxxx", "xx"),
    class_ifn4 = stringr::str_replace(class_ifn4, "xxxx", "xx")
  )


rm(list = c(
  "ifn_path", "ifn2_codes_temp", "ifn2_only", "ifn2_plots_processed", "ifn2_with_3",
  "ifn2_with_3_with_4", "ifn3_both", "ifn3_only", "ifn3_only_conflict",
  "ifn3_only_missing_ifn2", "ifn3_only_no_conflict", "ifn3_only_satellites",
  "ifn3_plots_processed", "ifn3_satellites", "ifn3_with_2", "ifn3_with_4",
  "ifn3_with_4_satellites", "ifn3_with_4_with_2", "ifn4_both", "ifn4_only",
  "ifn4_only_conflict", "ifn4_only_missing_ifn3", "ifn4_only_no_conflict",
  "ifn4_only_satellites", "ifn4_only_support", "ifn4_plots_processed", "ifn4_sat_rev",
  "ifn4_with_3", "ifn4_with_3_satellites", "ifn4_with_3_with_2", "plot_paths_ifn2",
  "plot_paths_ifn3", "plot_paths_ifn4", "plots_ifn2", "plots_ifn3", "plots_ifn4",
  "province_codes", "province_codes_ifn2", "province_codes_ifn3", "province_codes_ifn4"
))
gc()
