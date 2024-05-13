# libraries
library(esus)
library(sf)
library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(stringr)
library(furrr)
library(future)

# paths
ffi_folder <- Sys.getenv("ffi_path")
fia_folder <- Sys.getenv("fia_path")
ifn_folder <- Sys.getenv("ifn_path")

# esus:::ifn_provinces_dictionary

ne2ifn <- function(dep_name) {
  # browser()
  dep_name <- case_when(
    dep_name == "Gipuzkoa" ~ "Gipuzcoa",
    dep_name == "Lérida" ~ "Lleida",
    dep_name == "Gerona" ~ "Girona",
    dep_name == "Orense" ~ "Ourense",
    dep_name == "Almería" ~ "Almeria",
    dep_name == "La Coruña" ~ "A Coruña",
    dep_name == "Álava" ~ "Araba",
    .default = dep_name
  )
  
  res <- character()
  
  for (dep in dep_name) {
    res <- c(res, esus:::ifn_provinces_dictionary |>
      dplyr::filter(.data$province_name_original == dep) |>
      dplyr::pull(.data$province_code))
  }
  
  return(res)
}

province_polygons <- ne_states(country = "spain") |>
  select(dep_code = "iso_3166_2", dep_name = "name") |>
  mutate(
    dep_code = str_remove(dep_code, "ES-"),
    province_code = ne2ifn(dep_name)
  )


pcodes <- esus:::ifn_provinces_dictionary$province_code |> sort()

sf_list <- list()
visual_list <- list()

for (i in seq_along(pcodes)) {
  province_plots <- show_plots_from(
    "IFN", folder = ifn_folder,
    provinces = pcodes[i], versions = c("ifn2", "ifn3", "ifn4")
  ) |>
    mutate(crs = as.character(crs))
  sf_list[[i]] <- province_plots
  names(sf_list)[i] <- pcodes[i]

  pcode_name <- esus:::ifn_provinces_dictionary |>
     dplyr::filter(province_code == pcodes[i]) |>
     dplyr::pull(province_name_original)
  
  visual_test <- province_plots |>
    ggplot() +
    geom_sf(data = province_polygons |> filter(province_code == pcodes[i]), alpha = 0) +
    geom_sf(aes(geometry = geometry, color = crs)) +
    facet_wrap(vars(version)) +
    labs(title = pcodes[i], subtitle = pcode_name)
  
  visual_list[[i]] <- visual_test
  names(visual_list)[i] <- pcodes[i]
}

show_plots_from(
    "IFN", folder = ifn_folder,
    provinces = "08", versions = c("ifn2", "ifn3", "ifn4")
  )
