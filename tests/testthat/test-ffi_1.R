

folder =  "D:/international_inventories_emf/data/export_dataifn_2005_2021/"
folder <- "C:/Users/a.tovar/Documents/international_inventories_emf/data/export_dataifn_2005_2021/"


subset_ifn_fr <- .read_ffi_data(
  "C:/Users/a.tovar/Documents/international_inventories_emf/data/export_dataifn_2005_2021/PLACETTE.CSV"
  ) |>
  
  dplyr::group_by(DEP) |>
  slice_sample(n = 2, replace = FALSE) |>
   dplyr::filter(CAMPAGNE == 2019) |>
  dplyr::rename(
    plot = IDP,
    dep = DEP, 
    year = CAMPAGNE) |> 
  dplyr::select(
    dep,
    plot
  ) |>
  dplyr::as_tibble() |> 
  
  dplyr::group_by(dep) |>
  dplyr::summarise(plot = list(as.character(plot))) |>
   tibble::deframe() -> filter_list
 


# 
# filter_list <- list(
#   "02" = c("932915", "1433309"),
#   "50" = c("1422499","23087"),
#   "54" = c("973611",  "1415457"))


input_df <- .build_ffi_input_with(2014,filter_list,folder,.verbose = TRUE)


ffi_plot_table_process(plot_data = input_df[["plot_table"]][1], soil_data = input_df[["soils_table"]][1], plot = input_df[["plots"]][1],2010)

ffi_tree_table_process(tree_data = input_df[["tree_table"]][1], plot, 2014, espar_cdref13 , espar_ref)

ffi_shrub_table_process(shrub_data = input_df[["shrub_table"]][1], plot, 2014,cd_ref, growth_form_lignified_france)


esus:::ffi_tables_process(2014 , filter_list, folder , .verbose = TRUE)

