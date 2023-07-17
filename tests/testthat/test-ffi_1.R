

folder =  "D:/international_inventories_emf/data/export_dataifn_2005_2021/"
folder <- "C:/Users/a.tovar/Documents/international_inventories_emf/data/export_dataifn_2005_2021/"




subset_ifn_fr <- .read_ffi_data(
  "D:/international_inventories_emf/data/export_dataifn_2005_2021/PLACETTE.CSV"
  ) |>
  
  dplyr::group_by(DEP) |>
  dplyr::slice_sample(n = 6, replace = FALSE) |>
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


input_df <- esus::: .build_ffi_input_with(2014,filter_list,folder,.verbose = TRUE)



esus:::ffi_tables_process(2019 , filter_list, folder , .verbose = TRUE)

