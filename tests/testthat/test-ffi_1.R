# 
# 
# folder =  "D:/international_inventories_emf/data/export_dataifn_2005_2021/"
# folder <- "C:/Users/a.tovar/Documents/international_inventories_emf/data/export_dataifn_2005_2021/"
# 
# 
# 
# 
# subset_ifn_fr <- .read_ffi_data(
#   "D:/international_inventories_emf/data/export_dataifn_2005_2021/PLACETTE.CSV"
#   ) |>
#   
#   dplyr::group_by(DEP) |>
#   dplyr::slice_sample(n = 6, replace = FALSE) |>
#    dplyr::filter(CAMPAGNE == 2019) |>
#   dplyr::rename(
#     plot = IDP,
#     dep = DEP, 
#     year = CAMPAGNE) |> 
#   dplyr::select(
#     dep,
#     plot
#   ) |>
#   dplyr::as_tibble() |> 
#   
#   dplyr::group_by(dep) |>
#   dplyr::summarise(plot = list(as.character(plot))) |>
#    tibble::deframe() -> filter_list
#  


# 
# filter_list <- list(
#   "02" = c("932915", "1433309"),
#   "50" = c("1422499","23087"),
#   "54" = c("973611",  "1415457"))


# input_df <- esus::: .build_ffi_input_with(2014,filter_list,folder,.verbose = TRUE)
# 
# 
# 
# esus:::ffi_tables_process(2019 , filter_list, folder , .verbose = TRUE)
# 
# esus:::show_plots_from_ffi(dep = c(1:12),folder, .call = rlang::caller_env())
# 



test_that(".build_ffi_input_with and .build_ffi_file_path work as intended", {
  test_plots <- list(
      "01" = 1404119,
      "10" = 900863,
      "11" = c(1436508, 1410492),
      "17" = 1416895,
      "19" = 1407238,
      "27" = c(960830, 923517),
      "2A" = 973917,
      "32" = 1425386,
      "34" = 977193,
      "35" = 1404830,
      "36" = 1438616,
      "39" = 1424577,
      "42" = c(1422347, 939340),
      "44" = 920801,
      "51" = 938482,
      "59" = 960829,
      "64" = 912307,
      "76" = 951430,
      "80" = c(1417044,1452529),
      "81" = c(1428398, 973950),
      "86" = c(957495,921133),
      "221" = 20084,
      "88" = 20012,
      "89" = 1433956,
      "91" = 1406115,
    "tururu" = 3555
    )
  
  test_year <- 2019
  test_states <- names(test_plots)
  test_folder <- "D:/international_inventories_emf/data/export_dataifn_2005_2021/"
  expected_names <- c(
    "dep", 
    "plots",
    "plot_table",
    "tree_table",
    "shrub_table",
    "soils_table"
  )
  
  # warnings and messages
  expect_warning(
    test_res <- .build_ffi_input_with(test_year, test_states, test_plots, test_folder, .verbose = TRUE),
    "file doesn't exists"
  )
  
})
