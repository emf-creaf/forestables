
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
    "87" = c(975666,979897),
    "89" = 1433956,
    "91" = 1406115,
    "tururu" = 3555
  )
  
  test_year <- 2019
  test_departments <- names(test_plots)
  # test_folder <- "D:/international_inventories_emf/data/export_dataifn_2005_2021/"
  test_folder <- "C:/international_inventories_emf/data/export_dataifn_2005_2021/"
  expected_names <- c(
    "department", 
    "plots",
    "plot_table",
    "tree_table",
    "shrub_table",
    "soils_table"
  )
  
  # warnings and messages
  
  
   expect_warning(
    test_res <-  .build_ffi_input_with(test_departments,test_year, test_plots, ".", .verbose = TRUE),
    "file doesn't exist"
   )
  expect_message(
     .build_ffi_input_with(test_departments, test_year,  test_plots[-26], test_folder, .verbose = TRUE),
    "Getting ready to retrieve"
  )
  expect_no_message(
     .build_ffi_input_with(test_departmets, test_year , test_plots[-26], test_folder, .verbose = FALSE)
  )
  
  ## result tests
  #
  test_res <-  .build_ffi_input_with(test_departments,test_year, test_plots, test_folder, .verbose = TRUE)
  # we expect a tibble
  expect_s3_class(test_res, "tbl")
  # with the correct names
  expect_named(test_res, expected_names)
  # and for 31 plots as per the filter list we create
  expect_true(nrow(test_res) == 32L)
  # and for the correct counties
  expect_identical(
    unique(test_res[["department"]]) |> sort(),
    names(test_plots) |> sort()
    
  )
  
  
  #   # and correct_plots
  
  expect_identical(
    unique(test_res[["plots"]]) |> 
      unlist() |> 
      sort(),
    unique(test_plots) |> 
      unlist() |> 
      as.character() |> 
      sort()
    
  )
  
  
  # we can test here also if .build_fia_file_path works
  # .build_fia_file_path
  # a correct one
  expect_identical(
    as.character(test_res[["plot_table"]][1]),
    paste0(test_folder, "PLACETTE.csv")
  )
  #  # a correct custom one
    expect_identical(
      as.character(test_res[["plot_table"]][1]),
     paste0('grep -E "CAMPAGNE|.*;1404119;.*;01;" ', test_folder, "PLACETTE.csv")
  )
  # # # an incorrect one --< no funciona
   expect_identical(
     test_res[["plot_table"]][32], NA_character_
   )
})

 test_that(".read_inventory_data returns lazy_dt", {

 test_file <- fs::path("C:/international_inventories_emf/data/export_dataifn_2005_2021/PLACETTE.csv")
 test_cmd <- glue::glue('grep -E "CAMPAGNE|.*;900863;.*;10;" {test_file}')
 
 #ecologie table
 test_file <- fs::path("C:/international_inventories_emf/data/export_dataifn_2005_2021/ECOLOGIE.csv")
 test_cmd <- glue::glue('grep -E "CAMPAGNE|.*;900863;" {test_file}')
 
 #flore arbre table
 test_file<- fs::path("C:/international_inventories_emf/data/export_dataifn_2005_2021/FLORE.csv")
 test_cmd <- glue::glue('grep -E "CAMPAGNE|.*;900863;" {test_file}' )
 
 
# 
  expect_s3_class( .read_inventory_data(test_file, header = TRUE), "dtplyr_step_first")
  expect_s3_class( .read_inventory_data(test_cmd,header = TRUE), "dtplyr_step_first")
 })
# 
test_that(".get_plots_from_departments works as intended", {
  test_folder <- "C:/international_inventories_emf/data/export_dataifn_2005_2021/"
  test_departments <- c("01", "10", "11")
  
  # error
  expect_error(
    suppressWarnings( .get_plots_from_departments(test_departments[1], ".")),
    "folder doesn't contain"
  )
  ## results are ok
  # class
  expect_s3_class(test_res_ok <-  .get_plots_from_departments(test_departments[1], test_folder), "sf")
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(test_res_ok,c("CAMPAGNE", "IDP", "DEP", "geometry"))
  # expect rows
  expect_true(
    nrow(test_res_ok) > 0
  )
  
})

#
test_that("show_plots_from_ffi works as intended", {
  test_folder <- "C:/international_inventories_emf/data/export_dataifn_2005_2021/"
  test_departments <- c("01", "10", "11")
  
  # error
  expect_error(
    suppressWarnings( show_plots_from_ffi(  test_departments[1], ".",)),
    "folder doesn't contain"
  )
  
  ## results are ok
  # class
  expect_s3_class(test_res_ok <-  show_plots_from_ffi( test_departments, test_folder), "sf")
  # crs
  expect_identical(sf::st_crs(test_res_ok), sf::st_crs(4326))
  # names
  expect_named(test_res_ok, c("CAMPAGNE", "IDP", "DEP", "geometry"))
  # expect rows
  expect_true(
    nrow(test_res_ok) > 0
  )
  # we must have 3 states
  expect_identical(
    test_res_ok$DEP |> unique() |> length(), 3L
  )
})



test_that(".transform_plot_summary_ffi works as intended", {
  test_folder <- "C:/international_inventories_emf/data/export_dataifn_2005_2021/"
  test_departments <- c("01", "10", "11")
  test_summary <-  show_plots_from_ffi(test_departments, test_folder)
  test_years <- c(2005, 2010, 2015)
  
  # correct object
  expect_type(
    test_res_1 <- .transform_plot_summary_ffi(test_summary, test_years[1], test_departments[1]),
    "list"
  )
  # correct names
  expect_named(test_res_1,  c("01", "10", "11"))
  # expect results
  expect_length(test_res_1, 3)
  expect_true(length(test_res_1[[1]]) > 1)
})

