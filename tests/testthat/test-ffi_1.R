
# placette_FR<-transform_data_delim("C:/Users/a.tovar/Documents/international_inventories_emf/data/export_dataifn_2005_2021/PLACETTE.csv",delim = ";")
# 
# 
# subset_ifn_fr<-placette_FR|>
#   
#   dplyr::group_by(DEP)|>
#   slice_sample(n = 5, replace = FALSE)|>
#   dplyr::filter(CAMPAGNE==2019)|>
#   dplyr::select(
#     plot= IDP,
#     department = DEP, 
#     year= CAMPAGNE)|> 
#   dplyr::arrange(department)|>
#   tibble::tibble()


folder =  "D:/international_inventories_emf/data/export_dataifn_2005_2021/"
folder<- "C:/Users/a.tovar/Documents/international_inventories_emf/data/export_dataifn_2005_2021/"
filter_list<-list(
  "02" = c("932915", "1433309"),
  "50" = c("1422499","23087"),
  "54" = c("973611",  "1415457"))


input_df<-.build_ffi_input_with(2014,filter_list,folder,.verbose = TRUE)


ffi_plot_table_process(plot_data = input_df[["plot_table"]][1], soil_data =input_df[["soils_table"]][1], plot =input_df[["plots"]][1],2010)

ffi_tree_table_process(tree_data= input_df[["tree_table"]][1], plot, 2014, espar_cdref13 , espar_ref)

ffi_shrub_table_process(shrub_data=input_df[["shrub_table"]][1], plot, 2014,cd_ref, growth_form_lignified_france)


esus:::ffi_tables_process(2014 , filter_list, folder , .verbose =TRUE)

