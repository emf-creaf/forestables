
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



filter_list<-list(
  "02" = list("932915", "1433309" ),
  "50" = list("1422499","23087"),
  "54" = list("973611",  "1415457"))

# example_input

# example_input <- tibble::tibble(
#   
#   
#   plots = subset_ifn_fr$plot,
#   
#   department = subset_ifn_fr$department,
#   
#   plot_table = "data/export_dataifn_2005_2021/PLACETTE.csv",
#   
#   tree_table = "data/export_dataifn_2005_2021/ARBRE.csv",
#   
#   shrub_table =  "data/export_dataifn_2005_2021/FLORE.csv",
#   
#   soil_table = "data/export_dataifn_2005_2021/ECOLOGIE.csv"
#   
# )
# 
# fr_example_input<-unique(example_input)
# 
# #save(fr_example_input, file = "examples/fr_example_input.RData")
# 
# 
# 
# 




