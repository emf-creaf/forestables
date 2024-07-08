
# forestables

[![R-CMD-check](https://github.com/emf-creaf/forestables/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emf-creaf/forestables/actions/workflows/R-CMD-check.yaml)

Read and uniformize forest inventories data from the FIA (USA forest
inventory), FFI (France forest inventory) and IFN (Spain forest
inventory).

## Installation

You can install the development version of forestables from
[GitHub](https://github.com/emf-creaf/forestables) with:

``` r
# install.packages("devtools")
devtools::install_github("emf-creaf/forestables")
```

## Inventories files

`forestables` offers download functions to get the inventory files from
the official websites from each inventory:

- [FIA](https://www.fs.usda.gov/research/products/dataandtools/tools/fia-datamart)  
- [FFI](https://inventaire-forestier.ign.fr/dataifn/?lang=en)  
- IFN:
  - [IFN2](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_descargas.html)
  - [IFN3](https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_bbdd_descargas_htm.html)
    (*accdb* are be used)
  - [IFN4](https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/cuarto_inventario.html)

This sometimes fails due to the pages being unavailable for one reason
or another. We recommend to manually download and unzip the data in the
desired folder.  
Download functions are offered as helpers for programmatically
approaches, but users should be careful as data can be missing due to
connection errors, pages updates…

## Example

Download FFI (France forest inventory) and extract all plots in the
Loire department from 2015:

``` r
library(forestables)
#> Cargando paquete requerido: data.table
#> Cargando paquete requerido: dtplyr
```

``` r
library(dplyr)
#> 
#> Adjuntando el paquete: 'dplyr'
#> The following objects are masked from 'package:data.table':
#> 
#>     between, first, last
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

``` r

## Downloading all FFI data (if not already)
ffi_path <- tempdir()
download_inventory("FFI", destination = ffi_path)
#> ℹ Downloading FFI available data
#> ℹ Unzipping downloaded data in '/tmp/Rtmp1ykS9x'
#> ✔ Done!
```

``` r

## Get the plots
ffi_filter_list <-
  show_plots_from("FFI", folder = ffi_path, departments = "42") |>
  filter(CAMPAGNE == 2015) |>
  create_filter_list()

loire_plots_2015 <- ffi_to_tibble(
  "42",
  years = 2015,
  filter_list = ffi_filter_list,
  folder = ffi_path
) |>
  clean_empty(c("tree", "shrub", "regen")) |>
  inventory_as_sf()
#> Start
#> ℹ Processing 1 year
#> Getting ready to retrieve 113 plots for 2015
#> 
#> Adjuntando el paquete: 'purrr'
#> 
#> 
#> The following object is masked from 'package:data.table':
#> 
#>     transpose
```

``` r

## Explore the plots
loire_plots_2015
#> Simple feature collection with 50 features and 15 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 3.70236 ymin: 45.27425 xmax: 4.739146 ymax: 46.20189
#> Geodetic CRS:  WGS 84
#> # A tibble: 50 × 16
#>      crs id_unique_code  year plot    coord_sys_orig aspect slope country dep  
#>    <dbl> <chr>          <dbl> <chr>   <chr>           <dbl> <int> <chr>   <chr>
#>  1  4326 FR_42_1000969   2015 1000969 LAMBERT         342      12 FR      42   
#>  2  4326 FR_42_1002527   2015 1002527 LAMBERT         189      36 FR      42   
#>  3  4326 FR_42_1002664   2015 1002664 LAMBERT          54      26 FR      42   
#>  4  4326 FR_42_1003986   2015 1003986 LAMBERT          47.7    44 FR      42   
#>  5  4326 FR_42_1005427   2015 1005427 LAMBERT         153       7 FR      42   
#>  6  4326 FR_42_1005981   2015 1005981 LAMBERT         130.     25 FR      42   
#>  7  4326 FR_42_1006115   2015 1006115 LAMBERT         270      40 FR      42   
#>  8  4326 FR_42_1006386   2015 1006386 LAMBERT         324      22 FR      42   
#>  9  4326 FR_42_1006791   2015 1006791 LAMBERT         266.     45 FR      42   
#> 10  4326 FR_42_1006998   2015 1006998 LAMBERT          NA       0 FR      42   
#> # ℹ 40 more rows
#> # ℹ 7 more variables: dep_name <chr>, visite <int>, tree <list>,
#> #   understory <list>, regen <list>, geometry <POINT [°]>, crs_orig <dbl>
```
