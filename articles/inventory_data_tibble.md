# Inventory data output

## Forest inventory data outputs

`forestables` tries to harmonize, to some extent, the data of different
forest inventories. This makes easier to design workflows that work with
different inventories. This vignette shows and explains the
`forestables` output tibble.

### Example objects

`forestables` ships with some output examples:

``` r
# load the library
library(forestables)
#> Loading required package: data.table
#> Loading required package: dtplyr

# fia example
fia_output_example
#> # A tibble: 4,193 × 24
#>    id_unique_code  year plot  coordx coordy coord_sys   crs   elev aspect slope
#>    <chr>          <int> <chr>  <dbl>  <dbl> <chr>     <dbl>  <dbl>  <int> <int>
#>  1 US_2_20_39004   2015 39004  -149.   61.1 NAD83      4269  762       NA    NA
#>  2 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.     220    43
#>  3 US_2_20_1615    2015 1615   -149.   60.9 NAD83      4269  579.      NA    NA
#>  4 US_2_20_1470    2015 1470   -149.   61.2 NAD83      4269 1189.      NA    NA
#>  5 US_2_20_57145   2015 57145  -148.   61.1 NAD83      4269  945.      NA    NA
#>  6 US_2_20_29733   2015 29733  -149.   61.3 NAD83      4269 1006.      NA    NA
#>  7 US_2_20_63035   2015 63035  -149.   61.1 NAD83      4269  884.      NA    NA
#>  8 US_2_20_16154   2015 16154  -150.   61.1 NAD83      4269 1067.      NA    NA
#>  9 US_2_20_65841   2015 65841  -150.   61.1 NAD83      4269   30.5     NA    NA
#> 10 US_2_20_65097   2015 65097  -150.   61.4 NAD83      4269   30.5     NA    NA
#> # ℹ 4,183 more rows
#> # ℹ 14 more variables: country <chr>, state_code <int>, state_ab <chr>,
#> #   state_name <chr>, county_code <chr>, p3panel <int>,
#> #   p2veg_sampling_status_cd <int>, rscd <int>, design_code <int>,
#> #   subplot <list>, tree <list>, understory <list>, regen <list>,
#> #   p2veg_sampling_level_detail_cd <int>

# ffi example
ffi_output_example
#> # A tibble: 499 × 16
#>    id_unique_code  year plot  coordx coordy coord_sys   crs aspect slope country
#>    <chr>          <int> <chr>  <dbl>  <dbl> <chr>     <dbl>  <dbl> <int> <chr>  
#>  1 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  2 FR_42_503498    2015 5034… 7.61e5 6.53e6 LAMBERT    2154  130.     12 FR     
#>  3 FR_42_504950    2015 5049… 7.93e5 6.55e6 LAMBERT    2154  333      15 FR     
#>  4 FR_42_505301    2015 5053… 7.72e5 6.48e6 LAMBERT    2154   NA       0 FR     
#>  5 FR_42_505321    2015 5053… 8.22e5 6.48e6 LAMBERT    2154  180      18 FR     
#>  6 FR_42_507479    2015 5074… 7.76e5 6.50e6 LAMBERT    2154   NA      NA FR     
#>  7 FR_42_509007    2015 5090… 8.07e5 6.53e6 LAMBERT    2154  266.     28 FR     
#>  8 FR_42_512511    2015 5125… 8.08e5 6.46e6 LAMBERT    2154   23.4    15 FR     
#>  9 FR_42_512565    2015 5125… 8.22e5 6.47e6 LAMBERT    2154   22.5    46 FR     
#> 10 FR_42_512609    2015 5126… 7.97e5 6.53e6 LAMBERT    2154  313.     36 FR     
#> # ℹ 489 more rows
#> # ℹ 6 more variables: dep <chr>, dep_name <chr>, visite <int>, tree <list>,
#> #   understory <list>, regen <list>

# ifn example
ifn_output_example
#> # A tibble: 8,997 × 24
#>    id_unique_code    year plot  coordx coordy coord_sys   crs  elev aspect slope
#>    <chr>            <int> <chr>  <dbl>  <dbl> <chr>     <dbl> <dbl>  <dbl> <dbl>
#>  1 08_0001_NN_A1_A1  1990 0001  402000 4.68e6 ED50      23031  1900     NA    NA
#>  2 08_0002_NN_A1_A1  1990 0002  400000 4.68e6 ED50      23031  1700     NA    NA
#>  3 08_0003_NN_A1_A1  1990 0003  401000 4.68e6 ED50      23031  1700     NA    NA
#>  4 08_0004_NN_A1_A1  1990 0004  402000 4.68e6 ED50      23031  1400     NA    NA
#>  5 08_0005_NN_A1_A1  1990 0005  400000 4.68e6 ED50      23031  1300     NA    NA
#>  6 08_0006_NN_A1_A1  1990 0006  397000 4.68e6 ED50      23031  1700     NA    NA
#>  7 08_0007_NN_A1_xx  1990 0007  399000 4.68e6 ED50      23031  1400     NA    NA
#>  8 08_0008_NN_A1_xx  1990 0008  401000 4.68e6 ED50      23031  1100     NA    NA
#>  9 08_0009_NN_A1_xx  1990 0009  402000 4.68e6 ED50      23031  1100     52    NA
#> 10 08_0010_NN_A1_xx  1990 0010  394000 4.68e6 ED50      23031  1500     NA    NA
#> # ℹ 8,987 more rows
#> # ℹ 14 more variables: country <chr>, version <chr>, class <chr>,
#> #   subclass <chr>, province_code <chr>, province_name_original <chr>,
#> #   ca_name_original <chr>, sheet_ntm <chr>, huso <dbl>, slope_mean <chr>,
#> #   type <int>, tree <list>, understory <list>, regen <list>
```

We are going to use these examples to explain the structure of the
output.

### Output structure

Forest inventory data outputs in `forestables` are data frames (tibbles)
in which each row is a plot/year combination (plot/version in the IFN).
Metadata for the plot is presented in the first columns (plot IDs,
coordinates info, topography…). The last three columns are always nested
columns with the `tree` table for that plot/year, `understory` table for
that plot/year and `regen` table for that plot/year.

![](%22inventory_row.svg%22)

- Common metadata among different national forest inventories is:  
  `id_unique_code`, `year`, `plot`, `coordx`, `coordy`, `coord_sys`,
  `crs`, `elev`, `aspect`, `slope`, `country`.

- FIA unique metadata:  
  `state_code`, `state_ab`, `state_name`, `county_code`, `p3panel`,
  `p2veg_sampling_status_cd`, `p2veg_sampling_level_detail`, `rscd`,
  `design_code`, `subplot`.

- IFN unique metadata:  
  `version`, `class`, `subclass`, `province_code`,
  `province_name_original`, `ca_name_original`, `sheet_ntm`, `huso`,
  `slope_mean`, `type`.

- FFI unique metadata:  
  `dep`, `dep_name`, `visite`.

- Nested columns common to all national forest inventories:  
  `tree`, `understory`, `regen`

To know more about each variable, you can check the *Description of
functions* vignette.

### Post-processing outputs

1.  **Cleaning empty data**. By default, `ifn_`, `fia_` and
    `ffi_to_tibble` functions return all plots requested, even if they
    don’t have data for that year or version. The
    [`clean_empty()`](https://emf-creaf.github.io/forestables/reference/clean_empty.md)
    function can be used after retrieving the data to remove empty plots
    in `tree`, `shrub`, `herbs` and/or `regen` columns:

``` r
# FIA plots in AK for the 2015-2018 period
nrow(fia_output_example)
#> [1] 4193
# FIA plots in AK for the 2015-2018 period with tree data
nrow(clean_empty(fia_output_example, c("tree")))
#> [1] 1532
# FIA plots in AK for the 2015-2018 period with tree AND shrub AND herbs AND regen data
nrow(clean_empty(fia_output_example, c("tree", "shrub", "herbs", "regen")))
#> [1] 1166
```

2.  **Transforming to spatial object**. By default, `ifn_`, `fia_` and
    `ffi_to_tibble` functions all return a tibble (data frame). These
    tibbles have the spatial information to be able to convert plots to
    spatial points. But this conversion is not straightforward as
    sometimes plots from the same inventory have different coordinate
    systems or projections. To do this `inventory_to_sf()` function can
    be used directly and it will make the necessary coordinate
    transformations to return plots as points in latitude-longitude
    format (CRS = 4326):

``` r
ifn_output_example |>
  clean_empty("tree") |>
  inventory_as_sf()
#> Simple feature collection with 7774 features and 23 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1.378228 ymin: 41.21556 xmax: 2.770411 ymax: 42.30107
#> Geodetic CRS:  WGS 84
#> # A tibble: 7,774 × 24
#>      crs id_unique_code    year plot  coord_sys_orig  elev aspect slope country
#>    <dbl> <chr>            <int> <chr> <chr>          <dbl>  <dbl> <dbl> <chr>  
#>  1  4326 08_0001_NN_A1_A1  1990 0001  ED50            1900     NA    NA ES     
#>  2  4326 08_0002_NN_A1_A1  1990 0002  ED50            1700     NA    NA ES     
#>  3  4326 08_0003_NN_A1_A1  1990 0003  ED50            1700     NA    NA ES     
#>  4  4326 08_0004_NN_A1_A1  1990 0004  ED50            1400     NA    NA ES     
#>  5  4326 08_0005_NN_A1_A1  1990 0005  ED50            1300     NA    NA ES     
#>  6  4326 08_0006_NN_A1_A1  1990 0006  ED50            1700     NA    NA ES     
#>  7  4326 08_0007_NN_A1_xx  1990 0007  ED50            1400     NA    NA ES     
#>  8  4326 08_0008_NN_A1_xx  1990 0008  ED50            1100     NA    NA ES     
#>  9  4326 08_0009_NN_A1_xx  1990 0009  ED50            1100     52    NA ES     
#> 10  4326 08_0010_NN_A1_xx  1990 0010  ED50            1500     NA    NA ES     
#> # ℹ 7,764 more rows
#> # ℹ 15 more variables: version <chr>, class <chr>, subclass <chr>,
#> #   province_code <chr>, province_name_original <chr>, ca_name_original <chr>,
#> #   sheet_ntm <chr>, huso <dbl>, slope_mean <chr>, type <int>, tree <list>,
#> #   understory <list>, regen <list>, geometry <POINT [°]>, crs_orig <dbl>
```

### `tree` column

`tree` is a *nested column*, meaning that each cell contain the tree
data table for that row (i.e. the tree data table for that plot that
year/version). As with the plot metadata, an effort to harmonize tree
data between national forest inventories has been done.

- Common tree variables among different national forest inventories:  
  `tree`, `sp_code`, `sp_name`, `status`, `density_factor`, `dia`,
  `height`.

- IFN unique tree variables:  
  `tree_ifn2`, `tree_ifn3`, `tree_ifn4` `cubing_form`, `quality_wood` ,
  `bearing`, `distance` . Also, IFN lack the `status` variable.

- FFI unique tree variables:  
  `height_last_recorded`, `status5`, `espar`.

The easiest way to access the tree data is with
[`unnest()`](https://tidyr.tidyverse.org/reference/unnest.html):

``` r
fia_output_example |>
  clean_empty("tree") |>
  unnest("tree", names_sep = "_")
#> # A tibble: 55,220 × 30
#>    id_unique_code  year plot  coordx coordy coord_sys   crs  elev aspect slope
#>    <chr>          <int> <chr>  <dbl>  <dbl> <chr>     <dbl> <dbl>  <int> <int>
#>  1 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#>  2 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#>  3 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#>  4 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#>  5 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#>  6 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#>  7 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#>  8 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#>  9 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#> 10 US_2_20_6376    2015 6376   -149.   60.9 NAD83      4269  335.    220    43
#> # ℹ 55,210 more rows
#> # ℹ 20 more variables: country <chr>, state_code <int>, state_ab <chr>,
#> #   state_name <chr>, county_code <chr>, p3panel <int>,
#> #   p2veg_sampling_status_cd <int>, rscd <int>, design_code <int>,
#> #   subplot <list>, tree_tree_id <chr>, tree_sp_code <chr>, tree_sp_name <chr>,
#> #   tree_status <int>, tree_density_factor <dbl>, tree_dbh <dbl>,
#> #   tree_height <dbl>, understory <list>, regen <list>, …
```

### `understory` column

`understory` is also a *nested* column. It contains data tables for
shrubs and for herbs, if found in the forest inventory plot.

- Common understory variables among different national forest
  inventories:  
  `shrub`, `herbs`. Both are also nested columns, as each kind of
  understory has different data structure.

- FFI unique understory variables:  
  `lign1_pct`, `lign2_pct`.

#### `shrub` column

- Common shrub variables among different national forest inventories:  
  `sp_code`, `sp_name`, `cover`, `height`, `growth_form`

- FIA unique shrub variables:  
  `subplot`, `growth_form_code`.

#### `herbs` column

The `herbs` data is different in each inventory, and is not present in
the IFN.

- FIA unique herbs variables:  
  `subplot`, `sp_name`, `growth_form_code`, `height`, `cover`,
  `growth_form`, `sp_code`

- FFI unique herbs variables:  
  `herb_pct`

The easiest way to access the understory data is with
[`unnest()`](https://tidyr.tidyverse.org/reference/unnest.html):

``` r
ffi_output_example |>
  clean_empty("shrub") |>
  unnest("understory") |>
  unnest("shrub", names_sep = "_")
#> # A tibble: 4,760 × 23
#>    id_unique_code  year plot  coordx coordy coord_sys   crs aspect slope country
#>    <chr>          <int> <chr>  <dbl>  <dbl> <chr>     <dbl>  <dbl> <int> <chr>  
#>  1 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#>  2 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#>  3 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#>  4 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#>  5 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#>  6 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#>  7 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#>  8 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#>  9 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#> 10 FR_42_1000969   2015 1000… 8.24e5 6.47e6 LAMBERT    2154    342    12 FR     
#> # ℹ 4,750 more rows
#> # ℹ 13 more variables: dep <chr>, dep_name <chr>, visite <int>, tree <list>,
#> #   lign1_pct <int>, lign2_pct <int>, shrub_sp_code <chr>, shrub_sp_name <chr>,
#> #   shrub_cover <dbl>, shrub_height <lgl>, shrub_growth_form <chr>,
#> #   herbs <list>, regen <list>

ffi_output_example |>
  clean_empty("herbs") |>
  unnest("understory") |>
  unnest("herbs", names_sep = "_")
#> # A tibble: 499 × 19
#>    id_unique_code  year plot  coordx coordy coord_sys   crs aspect slope country
#>    <chr>          <int> <chr>  <dbl>  <dbl> <chr>     <dbl>  <dbl> <int> <chr>  
#>  1 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  2 FR_42_503498    2015 5034… 7.61e5 6.53e6 LAMBERT    2154  130.     12 FR     
#>  3 FR_42_504950    2015 5049… 7.93e5 6.55e6 LAMBERT    2154  333      15 FR     
#>  4 FR_42_505301    2015 5053… 7.72e5 6.48e6 LAMBERT    2154   NA       0 FR     
#>  5 FR_42_505321    2015 5053… 8.22e5 6.48e6 LAMBERT    2154  180      18 FR     
#>  6 FR_42_507479    2015 5074… 7.76e5 6.50e6 LAMBERT    2154   NA      NA FR     
#>  7 FR_42_509007    2015 5090… 8.07e5 6.53e6 LAMBERT    2154  266.     28 FR     
#>  8 FR_42_512511    2015 5125… 8.08e5 6.46e6 LAMBERT    2154   23.4    15 FR     
#>  9 FR_42_512565    2015 5125… 8.22e5 6.47e6 LAMBERT    2154   22.5    46 FR     
#> 10 FR_42_512609    2015 5126… 7.97e5 6.53e6 LAMBERT    2154  313.     36 FR     
#> # ℹ 489 more rows
#> # ℹ 9 more variables: dep <chr>, dep_name <chr>, visite <int>, tree <list>,
#> #   lign1_pct <int>, lign2_pct <int>, shrub <list>, herbs_herb_pct <int>,
#> #   regen <list>
```

### `regen` column

`regen` is a *nested column*, meaning that each cell contains the (tree)
regeneration data table for that row (i.e. the regeneration data table
for that plot that year/version). As with the plot metadata, an effort
to harmonize regeneration data between inventories has been done.

- Common regen variables among different national forest inventories:  
  `sp_code`, `sp_name`, `density_factor`, `dbh`, `height`, `n`.

- FIA unique regen variables:  
  `subplot`, `treecount_calc`.

- FFI unique regen variables:  
  `cover`, `growth_form`

The easiest way to access the regen data is with
[`unnest()`](https://tidyr.tidyverse.org/reference/unnest.html):

``` r
ifn_output_example |>
  clean_empty("regen") |>
  unnest("regen", names_sep = "_")
#> # A tibble: 66,851 × 29
#>    id_unique_code    year plot  coordx coordy coord_sys   crs  elev aspect slope
#>    <chr>            <int> <chr>  <dbl>  <dbl> <chr>     <dbl> <dbl>  <dbl> <dbl>
#>  1 08_0001_NN_A1_A1  1990 0001  402000 4.68e6 ED50      23031  1900     NA    NA
#>  2 08_0001_NN_A1_A1  1990 0001  402000 4.68e6 ED50      23031  1900     NA    NA
#>  3 08_0002_NN_A1_A1  1990 0002  400000 4.68e6 ED50      23031  1700     NA    NA
#>  4 08_0002_NN_A1_A1  1990 0002  400000 4.68e6 ED50      23031  1700     NA    NA
#>  5 08_0002_NN_A1_A1  1990 0002  400000 4.68e6 ED50      23031  1700     NA    NA
#>  6 08_0002_NN_A1_A1  1990 0002  400000 4.68e6 ED50      23031  1700     NA    NA
#>  7 08_0002_NN_A1_A1  1990 0002  400000 4.68e6 ED50      23031  1700     NA    NA
#>  8 08_0003_NN_A1_A1  1990 0003  401000 4.68e6 ED50      23031  1700     NA    NA
#>  9 08_0003_NN_A1_A1  1990 0003  401000 4.68e6 ED50      23031  1700     NA    NA
#> 10 08_0003_NN_A1_A1  1990 0003  401000 4.68e6 ED50      23031  1700     NA    NA
#> # ℹ 66,841 more rows
#> # ℹ 19 more variables: country <chr>, version <chr>, class <chr>,
#> #   subclass <chr>, province_code <chr>, province_name_original <chr>,
#> #   ca_name_original <chr>, sheet_ntm <chr>, huso <dbl>, slope_mean <chr>,
#> #   type <int>, tree <list>, understory <list>, regen_sp_code <chr>,
#> #   regen_sp_name <chr>, regen_dbh <dbl>, regen_height <dbl>,
#> #   regen_density_factor <dbl>, regen_n <dbl>
```

## Combining different inventories

Harmonization of inventories data is done in a way that allows to
combine different countries data in one table:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:data.table':
#> 
#>     between, first, last
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

ffi_output_example |>
  bind_rows(ifn_output_example) |>
  bind_rows(fia_output_example) |>
  clean_empty("tree") |>
  unnest("tree")
#> # A tibble: 214,387 × 51
#>    id_unique_code  year plot  coordx coordy coord_sys   crs aspect slope country
#>    <chr>          <int> <chr>  <dbl>  <dbl> <chr>     <dbl>  <dbl> <dbl> <chr>  
#>  1 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  2 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  3 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  4 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  5 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  6 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  7 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  8 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#>  9 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#> 10 FR_42_500534    2015 5005… 8.14e5 6.48e6 LAMBERT    2154   85.5    43 FR     
#> # ℹ 214,377 more rows
#> # ℹ 41 more variables: dep <chr>, dep_name <chr>, visite <int>, tree_id <chr>,
#> #   sp_code <chr>, sp_name <chr>, status <int>, density_factor <dbl>,
#> #   dbh <dbl>, height <dbl>, height_last_recorded <dbl>, status5 <int>,
#> #   espar <chr>, cubing_form <chr>, quality_wood <chr>, tree_ifn2 <int>,
#> #   tree_ifn3 <int>, tree_ifn4 <int>, understory <list>, regen <list>,
#> #   elev <dbl>, version <chr>, class <chr>, subclass <chr>, …
```
