# Raw IFN data to tibble

Transform raw IFN plot data into tidy data for easier use

## Usage

``` r
ifn_to_tibble(
  provinces,
  versions,
  filter_list = NULL,
  folder,
  clean_empty = NULL,
  as_sf = FALSE,
  ...,
  .parallel_options = furrr::furrr_options(scheduling = 1L, stdout = TRUE),
  .verbose = TRUE
)
```

## Arguments

- provinces:

  A character vector with the two-number codes for the provinces.

- versions:

  A character vector with the ifn versions. Valid versions are `"ifn2"`,
  `"ifn3"` and `"ifn4"`.

- filter_list:

  A list of provinces and plots to extract the data from.

- folder:

  The path to the folder containing the IFN db files, as character.

- clean_empty:

  Vector with column names from where to remove empty results. Can be
  one or more of `"tree"`, `"shrub"`, `"herbs"` and `"regen"`. If more
  than one, only plots with data in all columns selected will be
  retained. Default to NULL, no cleaning is done.

- as_sf:

  Logical indicating if the data must be returned as an spatial object.
  This always can be done later, as the data contains coordinates and
  crs info. Default to `FALSE`.

- ...:

  Not used at the moment

- .parallel_options:

  An object of class `furrr_options`. See
  [`furrr_options`](https://furrr.futureverse.org/reference/furrr_options.html).

- .verbose:

  Logical controlling if progress messages are shown.

## Value

A nested tibble. This tibble contains a row per plot/year combination,
with the plot metadata included, as well as columns containing tibbles
with tree, shrub, herbs and soil information. See
[`vignette("inventory_data_tibble", package = "forestables")`](https://emf-creaf.github.io/forestables/articles/inventory_data_tibble.md)

## Details

This function will take every year indicated and retrieve and transform
the plot data for the provinces, versions and plots provided. For that,
IFN db files must reside in the folder indicated in the `folder`
argument.

## Filter list

If no `filter_list` argument is provided, `ifn_to_tibble` will attempt
to process all plots for the provinces and ifn versions provided. This
will result in sometimes thousands plots to be extracted, processed and
returned, which in turn will cause a big use of memory (specially when
running in parallel processes) and long times of calculation. Is better
to provide a list of departments with the provinces and plots to look
for to narrow the process. This `filter_list` should have the following
structure:

       list(
       "01" = c("01_0644_NN_A1_A1"),
       "08" = c("08_1256_NN_A1_xx", "08_0056_xx_A4_xx"),
       "24" = c("24_0270_xx_A4_xx")
      )
      

`forestables` package offers workflows to create this automatically, see
[`vignette("selecting_plots", package = "forestables")`](https://emf-creaf.github.io/forestables/articles/selecting_plots.md)
for more details.

## Parallel

Processing the plots from within an IFN version can be done in parallel
(`forestables` uses internally the
[`furrr`](https://furrr.futureverse.org/reference/furrr-package.html)
package for this). This means that, if parallelization is active,
several processes are launched to retrieve the plots data for that IFN
version. This is repeated for all versions provided.

`.parallel_options` controls the finer details of how parallelization is
performed (see
[`furrr_options`](https://furrr.futureverse.org/reference/furrr_options.html)).
But no parallelization can occur without setting first a
[`plan`](https://future.futureverse.org/reference/plan.html). By
default, the chosen plan is
[`sequential`](https://future.futureverse.org/reference/sequential.html),
so no parellization is done. Changing the plan, i.e. to
[`multisession`](https://future.futureverse.org/reference/multisession.html)
will allow `ifn_to_tibble` to use parallelization when retrieving the
data.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
library(forestables)
ifn_to_tibble(
  provinces = c("24"), versions = c("ifn3"),
  filter_list = list("24" = c("24_0270_xx_A4_xx")),
  folder = "path/to/ifn/data"
)
} # }
# }
```
