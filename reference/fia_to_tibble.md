# Raw FIA data to tibble

Transform raw FIA plot data into tidy data for easier use

## Usage

``` r
fia_to_tibble(
  states,
  years,
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

- states:

  A character vector with the two letters code for the states to extract
  the data from.

- years:

  A numeric vector with the years to extract de data from.

- filter_list:

  A nested list of states, counties and plots to extract the data from.
  If left `NULL` all plots for the state for all years will be
  extracted, which can use a big amount of memory. See details.

- folder:

  The path to the folder containing the FIA csv files, as character.

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

This function will take every year specified and will retrieve and
transform the plot data for the states and plots provided. For that, csv
files from FIA must reside in the folder indicated in the `folder`
argument.

## Filter list

If no `filter_list` argument is provided, `fia_to_tibble` will attempt
to process all plots for the states and years provided. This will result
in sometimes hundred of thousands plots to be extracted, processed and
returned, which in turn will cause a big use of memory (specially when
running in parallel processes) and long times of calculation. Is better
to provide a list of states with the counties and plots to look for to
narrow the process. This `filter_list` should have the following
structure:

      list(
        "MN" = list("137" = c(29396, 25064), "71" = c(20210)),
        "OR" = list("59" = c(76413)),
        "CA" = list("105" = c(70128, 83043))
      )
      

`forestables` package offers workflows to create this automatically, see
[`vignette("selecting_plots", package = "forestables")`](https://emf-creaf.github.io/forestables/articles/selecting_plots.md)
for more details.

## Parallel

Processing the plots from within a year can be done in parallel
(`forestables` uses internally the
[`furrr`](https://furrr.futureverse.org/reference/furrr-package.html)
package for this). This means that, if parallelization is active,
several processes are launched to retrieve the plots data for that year.
This is repeated for all years provided.

`.parallel_options` controls the finer details of how parallelization is
performed (see
[`furrr_options`](https://furrr.futureverse.org/reference/furrr_options.html)).
But no parallelization can occur without setting first a
[`plan`](https://future.futureverse.org/reference/plan.html). By
default, the chosen plan is
[`sequential`](https://future.futureverse.org/reference/sequential.html),
so no parellization is done. Changing the plan, i.e. to
[`multisession`](https://future.futureverse.org/reference/multisession.html)
will allow `fia_to_tibble` to use parallelization when retrieving the
data.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
library(forestables)
fia_to_tibble(
  years = 2014, states = c("OR"),
  filter_list = list("OR" = list("59" = c(76413))),
  folder = "path/to/fia/data"
)
} # }
# }
```
