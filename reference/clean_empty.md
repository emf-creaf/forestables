# Cleaning empty results

Cleaning inventory results to filter out empty data

## Usage

``` r
clean_empty(inventory_data, cols)
```

## Arguments

- inventory_data:

  Data from an inventory as obtained from
  [`ifn_to_tibble`](https://emf-creaf.github.io/forestables/reference/ifn_to_tibble.md),
  [`fia_to_tibble`](https://emf-creaf.github.io/forestables/reference/fia_to_tibble.md)
  or
  [`ffi_to_tibble`](https://emf-creaf.github.io/forestables/reference/ffi_to_tibble.md).

- cols:

  vector with column names to clean from empty results. Can be one or
  more of `"tree"`, `"shrubs"`, `"herbs"` and `"regen"`. If more than
  one, only plots with data in all columns selected will be retained.
  `"shrubs"` and `"herbs"` are inside `"understory"` column, and should
  be noted that IFN inventory never have `"herbs"` data, so cleaning by
  it will always return an empty tibble.

## Value

A tibble the same as `inventory_data` with the empty data removed for
the columns selected.

## Details

This functions remove plot rows with empty data in the desired nested
columns.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
library(forestables)

# FFI
ffi_to_tibble(
  departments = c("01"), years = c(2019),
  filter_list = list("01" = c(1404119)),
  folder = "path/to/ffi/data"
) |>
  clean_empty(c("tree", "regen", "shrub", "herbs"))

# FIA
fia_to_tibble(
  years = 2019, states = c("OR"),
  filter_list = list("OR" = list("59" = c(76413))),
  folder = "path/to/fia/data"
) |>
  clean_empty(c("tree", "regen", "shrub", "herbs"))

# IFN (never clean by "herbs", as is always empty)
ifn_to_tibble(
  provinces = c("24"), versions = c("ifn3"),
  filter_list = list("24" = c("24_0270_xx_A4_xx")),
  folder = "path/to/ifn/data"
) |>
  clean_empty(c("tree", "regen", "shrub"))
} # }
# }
```
