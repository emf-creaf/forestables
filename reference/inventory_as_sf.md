# Convert inventory data to sf based on coords and crs present

Use coords vars and crs to convert to sf

## Usage

``` r
inventory_as_sf(inventory_data)
```

## Arguments

- inventory_data:

  Data from an inventory as obtained from
  [`ifn_to_tibble`](https://emf-creaf.github.io/forestables/reference/ifn_to_tibble.md),
  [`fia_to_tibble`](https://emf-creaf.github.io/forestables/reference/fia_to_tibble.md)
  or
  [`ffi_to_tibble`](https://emf-creaf.github.io/forestables/reference/ffi_to_tibble.md).

## Value

An sf object with the same data as `inventory_data` and a new column
with the original crs for traceability.

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
  inventory_as_sf()

# FIA
fia_to_tibble(
  years = 2019, states = c("OR"),
  filter_list = list("OR" = list("59" = c(76413))),
  folder = "path/to/fia/data"
) |>
  inventory_as_sf()

# IFN
ifn_to_tibble(
  provinces = c("24"), versions = c("ifn3"),
  filter_list = list("24" = c("24_0270_xx_A4_xx")),
  folder = "path/to/ifn/data"
) |>
  inventory_as_sf()
} # }
# }
```
