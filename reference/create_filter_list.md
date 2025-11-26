# Create a compatible plots filter list object

Create a compatible plots filter list from the result of
`link{show_plots_from}`

## Usage

``` r
create_filter_list(plots_info)
```

## Arguments

- plots_info:

  Object resulted from `link{show_plots_from}`, or a compatible one
  (*i.e.* the same object after some plot filtering).

## Value

A list object compatible with the `filter_list` argument of
[`ffi_to_tibble`](https://emf-creaf.github.io/forestables/reference/ffi_to_tibble.md),
[`fia_to_tibble`](https://emf-creaf.github.io/forestables/reference/fia_to_tibble.md)
or
[`ifn_to_tibble`](https://emf-creaf.github.io/forestables/reference/ifn_to_tibble.md)

## Details

This function takes the result of `link{show_plots_from}`, or a
compatible object and creates a filter list ready to be use with
[`ffi_to_tibble`](https://emf-creaf.github.io/forestables/reference/ffi_to_tibble.md),
[`fia_to_tibble`](https://emf-creaf.github.io/forestables/reference/fia_to_tibble.md)
or
[`ifn_to_tibble`](https://emf-creaf.github.io/forestables/reference/ifn_to_tibble.md).
Internal heuristics determine the inventory from the data supplied.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
library(forestables)

# FIA
show_plots_from("FIA", folder = ".", states = "OR") |>
  create_filter_list()
# FFI
show_plots_from("FFI", folder = ".", departments = "21") |>
  create_filter_list()
# IFN
show_plots_from("IFN", folder = ".", provinces = "24", version = "ifn4") |>
  create_filter_list()
} # }
# }
```
