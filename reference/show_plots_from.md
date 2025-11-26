# Show plots for any inventory

Show plots with minimal metadata from any inventory

## Usage

``` r
show_plots_from(inventory = c("FIA", "FFI", "IFN"), folder = ".", ...)
```

## Arguments

- inventory:

  Character indicating the inventory. Allowed values are `"FIA"` for the
  USA forest inventory, `"FFI"` for the French *Inventaire Forestier*
  and `"IFN"`, for the Spanish *Inventario Forestal Nacional*.

- folder:

  Character, path to the folder containing the `inventory` files.

- ...:

  Other arguments, depending on the `inventory`, see inventory sections.

## Value

A [`sf`](https://r-spatial.github.io/sf/reference/sf.html) spatial
object in which each row is a plot,. The metadata provided varies
depending on the inventory, but usually includes the state (FIA) /
department (FFI)/ provincia (IFN) and year/date/IFN version

## Details

This function show the plots available in any inventory for the given
administrative units. Take into account that this can potentially show
all plots in any inventory, so the object returned can be memory heavy.
This also will return all plots recorded, but it doesn't mean that those
plots have associated data (tree, understory...), just that they exist.

## FIA

FIA needs an extra argument, `states`, a character vector with the
two-letter code for the desired states.

## FFI

FFI needs an extra argument, `departments`, a character vector with the
desired department codes.

## IFN

IFN needs two extra arguments, `provinces`, a character vector with the
numeric codes for the provinces and `versions`, a character vector with
the IFN versions to look at (`"ifn2"`, `"ifn3"` or/and `"ifn4"`).

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
library(forestables)

# FIA
show_plots_from("FIA", folder = "path/to/fia/data", states = "OR")
# FFI
show_plots_from("FFI", folder = "path/to/ffi/data", departments = "21")
# IFN
show_plots_from("IFN", folder = "path/to/ifn/data", provinces = "24", versions = "ifn4")
} # }
# }
```
