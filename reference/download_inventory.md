# Download the especified inventory

Download and unzip the inventory data files at the desired destination

## Usage

``` r
download_inventory(
  inventory = c("FIA", "FFI", "IFN"),
  destination = ".",
  states = NULL,
  .verbose = TRUE
)
```

## Arguments

- inventory:

  Character with the inventory abbreviation

- destination:

  Path to the inventory destination folder. This folder must exists.

- states:

  Character vector indicating the FIA states to download. Only used if
  FIA is selected.

- .verbose:

  Logical indicating if progress messages should be shown.

## Value

Invisible TRUE if the download and unzip was succesful, an error
otherwise.

## Details

This function tries to download the available files for the especified
inventory from the official repositories of the inventory. It can fail
as can be connection problems or files missing temporary (This usually
happens for the version 4 of the IFN)

## Warning

IFN version 4 data is not stable. Links change and data formats and
content can also change. No warning is given of these changes
beforehand, so this function can sometimes not retrieve all available
data for IFN version 4.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
library(forestables)
download_inventory(ffi, destination = tempdir())
} # }
# }
```
