% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{inventory_as_sf}
\alias{inventory_as_sf}
\title{Convert inventory data to sf based on coords and crs present}
\usage{
inventory_as_sf(inventory_data)
}
\arguments{
\item{inventory_data}{Data from an inventory as obtained from
\code{\link{ifn_to_tibble}}, \code{\link{fia_to_tibble}} or
\code{\link{ffi_to_tibble}}.}
}
\value{
An sf object with the same data as \code{inventory_data}
and a new column with the original crs for traceability.
}
\description{
Use coords vars and crs to convert to sf
}
\examples{
\donttest{
\dontrun{
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
}
}

}
