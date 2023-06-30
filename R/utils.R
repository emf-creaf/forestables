#' Util to control messaging
#'
#' @param msg Usually a \code{\link[cli]{cli_inform}} call
#'
#' @noRd
verbose_msg <- function(msg, .verbose) {
  if (.verbose) {
    msg
  }
}
