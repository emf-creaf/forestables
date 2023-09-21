# on load function, to check for grep dependency
.onLoad <- function(libname, pkgname) {

  # Check if grep is installed
  grep_check <- .sys_cmd_warning()

  invisible()
}
