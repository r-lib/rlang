#' @useDynLib rlanglibtest, .registration = TRUE
NULL

.onLoad <- function(lib, pkg) {
  .Call(rlanglibtest_library_load)
}
