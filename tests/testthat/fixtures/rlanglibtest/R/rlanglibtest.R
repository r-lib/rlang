#' @useDynLib rlanglibtest, .registration = TRUE
NULL

.onLoad <- function(lib, pkg) {
  # Causes rlang package to load and register native routines
  rlang::dots_list()

  .Call(rlanglibtest_library_load)
}
