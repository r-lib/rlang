#' @useDynLib rlanglibtest, .registration = TRUE
NULL

.onLoad <- function(lib, pkg) {
  # Causes rlang package to load and register native routines
  rlang::dots_list()

  .Call(rlanglibtest_library_load)
}

test_trace_unexported <- function(e) {
  trace_back(e)
}
test_trace_unexported_child <- local(function(e) {
  test_trace_unexported(e)
})
