#' @useDynLib rlang, .registration = TRUE
NULL

.onLoad <- function(lib, pkg) {
  .Call(rlang_library_load)
}
.onUnload <- function(lib) {
  .Call(rlang_library_unload)
}
