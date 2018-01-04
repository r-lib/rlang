#' @useDynLib rlang, .registration = TRUE
NULL

.onLoad <- function(lib, pkg) {
  init_c_constants()
  .Call(rlang_library_load)
}
.onUnload <- function(lib) {
  .Call(rlang_library_unload)
}
