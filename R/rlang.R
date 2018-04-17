#' @useDynLib rlang, .registration = TRUE
NULL

.onLoad <- function(lib, pkg) {
  init_c_constants()
  .Call(rlang_library_load)

  # Has to register after loading since this uses rlang functions
  register_experimental("rlang::modify")
}
.onUnload <- function(lib) {
  .Call(rlang_library_unload)
}
