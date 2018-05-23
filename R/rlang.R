#' @useDynLib rlang, .registration = TRUE
NULL

.onLoad <- function(lib, pkg) {
  init_c_constants()
  .Call(rlang_library_load)

  while ("rlang" %in% search()) {
    detach("rlang")
  }
  get("attach")(list(), name = "rlang")

  set_last_error_binding()
}
.onUnload <- function(lib) {
  .Call(rlang_library_unload)
  get("detach")("rlang")
}

rlang_attached_env <- function() scoped_env("rlang")
