#' @useDynLib rlang, .registration = TRUE
NULL

# For cnd.R
is_same_body <- NULL

.onLoad <- function(lib, pkg) {
  if (getRversion() < "3.5") {
    is_same_body <<- function(x, y) identical(x, y)
  } else {
    is_same_body <<- is_reference
  }

  .Call(rlang_library_load)
}
.onUnload <- function(lib) {
  .Call(rlang_library_unload)
}
