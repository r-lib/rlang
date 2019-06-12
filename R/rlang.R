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

  .Call(r_init_library)
  .Call(rlang_library_load)

  s3_register("pillar::pillar_shaft", "quosures", pillar_shaft.quosures)
  s3_register("pillar::type_sum", "quosures", type_sum.quosures)
}
.onUnload <- function(lib) {
  .Call(rlang_library_unload)
}
