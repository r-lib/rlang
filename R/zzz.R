#' @useDynLib rlang, .registration = TRUE
NULL

# For cnd.R
is_same_body <- NULL


base_ns_env <- NULL
base_pkg_env <- NULL

.onLoad <- function(lib, pkg) {
  if (getRversion() < "3.5") {
    is_same_body <<- function(x, y) identical(x, y)
  } else {
    is_same_body <<- is_reference
  }

  check_linked_version(pkg, with_rlang = FALSE)

  check_downstream_deps(
    pkg,
    dplyr = c(min = "0.8.0", from = "0.4.0"),
    with_rlang = FALSE
  )

  on_package_load("glue", .Call(ffi_glue_is_here))

  rlang_ns <- topenv(environment())
  .Call(ffi_init_r_library, rlang_ns)
  .Call(ffi_init_rlang, rlang_ns)

  run_on_load()

  s3_register("pillar::pillar_shaft", "quosures", pillar_shaft.quosures)
  s3_register("pillar::type_sum", "quosures", type_sum.quosures)

  base_ns_env <<- ns_env("base")
  base_pkg_env <<- baseenv()
}
.onUnload <- function(lib) {
  .Call(ffi_fini_rlang)
}

on_package_load <- function(pkg, expr) {
  if (isNamespaceLoaded(pkg)) {
    expr
  } else {
    thunk <- function(...) expr
    setHook(packageEvent(pkg, "onLoad"), thunk)
  }
}
