#' @useDynLib rlang, .registration = TRUE
NULL

.onLoad <- function(lib, pkg) {
  check_linked_version(pkg, with_rlang = FALSE)

  check_downstream_deps(
    pkg,
    dplyr = c(min = "0.8.0", from = "0.4.0"),
    with_rlang = FALSE
  )

  rlang_ns <- topenv(environment())
  .Call(ffi_init_r_library, rlang_ns)
  .Call(ffi_init_rlang, rlang_ns)

  run_on_load()
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
