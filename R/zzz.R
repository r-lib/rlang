.onLoad <- function(lib, pkg) {
  check_linked_version(pkg, with_rlang = FALSE)

  rlang_ns <- topenv(environment())
  .Call(ffi_init_r_library, rlang_ns)
  .Call(ffi_init_rlang, rlang_ns)

  run_on_load()
}
.onUnload <- function(lib) {
  .Call(ffi_fini_rlang)
}
