.onLoad <- function(lib, pkg) {
  check_linked_version(pkg, with_rlang = FALSE)

  rlang_ns <- topenv(environment())
  .Call(ffi_init_r_library, rlang_ns)
  .Call(ffi_init_rlang, rlang_ns)

  run_on_load()

  # Avoid warnings from clash between rlang::deprecated and lifecycle::deprecated
  on_package_load("lifecycle", {
    env <- get_env(deprecated)
    old <- env_binding_unlock(env, "deprecated")
    assign("deprecated", lifecycle::deprecated, env)
    if (old) {
      env_binding_lock(env, "deprecated")
    }
  })
}
.onUnload <- function(lib) {
  .Call(ffi_fini_rlang)
}
