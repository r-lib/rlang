#' @useDynLib rlang, .registration = TRUE
#' @keywords internal
"_PACKAGE"

on_load({
  local_use_cli()
})

compiled_by_gcc <- function() {
  .Call(ffi_compiled_by_gcc)
}

#' @rawNamespace export(ffi_standalone_check_is_bool)
NULL
