#' @useDynLib rlang, .registration = TRUE
#' @keywords internal
"_PACKAGE"

on_load({
  local_use_cli()
})

compiled_by_gcc <- function() {
  .Call(ffi_compiled_by_gcc)
}

#' Internal API for standalone-types-check
#' @name ffi_standalone_types_check
#' @keywords internal
#' @rawNamespace export(ffi_standalone_check_is_bool)
NULL
