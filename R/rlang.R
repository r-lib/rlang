#' @useDynLib rlang, .registration = TRUE
"_PACKAGE"

on_load({
  check_downstream(
    "0.4.0",
    "dplyr (>= 0.8.0)"
  )
})

.rlang_use_cli_format <- TRUE
