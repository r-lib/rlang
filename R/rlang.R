#' @useDynLib rlang, .registration = TRUE
#' @keywords internal
"_PACKAGE"

on_load({
  check_downstream("0.4.0", "dplyr (>= 0.8.0)")
  check_downstream(
    "1.0.0",
    "ellipsis (>= 0.3.2)",
    "vctrs (>= 0.3.8)",
    info = "Not updating now is completely safe and will only cause import warnings."
  )

  local_use_cli()
})
