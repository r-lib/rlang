#' @useDynLib rlang, .registration = TRUE
"_PACKAGE"

on_load({
  check_downstream("0.4.0", "dplyr (>= 0.8.0)")
  check_downstream(
    "0.5.0",
    "ellipsis (>= 0.3.2)",
    "vctrs (>= 0.3.8)",
    info = "Not updating now is completely safe and will only cause import warnings."
  )
})

.rlang_use_cli_format <- "try"
