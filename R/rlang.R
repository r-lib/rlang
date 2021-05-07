#' @useDynLib rlang, .registration = TRUE
"_PACKAGE"

on_load({
  check_downstream_deps(
    "rlang",
    dplyr = c(min = "0.8.0", from = "0.4.0"),
    with_rlang = FALSE
  )
})
