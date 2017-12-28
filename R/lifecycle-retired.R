
signal_soft_deprecation <- function(msg) {
  if (is_true(peek_option("lifecycle_verbose_retirement"))) {
    warn(msg)
  }
  invisible(NULL)
}

#' @rdname parse_expr
#' @export
parse_quosure <- function(x, env = caller_env()) {
  signal_soft_deprecation(
    "`parse_quosure()` is soft-deprecated as of rlang 0.2.0. Please use `parse_quo()` instead."
  )
  parse_quo(x, env = env)
}
#' @rdname parse_expr
#' @export
parse_quosures <- function(x, env = caller_env()) {
  signal_soft_deprecation(
    "`parse_quosures()` is soft-deprecated as of rlang 0.2.0. Please use `parse_quos()` instead."
  )
  parse_quos(x, env = env)
}
