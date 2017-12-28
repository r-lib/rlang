
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

#' @rdname is_quosure
#' @export
is_quosureish <- function(x, scoped = NULL) {
  warn("`is_quosureish()` is deprecated as of rlang 0.2.0")
  is_formula(x, scoped = scoped, lhs = FALSE)
}
#' @rdname as_quosure
#' @export
as_quosureish <- function(x, env = caller_env()) {
  warn("`as_quosureish()` is deprecated as of rlang 0.2.0")
  if (is_quosureish(x)) {
    if (!is_environment(get_env(x))) {
      set_env(x, env)
    }
    x
  } else if (is_frame(x)) {
    new_quosure(x$expr, sys_frame(x$caller_pos))
  } else {
    new_quosure(get_expr(x), get_env(x, env))
  }
}
