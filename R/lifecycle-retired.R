
signal_soft_deprecation <- function(msg) {
  if (is_true(peek_option("lifecycle_force_verbose_retirement"))) {
    warn(msg)
  }
  invisible(NULL)
}


# Soft-deprecated ----------------------------------------------------

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

#' Squash a quosure
#'
#' This function is soft-deprecated, please use [quo_squash()] instead.
#'
#' @inheritParams quo_squash
#' @keywords internal
#' @export
quo_expr <- function(quo, warn = FALSE) {
  quo_squash(quo, warn = warn)
}

#' Create a call
#'
#' This function is soft-deprecated, please use [call2()] instead.
#'
#' @inheritParams call2
#' @keywords internal
#' @export
lang <- function(.fn, ..., .ns = NULL) {
  call2(.fn, ..., .ns = NULL)
}
#' @rdname lang
#' @inheritParams new_call
#' @export
new_language <- function(head, tail = NULL) {
  new_call(head, tail)
}


# Deprecated ---------------------------------------------------------

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
