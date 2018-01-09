
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

#' Is object a call?
#'
#' These functions are soft-deprecated, please use [is_call()] and its
#' `n` argument instead.
#'
#' @inheritParams is_call
#' @keywords internal
#' @export
is_lang <- function(x, name = NULL, n = NULL, ns = NULL) {
  is_call(x, name, n, ns)
}
#' @rdname is_lang
#' @export
is_unary_lang <- function(x, name = NULL, ns = NULL) {
  is_unary_call(x, name, ns)
}
#' @rdname is_lang
#' @export
is_binary_lang <- function(x, name = NULL, ns = NULL) {
  is_call(x, name, n = 2L, ns = ns)
}

#' Manipulate or access a call
#'
#' These functions are soft-deprecated, please use [call_modify()],
#' [call_standardise()], or [call_fn()] instead.
#'
#' @inheritParams call_modify
#' @param lang,.lang The `call` or `.call` argument of the renamed
#'   functions.
#' @keywords internal
#' @export
lang_modify <- function(.lang, ..., .standardise = FALSE) {
  call_modify(.lang, ..., .standardise = .standardise)
}
#' @rdname lang_modify
#' @export
lang_standardise <- function(lang) {
  call_standardise(lang)
}
#' @rdname lang_modify
#' @export
lang_fn <- function(lang) {
  call_fn(lang)
}
#' @rdname lang_modify
#' @export
lang_name <- function(lang) {
  call_name(lang)
}
#' @rdname lang_modify
#' @export
lang_head <- function(lang) {
  call_head(lang)
}
#' @rdname lang_modify
#' @export
lang_tail <- function(lang) {
  call_tail(lang)
}
#' @rdname lang_modify
#' @export
lang_args <- function(lang) {
  call_args(lang)
}
#' @rdname lang_modify
#' @export
lang_args_names <- function(lang) {
  call_args_names(lang)
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
