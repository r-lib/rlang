#' Create a formula object by "hand".
#'
#' @param lhs,rhs A call, name, or atomic vector.
#' @param env An environment
#' @return A formula object
#' @export
#' @examples
#' f_new(quote(a))
#' f_new(quote(a), quote(b))
f_new <- function(rhs, lhs = NULL, env = parent.frame()) {
  if (!is.environment(env)) {
    stop("`env` must be an environment", call. = FALSE)
  }

  if (is.null(lhs)) {
    f <- call_new("~", rhs)
  } else {
    f <- call_new("~", lhs, rhs)
  }

  structure(
    f,
    class = "formula",
    .Environment = env
  )
}

#' Get/set formula components.
#'
#' \code{f_rhs} extracts the righthand side, \code{f_lhs} extracts the
#' lefthand side, and \code{f_env} extracts the environment. All functions
#' throw an error if \code{f} is not a formula.
#'
#' @param f,x A formula
#' @param value The value to replace with.
#' @export
#' @return \code{f_rhs} and \code{f_lhs} return language objects (i.e.
#'   atomic vectors of length 1, a name, or a call). \code{f_env}
#'   returns an environment.
#' @examples
#' f_rhs(~ 1 + 2 + 3)
#' f_rhs(~ x)
#' f_rhs(~ "A")
#' f_rhs(1 ~ 2)
#'
#' f_lhs(~ y)
#' f_lhs(x ~ y)
#'
#' f_env(~ x)
#' @useDynLib rlang rhs
f_rhs <- function(f) {
  .Call(rhs, f)
}

#' @export
#' @rdname f_rhs
`f_rhs<-` <- function(x, value) {
  stopifnot(is_formula(x))
  f_new(value, f_lhs(x), f_env(x))
}

#' @export
#' @rdname f_rhs
#' @useDynLib rlang lhs
f_lhs <- function(f) {
  .Call(lhs, f)
}

#' @export
#' @rdname f_rhs
`f_lhs<-` <- function(x, value) {
  stopifnot(is_formula(x))
  f_new(f_rhs(x), value, f_env(x))
}

#' @export
#' @rdname f_rhs
f_env <- function(f) {
  if(!is_formula(f)) {
    stop("`f` is not a formula", call. = FALSE)
  }
  attr(f, ".Environment")
}

#' @export
#' @rdname f_rhs
`f_env<-` <- function(x, value) {
  stopifnot(is_formula(x))
  f_new(f_rhs(x), f_lhs(x), value)
}

#' Turn RHS of formula into a string/label.
#'
#' Equivalent of \code{\link{arg_text}()} and \code{\link{arg_label}()} for
#' formulas.
#'
#' @param x A formula.
#' @inheritParams arg_text
#' @export
#' @examples
#' f <- ~ a + b + bc
#' f_text(f)
#' f_label(f)
#'
#' # Names a quoted with ``
#' f_label(~ x)
#' # Strings are encoded
#' f_label(~ "a\nb")
#' # Long expressions are collapsed
#' f_label(~ foo({
#'   1 + 2
#'   print(x)
#' }))
f_text <- function(x, width = 60L, nlines = Inf) {
  arg_text_(f_rhs(x), width = width, nlines = nlines)
}

#' @export
#' @rdname f_text
f_label <- function(x) {
  arg_label_(f_rhs(x))
}

#' Unwrap a formula
#'
#' This interpolates values in the formula that are defined in its environment,
#' replacing the environment with its parent.
#'
#' @export
#' @param f A formula to unwrap.
#' @examples
#' n <- 100
#' f <- ~ x + n
#' f_unwrap(f)
f_unwrap <- function(f) {
  stopifnot(is_formula(f))

  e <- environment(f)
  if (identical(e, emptyenv())) {
    f
  } else {
    f_new(substitute_(f_rhs(f), e), f_lhs(f), parent.env(e))
  }
}

#' Build a named list from the LHS of formulas
#'
#' \code{f_list} makes a new list; \code{as_f_list} takes an existing list.
#' Both take the LHS of any two-sided formulas and evaluate it, replacing the
#' current name with the result.
#'
#' @param ... Named arguments.
#' @param x An existing list
#' @return A named list.
#' @export
#' @useDynLib rlang lhs_name
#' @examples
#' f_list("y" ~ x)
#' f_list(a = "y" ~ a, ~ b, c = ~c)
f_list <- function(...) {
  .Call(lhs_name, list(...))
}

#' @export
#' @rdname f_list
as_f_list <- function(x) {
  .Call(lhs_name, x)
}

#' Coerce quoted expressions to quoted formula.
#'
#' Creates a formula from an expression and an environment. This
#' function is a helper for tools that work with captured
#' expressions. It makes your tools callable with either an expression
#' or a formula, and with an optional environment. When not supplied,
#' the calling environment of the function that called
#' \code{as_quoted_f()} is taken as default.
#'
#' \code{as_quoted_f()} makes it straightforward to take an optional
#' environment to associate with a quoted expression. An alternative
#' would be to specify a default environment at each step,
#' e.g. \code{env = env_caller()}. In that case however, there is no
#' easy way of communicating the optional nature of \code{env}. This
#' is necessary to avoid overriding the environment of formulas
#' supplied as \code{expr} with the optional default.
#'
#' @param expr A quoted expression or a formula.
#' @param env The environment of the returned formula. If \code{expr}
#'   is a formula and if \code{env} is supplied, the formula
#'   environment is changed to \code{env}. If \code{expr} is a quoted
#'   expression and \code{env} is not supplied, the environment is
#'   taken from the frame of the function that called the function
#'   that called \code{as_quoted_f()} (the grand-parent frame).
#' @export
#' @examples
#' # as_quoted_f() is meant to be called at every step of the way, so
#' # that each function can figure out a good default for `env`:
#' api_function <- function(expr, env = NULL) {
#'   f <- as_quoted_f(expr, env)
#'   expr_tool(f)
#' }
#' expr_tool <- function(expr, env = NULL) {
#'   f <- as_quoted_f(expr, env)
#'   # *** Do something useful with f ***
#'   f
#' }
#'
#' # Then the user can supply an expression or a formula. If an
#' # expression, and no environment is supplied, the caller
#' # environment is taken as default:
#' f <- api_function(quote(foobar))
#' env(f)
#'
#' # The user can supply her own environment:
#' env <- env_new()
#' f <- api_function(quote(foobar), env)
#' identical(env(f), env)
#'
#' # With a formula, the default is to take the formula environment
#' # rather than the caller environment:
#' my_f <- env_set(~expr, env)
#' f <- api_function(my_f)
#' identical(env(f), env)
#'
#' # But the user can choose to provide her own environment as well:
#' f <- api_function(my_f, env_base())
#' identical(env(f), env_base())
as_quoted_f <- function(expr, env = NULL) {
  if (is_formula(expr)) {
    if (!is_null(env)) {
      f_env(expr) <- env
    }
    expr
  } else {
    env <- env %||% env_caller(2)
    f_new(expr, env = env)
  }
}
