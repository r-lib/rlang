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

#' Is object a formula?
#'
#' @param x Object to test
#' @export
#' @examples
#' is_formula(~ 10)
#' is_formula(10)
is_formula <- function(x) {
  typeof(x) == "language" && inherits(x, "formula")
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
#' @useDynLib lazyeval rhs
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
#' @useDynLib lazyeval lhs
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
#' @useDynLib lazyeval env
f_env <- function(f) {
  .Call(env, f)
}

#' @export
#' @rdname f_rhs
`f_env<-` <- function(x, value) {
  stopifnot(is_formula(x))
  f_new(f_rhs(x), f_lhs(x), value)
}

#' Turn RHS of formula into a string/label.
#'
#' Equivalent of \code{\link{expr_text}()} and \code{\link{expr_label}()} for
#' formulas.
#'
#' @param x A formula.
#' @inheritParams expr_text
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
  expr_text_(f_rhs(x), width = width, nlines = nlines)
}

#' @export
#' @rdname f_text
f_label <- function(x) {
  expr_label_(f_rhs(x))
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
#' @useDynLib lazyeval lhs_name
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
