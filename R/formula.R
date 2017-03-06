#' Create a formula object by "hand".
#'
#' @param lhs,rhs A call, name, or atomic vector.
#' @param env An environment.
#' @return A formula object.
#' @seealso \code{\link{quosure}()}
#' @export
#' @examples
#' new_formula(quote(a), quote(b))
#' new_formula(NULL, quote(b))
new_formula <- function(lhs, rhs, env = caller_env()) {
  if (!is_env(env) && !is_null(env)) {
    abort("`env` must be an environment")
  }

  if (is_null(lhs)) {
    f <- lang("~", rhs)
  } else {
    f <- lang("~", lhs, rhs)
  }

  structure(f, class = "formula", .Environment = env)
}

as_formula <- NULL

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
#' @useDynLib rlang f_rhs_
f_rhs <- function(f) {
  .Call(f_rhs_, f)
}

#' @export
#' @rdname f_rhs
`f_rhs<-` <- function(x, value) {
  stopifnot(is_formula(x))
  f <- new_formula(f_lhs(x), value, f_env(x))
  copy_lang_name(f, x)
}

#' @export
#' @rdname f_rhs
#' @useDynLib rlang f_lhs_
f_lhs <- function(f) {
  .Call(f_lhs_, f)
}

#' @export
#' @rdname f_rhs
`f_lhs<-` <- function(x, value) {
  stopifnot(is_formula(x))
  f <- new_formula(value, f_rhs(x), f_env(x))
  copy_lang_name(f, x)
}

copy_lang_name <- function(f, x) {
  f[[1]] <- x[[1]]
  f
}

#' @export
#' @rdname f_rhs
f_env <- function(f) {
  if(!is_formula(f)) {
    abort("`f` is not a formula")
  }
  attr(f, ".Environment")
}

#' @export
#' @rdname f_rhs
`f_env<-` <- function(x, value) {
  stopifnot(is_formula(x))
  f <- new_formula(f_lhs(x), f_rhs(x), value)
  copy_lang_name(f, x)
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
  expr_text(f_rhs(x), width = width, nlines = nlines)
}

#' @export
#' @rdname f_text
f_label <- function(x) {
  expr_label(f_rhs(x))
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
  if (identical(e, empty_env())) {
    f
  } else {
    new_formula(f_lhs(f), substitute_(f_rhs(f), e), env_parent(e))
  }
}
