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

#' Extract the components of a formula.
#'
#' \code{f_rhs} extracts the righthand side, \code{f_lhs} extracts the
#' lefthand side, and \code{f_env} extracts the environment. All functions
#' throw an error if \code{f} is not a formula.
#'
#' @param f A formula
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
#' @useDynLib lazyeval lhs
f_lhs <- function(f) {
  .Call(lhs, f)
}

#' @export
#' @rdname f_rhs
#' @useDynLib lazyeval env
f_env <- function(f) {
  .Call(env, f)
}


#' Create a formula object by "hand".
#'
#' @param expr A call, name, or atomic vector.
#' @param env An environment
#' @return A formula object
#' @export
#' @keywords internal
f_new <- function(expr, env = parent.frame()) {
  if (!is.call(expr) && !is.name(expr) && !is.atomic(expr)) {
    stop("`expr` is not a valid language object", call. = FALSE)
  }

  structure(
    call("~", expr),
    class = "formula",
    .Environment = env
  )
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
    f_new(substitute_(f_rhs(f), e), parent.env(e))
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
