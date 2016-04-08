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

#' Extract the right-hand-side of a formula
#'
#' Throws an error if \code{f} is not a formula.
#'
#' @param f A formula
#' @export
#' @return An atomic vector of length 1, a name, or a call.
#' @examples
#' f_rhs(~ 1 + 2 + 3)
#' f_rhs(~ x)
#' f_rhs(~ "A")
#' f_rhs(1 ~ 2)
#'
#' f_lhs(~ y)
#' f_lhs(x ~ y)
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

#' @useDynLib lazyeval lhs_name
f_list <- function(...) {
  .Call(lhs_name, list(...))
}
