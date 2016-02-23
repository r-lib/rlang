#' Make a promise explicit by converting into a formula.
#'
#' This should be used sparingly if you want to implement true non-standard
#' evaluation with 100\% magic. I recommend avoiding this unless you have
#' strong reasons otherwise since requiring arguments to be formulas only
#' adds one extra character to the inputs, and otherwise makes life much much
#' simpler.
#'
#' @param x,... An unevaluated promises
#' @param .ignore_empty If \code{TRUE}, empty arguments will be silently
#'    dropped.
#' @export
#' @return \code{explicit_promise} returns a formula; \code{explicit_dots}
#'   returns a list of formulas.
#' @examples
#' explicit_promise(a + b)
#' explicit_dots(a + b, c + d, e + f)
#'
#' # These functions will follow a chain of promises back to the
#' # original definition
#' f <- function(x) g(x)
#' g <- function(y) h(y)
#' h <- function(z) explicit_promise(z)
#' f(a + b + c)
explicit_promise <- function(x) {
  lazy <- .Call(make_lazy, quote(x), environment(), TRUE)
  make_formula(lazy$expr, lazy$env)
}

#' @export
explicit_dots <- function(..., .ignore_empty = TRUE) {
  if (nargs() == 0 || (nargs() == 1 && !missing(.ignore_empty))) {
    return(structure(list()))
  }

  lazies <- .Call(make_lazy_dots, environment(), TRUE, .ignore_empty)
  lapply(lazies, function(x) make_formula(x$expr, x$env))
}
