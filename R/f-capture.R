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
#' @return \code{f_capture()} returns a formula; \code{dots_capture()}
#'   returns a list of formulas.
#' @examples
#' f_capture(a + b)
#' dots_capture(a + b, c + d, e + f)
#'
#' # These functions will follow a chain of promises back to the
#' # original definition
#' f <- function(x) g(x)
#' g <- function(y) h(y)
#' h <- function(z) f_capture(z)
#' f(a + b + c)
f_capture <- function(x) {
  info <- arg_info(x)
  f_new(info$expr, env = info$eval_frame$env)
}

#' @export
#' @rdname f_capture
dots_capture <- function(..., .ignore_empty = TRUE) {
  lazies <- .Call(make_lazy_dots, environment(), TRUE, .ignore_empty)
  lapply(lazies, function(x) f_new(x$expr, env = x$env))
}
