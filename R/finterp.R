#' Interpolate a formula
#'
#' Interpolation replaces sub-expressions of the form \code{(( x ))} with
#' the evaluated value of \code{x}.
#'
#' @param f A one-sided formula.
#' @export
#' @examples
#' finterp(~ 1 + ((1 + 2 + 3)) + 10)
#'
#' foo <- function(n) {
#'   ~ 1 + ((n))
#' }
#' f <- foo(10)
#' f
#' finterp(f)
#' @useDynLib lazyeval quasiquote_c
finterp <- function(f) {
  f[[2]] <- .Call(quasiquote_c, rhs(f), environment(f))
  f
}

quasiquote_ <- function(x, env = parent.frame()) {
  .Call(quasiquote_c, x, env)
}

#' @useDynLib lazyeval is_unquote_c
is_unquote <- function(x) {
  .Call(is_unquote_c, substitute(x))
}
#' @useDynLib lazyeval is_unquote_splice_c
is_unquote_splice <- function(x) {
  .Call(is_unquote_splice_c, substitute(x))
}
