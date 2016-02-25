#' Interpolate a formula
#'
#' Interpolation replaces sub-expressions of the form \code{(( x ))} with
#' the evaluated value of \code{x}, and inlines sub-expressions of
#' the form \code{({ x })}.
#'
#' @section Theory:
#' Formally, \code{finterp} is a quasiquote function, \code{((} is the
#' unquote operator, and \code{(\{} is the unquote splice operator.
#' These terms have a rich history in LISP, and live on in modern languages
#' like \href{Julia}{http://docs.julialang.org/en/release-0.1/manual/metaprogramming/}
#' and \href{Racket}{https://docs.racket-lang.org/reference/quasiquote.html}.
#'
#' @param f A one-sided formula.
#' @export
#' @aliases unquote unquote_splice (\{ ((
#' @examples
#' finterp(~ 1 + ((1 + 2 + 3)) + 10)
#'
#' # Use ({ }) if you want to add multiple arguments to a function
#' # It must evaluate to a list
#' args <- list(1:10, na.rm = TRUE)
#' finterp(~ mean( ({args}) ))
#'
#' # You can combine the two
#' var <- quote(xyz)
#' extra_args <- list(trim = 0.9)
#' finterp(~ mean( ((var)) , ({extra_args}) ))
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


# Functions for testing ---------------------------------------------------

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
