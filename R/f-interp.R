#' Interpolate a formula
#'
#' Interpolation replaces sub-expressions of the form \code{uq(x)} with
#' the evaluated value of \code{x}, and inlines sub-expressions of
#' the form \code{uqs(x)}.
#'
#' @section Theory:
#' Formally, \code{f_interp} is a quasiquote function, \code{uq()} is the
#' unquote operator, and \code{uqs()} is the unquote splice operator.
#' These terms have a rich history in LISP, and live on in modern languages
#' like \href{Julia}{http://docs.julialang.org/en/release-0.1/manual/metaprogramming/}
#' and \href{Racket}{https://docs.racket-lang.org/reference/quasiquote.html}.
#'
#' @param f A one-sided formula.
#' @param x For
#' @export
#' @aliases uq uqs
#' @examples
#' f_interp(~ 1 + uq(1 + 2 + 3) + 10)
#'
#' # Use uqs() if you want to add multiple arguments to a function
#' # It must evaluate to a list
#' args <- list(1:10, na.rm = TRUE)
#' f_interp(~ mean( uqs(args) ))
#'
#' # You can combine the two
#' var <- quote(xyz)
#' extra_args <- list(trim = 0.9)
#' f_interp(~ mean( uq(var) , uqs(extra_args) ))
#'
#' foo <- function(n) {
#'   ~ 1 + uq(n)
#' }
#' f <- foo(10)
#' f
#' f_interp(f)
#' @useDynLib lazyeval quasiquote_c
f_interp <- function(f) {
  f[[2]] <- .Call(quasiquote_c, f_rhs(f), environment(f))
  f
}


# Functions for testing ---------------------------------------------------

quasiquote_ <- function(x, env = parent.frame()) {
  .Call(quasiquote_c, x, env)
}

#' @export
#' @rdname f_interp
uq <- function(x) {
  x
}

#' @export
#' @rdname f_interp
uqs <- function(x) {
  if (!is_vector(x)) {
    stop("`x` must be a vector")
  }

  as.pairlist(x)
}
