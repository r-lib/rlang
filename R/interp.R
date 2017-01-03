#' Interpolate a formula
#'
#' Interpolation replaces sub-expressions of the form \code{UQ(x)} with
#' the evaluated value of \code{x}, and inlines sub-expressions of
#' the form \code{UQS(x)}.
#'
#' @section Theory:
#' Formally, \code{interp} is a quasiquote function, \code{UQ()} is the
#' unquote operator, and \code{UQS()} is the unquote splice operator.
#' These terms have a rich history in LISP, and live on in modern languages
#' like \href{Julia}{http://docs.julialang.org/en/release-0.1/manual/metaprogramming/}
#' and \href{Racket}{https://docs.racket-lang.org/reference/quasiquote.html}.
#'
#' @param f A one-sided formula or a function.
#' @param x For \code{UQ} and \code{UQF}, a formula. For \code{UQS}, a
#'   a vector.
#' @param data When called from inside \code{f_eval}, this is used to pass on
#'   the data so that nested formulas are evaluated in the correct environment.
#' @export
#' @aliases UQ UQS
#' @examples
#' interp(x ~ 1 + UQ(1 + 2 + 3) + 10)
#'
#' # Use UQS() if you want to add multiple arguments to a function
#' # It must evaluate to a list
#' args <- list(1:10, na.rm = TRUE)
#' interp(~ mean( UQS(args) ))
#'
#' # You can combine the two
#' var <- quote(xyz)
#' extra_args <- list(trim = 0.9)
#' interp(~ mean( UQ(var) , UQS(extra_args) ))
#'
#' foo <- function(n) {
#'   ~ 1 + UQ(n)
#' }
#' f <- foo(10)
#' f
#' interp(f)
#' @useDynLib rlang interp_
interp <- function(f, data = NULL) {
  f_rhs(f) <- .Call(interp_, f_rhs(f), f_env(f), data)
  f
}

#' @export
#' @rdname interp
UQ <- function(x, data = NULL) {
  if (is_formula(x)) {
    if (is.null(data)) {
      f_rhs(interp(x))
    } else {
      f_eval(x, data = data)
    }
  } else {
    x
  }
}

#' @export
#' @rdname interp
UQF <- function(x) {
  if (!is_formula(x))
    stop("`x` must be a formula", call. = FALSE)
  x
}

#' @export
#' @rdname interp
UQS <- function(x) {
  if (!is_vector(x)) {
    stop("`x` must be a vector")
  }

  as.pairlist(x)
}
