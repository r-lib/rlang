#' Interpolate a formula
#'
#' Interpolation replaces sub-expressions of the form \code{UQ(x)}
#' with the evaluated value of \code{x}, and inlines sub-expressions
#' of the form \code{UQS(x)}. Syntactic shortcuts are provided for
#' unquoting and unquote-splicing by prefixing with \code{!!} and
#' \code{!!!}.
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
#' @export
#' @aliases UQ UQE UQF UQS
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
#'
#'
#' # You can also unquote and splice syntactically with bang operators:
#' interp(~mean(!!! args))
#'
#' # However you need to be a bit careful with operator precedence.
#' # All arithmetic and comparison operators bind more tightly than `!`:
#' interp(x ~ 1 +  !! (1 + 2 + 3) + 10)
#' interp(x ~ 1 + (!! (1 + 2 + 3)) + 10)
#'
#'
#' # When a formula is unquoted, interp() checks whether its
#' # environment is informative. It is not informative when the object
#' # within the formula is a constant (for example, a string) or when
#' # the environment recorded in the formula is the same as the outer
#' # formula in which it is unquoted. In those cases, the formula is
#' # embedded as is:
#' var <- ~letters
#' interp(~toupper(!!var))
#'
#' # On the other hand, if the environment is informative (i.e., if
#' # the symbols within the inner formula could represent other
#' # objects than in the outer formula because they have different
#' # scopes), it is embedded as a promise:
#' var <- local(~letters)
#' interp(~toupper(!!var))
#'
#'
#' # The formula-promise representation is necessary to preserve scope
#' # information and make sure objects are looked up in the right
#' # place. However, there are situations where it can get in the way.
#' # This is the case when you deal with non-tidy NSE functions that do
#' # not understand formulas. You can inline the RHS of a formula in a
#' # call thanks to the UQE() operator:
#' nse_function <- function(arg) substitute(arg)
#' var <- ~foo(bar)
#' interp(~nse_function(UQE(var)))
#'
#' # This is equivalent to unquoting and taking the RHS:
#' interp(~nse_function(!! f_rhs(var)))
#'
#' # One of the most important old-style NSE function is the dollar
#' # operator. You need to use UQE() for subsetting with dollar:
#' var <- ~cyl
#' interp(~mtcars$UQE(var))
#'
#' # `!!`() is also treated as a shortcut. It is meant for situations
#' # where the bang operator would not parse, such as subsetting with
#' # $. Since that's its main purpose, we've made it a shortcut for
#' # UQE() rather than UQ():
#' var <- ~cyl
#' interp(~mtcars$`!!`(var))
#'
#'
#' # Sometimes you would like to unquote an object containing a
#' # formula but include it as is rather than treating it as a
#' # promise. You can use UQF() for this purpose:
#' var <- disp ~ am
#' interp(~lm(!!var, mtcars))
#' interp(~lm(UQF(var), mtcars))
#' f_eval(~lm(UQF(var), mtcars))
#'
#'
#' # Finally, you can also interpolate a closure's body. This is
#' # useful to inline a function within another:
#' other_fn <- function(x) toupper(x)
#' fn <- interp(function(x) {
#'   x <- paste0(x, "_suffix")
#'   !!! body(other_fn)
#' })
#' fn
#' fn("foo")
#' @useDynLib rlang interp_
interp <- function(f) {
  if (is_formula(f)) {
    f_rhs(f) <- .Call(interp_, f_rhs(f), f_env(f), TRUE)
  } else if (is_closure(f)) {
    body(f) <- .Call(interp_, body(f), fn_env(f), FALSE)
  } else {
    abort("`f` must be a formula or a closure")
  }
  f
}

#' @export
#' @rdname interp
UQ <- function(x) {
  x
}
#' @export
#' @rdname interp
UQE <- function(x) {
  if (is_formula(x)) {
    f_rhs(x)
  } else {
    x
  }
}
#' @export
#' @rdname interp
UQF <- function(x) {
  bquote(`_F`(.(x)))
}

#' @export
#' @rdname interp
UQS <- function(x) {
  if (is_pairlist(x)) {
    x
  } else if (is_vector(x)) {
    as.pairlist(x)
  } else if (inherits(x, "{")) {
    cdr(x)
  } else if (is_lang(x)) {
    pairlist(x)
  } else {
    abort("`x` must be a vector or a language object")
  }
}
