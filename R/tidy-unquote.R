#' Process unquote operators in a captured expression.
#'
#' While all capturing functions in the tidy evaluation framework
#' perform unquote on capture (most notably [quosure()]),
#' `tidy_interp()` manually processes unquoting operators in
#' expressions that are already captured. `tidy_interp()` should be
#' called in all user-facing functions expecting a formula as argument
#' to provide the same quasiquotation functionality as NSE functions.
#'
#' @param x A function, raw expression, or formula to interpolate.
#' @param env The environment in which unquoted expressions should be
#'   evaluated. By default, the formula or closure environment if a
#'   formula or a function, or the current environment otherwise.
#' @export
#' @examples
#' # All tidy NSE functions like quosure() unquote on capture:
#' quosure(list(!! 1 + 2))
#'
#' # tidy_interp() is meant to provide the same functionality when you
#' # have a formula or expression that might contain unquoting
#' # operators:
#' f <- ~list(!! 1 + 2)
#' tidy_interp(f)
#'
#' # Note that only the outer formula is unquoted (which is a reason
#' # to use tidy_interp() as early as possible in all user-facing
#' # functions):
#' f <- ~list(~!! 1 + 2, !! 1 + 2)
#' tidy_interp(f)
#'
#'
#' # Another purpose for tidy_interp() is to interpolate a closure's
#' # body. This is useful to inline a function within another. The
#' # important limitation is that all formal arguments of the inlined
#' # function should be defined in the receiving function:
#' other_fn <- function(x) toupper(x)
#'
#' fn <- tidy_interp(function(x) {
#'   x <- paste0(x, "_suffix")
#'   !!! body(other_fn)
#' })
#' fn
#' fn("foo")
tidy_interp <- function(x, env = NULL) {
  if (is_formula(x)) {
    f_rhs(x) <- .Call(rlang_interp, f_rhs(x), env %||% f_env(x))
  } else if (is_closure(x)) {
    body(x) <- .Call(rlang_interp, body(x), env %||% fn_env(x))
  } else {
    x <- .Call(rlang_interp, x, env %||% parent.frame())
  }
  x
}

#' @export
#' @rdname quosure
UQ <- function(x) {
  x
}
#' @export
#' @rdname quosure
UQE <- function(x) {
  if (is_formula(x)) {
    f_rhs(x)
  } else {
    x
  }
}
#' @export
#' @rdname quosure
UQF <- function(x) {
  x
}

#' @export
#' @rdname quosure
UQS <- function(x) {
  if (is_pairlist(x)) {
    x
  } else if (is_vector(x)) {
    as.pairlist(x)
  } else if (inherits(x, "{")) {
    node_cdr(x)
  } else if (is_null(x)) {
    NULL
  } else if (is_expr(x)) {
    pairlist(x)
  } else {
    abort("`x` must be a vector or a language object")
  }
}
