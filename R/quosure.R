#' Create a quosure by hand.
#'
#' This is similar to \code{\link{new_formula}()} but for one-sided
#' formulas. See \code{\link{tidy_quote}()} for the role played by
#' such formulas in the tidy evaluation framework.
#'
#' @inheritParams new_formula
#' @export
#' @examples
#' f <- new_tidy_quote(quote(mtcars), env("datasets"))
#' f
#' tidy_eval(f)
new_tidy_quote <- function(rhs, env = caller_env()) {
  new_formula(NULL, rhs, env)
}

#' Coerce expressions to a tidy formula quote.
#'
#' A tidy quote is a formula or definition (see
#' \code{\link{op-definition}}) that bundles an expression and an
#' environment. In some situations a formula will not be a tidy quote
#' because it does not carry environment information. That happens for
#' instance when you quote a formula, e.g. in this snippet the outer
#' formula is a tidy quote but not the inner one:
#' \code{~~expr}. \code{\link{is_formula}()} will return \code{TRUE}
#' for those degenerate formulas but \code{is_tidy_quote()} will
#' return \code{FALSE}. Note that in the tidy evaluation framework
#' (see \code{\link{tidy_eval}()}), untidy formulas are automatically
#' given the environment of the outer formula and do not require
#' special actions on your part.
#'
#' \code{as_tidy_quote()} is useful for SE functions that expect a
#' tidy formula quote but allow specifying a raw expression as
#' well. It has two possible effects: if \code{x} is not a formula, it
#' wraps it into a formula with \code{env}. If \code{x} is a
#' degenerate formula, it turns it into a tidy quote by adding
#' \code{env}. Finally, if \code{x} is a tidy quote, it is left alone
#' (even if \code{env} is not the same as the formula environment).
#'
#' @param x An object to test or convert.
#' @param env The environment for the returned tidy quote.
#' @export
#' @examples
#' # Degenerate formulas are often created by quoting, since `~`
#' # records the environment when it is evaluated the first time:
#' f <- ~~expr
#'
#' # The outer formula has been evaluated and is a tidy quote:
#' is_tidy_quote(f)
#'
#' # But the inner formula is not:
#' inner_f <- f_rhs(f)
#' is_tidy_quote(inner_f)
#'
#' # You can use as_tidy_quote() to add the environment information:
#' as_tidy_quote(inner_f, base_env())
#'
#' # Or turn expressions or any R object in a tidy quote:
#' as_tidy_quote(quote(expr), env())
#' as_tidy_quote(10L, env())
as_tidy_quote <- function(x, env) {
  if (is_formula(x)) {
    if (is_null(f_env(x))) {
      f_env(x) <- env
    }
    x
  } else if (is_frame(x)) {
    new_tidy_quote(x$expr, sys_frame(x$caller_pos))
  } else {
    new_tidy_quote(x, env)
  }
}

#' @rdname as_tidy_quote
#' @export
is_tidy_quote <- function(x) {
  is_formula(x) && is_env(f_env(x))
}
#' @rdname as_tidy_quote
#' @export
is_tquote <- is_tidy_quote
