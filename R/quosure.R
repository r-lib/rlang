#' Create a quosure by hand.
#'
#' This is similar to \code{\link{new_formula}()} but for one-sided
#' formulas. See \code{\link{tidy_quote}()} for the role played by
#' such formulas in the tidy evaluation framework.
#'
#' @inheritParams new_formula
#' @export
#' @examples
#' f <- quosure(quote(mtcars), env("datasets"))
#' f
#' tidy_eval(f)
quosure <- function(rhs, env = caller_env()) {
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
#' for those degenerate formulas but \code{is_quosure()} will
#' return \code{FALSE}. Note that in the tidy evaluation framework
#' (see \code{\link{tidy_eval}()}), untidy formulas are automatically
#' given the environment of the outer formula and do not require
#' special actions on your part.
#'
#' \code{as_quosure()} is useful for SE functions that expect a
#' tidy formula quote but allow specifying a raw expression as
#' well. It has two possible effects: if \code{x} is not a formula, it
#' wraps it into a formula with \code{env}. If \code{x} is a
#' degenerate formula, it turns it into a tidy quote by adding
#' \code{env}. Finally, if \code{x} is a tidy quote, it is left alone
#' (even if \code{env} is not the same as the formula environment).
#'
#' @param x An object to test or convert.
#' @param env The environment for the returned tidy quote.
#' @param scoped Whether the quosure is scoped, that is, has a valid
#'   environment attribute. If \code{NULL}, the quosure scope is not
#'   inspected.
#' @export
#' @examples
#' # Degenerate formulas are often created by quoting, since `~`
#' # records the environment when it is evaluated the first time:
#' f <- ~~expr
#'
#' # The outer formula has been evaluated and is a tidy quote:
#' is_quosure(f)
#'
#' # But the inner formula is not:
#' inner_f <- f_rhs(f)
#' is_quosure(inner_f)
#'
#' # You can use as_quosure() to add the environment information:
#' as_quosure(inner_f, base_env())
#'
#' # Or turn expressions or any R object in a tidy quote:
#' as_quosure(quote(expr), env())
#' as_quosure(10L, env())
as_quosure <- function(x, env) {
  if (is_formula(x)) {
    if (is_null(f_env(x))) {
      f_env(x) <- env
    }
    x
  } else if (is_frame(x)) {
    quosure(x$expr, sys_frame(x$caller_pos))
  } else {
    quosure(x, env)
  }
}

#' @rdname as_quosure
#' @export
is_quosure <- function(x, scoped = NULL) {
  if (!is_one_sided(x)) {
    return(FALSE)
  }
  if (!is_null(scoped) && scoped != is_env(f_env(x))) {
    return(FALSE)
  }
  TRUE
}
#' @rdname as_quosure
#' @export
is_quosureish <- function(x, scoped = NULL) {
  if (!is_formula(x)) {
    return(FALSE)
  }
  if (!is_null(scoped) && scoped != is_env(f_env(x))) {
    return(FALSE)
  }
  TRUE
}
is_one_sided <- function(x, lang_sym = sym_tilde) {
  typeof(x) == "language" &&
    identical(node_car(x), lang_sym) &&
    is_null(node_cadr(node_cdr(x)))
}
