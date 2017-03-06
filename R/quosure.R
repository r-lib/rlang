#' Is an object a quosure or quosure-like?
#'
#' @description
#'
#' These predicates test for [quosure][quosure] objects.
#'
#' - `is_quosure()` tests for the canonical R quosure: the one-sided
#'   "formula".
#'
#' - `is_quosureish()` tests for general R quosure objects: quosures,
#'   two-sided formulas, and [definitions][op-definition].
#'
#'
#' @details
#'
#' The `scoped` argument patterns-match on whether the scoped bundled
#' with the quosure is valid or not. Invalid scopes may happen in
#' nested quotations like `~~expr`, where the outer quosure is validly
#' scoped but not the inner one. This is because `~` saves the
#' environment when it is evaluated, and quoted quosures are by
#' definition not evaluated. Note that in the [tidy evaluation
#' framework][tidy_eval], unscoped quosures are automatically given
#' the environment of the outer quosure during the evaluation process.
#'
#' @param x An object to test.
#' @param scoped Whether the quosure is scoped, that is, has a valid
#'   environment attribute. If `NULL`, the quosure scope is not
#'   inspected.
#' @seealso [as_quosure()][quosure] and [quosure()] for creating
#'   quosures, and [tidy_quote()] or [tidy_eval()] for information
#'   about the role of quosures in the tidy evaluation framework.
#' @export
#' @examples
#' # Degenerate quosures are often created by quoting, since `~`
#' # records the environment when it is evaluated the first time:
#' f <- ~~expr
#'
#' # The outer quosure has been evaluated and is scoped:
#' is_quosure(f, scoped = TRUE)
#'
#' # But the inner formula is not:
#' inner_f <- f_rhs(f)
#' is_quosure(inner_f, scoped = TRUE)
#'
#'
#' # Formulas and definitions are not quosures:
#' is_quosure(a := b)
#' is_quosure(a ~ b)
#'
#' # But they are quosureish objects:
#' is_quosureish(a := b)
#' is_quosureish(a ~ b)
#' @md
is_quosure <- function(x, scoped = NULL) {
  if (!is_one_sided(x)) {
    return(FALSE)
  }
  if (!is_null(scoped) && scoped != is_env(f_env(x))) {
    return(FALSE)
  }
  TRUE
}
#' @rdname is_quosure
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


#' Create quosures.
#'
#' @description
#'
#' Quosure objects wrap an [expression][is_expr] with a [lexical
#' enclosure][env]. This is a powerful quoting (see [base::quote()]
#' and [tidy_quote()]) mechanism that makes it possible to carry and
#' manipulate expressions while making sure that its symbolic content
#' (symbols and named calls, see [is_symbolic()]) is correctly looked
#' up during evaluation.
#'
#' - `quosure()` creates a quosure from a raw expression and an
#'   environment.
#'
#' - `as_quosure()` is useful for functions that expect quosures but
#'   allow specifying a raw expression as well. It has two possible
#'   effects: if `x` is not a quosure, it wraps it into a quosure
#'   bundling `env` as scope. If `x` is an unscoped quosure (see
#'   [is_quosure()]), `env` is used as a default scope. On the other
#'   hand if `x` has a valid enclosure, it is returned as is (even if
#'   `env` is not the same as the formula environment).
#'
#' - While `as_quosure()` always returns a quosure (a one-sided
#'   formula), even when its input is a [formula][new_formula] or a
#'   [definition][op-definition], `as_quosureish()` returns quosureish
#'   inputs as is.
#'
#' @inheritParams new_formula
#' @param x An object to convert.
#' @param env An environment specifying the lexical enclosure of the
#'   quosure.
#' @seealso [is_quosure()]
#' @export
#' @examples
#' f <- quosure(quote(mtcars), env("datasets"))
#' f
#' tidy_eval(f)
#'
#'
#' # Sometimes you get unscoped quosures because of quotation:
#' f <- ~~expr
#' inner_f <- f_rhs(f)
#' inner_f
#' is_quosure(inner_f, scoped = TRUE)
#'
#' # You can use as_quosure() to provide a default environment:
#' as_quosure(inner_f, base_env())
#'
#' # Or convert expressions or any R object to a validly scoped quosure:
#' as_quosure(quote(expr), base_env())
#' as_quosure(10L, base_env())
#'
#'
#' # While as_quosure() always returns a quosure (one-sided formula),
#' # as_quosureish() returns quosureish objects:
#' as_quosure(a := b)
#' as_quosureish(a := b)
#' as_quosureish(10L)
#' @md
quosure <- function(rhs, env = caller_env()) {
  new_formula(NULL, rhs, env)
}
#' @rdname quosure
#' @export
as_quosure <- function(x, env = caller_env()) {
  if (is_quosureish(x)) {
    env <- f_env(x) %||% env
    quosure(f_rhs(x), env)
  } else if (is_frame(x)) {
    quosure(x$expr, sys_frame(x$caller_pos))
  } else {
    quosure(x, env)
  }
}
#' @rdname quosure
#' @export
as_quosureish <- function(x, env = caller_env()) {
  if (is_quosureish(x)) {
    f_env(x) <- f_env(x) %||% env
    x
  } else if (is_frame(x)) {
    quosure(x$expr, sys_frame(x$caller_pos))
  } else {
    quosure(x, env)
  }
}
