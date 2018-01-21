#' Default value for `NULL`
#'
#' This infix function makes it easy to replace `NULL`s with a default
#' value. It's inspired by the way that Ruby's or operation (`||`)
#' works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @export
#' @name op-null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function(x, y) {
  if (is_null(x)) y else x
}

#' Replace missing values
#'
#' This infix function is similar to \code{\%||\%} but is vectorised
#' and provides a default value for missing elements. It is faster
#' than using [base::ifelse()] and does not perform type conversions.
#'
#' @param x,y `y` for elements of `x` that are NA; otherwise, `x`.
#' @export
#' @name op-na-default
#' @seealso [op-null-default]
#' @examples
#' c("a", "b", NA, "c") %|% "default"
`%|%` <- function(x, y) {
  stopifnot(is_atomic(x) && is_scalar_atomic(y))
  stopifnot(typeof(x) == typeof(y))
  .Call(rlang_replace_na, x, y)
}

#' Infix attribute accessor
#'
#' @param x Object
#' @param name Attribute name
#' @export
#' @name op-get-attr
#' @examples
#' factor(1:3) %@% "levels"
#' mtcars %@% "class"
`%@%` <- function(x, name) {
  attr(x, name, exact = TRUE)
}

#' Definition operator
#'
#' @description
#'
#' The definition operator is typically used in DSL packages like
#' `ggvis` and `data.table`. It is also used in the tidyverse as a way
#' of unquoting names (see [quasiquotation]).
#'
#' * `is_definition()` returns `TRUE` for calls to `:=`.
#'
#' * `is_formulaish()` returns `TRUE` for both formulas and
#'   colon-equals operators.
#'
#'
#' @details
#'
#' The recommended way to use it is to capture arguments as
#' expressions or quosures. You can then give a special function
#' definition for the `:=` symbol in an overscope. Note that if you
#' capture dots with [exprs()] or [quos()], you need to disable
#' interpretation of `:=` by setting `.unquote_names` to `FALSE`.
#'
#' From rlang and data.table perspectives, this operator is not meant
#' to be evaluated directly at top-level which is why the exported
#' definitions issue an error.
#'
#'
#' @section Life cycle:
#'
#' These functions are experimental.
#'
#' @name op-definition
#' @param x An object to test.
#' @keywords internal
#' @export
#' @examples
#'
#' # A predicate is provided to distinguish formulas from the
#' # colon-equals operator:
#' is_definition(quote(a := b))
#' is_definition(a ~ b)
#'
#'
#' # is_formulaish() tests for both definitions and formulas:
#' is_formulaish(a ~ b)
#' is_formulaish(quote(a := b))
is_definition <- function(x) {
  is_formulaish(x) && identical(node_car(x), colon_equals_sym)
}
#' @rdname op-definition
#' @export
#' @param lhs,rhs Expressions for the LHS and RHS of the definition.
#' @param env The evaluation environment bundled with the definition.
new_definition <- function(lhs, rhs, env = caller_env()) {
  def <- new_formula(lhs, rhs, env)
  node_poke_car(def, colon_equals_sym)
  def
}
#' @rdname op-definition
#' @export
is_formulaish <- function(x, scoped = NULL, lhs = NULL) {
  .Call(rlang_is_formulaish, x, scoped, lhs)
}
