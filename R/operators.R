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
#' @param x The original values.
#' @param y The replacement values. Must be of length 1 or the same length as `x`.
#' @export
#' @name op-na-default
#' @seealso [op-null-default]
#' @examples
#' c("a", "b", NA, "c") %|% "default"
#' c(1L, NA, 3L, NA, NA) %|% (6L:10L)
`%|%` <- function(x, y) {
  stopifnot(is_atomic(x) && is_atomic(y))
  stopifnot(length(y) == 1 || length(y) == length(x))
  stopifnot(typeof(x) == typeof(y))
  .Call(rlang_replace_na, x, y)
}

#' Infix attribute accessor and setter
#'
#' This operator extracts or sets attributes for regular objects and
#' S4 fields for S4 objects.
#'
#' @param x Object
#' @param name Attribute name
#' @export
#' @name op-get-attr
#' @examples
#' # Unlike `@`, this operator extracts attributes for any kind of
#' # objects:
#' factor(1:3) %@% "levels"
#' mtcars %@% class
#'
#' mtcars %@% class <- NULL
#' mtcars
#'
#' # It also works on S4 objects:
#' .Person <- setClass("Person", slots = c(name = "character", species = "character"))
#' fievel <- .Person(name = "Fievel", species = "mouse")
#' fievel %@% name
`%@%` <- function(x, name) {
  name <- as_string(ensym(name))
  if (isS4(x)) {
    eval_bare(expr(`@`(x, !!name)))
  } else {
    attr(x, name, exact = TRUE)
  }
}
#' @rdname op-get-attr
#' @param value New value for attribute `name`.
#' @usage x \%@\% name <- value
#' @export
`%@%<-` <- function(x, name, value) {
  name <- as_string(ensym(name))
  if (isS4(x)) {
    eval_bare(expr(`@`(x, !!name) <- value))
  } else {
    eval_bare(expr(attr(x, !!name) <- value))
  }
  x
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
