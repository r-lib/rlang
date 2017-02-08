#' Default value for \code{NULL}.
#'
#' This infix function makes it easy to replace \code{NULL}s with a
#' default value. It's inspired by the way that Ruby's or operation
#' (\code{||}) works.
#'
#' @param x,y If \code{x} is NULL, will return \code{y}; otherwise
#'   returns \code{x}.
#' @export
#' @name op-null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function(x, y) {
  if (is_null(x)) y else x
}

#' Replace missing values.
#'
#' This infix function is similar to \code{\%||\%} but is vectorised
#' and provides a default value for missing elements. It is faster
#' than using \code{\link[base]{ifelse}()} and does not perform type
#' conversions.
#'
#' @param x,y \code{y} for elements of \code{x} that are NA;
#'   otherwise, \code{x}.
#' @useDynLib rlang replace_na
#' @export
#' @name op-na-default
#' @seealso \link{op-null-default}
#' @examples
#' c("a", "b", NA, "c") %|% "default"
`%|%` <- function(x, y) {
  stopifnot(is_atomic(x) && is_scalar_atomic(y))
  stopifnot(typeof(x) == typeof(y))
  .Call(replace_na, x, y)
}

#' Infix attribute accessor.
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

#' Definition operator.
#'
#' The definition operator is typically used in DSL packages like
#' \code{ggvis} and \code{data.table}. It is exported in rlang as a
#' alias to \code{~}. This makes it a quoting operator that can be
#' shared between packages for computing on the language. Since it
#' effectively creates formulas, it is immediately compatible with
#' rlang's formulas and interpolation features.
#'
#' @export
#' @examples
#' # This is useful to provide an alternative way of specifying
#' # arguments in DSLs:
#' fn <- function(...) ..1
#' f <- fn(arg := foo(bar) + baz)
#'
#' is_formula(f)
#' f_lhs(f)
#' f_rhs(f)
#' @name op-definition
`:=` <- `~`

#' @rdname op-definition
#' @param x An object to test.
#' @export
#' @examples
#'
#' # A predicate is provided to distinguish formulas from the
#' # colon-equals operator:
#' is_definition(a := b)
#' is_definition(a ~ b)
is_definition <- function(x) {
  is_formula(x) && identical(x[[1]], quote(`:=`))
}

#' @rdname op-definition
#' @export
#' @param lhs,rhs Expressions for the LHS and RHS of the definition.
#' @param env The evaluation environment bundled with the definition.
new_definition <- function(lhs, rhs, env = caller_env()) {
  pat <- new_formula(lhs, rhs, env)
  pat[[1]] <- quote(`:=`)
  pat
}
