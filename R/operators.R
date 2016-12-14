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
