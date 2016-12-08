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
`%||%` <- function(x, y) if (is_null(x)) y else x

#' Infix attribute accessor
#'
#' @param x Object
#' @param name Attribute name
#' @export
#' @name op-get-attr
#' @examples
#' factor(1:3) %@% "levels"
#' mtcars %@% "class"
`%@%` <- function(x, name) attr(x, name, exact = TRUE)
