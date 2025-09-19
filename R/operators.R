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

# Reexport from base on newer versions of R to avoid conflict messages
if (exists("%||%", envir = baseenv())) {
  `%||%` <- get("%||%", envir = baseenv())
}

#' Default value for non-`NULL`
#'
#' This infix operator is the conceptual opposite of `%||%`, providing a fallback
#' only if `x` is defined.
#'
#' @param x,y If `x` is NULL, will return `x`; otherwise returns `y`.
#' @export
#' @name op-null-continuation
#' @seealso [op-null-default]
#' @examples
#' 1 %&&% 2
#' NULL %&&% 2
`%&&%` <- function(x, y) {
  if (!is_null(x)) y else x
}

`%|0|%` <- function(x, y) {
  if (!length(x)) y else x
}

#' Replace missing values
#'
#' @description
#' __Note__: This operator is now out of scope for rlang. It will be
#' replaced by a vctrs-powered operator (probably in the [funs
#' package](https://github.com/tidyverse/funs)) at which point the
#' rlang version of `%|%` will be deprecated.
#'
#' This infix function is similar to \code{\%||\%} but is vectorised
#' and provides a default value for missing elements. It is faster
#' than using [base::ifelse()] and does not perform type conversions.
#'
#' @param x The original values.
#' @param y The replacement values. Must be of length 1 or the same length as `x`.
#' @keywords internal
#' @export
#' @name op-na-default
#' @seealso [op-null-default]
#' @examples
#' c("a", "b", NA, "c") %|% "default"
#' c(1L, NA, 3L, NA, NA) %|% (6L:10L)
`%|%` <- function(x, y) {
  .Call(ffi_replace_na, x, y)
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
