#' Coerce an object to a name or call.
#'
#' These are a S3 generics with built-in methods for names, calls, formuals,
#' and strings. The distinction between a name and a call is particularly
#' important when coercing from a string. Coercing to a call will parse the
#' string, coercing to a name will create a (potentially) non-syntactic name.
#'
#' @param x An object to coerce
#' @export
#' @examples
#' as_name("x + y")
#' as_call("x + y")
#'
#' as_call(~ f)
#' as_name(~ f())
as_name <- function(x) UseMethod("as_name")

#' @export
as_name.name <- function(x) x

#' @export
as_name.character <- function(x) {
  if (length(x) > 1) {
    stop("Can not coerce character vector of length > 1 to name", call. = FALSE)
  }

  as.name(x)
}

#' @export
as_name.call <- function(x) x[[1]]

#' @export
as_name.formula <- function(x) {
  as_name(f_rhs(x))
}

#' @export
#' @rdname as_name
as_call <- function(x) {
  UseMethod("as_call")
}

#' @export
as_call.name <- function(x) {
  as.call(list(x))
}

#' @export
as_call.call <- function(x) {
  x
}

#' @export
as_call.character <- function(x) {
  if (length(x) > 1) {
    stop("Can not coerce character vector of length > 1 to name", call. = FALSE)
  }

  parse(text = x)[[1]]
}

#' @export
as_call.formula <- function(x) {
  as_call(f_rhs(x))
}
