#' Is an object a language object?
#'
#' These helpers are consistent wrappers around their base R equivalents.
#' A language object is either an atomic vector (typically a scalar), a
#' name (aka a symbol), a call, or a pairlist (used for function arguments).
#'
#' @param x An object to test.
#' @seealso \code{\link{as_name}()} and \code{\link{as_call}()} for coercion
#'   functions.
#' @export
#' @examples
#' q1 <- quote(1)
#' is_lang(q1)
#' is_atomic(q1)
#'
#' q2 <- quote(x)
#' is_lang(q2)
#' is_name(q2)
#'
#' q3 <- quote(x + 1)
#' is_lang(q3)
#' is_call(q3)
is_lang <- function(x) {
  is_call(x) || is_pairlist(x) || is_atomic(x) || is_name(x) || is.null(x)
}

#' @rdname is_lang
#' @export
is_name <- function(x) {
  typeof(x) == "symbol"
}

#' @rdname is_lang
#' @export
is_call <- function(x) {
  typeof(x) == "language"
}

#' @rdname is_lang
#' @export
is_pairlist <- function(x) {
  typeof(x) == "pairlist"
}

#' @rdname is_lang
#' @export
is_atomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double", "complex", "character", "raw")
}


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
  call_new(x)
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
