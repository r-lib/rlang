#' Is an object a language object?
#'
#' These helpers are consistent wrappers around their base R equivalents.
#' A language object is either an atomic vector (typically a scalar), a
#' name (aka a symbol), a call, or a pairlist (used for function arguments).
#'
#' @param x An object to test.
#' @seealso \code{\link{is_call}()} for a call predicate.
#'   \code{\link{as_name}()} and \code{\link{as_call}()} for coercion
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
#'
#' # A literal is either a scalar atomic vector or a name:
#' is_literal("string")
#' is_literal(letters)
#' is_literal(quote(name))
is_lang <- function(x) {
  is_call(x) || is_pairlist(x) || is_literal(x) || is_null(x)
}
#' @rdname is_lang
#' @export
is_name <- function(x) {
  typeof(x) == "symbol"
}
#' @rdname is_lang
#' @export
is_pairlist <- function(x) {
  typeof(x) == "pairlist"
}
#' @export
#' @rdname is_lang
is_literal <- function(x) {
  is_name(x) || is_scalar_atomic(x)
}

#' Is object a call?
#'
#' This function tests if \code{x} is a call. This is a
#' pattern-matching predicate that will return \code{FALSE} if
#' \code{name} and \code{n} are supplied and the call does not match
#' these properties. \code{is_unary_call()} and
#' \code{is_binary_call()} hardcode \code{n} to 1 and 2.
#'
#' @param x An object to test. If a formula, the right-hand side is
#'   extracted.
#' @param name An optional name that the call should match. It is
#'   passed to \code{\link{as_name}()} before matching.
#' @param n An optional number of arguments that the call should
#'   match.
#' @seealso \code{\link{is_lang}()}
#' @export
#' @examples
#' is_call(quote(foo(bar)))
#'
#' # Right-hand sides are extracted from formulas:
#' is_call(~foo(bar))
#'
#' # You can pattern-match the call with additional arguments:
#' is_call(~foo(bar), "foo")
#' is_call(~foo(bar), "bar")
#' is_call(~foo(bar), quote(foo))
#'
#' # Match the number of arguments with is_call():
#' is_call(~foo(bar), "foo", 1)
#' is_call(~foo(bar), "foo", 2)
#'
#' # Or more specifically:
#' is_unary_call(~foo(bar))
#' is_unary_call(~ +3)
#' is_unary_call(~ 1 + 3)
#' is_binary_call(~ 1 + 3)
#'
#' # Namespaced calls are a bit tricky. Strings won't work because
#' # as_name("base::list") returns a symbol rather than a namespace
#' # call:
#' is_call(~base::list(baz), "base::list")
#'
#' # However you can use the fact that as_name(quote(base::list()))
#' # extracts the function identifier as is, and thus returns the call
#' # base::list:
#' is_call(~base::list(baz), ~base::list(), 1)
is_call <- function(x, name = NULL, n = NULL) {
  if (is_formula(x)) {
    x <- f_rhs(x)
  }

  if (!typeof(x) == "language") {
    return(FALSE)
  }

  if (!is_null(name) && !identical(x[[1]], as_name(name))) {
    return(FALSE)
  }

  if (!is_null(n) && !has_length(x, n + 1L)) {
    return(FALSE)
  }

  TRUE
}

#' @rdname is_call
#' @export
is_unary_call <- function(x, name = NULL) {
  is_call(x, name, n = 1L)
}
#' @rdname is_call
#' @export
is_binary_call <- function(x, name = NULL) {
  is_call(x, name, n = 2L)
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
  as.name(read_validate(x))
}

#' @export
as_name.call <- function(x) {
  if (is_prefixed_name(x)) {
    x
  } else {
    as_name(x[[1]])
  }
}
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
  read(x)
}

#' @export
as_call.formula <- function(x) {
  as_call(f_rhs(x))
}

is_prefixed_name <- function(x) {
  fn <- x[[1]]
  if (is_name(fn)) {
    as.character(fn) %in% c("::", ":::", "$", "@")
  } else {
    FALSE
  }
}
