#' Is an object a language object?
#'
#' These helpers are consistent wrappers around their base R equivalents.
#' A language object is either an atomic vector (typically a scalar), a
#' name (aka a symbol), or a call.
#'
#' @param x An object to test.
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
  is_name(x) || is_call(x) || is_atomic(x)
}

#' @rdname is_lang
#' @export
is_name <- function(x) {
  typeof(x) == "symbol"
}

#' @rdname is_lang
#' @export
is_call <- function(x) {
  typeof(x) == "call"
}

#' @rdname is_lang
#' @export
is_atomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double", "complex", "character", "raw")
}

