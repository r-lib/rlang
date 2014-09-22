#' Generate a call.
#'
#' @param fun Function as symbol or quoted call.
#' @param dots Arguments to function; must be a \code{lazy_dots} object.
#' @useDynLib lazyeval make_call_
#' @export
#' @examples
#' make_env <- function(...) list2env(list(...), parent = emptyenv())
#'
#' f1 <- as.lazy(quote(a()), make_env(a = function() {message("!"); 1}))
#' f2 <- as.lazy(quote(a), make_env(a = 10))
#' args <- as.lazy_dots(list(f1, f2))
#' call <- make_call(quote(`+`), args)
#'
#' a <- 100
#' # eval(call)
make_call <- function(fun, dots) {
  .Call(make_call_, fun, dots)
}
