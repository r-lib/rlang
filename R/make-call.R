#' Evaluate a call with \code{lazy_dots} as argument.
#'
#' @param fun Function as symbol or quoted call.
#' @param dots Arguments to function; must be a \code{lazy_dots} object.
#' @param env Environment in which to evaluate call. Defaults to
#'   \code{\link{parent.frame}()}.
#' @useDynLib lazyeval eval_call_
#' @export
#' @examples
#' make_env <- function(...) list2env(list(...), parent = emptyenv())
#'
#' f1 <- as.lazy(quote(a()), make_env(a = function() {message("!"); 1}))
#' f2 <- as.lazy(quote(a), make_env(a = 10))
#' args <- as.lazy_dots(list(f1, f2))
#'
#' a <- 100
#' eval_call(quote(`+`), args)
eval_call <- function(fun, dots, env = parent.frame()) {
  .Call(eval_call_, fun, dots, env)
}
