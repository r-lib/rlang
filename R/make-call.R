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

#' Make a call with \code{lazy_dots} as arguments.
#'
#' In order to make a valid call, the environment must be the same for
#' all of the dots.
#'
#' @inheritParams eval_call
#' @return A list:
#'   \item{env}{The common environment for all elements}
#'   \item{expr}{The expression}
#' @export
#' @examples
#' make_call(quote(f), lazy_dots(x = 1, 2))
make_call <- function(fun, dots) {
  args <- lapply(dots, "[[", "expr")

  list(
    env = common_env(dots),
    expr = as.call(c(fun, args)))
}

#' Find common environment in list of lazy objects.
#'
#' @param dots A list of lazy objects
#' @keywords internal
#' @export
common_env <- function(dots) {
  if (length(dots) == 0) return(NULL)

  env <- dots[[1]]$env
  if (length(dots) == 1) return(env)

  for (i in 2:length(dots)) {
    if (!identical(env, dots[[i]]$env)) {
      stop("Enviorments not identical.", call. = FALSE)
    }
  }
  env
}
