#' Make a call with \code{lazy_dots} as arguments.
#'
#' In order to exactly replay the original call, the environment must be the
#' same for all of the dots. This function circumvents that a little,
#' falling back to the \code{\link{baseenv}()} if all environments aren't
#' the same.
#'
#' @param fun Function as symbol or quoted call.
#' @param args Arguments to function; must be a \code{lazy_dots} object,
#'   or something \code{\link{as.lazy_dots}()} can coerce..
#' @return A list:
#'   \item{env}{The common environment for all elements}
#'   \item{expr}{The expression}
#' @export
#' @examples
#' make_call(quote(f), lazy_dots(x = 1, 2))
#' make_call(quote(f), list(x = 1, y = ~x))
#' make_call(quote(f), ~x)
#'
#' # If no known or no common environment, fails back to baseenv()
#' make_call(quote(f), quote(x))
make_call <- function(fun, args) {
  stopifnot(is.call(fun) || is.name(fun))
  args <- as.lazy_dots(args)
  expr <- lapply(args, `[[`, "expr")

  lazy_(
    as.call(c(fun, expr)),
    common_env(args)
  )
}

#' Find common environment in list of lazy objects.
#'
#' If no common environment is found, will return \code{baseenv()}.
#'
#' @param dots A list of lazy objects
#' @keywords internal
#' @export
#' @examples
#' common_env(lazy_dots(a, b, c))
#'
#' f <- function(x) ~x
#' common_env(list(f(1)))
#' common_env(list(f(1), f(2)))
common_env <- function(dots) {
  if (!is.list(dots)) stop("dots must be a list", call. = FALSE)
  if (length(dots) == 0) return(baseenv())

  dots <- as.lazy_dots(dots)
  env <- dots[[1]]$env
  if (length(dots) == 1) return(env)

  for (i in 2:length(dots)) {
    if (!identical(env, dots[[i]]$env)) {
      return(baseenv())
    }
  }
  env
}

# ------------------------------------------------------------------------------

#' Evaluate a call with \code{lazy_dots} as argument.
#'
#' This simulates the original call as closely as possible by creating
#' a temporary environment where each \code{lazy} object is bound to
#' a promise by \code{\link{delayedAssign}}.
#'
#' @noRd
#' @param env Environment in which to evaluate call. Defaults to
#'   \code{\link{parent.frame}()}.
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

  vars <- paste0("x", seq_along(dots))
  names(vars) <- names(dots)

  # Create environment containing promises
  env <- new.env(parent = env)
  for(i in seq_along(dots)) {
    dot <- dots[[i]]

    assign_call <- substitute(
      delayedAssign(vars[i], expr, dot$env, assign.env = env),
      list(expr = dot$expr)
    )
    eval(assign_call)
  }

  args <- lapply(vars, as.symbol)
  call <- as.call(c(fun, args))

  eval(call, env)
}

