#' Evaluate a lazy expression.
#'
#' @param x A lazy object or a formula.
#' @param data Option, a data frame or list in which to preferentially look
#'   for variables before using the environment associated with the lazy
#'   object.
#' @export
#' @examples
#' f <- function(x) {
#'   z <- 100
#'   ~ x + z
#' }
#' z <- 10
#' lazy_eval(f(10))
#' lazy_eval(f(10), list(x = 100))
#' lazy_eval(f(10), list(x = 1, z = 1))
#'
#' lazy_eval(lazy_dots(a = x, b = z), list(x = 10))
lazy_eval <- function(x, data = NULL) {
  if (is.lazy_dots(x)) {
    return(lapply(x, lazy_eval, data = data))
  }

  x <- as.lazy(x)

  if (!is.null(data)) {
    eval(x$expr, data, x$env)
  } else {
    eval(x$expr, x$env, emptyenv())
  }
}
