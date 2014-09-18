#' Capture ... (dots) for later lazy evaluation.
#'
#' @param ... Dots from another function
#' @return A named list of \code{\link{lazy}} expressions.
#' @export
#' @useDynLib lazyeval make_lazy_dots
#' @examples
#' lazy_dots(x = 1)
#' lazy_dots(a, b, c * 4)
#'
#' f <- function(x = a + b, ...) {
#'   lazy_dots(x = x, y = a + b, ...)
#' }
#' f(z = a + b)
lazy_dots <- function(...) {
  if (nargs() == 0) return(structure(list(), class = "lazy_dots"))

  .Call(make_lazy_dots, environment())
}

is.lazy_dots <- function(x) inherits(x, "lazy_dots")
