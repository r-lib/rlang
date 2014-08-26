#' Capture ... (dots) for later lazy evaluation.
#'
#' @param ... Dots from another function
#' @return A named list of \code{\link{lazy}} expressions.
#' @export
#' @useDynLib lazy make_lazy_dots
#' @examples
#' lazy_dots(x = 1)
#' lazy_dots(a, b, c * 4)
#'
#' f <- function(x = a + b, ...) {
#'   lazy_dots(x = x, y = a + b, ...)
#' }
#' f(z = a + b)
lazy_dots <- function(...) {
  .Call(make_lazy_dots, environment())
}
