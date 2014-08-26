#' Capture ... (dots) for later lazy evaluation.
#'
#' @param ... Dots from another function
#' @return A named list of \code{\link{lazy}} expressions.
#' @export
#' @useDynLib lazy make_lazy_dots
#' @examples
#' f <- function(...) {
#'   lazy_dots(...)
#' }
#' f(x = 1)
#' f(a, b, c)
lazy_dots <- function(...) {
  .Call(make_lazy_dots, environment())
}
