#' Capture ... (dots) for later lazy evaluation.
#'
#' @param ... Dots from another function
#' @return A named list of \code{\link{lazy}} expressions.
#' @inheritParams lazy
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
#' f(z = a + b, .follow_symbols = TRUE)
#'
#' # .follow_symbols is off by default because it causes problems
#' # with lazy loaded objects
#' lazy_dots(letters)
#' lazy_dots(letters, .follow_symbols = TRUE)
#'
#' # You can also modify a dots like a list. Anything on the RHS will
#' # be coerced to a lazy.
#' l <- lazy_dots(x = 1)
#' l$y <- quote(f)
#' l[c("y", "x")]
#' l["z"] <- list(~g)
#'
#' c(lazy_dots(x = 1), lazy_dots(f))
lazy_dots <- function(..., .follow_symbols = FALSE) {
  if (nargs() == 0) return(structure(list(), class = "lazy_dots"))

  .Call(make_lazy_dots, environment(), .follow_symbols)
}

is.lazy_dots <- function(x) inherits(x, "lazy_dots")

#' @export
`[.lazy_dots` <- function(x, i) {
  structure(NextMethod(), class = "lazy_dots")
}

#' @export
`$<-.lazy_dots` <- function(x, i, value) {
  value <- as.lazy(value, parent.frame())
  x[[i]] <- value
  x
}

#' @export
`[<-.lazy_dots` <- function(x, i, value) {
  value <- lapply(value, as.lazy, env = parent.frame())
  NextMethod()
}

#' @export
c.lazy_dots <- function(..., recursive = FALSE) {
  structure(NextMethod(), class = "lazy_dots")
}
