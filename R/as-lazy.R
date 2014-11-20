#' Convert an object to a lazy expression or lazy dots.
#'
#' @param x An R object. Current methods for \code{as.lazy()} convert formulas,
#'   character vectors, calls and names. Methods for \code{as.lazy_dots()}
#'   convert lists and character vectors (by calling \code{\link{lapply}()}
#'   with \code{as.lazy()}.)
#' @param env Environment to use for objects that don't already have
#'   associated environment.
#' @export
#' @examples
#' as.lazy(~ x + 1)
#' as.lazy(quote(x + 1), globalenv())
#' as.lazy("x + 1", globalenv())
#'
#' as.lazy_dots(list(~x, y = ~z + 1))
#' as.lazy_dots(c("a", "b", "c"), globalenv())
#' as.lazy_dots(~x)
#' as.lazy_dots(quote(x), globalenv())
#' as.lazy_dots(quote(f()), globalenv())
#' as.lazy_dots(lazy(x))
as.lazy <- function(x, env = baseenv()) UseMethod("as.lazy")

#' @export
as.lazy.lazy <- function(x, env = baseenv()) x

#' @export
as.lazy.formula <- function(x, env = baseenv()) lazy_(x[[2]], environment(x))

#' @export
as.lazy.character <- function(x, env = baseenv()) lazy_(parse(text = x)[[1]], env)

#' @export
as.lazy.call <- function(x, env = baseenv()) lazy_(x, env)

#' @export
as.lazy.name <- function(x, env = baseenv()) lazy_(x, env)

#' @export
as.lazy.numeric <- function(x, env = baseenv()) {
  if (length(x) > 1) {
    warning("Truncating vector to length 1", call. = FALSE)
    x <- x[1]
  }
  lazy_(x, env)
}
#' @export
as.lazy.logical <- as.lazy.numeric

#' @export
#' @rdname as.lazy
as.lazy_dots <- function(x, env) UseMethod("as.lazy_dots")

#' @export
as.lazy_dots.NULL <- function(x, env = baseenv()) {
  structure(list(), class = "lazy_dots")
}

#' @export
as.lazy_dots.list <- function(x, env = baseenv()) {
  structure(lapply(x, as.lazy, env = env), class = "lazy_dots")
}

#' @export
as.lazy_dots.name <- function(x, env = baseenv()) {
  structure(list(as.lazy(x, env)), class = "lazy_dots")
}
#' @export
as.lazy_dots.formula <- as.lazy_dots.name
#' @export
as.lazy_dots.call <- as.lazy_dots.name

#' @export
as.lazy_dots.lazy <- function(x, env = baseenv()) {
  structure(list(x), class = "lazy_dots")
}

#' @export
as.lazy_dots.character <- function(x, env = baseenv()) {
  structure(lapply(x, as.lazy, env = env), class = "lazy_dots")
}

#' @export
as.lazy_dots.lazy_dots <- function(x, env = baseenv()) {
  x
}

#' Combine explicit and implicit dots.
#'
#' @param ... Individual lazy objects
#' @param .dots A list of lazy objects
#' @param all_named If \code{TRUE}, uses \code{\link{auto_name}} to ensure
#'   every component has a name.
#' @return A \code{\link{lazy_dots}}
#' @keywords internal
#' @export
all_dots <- function(.dots, ..., all_named = FALSE) {
  dots <- as.lazy_dots(list(...))
  if (!missing(.dots)) {
    dots2 <- as.lazy_dots(.dots)
    dots <- c(dots, dots2)
  }

  if (all_named) {
    dots <- auto_name(dots)
  }

  dots

}
