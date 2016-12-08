

has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !(is.na(nms) | nms == "")
  }
}

#' Set names of a vector.
#'
#' This is a snake case wrapper for \code{\link[stats]{setNames}},
#' with tweaked defaults, and stricter argument checking.
#'
#' @param x Vector to name.
#' @param nm Vector of names, the same length as \code{x}.
#' @export
#' @examples
#' set_names(1:4, c("a", "b", "c", "d"))
#'
#' # If the second argument is ommitted a vector is named with itself
#' set_names(letters[1:5])
set_names <- function(x, nm = x) {
  if (!is_vector(x)) {
    stop("`x` must be a vector", call. = FALSE)
  }
  if (!is_null(nm) && length(x) != length(nm)) {
    stop("`x` and `nm` must be the same length", call. = FALSE)
  }

  names(x) <- nm
  x
}

#' Get names of a vector.
#'
#' This names getter always returns a character vector, even when an
#' object does not have a \code{names} attribute. In this case, it
#' returns a vector of empty names \code{""}.
#'
#' @param x A vector.
#' @export
#' @examples
#' names2(letters)
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}
