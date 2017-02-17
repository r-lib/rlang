#' Create a raw vector.
#'
#' This is a convenient raw vector constructor. It takes integerish
#' input (see \code{\link{is_integerish}()}).
#'
#' @param x An integerish vector of bytes.
#' @export
#' @examples
#' bytes(1:10)
#' bytes(c(0x01, 0xff))
bytes <- function(x) {
  if (is_integerish(x)) {
    as.raw(x)
  } else if (!is_raw(x)) {
    abort("`x` should be integerish")
  }
}

#' Coerce to a raw vector.
#'
#' This currently only works with strings, and returns its hexadecimal
#' representation.
#'
#' @param x A string.
#' @return A raw vector of bytes.
#' @export
as_bytes <- function(x) {
  if (is_bytes(x)) {
    return(x)
  }
  stopifnot(is_string(x))
  charToRaw(x)
}
