
new_bytes <- function(x) {
  if (is_integerish(x)) {
    as.raw(x)
  } else if (is_raw(x)) {
    x
  } else {
    abort("input should be integerish")
  }
}

#' Coerce to a raw vector
#'
#' This currently only works with strings, and returns its hexadecimal
#' representation.
#'
#' @param x A string.
#' @return A raw vector of bytes.
#' @export
as_bytes <- function(x) {
  switch(typeof(x),
    raw = return(x),
    character = if (is_string(x)) return(charToRaw(x))
  )
  abort("`x` must be a string or raw vector")
}
