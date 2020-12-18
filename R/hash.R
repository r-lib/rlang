#' Hash an object
#'
#' @description
#' `hash()` hashes an arbitrary R object.
#'
#' The generated hash is guaranteed to be reproducible across platforms, but
#' not across R versions.
#'
#' @details
#' `hash()` uses the XXH128 hash algorithm of the xxHash library, which
#' generates a 128-bit hash. It is implemented as a streaming hash, which
#' generates the hash with minimal extra memory usage.
#'
#' Objects are converted to binary using R's native serialization tools.
#' On R >= 3.5.0, serialization version 3 is used, otherwise version 2 is used.
#' See [serialize()] for more information about the serialization version.
#'
#' @param x An object.
#'
#' @export
#' @examples
#' hash(c(1, 2, 3))
#' hash(mtcars)
hash <- function(x) {
  .Call(rlang_hash, x)
}
