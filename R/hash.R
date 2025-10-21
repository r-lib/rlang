#' Hashing
#'
#' @description
#' - `hash()` hashes an arbitrary R object.
#'
#' - `hash_file()` hashes the data contained in a file.
#'
#' The generated hash is guaranteed to be reproducible across platforms that
#' have the same endianness and are using the same R version.
#'
#' @details
#' These hashers use the XXH128 hash algorithm of the xxHash library, which
#' generates a 128-bit hash. Both are implemented as streaming hashes, which
#' generate the hash with minimal extra memory usage.
#'
#' For `hash()`, objects are converted to binary using R's native serialization
#' tools. Serialization version 3 is used. See [serialize()] for more
#' information about the serialization version.
#'
#' @param x An object.
#'
#' @param path A character vector of paths to the files to be hashed.
#'
#' @return
#' - For `hash()`, a single character string containing the hash.
#'
#' - For `hash_file()`, a character vector containing one hash per file.
#'
#' @export
#' @examples
#' hash(c(1, 2, 3))
#' hash(mtcars)
#'
#' authors <- file.path(R.home("doc"), "AUTHORS")
#' copying <- file.path(R.home("doc"), "COPYING")
#' hashes <- hash_file(c(authors, copying))
#' hashes
#'
#' # If you need a single hash for multiple files,
#' # hash the result of `hash_file()`
#' hash(hashes)
hash <- function(x) {
  .Call(ffi_hash, x)
}

# Keep this alias for a while
# https://github.com/r-lib/rlang/issues/1177
on_load(
  rlang_hash <- ffi_hash
)

#' @rdname hash
#' @export
hash_file <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  .Call(ffi_hash_file, path)
}

# ------------------------------------------------------------------------------

hasher_init <- function() {
  .Call(ffi_hasher_init)
}

hasher_update <- function(x, data) {
  .Call(ffi_hasher_update, x, data)
}

hasher_value <- function(x) {
  .Call(ffi_hasher_value, x)
}
