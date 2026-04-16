#' Hashing
#'
#' @description
#' - `hash()` hashes an arbitrary R object.
#'
#' - `hash_file()` hashes the data contained in a file.
#'
#' For ordinary data objects (vectors, lists, data frames, etc.), the generated
#' hash is reproducible across sessions and R versions on platforms that have
#' the same endianness. However, hash values may change between rlang versions,
#' although that should be rare. Reference-like objects (environments, external
#' pointers, builtins) are hashed by identity, so their hashes are only stable
#' within a session. Closures hash their formals, body, and environment
#' identity.
#'
#' By default, source references are stripped before hashing so that
#' closures and calls that are textually identical produce the same
#' hash regardless of where they were parsed. Set `zap_srcref` to
#' `FALSE` to include source references in the hash.
#'
#' @details
#' These hashers use the XXH128 hash algorithm of the xxHash library, which
#' generates a 128-bit hash. Both are implemented as streaming hashes, which
#' generate the hash with minimal extra memory usage.
#'
#' For `hash()`, a custom object walker feeds the object's type, length, data
#' bytes, and attributes directly into the hash algorithm. This avoids
#' dependency on R's serialization format, making the hash immune to internal
#' representation details (e.g. ALTREP compact forms, the growable vector bit).
#'
#' @param x An object.
#'
#' @param zap_srcref Whether to ignore source references when hashing
#'   (default `TRUE`). Source references depend on parse location, so
#'   including them makes hashes of closures and calls
#'   non-reproducible across sessions.
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
hash <- function(x, zap_srcref = TRUE) {
  .Call(ffi_hash, x, zap_srcref)
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
