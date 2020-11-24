hash <- function(x) {
  raw <- serialize(x, connection = NULL, version = 2L, ascii = FALSE)
  .Call(rlang_hash_raw, raw)
}

hash_string <- function(x) {
  .Call(rlang_hash_string, x)
}

xxhash <- function(x) {
  .Call(rlang_xxhash, x)
}
