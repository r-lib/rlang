hash <- function(x) {
  raw <- serialize(x, connection = NULL, version = 2L)
  .Call(rlang_hash_raw, raw)
}

hash_string <- function(x) {
  .Call(rlang_hash_string, x)
}
