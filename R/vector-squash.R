
flatten <- function(x) {
  .Call(rlang_splice, x, "list", bare = TRUE, 1L)
}
flatten_lgl <- function(x) {
  .Call(rlang_splice, x, "logical", bare = TRUE, 1L)
}
flatten_int <- function(x) {
  .Call(rlang_splice, x, "integer", bare = TRUE, 1L)
}
flatten_dbl <- function(x) {
  .Call(rlang_splice, x, "double", bare = TRUE, 1L)
}
flatten_cpl <- function(x) {
  .Call(rlang_splice, x, "complex", bare = TRUE, 1L)
}
flatten_raw <- function(x) {
  .Call(rlang_splice, x, "raw", bare = TRUE, 1L)
}

squash <- function(x) {
  .Call(rlang_splice, x, "list", bare = TRUE, -1L)
}
squash_lgl <- function(x) {
  .Call(rlang_splice, x, "logical", bare = TRUE, -1L)
}
squash_int <- function(x) {
  .Call(rlang_splice, x, "integer", bare = TRUE, -1L)
}
squash_dbl <- function(x) {
  .Call(rlang_splice, x, "double", bare = TRUE, -1L)
}
squash_cpl <- function(x) {
  .Call(rlang_splice, x, "complex", bare = TRUE, -1L)
}
squash_raw <- function(x) {
  .Call(rlang_splice, x, "raw", bare = TRUE, -1L)
}
