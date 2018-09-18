#' Flatten or squash a list of lists into a simpler vector
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' `flatten()` removes one level hierarchy from a list, while
#' `squash()` removes all levels. These functions are similar to
#' [unlist()] but they are type-stable so you always know what the
#' type of the output is.
#'
#'
#' @section Life cycle:
#'
#' These functions are in the questioning stage. They have slightly
#' different semantics than the flattening functions in purrr and we
#' are currently rethinking our approach to flattening with the new
#' typing facilities of the vctrs package.
#'
#' @param x A list to flatten or squash. The contents of the list can
#'   be anything for unsuffixed functions `flatten()` and `squash()`
#'   (as a list is returned), but the contents must match the type for
#'   the other functions.
#' @return `flatten()` returns a list, `flatten_lgl()` a logical
#'   vector, `flatten_int()` an integer vector, `flatten_dbl()` a
#'   double vector, and `flatten_chr()` a character vector. Similarly
#'   for `squash()` and the typed variants (`squash_lgl()` etc).
#' @export
#' @keywords internal
#' @examples
#' x <- replicate(2, sample(4), simplify = FALSE)
#' x
#'
#' flatten(x)
#' flatten_int(x)
#'
#' # With flatten(), only one level gets removed at a time:
#' deep <- list(1, list(2, list(3)))
#' flatten(deep)
#' flatten(flatten(deep))
#'
#' # But squash() removes all levels:
#' squash(deep)
#' squash_dbl(deep)
#'
#' # The typed flatten functions remove one level and coerce to an atomic
#' # vector at the same time:
#' flatten_dbl(list(1, list(2)))
#'
#' # Only bare lists are flattened, but you can splice S3 lists
#' # explicitly:
#' foo <- set_attrs(list("bar"), class = "foo")
#' str(flatten(list(1, foo, list(100))))
#' str(flatten(list(1, splice(foo), list(100))))
#'
#' # Instead of splicing manually, flatten_if() and squash_if() let
#' # you specify a predicate function:
#' is_foo <- function(x) inherits(x, "foo") || is_bare_list(x)
#' str(flatten_if(list(1, foo, list(100)), is_foo))
#'
#' # squash_if() does the same with deep lists:
#' deep_foo <- list(1, list(foo, list(foo, 100)))
#' str(deep_foo)
#'
#' str(squash(deep_foo))
#' str(squash_if(deep_foo, is_foo))
flatten <- function(x) {
  .Call(rlang_squash, x, "list", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_lgl <- function(x) {
  .Call(rlang_squash, x, "logical", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_int <- function(x) {
  .Call(rlang_squash, x, "integer", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_dbl <- function(x) {
  .Call(rlang_squash, x, "double", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_cpl <- function(x) {
  .Call(rlang_squash, x, "complex", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_chr <- function(x) {
  .Call(rlang_squash, x, "character", is_spliced_bare, 1L)
}
#' @rdname flatten
#' @export
flatten_raw <- function(x) {
  .Call(rlang_squash, x, "raw", is_spliced_bare, 1L)
}

#' @rdname flatten
#' @export
squash <- function(x) {
  .Call(rlang_squash, x, "list", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_lgl <- function(x) {
  .Call(rlang_squash, x, "logical", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_int <- function(x) {
  .Call(rlang_squash, x, "integer", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_dbl <- function(x) {
  .Call(rlang_squash, x, "double", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_cpl <- function(x) {
  .Call(rlang_squash, x, "complex", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_chr <- function(x) {
  .Call(rlang_squash, x, "character", is_spliced_bare, -1L)
}
#' @rdname flatten
#' @export
squash_raw <- function(x) {
  .Call(rlang_squash, x, "raw", is_spliced_bare, -1L)
}

#' @rdname flatten
#' @param predicate A function of one argument returning whether it
#'   should be spliced.
#' @export
flatten_if <- function(x, predicate = is_spliced) {
  .Call(rlang_squash, x, "list", predicate, 1L)
}
#' @rdname flatten
#' @export
squash_if <- function(x, predicate = is_spliced) {
  .Call(rlang_squash, x, "list", predicate, -1L)
}
