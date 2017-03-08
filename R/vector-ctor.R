#' Construct new vectors.
#'
#' @param ... Components of the new vector. Bare lists and explicitly
#'   spliced lists are spliced.
#' @name vector-ctors
NULL

#' @useDynLib rlang rlang_splice
#' @rdname vector-ctors
#' @export
lgl <- function(...) {
  .Call(rlang_splice, list(...), "logical", bare = TRUE)
}
#' @rdname vector-ctors
#' @export
int <- function(...) {
  .Call(rlang_splice, list(...), "integer", bare = TRUE)
}
#' @rdname vector-ctors
#' @export
dbl <- function(...) {
  .Call(rlang_splice, list(...), "double", bare = TRUE)
}
#' @rdname vector-ctors
#' @export
cpl <- function(...) {
  .Call(rlang_splice, list(...), "complex", bare = TRUE)
}
#' @rdname vector-ctors
#' @export
#' @param .encoding If non-null, passed to [chr_set_encoding()] to add
#'   an encoding mark. This is only declarative, no encoding
#'   conversion is performed.
#' @export
chr <- function(..., .encoding = NULL) {
  out <- .Call(rlang_splice, list(...), "character", bare = TRUE)
  chr_set_encoding(out, .encoding)
}
#' @rdname vector-ctors
#' @export
mk_raw <- function(...) {
  .Call(rlang_splice, list(...), "raw", bare = TRUE)
}
#' @rdname vector-ctors
#' @export
mk_list <- function(...) {
  .Call(rlang_splice, list(...), "list", bare = TRUE)
}


#' Helper to create vectors with matching length.
#'
#' These functions take the idea of [seq_along()] and generalise it to
#' creating lists (`lst_along`) and repeating values (`rep_along`).
#' The dots and `.attrs` are forwarded to [with_attributes()] to make
#' it easy to add attributes. Except for `lst_along()` and
#' `raw_along()`, the empty vectors are filled with typed `missing`
#' values.
#'
#' @inheritParams with_attributes
#' @param .x A vector.
#' @param .y Values to repeat.
#' @examples
#' x <- 0:5
#' rep_along(x, 1:2)
#' rep_along(x, 1)
#' lst_along(x)
#'
#' # You can also add attributes with additional arguments:
#' rep_along(x, 1, class = "my_class")
#' dbl_along(x, class = "my_class")
#' @name along
#' @seealso new-vectors
NULL

#' @export
#' @rdname along
lgl_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_lgl, length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
int_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_int, length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
dbl_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_dbl, length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
chr_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_chr, length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
lst_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("list", length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
cpl_along <- function(.x, ..., .attrs = list()) {
  with_attributes(rep_len(na_cpl, length(.x)), ..., .attrs = .attrs)
}
#' @export
#' @rdname along
raw_along <- function(.x, ..., .attrs = list()) {
  with_attributes(vector("raw", length(.x)), ..., .attrs = .attrs)
}

#' @export
#' @rdname along
rep_along <- function(.x, .y, ..., .attrs = list()) {
  with_attributes(rep(.y, length.out = length(.x)), ..., .attrs = .attrs)
}


#' Create new vectors.
#'
#' These functions construct vectors of given length, with attributes
#' specified via dots. Except for `list_len()` and `bytes_len()`, the
#' empty vectors are filled with typed [missing] values. This is in
#' contrast to the base function [base::vector()] which creates
#' zero-filled vectors.
#'
#' @inheritParams with_attributes
#' @param .n The vector length.
#' @examples
#' list_len(10)
#'
#' # Add attributes, including the S3 class:
#' int_len(0, index = 1)
#' dbl_len(10, class = "my_class")
#' @name new-vectors
#' @seealso along
NULL

#' @export
#' @rdname new-vectors
lgl_len <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_lgl, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
int_len <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_int, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
dbl_len <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_dbl, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
chr_len <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_chr, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
cpl_len <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(rep_len(na_cpl, .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
bytes_len <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("raw", .n), ..., .attrs = .attrs)
}
#' @export
#' @rdname new-vectors
list_len <- function(.n = 0, ..., .attrs = list()) {
  with_attributes(vector("list", .n), ..., .attrs = .attrs)
}
