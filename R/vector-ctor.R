#' Construct new atomic vectors.
#'
#' These vector constructors are equivalent to [c()] but with explicit
#' output types. Implicit coercions (e.g. from integer to logical)
#' follow the rules described in [vector-coercion]. If you supply
#' [bare lists][is_bare_list] or explicitly spliced lists, their
#' contents are spliced into the output vectors.
#'
#' @param ... Components of the new vector. Bare lists and explicitly
#'   spliced lists are spliced.
#' @name vector-construction
#' @seealso [splice()]
#' @examples
#' # These constructors are like a typed version of c():
#' c(TRUE, FALSE)
#' lgl(TRUE, FALSE)
#'
#' # They follow a restricted set of coercion rules:
#' int(TRUE, FALSE, 20)
#'
#' # Lists can be spliced:
#' dbl(10, list(1, 2L), TRUE)
#'
#'
#' # They splice names a bit differently than c(). The latter
#' # automatically composes inner and outer names:
#' c(a = c(A = 10), b = c(B = 20, C = 30))
#'
#' # On the other hand, rlang's ctors use the inner names and issue a
#' # warning to inform the user that the outer names are ignored:
#' dbl(a = c(A = 10), b = c(B = 20, C = 30))
#' dbl(a = c(1, 2))
#'
#' # As an exception, it is allowed to provide an outer name when the
#' # inner vector is an unnamed scalar atomic:
#' dbl(a = 1)
#'
#' # Spliced lists behave the same way:
#' dbl(list(a = 1))
#' dbl(list(a = c(A = 1)))
NULL

#' @useDynLib rlang rlang_splice
#' @rdname vector-construction
#' @export
lgl <- function(...) {
  .Call(rlang_splice, list(...), "logical", bare = TRUE)
}
#' @rdname vector-construction
#' @export
int <- function(...) {
  .Call(rlang_splice, list(...), "integer", bare = TRUE)
}
#' @rdname vector-construction
#' @export
dbl <- function(...) {
  .Call(rlang_splice, list(...), "double", bare = TRUE)
}
#' @rdname vector-construction
#' @export
cpl <- function(...) {
  .Call(rlang_splice, list(...), "complex", bare = TRUE)
}
#' @rdname vector-construction
#' @export
#' @param .encoding If non-null, passed to [chr_set_encoding()] to add
#'   an encoding mark. This is only declarative, no encoding
#'   conversion is performed.
#' @export
chr <- function(..., .encoding = NULL) {
  out <- .Call(rlang_splice, list(...), "character", bare = TRUE)
  chr_set_encoding(out, .encoding)
}
#' @rdname vector-construction
#' @export
#' @examples
#'
#' # bytes() accepts integerish inputs
#' bytes(1:10)
#' bytes(0x01, 0xff, c(0x03, 0x05), list(10, 20, 30L))
bytes <- function(...) {
  dots <- map(list(...), function(dot) {
    if (is_bare_list(dot) || is_spliced(dot)) {
      map(dot, new_bytes)
    } else {
      new_bytes(dot)
    }
  })
  .Call(rlang_splice, dots, "raw", bare = TRUE)
}

#' Construct a list with splicing.
#' @param ... Components of the new vector. Explicitly spliced lists
#'   are always spliced. Bare lists are spliced depending on the value
#'   of `.bare`.
#' @param .bare Whether to splice bare lists. If `FALSE`, only lists
#'   inheriting from `"spliced"` are spliced.
#' @export
#' @examples
#' # splice() is like the atomic vector constructors but for lists:
#' dbl(1, list(1, 2))
#' splice(1, list(1, 2))
#'
#' # Only bare lists are spliced. Objects like data frames are not spliced:
#' splice(1, mtcars)
#'
#' # Use the spliced() adjective to splice objects:
#' splice(1, spliced(mtcars))
#'
#' # You can chose not to splice bare lists with `.bare`:
#' splice(list(1, 2), .bare = FALSE)
#'
#' # Note that explicitly spliced lists are always spliced:
#' splice(spliced(list(1, 2)), .bare = FALSE)
splice <- function(..., .bare = TRUE) {
  .Call(rlang_splice, list(...), "list", bare = .bare)
}

spliced <- function(x) {
  if (!is_list(x)) {
    abort("Only lists can be spliced")
  }
  structure(x, class = "spliced")
}
is_spliced <- function(x) {
  inherits(x, "spliced")
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
