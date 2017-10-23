#' Create vectors
#'
#' The atomic vector constructors are equivalent to [c()] but allow
#' you to be more explicit about the output type. Implicit coercions
#' (e.g. from integer to logical) follow the rules described in
#' [vector-coercion]. In addition, all constructors support splicing:
#' if you supply [bare][is_bare_list] lists or [explicitly
#' spliced][is_spliced] lists, their contents are spliced into the
#' output vectors (see below for details). `ll()` is a list
#' constructor similar to [base::list()] but with splicing semantics.
#'
#' @section Splicing:
#'
#' Splicing is an operation similar to flattening one level of nested
#' lists, e.g. with \code{\link[=unlist]{base::unlist(x, recursive =
#' FALSE)}} or `purrr::flatten()`. `ll()` returns its arguments as a
#' list, just like `list()` would, but inner lists qualifying for
#' splicing are flattened. That is, their contents are embedded in the
#' surrounding list. Similarly, `chr()` concatenates its arguments and
#' returns them as a single character vector, but inner lists are
#' flattened before concatenation.
#'
#' Whether an inner list qualifies for splicing is determined by the
#' type of splicing semantics. All the atomic constructors like
#' `chr()` have _list splicing_ semantics: [bare][is_bare_list] lists
#' and [explicitly spliced][is_spliced] lists are spliced.
#'
#' There are two list constructors with different splicing
#' semantics. `ll()` only splices lists explicitly marked with
#' [splice()].
#'
#' @param ... Components of the new vector. Bare lists and explicitly
#'   spliced lists are spliced.
#' @name vector-construction
#' @seealso [ll()]
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

#' @rdname vector-construction
#' @export
lgl <- function(...) {
  .Call(rlang_squash, dots_values(...), "logical", is_spliced_bare, 1L)
}
#' @rdname vector-construction
#' @export
int <- function(...) {
  .Call(rlang_squash, dots_values(...), "integer", is_spliced_bare, 1L)
}
#' @rdname vector-construction
#' @export
dbl <- function(...) {
  .Call(rlang_squash, dots_values(...), "double", is_spliced_bare, 1L)
}
#' @rdname vector-construction
#' @export
cpl <- function(...) {
  .Call(rlang_squash, dots_values(...), "complex", is_spliced_bare, 1L)
}
#' @rdname vector-construction
#' @export
#' @param .encoding If non-null, passed to [set_chr_encoding()] to add
#'   an encoding mark. This is only declarative, no encoding
#'   conversion is performed.
#' @export
chr <- function(..., .encoding = NULL) {
  out <- .Call(rlang_squash, dots_values(...), "character", is_spliced_bare, 1L)
  set_chr_encoding(out, .encoding)
}
#' @rdname vector-construction
#' @export
#' @examples
#'
#' # bytes() accepts integerish inputs
#' bytes(1:10)
#' bytes(0x01, 0xff, c(0x03, 0x05), list(10, 20, 30L))
bytes <- function(...) {
  dots <- map(dots_values(...), function(dot) {
    if (is_bare_list(dot) || is_spliced(dot)) {
      map(dot, new_bytes)
    } else {
      new_bytes(dot)
    }
  })
  .Call(rlang_squash, dots, "raw", is_spliced_bare, 1L)
}

#' @rdname vector-construction
#' @export
#' @examples
#'
#' # The list constructor has explicit splicing semantics:
#' ll(1, list(2))
#'
#' # Note that explicitly spliced lists are always spliced:
#' ll(!!! list(1, 2))
ll <- function(...) {
  .Call(rlang_squash, dots_values(...), "list", is_spliced, 1L)
}


#' Create vectors matching the length of a given vector
#'
#' These functions take the idea of [seq_along()] and generalise it to
#' creating lists (`list_along`) and repeating values (`rep_along`).
#' Except for `list_along()` and `raw_along()`, the empty vectors are
#' filled with typed `missing` values.
#'
#' @inheritParams set_attrs
#' @param .x A vector.
#' @param .y Values to repeat.
#' @examples
#' x <- 0:5
#' rep_along(x, 1:2)
#' rep_along(x, 1)
#' list_along(x)
#' @name vector-along
#' @seealso vector-len
NULL

#' @export
#' @rdname vector-along
lgl_along <- function(.x) {
  rep_len(na_lgl, length(.x))
}
#' @export
#' @rdname vector-along
int_along <- function(.x) {
  rep_len(na_int, length(.x))
}
#' @export
#' @rdname vector-along
dbl_along <- function(.x) {
  rep_len(na_dbl, length(.x))
}
#' @export
#' @rdname vector-along
chr_along <- function(.x) {
  rep_len(na_chr, length(.x))
}
#' @export
#' @rdname vector-along
cpl_along <- function(.x) {
  rep_len(na_cpl, length(.x))
}
#' @export
#' @rdname vector-along
raw_along <- function(.x) {
  vector("raw", length(.x))
}
#' @export
#' @rdname vector-along
bytes_along <- function(.x) {
  vector("raw", length(.x))
}
#' @export
#' @rdname vector-along
list_along <- function(.x) {
  vector("list", length(.x))
}

#' @export
#' @rdname vector-along
rep_along <- function(.x, .y) {
  rep(.y, length.out = length(.x))
}


#' Create vectors matching a given length
#'
#' These functions construct vectors of given length, with attributes
#' specified via dots. Except for `list_len()` and `bytes_len()`, the
#' empty vectors are filled with typed [missing] values. This is in
#' contrast to the base function [base::vector()] which creates
#' zero-filled vectors.
#'
#' @inheritParams set_attrs
#' @param .n The vector length.
#' @examples
#' list_len(10)
#' lgl_len(10)
#' @name vector-len
#' @seealso vector-along
NULL

#' @export
#' @rdname vector-len
lgl_len <- function(.n) {
  rep_len(na_lgl, .n)
}
#' @export
#' @rdname vector-len
int_len <- function(.n) {
  rep_len(na_int, .n)
}
#' @export
#' @rdname vector-len
dbl_len <- function(.n) {
  rep_len(na_dbl, .n)
}
#' @export
#' @rdname vector-len
chr_len <- function(.n) {
  rep_len(na_chr, .n)
}
#' @export
#' @rdname vector-len
cpl_len <- function(.n) {
  rep_len(na_cpl, .n)
}
#' @export
#' @rdname vector-len
raw_len <- function(.n) {
  vector("raw", .n)
}
#' @export
#' @rdname vector-len
bytes_len <- function(.n) {
  vector("raw", .n)
}
#' @export
#' @rdname vector-len
list_len <- function(.n) {
  vector("list", .n)
}
