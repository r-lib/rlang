#' Construct new vectors.
#'
#' The atomic vector constructors are equivalent to [c()] but allow
#' you to be more explicit about the output type. Implicit coercions
#' (e.g. from integer to logical) follow the rules described in
#' [vector-coercion]. In addition, all constructors support splicing:
#' if you supply [bare][is_bare_list] lists or [explicitly
#' spliced][is_spliced] lists, their contents are spliced into the
#' output vectors (see below for details). `list_splice()` is a list
#' constructor similar to [base::list()] but with splicing semantics.
#'
#' @section Splicing:
#'
#' Splicing is an operation similar to flattening one level of nested
#' lists, e.g. with \code{\link[=unlist]{base::unlist(x, recursive =
#' FALSE)}} or `purrr::flatten()`. `list_splice()` returns its
#' arguments as a list, just like `list()` would, but inner lists
#' qualifying for splicing are flattened. That is, their contents are
#' embedded in the surrounding list. Similarly, `chr()` concatenates
#' its arguments and returns them as a single character vector, but
#' inner lists are flattened before concatenation.
#'
#' Whether an inner list qualifies for splicing is determined by the
#' type of splicing semantics. All the atomic constructors like
#' `chr()` have _list splicing_ semantics: [bare][is_bare_list] lists
#' and [explicitly spliced][is_spliced] lists are spliced.
#'
#' There are two list constructors with different splicing
#' semantics. `ll()` only splices lists explicitly marked with
#' [splice()], while `list_splice()` has list splicing semantics.
#'
#' @param ... Components of the new vector. Bare lists and explicitly
#'   spliced lists are spliced.
#' @name vector-construction
#' @seealso [list_splice()]
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
  .Call(rlang_splice, dots_values(...), "logical", bare = TRUE)
}
#' @rdname vector-construction
#' @export
int <- function(...) {
  .Call(rlang_splice, dots_values(...), "integer", bare = TRUE)
}
#' @rdname vector-construction
#' @export
dbl <- function(...) {
  .Call(rlang_splice, dots_values(...), "double", bare = TRUE)
}
#' @rdname vector-construction
#' @export
cpl <- function(...) {
  .Call(rlang_splice, dots_values(...), "complex", bare = TRUE)
}
#' @rdname vector-construction
#' @export
#' @param .encoding If non-null, passed to [set_chr_encoding()] to add
#'   an encoding mark. This is only declarative, no encoding
#'   conversion is performed.
#' @export
chr <- function(..., .encoding = NULL) {
  out <- .Call(rlang_splice, dots_values(...), "character", bare = TRUE)
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
  .Call(rlang_splice, dots, "raw", bare = TRUE)
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
  .Call(rlang_splice, dots_values(...), "list", bare = FALSE)
}
#' @rdname vector-construction
#' @param x A list to splice.
#' @export
list_splice <- function(x) {
  .Call(rlang_splice, x, "list", bare = TRUE)
}
#' @rdname vector-construction
#' @param predicate A function of one argument returning whether it
#'   should be spliced.
#' @export
list_splice_if <- function(x, predicate = is_spliced) {
  .Call(rlang_splice, x, "list", predicate)
}

#' Splice a list within a vector.
#'
#' This adjective signals to functions taking dots that `x` should be
#' spliced in a surrounding vector. Examples of functions that support
#' such explicit splicing are [list_splice()], [chr()], etc.
#'
#' @param x A list to splice.
#' @seealso [list_splice()], [vector-construction]
#' @export
splice <- function(x) {
  if (!is_list(x)) {
    abort("Only lists can be spliced")
  }
  structure(x, class = "spliced")
}
#' @rdname splice
#' @export
is_spliced <- function(x) {
  inherits(x, "spliced")
}

#' Helper to create vectors with matching length.
#'
#' These functions take the idea of [seq_along()] and generalise it to
#' creating lists (`lst_along`) and repeating values (`rep_along`).
#' The dots are forwarded to [struct()] to make it easy to add
#' attributes. Except for `lst_along()` and `raw_along()`, the empty
#' vectors are filled with typed `missing` values.
#'
#' @inheritParams struct
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
#' @name vector-along
#' @seealso vector-len
NULL

#' @export
#' @rdname vector-along
lgl_along <- function(.x, ...) {
  struct(rep_len(na_lgl, length(.x)), ...)
}
#' @export
#' @rdname vector-along
int_along <- function(.x, ...) {
  struct(rep_len(na_int, length(.x)), ...)
}
#' @export
#' @rdname vector-along
dbl_along <- function(.x, ...) {
  struct(rep_len(na_dbl, length(.x)), ...)
}
#' @export
#' @rdname vector-along
chr_along <- function(.x, ...) {
  struct(rep_len(na_chr, length(.x)), ...)
}
#' @export
#' @rdname vector-along
lst_along <- function(.x, ...) {
  struct(vector("list", length(.x)), ...)
}
#' @export
#' @rdname vector-along
cpl_along <- function(.x, ...) {
  struct(rep_len(na_cpl, length(.x)), ...)
}
#' @export
#' @rdname vector-along
raw_along <- function(.x, ...) {
  struct(vector("raw", length(.x)), ...)
}

#' @export
#' @rdname vector-along
rep_along <- function(.x, .y, ...) {
  struct(rep(.y, length.out = length(.x)), ...)
}


#' Create new vectors.
#'
#' These functions construct vectors of given length, with attributes
#' specified via dots. Except for `list_len()` and `bytes_len()`, the
#' empty vectors are filled with typed [missing] values. This is in
#' contrast to the base function [base::vector()] which creates
#' zero-filled vectors.
#'
#' @inheritParams struct
#' @param .n The vector length.
#' @examples
#' list_len(10)
#'
#' # Add attributes, including the S3 class:
#' int_len(0, index = 1)
#' dbl_len(10, class = "my_class")
#' @name vector-len
#' @seealso vector-along
NULL

#' @export
#' @rdname vector-len
lgl_len <- function(.n = 0, ...) {
  struct(rep_len(na_lgl, .n), ...)
}
#' @export
#' @rdname vector-len
int_len <- function(.n = 0, ...) {
  struct(rep_len(na_int, .n), ...)
}
#' @export
#' @rdname vector-len
dbl_len <- function(.n = 0, ...) {
  struct(rep_len(na_dbl, .n), ...)
}
#' @export
#' @rdname vector-len
chr_len <- function(.n = 0, ...) {
  struct(rep_len(na_chr, .n), ...)
}
#' @export
#' @rdname vector-len
cpl_len <- function(.n = 0, ...) {
  struct(rep_len(na_cpl, .n), ...)
}
#' @export
#' @rdname vector-len
bytes_len <- function(.n = 0, ...) {
  struct(vector("raw", .n), ...)
}
#' @export
#' @rdname vector-len
list_len <- function(.n = 0, ...) {
  struct(vector("list", .n), ...)
}
