#' Create vectors
#'
#' @description
#'
#' The atomic vector constructors are equivalent to [c()] but:
#'
#' * They allow you to be more explicit about the output
#'   type. Implicit coercions (e.g. from integer to logical) follow
#'   the rules described in [vector-coercion].
#'
#' * They use [tidy dots][tidy-dots] and thus support splicing with `!!!`.
#'
#'
#' @section Life cycle:
#'
#' * Automatic splicing is soft-deprecated and will trigger a warning
#'   in a future version. Please splice explicitly with `!!!`.
#'
#' @param ... Components of the new vector. Bare lists and explicitly
#'   spliced lists are spliced.
#' @name vector-construction
#' @examples
#' # These constructors are like a typed version of c():
#' c(TRUE, FALSE)
#' lgl(TRUE, FALSE)
#'
#' # They follow a restricted set of coercion rules:
#' int(TRUE, FALSE, 20)
#'
#' # Lists can be spliced:
#' dbl(10, !!! list(1, 2L), TRUE)
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
#' dbl(!!! list(a = 1))
#' dbl(!!! list(a = c(A = 1)))
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

#' @rdname tidy-dots
#' @export
list2 <- function(...) {
  .Call(rlang_dots_list, environment(), FALSE, "trailing", TRUE)
}
#' @rdname vector-construction
#' @export
ll <- function(...) {
  .Call(rlang_dots_list, environment(), FALSE, "trailing", TRUE)
}


#' Create vectors matching a given length
#'
#' These functions construct vectors of given length, with attributes
#' specified via dots. Except for `new_list()` and `new_bytes()`, the
#' empty vectors are filled with typed [missing] values. This is in
#' contrast to the base function [base::vector()] which creates
#' zero-filled vectors.
#'
#' @param n The vector length.
#' @param names Names for the new vector.
#' @examples
#' new_list(10)
#' new_logical(10)
#' @name new-vector
#' @seealso new-vector-along
NULL

#' @rdname new-vector
#' @export
new_logical <- function(n, names = NULL) {
  set_names(rep_len(na_lgl, n), names)
}
#' @rdname new-vector
#' @export
new_integer <- function(n, names = NULL) {
  set_names(rep_len(na_int, n), names)
}
#' @rdname new-vector
#' @export
new_double <- function(n, names = NULL) {
  set_names(rep_len(na_dbl, n), names)
}
#' @rdname new-vector
#' @export
new_character <- function(n, names = NULL) {
  set_names(rep_len(na_chr, n), names)
}
#' @rdname new-vector
#' @export
new_complex <- function(n, names = NULL) {
  set_names(rep_len(na_cpl, n), names)
}
#' @rdname new-vector
#' @export
new_raw <- function(n, names = NULL) {
  set_names(vector("raw", n), names)
}
#' @rdname new-vector
#' @export
new_list <- function(n, names = NULL) {
  set_names(vector("list", n), names)
}

#' Create vectors matching the length of a given vector
#'
#' These functions take the idea of [seq_along()] and generalise it to
#' creating lists (`new_list_along`) and repeating values (`rep_along`).
#' Except for `new_list_along()` and `new_raw_along()`, the empty
#' vectors are filled with typed `missing` values.
#'
#' @param x,.x A vector.
#' @param .y Values to repeat.
#' @param names Names for the new vector. Defaults to the names of
#'   `x`. This can be a function to apply to the names of `x` as in
#'   [set_names()].
#' @examples
#' x <- 0:5
#' rep_along(x, 1:2)
#' rep_along(x, 1)
#' new_list_along(x)
#'
#' # The default names are picked up from the input vector
#' x <- c(a = "foo", b = "bar")
#' new_character_along(x)
#' @name new-vector-along
#' @seealso new-vector
NULL

#' @export
#' @rdname new-vector-along
new_logical_along <- function(x, names = base::names(x)) {
  set_names_impl(rep_len(na_lgl, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along
new_integer_along <- function(x, names = base::names(x)) {
  set_names_impl(rep_len(na_int, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along
new_double_along <- function(x, names = base::names(x)) {
  set_names_impl(rep_len(na_dbl, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along
new_character_along <- function(x, names = base::names(x)) {
  set_names_impl(rep_len(na_chr, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along
new_complex_along <- function(x, names = base::names(x)) {
  set_names_impl(rep_len(na_cpl, length(x)), x, names)
}
#' @export
#' @rdname new-vector-along
new_raw_along <- function(x, names = base::names(x)) {
  set_names_impl(vector("raw", length(x)), x, names)
}
#' @export
#' @rdname new-vector-along
new_list_along <- function(x, names = base::names(x)) {
  set_names_impl(vector("list", length(x)), x, names)
}

#' @export
#' @rdname new-vector-along
rep_along <- function(.x, .y) {
  rep(.y, length.out = length(.x))
}
