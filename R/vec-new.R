#' Create vectors
#'
#' @description
#'
#' `r lifecycle::badge("questioning")`
#'
#' The atomic vector constructors are equivalent to [c()] but:
#'
#' * They allow you to be more explicit about the output
#'   type. Implicit coercions (e.g. from integer to logical) follow
#'   the rules described in [vector-coercion].
#'
#' * They use [dynamic dots][dyn-dots].
#'
#'
#' @section Life cycle:
#'
#' * All the abbreviated constructors such as `lgl()` will probably be
#'   moved to the vctrs package at some point. This is why they are
#'   marked as questioning.
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
  .Call(ffi_squash, dots_values(...), "logical", is_spliced_bare, 1L)
}
#' @rdname vector-construction
#' @export
int <- function(...) {
  .Call(ffi_squash, dots_values(...), "integer", is_spliced_bare, 1L)
}
#' @rdname vector-construction
#' @export
dbl <- function(...) {
  .Call(ffi_squash, dots_values(...), "double", is_spliced_bare, 1L)
}
#' @rdname vector-construction
#' @export
cpl <- function(...) {
  .Call(ffi_squash, dots_values(...), "complex", is_spliced_bare, 1L)
}
#' @rdname vector-construction
#' @export
#' @export
chr <- function(...) {
  .Call(ffi_squash, dots_values(...), "character", is_spliced_bare, 1L)
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
      map(dot, cast_raw)
    } else {
      cast_raw(dot)
    }
  })
  .Call(ffi_squash, dots, "raw", is_spliced_bare, 1L)
}

#' Create vectors matching a given length
#'
#' @description
#'
#' `r lifecycle::badge("questioning")`
#'
#' These functions construct vectors of a given length, with attributes
#' specified via dots. Except for `new_list()` and `new_raw()`, the
#' empty vectors are filled with typed [missing] values. This is in
#' contrast to the base function [base::vector()] which creates
#' zero-filled vectors.
#'
#' @param n The vector length.
#' @param names Names for the new vector.
#'
#' @section Lifecycle:
#'
#' These functions are likely to be replaced by a vctrs equivalent in
#' the future. They are in the questioning lifecycle stage.
#'
#' @keywords internal
#' @examples
#' new_list(10)
#' new_logical(10)
#' @name new-vector
#' @seealso rep_along
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
#' These functions take the idea of [seq_along()] and apply it to
#' repeating values.
#'
#' @param x Values to repeat.
#' @param along Vector whose length determine how many times `x`
#'   is repeated.
#' @param names Names for the new vector. The length of `names`
#'   determines how many times `x` is repeated.
#'
#' @seealso new-vector
#' @export
#' @examples
#' x <- 0:5
#' rep_along(x, 1:2)
#' rep_along(x, 1)
#'
#' # Create fresh vectors by repeating missing values:
#' rep_along(x, na_int)
#' rep_along(x, na_chr)
#'
#' # rep_named() repeats a value along a names vectors
#' rep_named(c("foo", "bar"), list(letters))
rep_along <- function(along, x) {
  rep_len(x, length(along))
}
#' @export
#' @rdname rep_along
rep_named <- function(names, x) {
  names <- names %||% chr()
  check_character(names, what = "`NULL` or a character vector")

  set_names(rep_len(x, length(names)), names)
}
