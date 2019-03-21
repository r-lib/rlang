#' Duplicate an R object
#'
#' In R semantics, objects are copied by value. This means that
#' modifying the copy leaves the original object intact. Since
#' copying data in memory is an expensive operation, copies in R are
#' as lazy as possible. They only happen when the new object is
#' actually modified. However, some operations (like [node_poke_car()]
#' or [node_poke_cdr()]) do not support copy-on-write. In those cases,
#' it is necessary to duplicate the object manually in order to
#' preserve copy-by-value semantics.
#'
#' Some objects are not duplicable, like symbols and environments.
#' `duplicate()` returns its input for these unique objects.
#'
#' @param x Any R object. However, uncopyable types like symbols and
#'   environments are returned as is (just like with `<-`).
#' @param shallow This is relevant for recursive data structures like
#'   lists, calls and pairlists. A shallow copy only duplicates the
#'   top-level data structure. The objects contained in the list are
#'   still the same.
#' @seealso pairlist
#' @keywords internal
#' @export
duplicate <- function(x, shallow = FALSE) {
  .Call(rlang_duplicate, x, shallow)
}


# nocov start - These functions are mostly for interactive experimentation

poke_type <- function(x, type) {
  invisible(.Call(rlang_poke_type, x, type))
}
sexp_address <- function(x) {
  .Call(rlang_sexp_address, x)
}
sexp_named <- function(x) {
  # Don't use `substitute()` because dots might be forwarded
  arg <- match.call(expand.dots = FALSE)$x
  .Call(rlang_named, arg, parent.frame())
}

mark_object <- function(x) {
  invisible(.Call(rlang_mark_object, x))
}
unmark_object <- function(x) {
  invisible(.Call(rlang_unmark_object, x))
}

true_length <- function(x) {
  .Call(rlang_true_length, x)
}
env_frame <- function(x) {
  .Call(rlang_env_frame, x)
}
env_hash_table <- function(x) {
  .Call(rlang_env_hash_table, x)
}

promise_expr <- function(name) {
  .Call(rlang_promise_expr, name, caller_env())
}
promise_env <- function(name) {
  .Call(rlang_promise_env, name, caller_env())
}
promise_value <- function(name) {
  .Call(rlang_promise_value, name, caller_env())
}

warningcall <- function(call, msg) {
  .Call(rlang_test_Rf_warningcall, call, msg)
}
errorcall <- function(call, msg) {
  .Call(rlang_test_Rf_errorcall, call, msg)
}

sexp_attrib <- function(x) {
  .Call(rlang_attrib, x)
}

vec_alloc <- function(type, n) {
  stopifnot(
    is_string(type),
    is_integer(n, 1)
  )
  .Call(rlang_vec_alloc, type, n)
}

# nocov end
