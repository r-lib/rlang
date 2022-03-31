#' Duplicate an R object
#'
#' `duplicate()` is an interface to the C-level `duplicate()` and
#' `shallow_duplicate()` functions. It is mostly meant for users of
#' the C API of R, e.g. for debugging, experimenting, or prototyping C
#' code in R.
#'
#' @param x An R object. Uncopyable objects like symbols and
#'   environments are returned as is (just like with `<-`).
#' @param shallow Recursive data structures like lists, calls and
#'   pairlists are duplicated in full by default. A shallow copy only
#'   duplicates the top-level data structure.
#' @seealso pairlist
#' @keywords internal
#' @export
duplicate <- function(x, shallow = FALSE) {
  .Call(ffi_duplicate, x, shallow)
}

#' Address of an R object
#' @param x Any R object.
#' @return Its address in memory in a string.
#' @keywords internal
#' @export
obj_address <- function(x) {
  .Call(ffi_obj_address, x)
}

# Imported from lifecycle
sexp_address <- obj_address

# nocov start - These functions are mostly for interactive experimentation

poke_type <- function(x, type) {
  invisible(.Call(ffi_poke_type, x, type))
}
sexp_named <- function(x) {
  # Don't use `substitute()` because dots might be forwarded
  arg <- match.call(expand.dots = FALSE)$x
  .Call(ffi_named, arg, parent.frame())
}

mark_object <- function(x) {
  invisible(.Call(ffi_mark_object, x))
}
unmark_object <- function(x) {
  invisible(.Call(ffi_unmark_object, x))
}

true_length <- function(x) {
  .Call(ffi_true_length, x)
}
env_frame <- function(x) {
  .Call(ffi_env_frame, x)
}
env_hash_table <- function(x) {
  .Call(ffi_env_hash_table, x)
}

promise_expr <- function(name, env = caller_env()) {
  .Call(ffi_promise_expr, name, env)
}
promise_env <- function(name, env = caller_env()) {
  .Call(ffi_promise_env, name, env)
}
promise_value <- function(name, env = caller_env()) {
  .Call(ffi_promise_value, name, env)
}

c_warning <- function(msg) {
  .Call(ffi_test_Rf_warning, msg)
}
c_error <- function(msg) {
  .Call(ffi_test_Rf_error, msg)
}
warningcall <- function(call, msg) {
  .Call(ffi_test_Rf_warningcall, call, msg)
}
errorcall <- function(call, msg) {
  .Call(ffi_test_Rf_errorcall, call, msg)
}

obj_attrib <- function(x) {
  .Call(ffi_attrib, x)
}

vec_alloc <- function(type, n) {
  stopifnot(
    is_string(type),
    is_integer(n, 1) && is.finite(n)
  )
  .Call(ffi_vec_alloc, type, n)
}

# Note that the C-level function has inverted arguments
find_var <- function(env, sym) {
  .Call(ffi_find_var, env, sym);
}
find_var_in_frame <- function(env, sym) {
  .Call(ffi_find_var, env, sym);
}

chr_get <- function(x, i = 0L) {
  .Call(ffi_chr_get, x, i)
}

list_poke <- function(x, i, value) {
  .Call(ffi_list_poke, x, i, value)
}

# nocov end
