
r_env_unbind <- function(env, names, inherits = FALSE) {
  invisible(.Call(rlang_env_unbind, env, names, inherits))
}

r_parse_eval <- function(x, env = caller_env()) {
  .Call(rlang_test_parse_eval, x, env)
}

r_nms_are_duplicated <- function(nms, from_last = FALSE) {
  .Call(rlang_nms_are_duplicated, nms, from_last)
}

r_lgl_sum <- function(x, na_true) {
  stopifnot(is_logical(x), is_bool(na_true))
  .Call(rlang_test_lgl_sum, x, na_true)
}

r_lgl_which <- function(x, na_propagate) {
  stopifnot(is_logical(x), is_bool(na_propagate))
  .Call(rlang_test_lgl_which, x, na_propagate)
}
