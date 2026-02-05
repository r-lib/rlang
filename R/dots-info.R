#' These functions mirror the C API proposed in R-devel for programmatically
#' working with dots.

dots_exist <- function(env = caller_env()) {
  check_environment(env)
  .Call(ffi_dots_exist, env)
}

dots_length <- function(env = caller_env()) {
  check_environment(env)
  .Call(ffi_dots_length, env)
}

dots_names <- function(env = caller_env()) {
  check_environment(env)
  .Call(ffi_dots_names, env)
}

dots_elt <- function(i, env = caller_env()) {
  check_number_whole(i, min = 1)
  check_environment(env)
  .Call(ffi_dots_elt, as.integer(i), env)
}

dot_type <- function(i, env = caller_env()) {
  check_number_whole(i, min = 1)
  check_environment(env)
  .Call(ffi_dot_type, as.integer(i), env)
}

dot_delayed_expr <- function(i, env = caller_env()) {
  check_number_whole(i, min = 1)
  check_environment(env)
  .Call(ffi_dot_delayed_expr, as.integer(i), env)
}

dot_delayed_env <- function(i, env = caller_env()) {
  check_number_whole(i, min = 1)
  check_environment(env)
  .Call(ffi_dot_delayed_env, as.integer(i), env)
}

dot_forced_expr <- function(i, env = caller_env()) {
  check_number_whole(i, min = 1)
  check_environment(env)
  .Call(ffi_dot_forced_expr, as.integer(i), env)
}
