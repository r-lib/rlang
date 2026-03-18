# These functions mirror the C API proposed in R-devel for programmatically
# working with dots.

env_dots_exist <- function(env = caller_env()) {
  check_environment(env)
  .Call(ffi_env_dots_exist, env)
}

env_dots_length <- function(env = caller_env()) {
  check_environment(env)
  .Call(ffi_env_dots_length, env)
}

env_dots_names <- function(env = caller_env()) {
  check_environment(env)
  .Call(ffi_env_dots_names, env)
}

env_dot_get <- function(env, i) {
  check_environment(env)
  check_number_whole(i, min = 1)
  .Call(ffi_env_dot_get, as.integer(i), env)
}

env_dot_type <- function(env, i) {
  check_environment(env)
  check_number_whole(i, min = 1)
  .Call(ffi_env_dot_type, as.integer(i), env)
}

env_dot_delayed_expr <- function(env, i) {
  check_environment(env)
  check_number_whole(i, min = 1)
  .Call(ffi_env_dot_delayed_expr, as.integer(i), env)
}

env_dot_delayed_env <- function(env, i) {
  check_environment(env)
  check_number_whole(i, min = 1)
  .Call(ffi_env_dot_delayed_env, as.integer(i), env)
}

env_dot_forced_expr <- function(env, i) {
  check_environment(env)
  check_number_whole(i, min = 1)
  .Call(ffi_env_dot_forced_expr, as.integer(i), env)
}
