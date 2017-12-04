
poke_type <- function(x, type) {
  invisible(.Call(rlang_poke_type, x, type))
}


# The functions below are for interactive testing

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
