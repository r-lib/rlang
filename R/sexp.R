
mut_type <- function(x, type) {
  invisible(.Call(rlang_poke_type, x, type))
}

mark_object <- function(x) {
  invisible(.Call(rlang_mark_object, x))
}
unmark_object <- function(x) {
  invisible(.Call(rlang_unmark_object, x))
}
