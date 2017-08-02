
mut_type <- function(x, type) {
  invisible(.Call(rlang_mut_type, x, type))
}
