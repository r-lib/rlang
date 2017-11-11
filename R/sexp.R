
mut_type <- function(x, type) {
  invisible(.Call(rlang_poke_type, x, type))
}
