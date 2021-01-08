
# dict.c

new_dict <- function(size, prevent_resize = FALSE) {
  .Call(rlang_new_dict, size, prevent_resize)
}

dict_size <- function(dict) {
  length(dict[[2]][[1]])
}

dict_resize <- function(dict, size) {
  .Call(rlang_dict_resize, dict, size)
}

dict_put <- function(dict, key, value) {
  .Call(rlang_dict_put, dict, key, value)
}

dict_has <- function(dict, key) {
  .Call(rlang_dict_has, dict, key)
}

dict_get <- function(dict, key) {
  .Call(rlang_dict_get, dict, key)
}
