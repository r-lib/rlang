
# dict.c

new_dict <- function(size, resize = TRUE) {
  .Call(rlang_new_dict, size, resize)
}

dict_put <- function(dict, key, value) {
  .Call(rlang_dict_put, dict, key, value)
}
