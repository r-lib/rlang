is_altrep <- function(x) {
  .Call(ffi_is_altrep, x)
}
