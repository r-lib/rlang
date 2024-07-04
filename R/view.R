vec_view <- function(x, start, size) {
  check_number_whole(start)
  check_number_whole(size)
  .Call(ffi_vec_view, x, start, size)
}

view_inspect <- function(x) {
  invisible(.Call(ffi_view_inspect, x))
}

view_is_materialized <- function(x) {
  .Call(ffi_view_is_materialized, x)
}

view_materialize <- function(x) {
  .Call(ffi_view_materialize, x)
}
