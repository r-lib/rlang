
named_list <- function(...) {
  x <- list(...)
  set_names(x, names2(x))
}
quos_list <- function(...) {
  struct(named_list(...), class = "quosures")
}
