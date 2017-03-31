
named <- function(x) {
  set_names(x, names2(x))
}
named_list <- function(...) {
  named(list(...))
}
quos_list <- function(...) {
  struct(named_list(...), class = "quosures")
}
