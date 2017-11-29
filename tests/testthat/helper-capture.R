
named <- function(x) {
  set_names(x, names2(x))
}
named_list <- function(...) {
  named(list(...))
}
quos_list <- function(...) {
  set_attrs(named_list(...), class = "quosures")
}
