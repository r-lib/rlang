
named <- function(x) {
  set_names(x, names2(x))
}
named_list <- function(...) {
  named(list(...))
}
quos_list <- function(...) {
  set_attrs(named_list(...), class = "quosures")
}

bare_expr <- function(expr) enexpr(expr, unquote_names = FALSE)
bare_quo <- function(expr) enquo(expr, unquote_names = FALSE)
