
named <- function(x) {
  set_names(x, names2(x))
}
named_list <- function(...) {
  named(list(...))
}
quos_list <- function(...) {
  set_attrs(named_list(...), class = "quosures")
}

expect_error_ <- function(object, ...) {
  expect_error(object, ...)
}
expect_warning_ <- function(object, ...) {
  expect_warning(object, ...)
}
expect_identical_ <- function(object, expected, ...) {
  expect_identical(object, expected, ...)
}
