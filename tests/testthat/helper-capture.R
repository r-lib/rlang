
named <- function(x) {
  set_names(x, names2(x))
}
named_list <- function(...) {
  named(list(...))
}
quos_list <- function(...) {
  structure(named_list(...), class = "quosures")
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
expect_equal_ <- function(object, expected, ...) {
  expect_equal(object, expected, ...)
}

expect_no_warning <- function(object, ...) {
  expect_warning(!!enquo(object), NA, ...)
}
expect_no_warning_ <- function(object, ...) {
  expect_warning(object, NA, ...)
}
expect_no_error <- function(object, ...) {
  expect_error(!!enquo(object), NA, ...)
}
expect_no_error_ <- function(object, ...) {
  expect_error(object, NA, ...)
}

expect_null_ <- function(object, ...) {
  expect_null(object, ...)
}
