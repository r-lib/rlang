context("dots")

test_that("dots are retrieved from arguments", {
  fn <- function(f, ...) f(...)
  expect_identical(fn(exprs), named_list())

  g <- function(f, ...) fn(f, ...)
  expect_identical(g(exprs, a = 1, foo = bar), list(a = 1, foo = quote(bar)))
})

test_that("exprs() captures empty arguments", {
  expect_identical(exprs(, , .ignore_empty = "none"), set_names(list(missing_arg(), missing_arg()), c("", "")))
})

test_that("dots are always named", {
  expect_named(dots_list("foo"), "")
  expect_named(dots_splice("foo", list("bar")), c("", ""))
  expect_named(exprs(foo, bar), c("", ""))
})

test_that("dots can be spliced", {
  expect_identical(dots_values(!!! list(letters)), named_list(splice(list(letters))))
  expect_identical(flatten(dots_values(!!! list(letters))), list(letters))
  expect_identical(ll(!!! list(letters)), list(letters))
  wrapper <- function(...) ll(...)
  expect_identical(wrapper(!!! list(letters)), list(letters))
})

test_that("interpolation by value does not guard formulas", {
  expect_identical(dots_values(~1), named_list(~1))
})

test_that("dots names can be unquoted", {
  expect_identical(dots_values(!! paste0("foo", "bar") := 10), list(foobar = 10))
})

test_that("can take forced dots with strict = FALSE", {
  fn <- function(strict, ...) {
    force(..1)
    captureDots(strict)
  }
  expect_error(fn(TRUE, letters), "already been evaluated")
  expect_identical(fn(FALSE, letters), NULL)
})

test_that("dots_values() handles forced dots", {
  fn <- function(...) {
    force(..1)
    dots_values(...)
  }
  expect_identical(fn("foo"), named_list("foo"))

  expect_identical(lapply(1:2, function(...) dots_values(...)), list(named_list(1L), named_list(2L)))
})

test_that("cleans empty arguments", {
  expect_identical(dots_list(1, a = ), named_list(1))
  expect_identical(ll(1, a = ), list(1))

  expect_identical(dots_list(, 1, a = , .ignore_empty = "all"), named_list(1))
})
