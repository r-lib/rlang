context("arg")

test_that("matches arg", {
  myarg <- c("foo", "baz")
  expect_identical(arg_match(myarg, c("bar", "foo")), "foo")
  expect_error(
    regex = "`myarg` must be one of \"bar\" or \"baz\"",
    arg_match(myarg, c("bar", "baz"))
  )
})

test_that("informative error message on partial match", {
  myarg <- "f"
  expect_error(
    regex = "Did you mean \"foo\"?",
    arg_match(myarg, c("bar", "foo"))
  )
})

test_that("gets choices from function", {
  fn <- function(myarg = c("bar", "foo")) arg_match(myarg)
  expect_error(fn("f"), "Did you mean \"foo\"?")
  expect_identical(fn("foo"), "foo")
})

test_that("is_missing() works with symbols", {
  x <- missing_arg()
  expect_true(is_missing(x))
})

test_that("is_missing() works with non-symbols", {
  expect_true(is_missing(missing_arg()))

  l <- list(missing_arg())
  expect_true(is_missing(l[[1]]))
  expect_error(missing(l[[1]]), "invalid use")
})

test_that("maybe_missing() forwards missing value", {
  x <- missing_arg()
  expect_true(is_missing(maybe_missing(x)))
  expect_false(is_missing(maybe_missing(1L)))
})
