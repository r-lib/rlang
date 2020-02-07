context("arg")

test_that("matches arg", {
  myarg <- "foo"
  expect_identical(arg_match(myarg, c("bar", "foo")), "foo")
  expect_error(
    arg_match(myarg, c("bar", "baz")),
    "`myarg` must be one of \"bar\" or \"baz\""
  )
})

test_that("gives an error with more than one arg", {
  myarg <- c("bar","fun")
  expect_error(
    regex = "`myarg` must be one of \"bar\" or \"baz\".",
    arg_match(myarg, c("bar", "baz"))
  )
})

test_that("gives error with different than rearranged arg vs value", {
  f <- function(myarg = c("foo", "bar", "fun")) {
    arg_match(myarg, c("fun", "bar"))
  }
  expect_error(f(), regex = "`myarg` must be one of \"fun\" or \"bar\"")
})

test_that("gives no error with rearranged arg vs value", {
  f <- function(myarg = c("bar", "fun")) {
    arg_match(myarg, c("fun", "bar"))
  }
  expect_identical(f(), "bar")
})

test_that("uses first value when called with all values", {
  myarg <- c("bar","baz")
  expect_identical(arg_match(myarg, c("bar", "baz")), "bar")
})

test_that("informative error message on partial match", {
  myarg <- "f"
  expect_error(
    arg_match(myarg, c("bar", "foo")),
    "Did you mean \"foo\"?"
  )
})

test_that("informative error message on a typo", {
  verify_output("test-typo-suggest.txt", {
    myarg <- "continuuos"
    arg_match(myarg, c("discrete", "continuous"))
    myarg <- "fou"
    arg_match(myarg, c("bar", "foo"))
    myarg <- "fu"
    arg_match(myarg, c("ba", "fo"))

    "# No suggestion when the edit distance is too large"
    myarg <- "foobaz"
    arg_match(myarg, c("fooquxs", "discrete"))
    myarg <- "a"
    arg_match(myarg, c("b", "c"))

    "# Even with small possible typos, if there's a match it returns the match"
    myarg <- "bas"
    arg_match(myarg, c("foo", "baz", "bas"))
  })
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
