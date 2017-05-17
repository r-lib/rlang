context("quo-unquo")

test_that("explicit promise makes a formula", {
  capture <- function(x) enquo(x)
  f1 <- capture(1 + 2 + 3)
  f2 <- ~ 1 + 2 + 3

  expect_equal(f1, f2)
})

test_that("explicit promise works only one level deep", {
  f <- function(x) list(env = get_env(), f = g(x))
  g <- function(y) enquo(y)
  out <- f(1 + 2 + 3)
  expected_f <- with_env(out$env, quo(x))

  expect_identical(out$f, expected_f)
})

test_that("can capture optimised constants", {
  arg <- function() {
    quo("foobar")
  }
  arg_bytecode <- compiler::cmpfun(arg)

  expect_identical(arg(), quo("foobar"))
  expect_identical(arg_bytecode(), quo("foobar"))

  dots <- function() {
    quos("foo", "bar")
  }
  dots_bytecode <- compiler::cmpfun(dots)

  expect_identical(dots(), quos("foo", "bar"))
  expect_identical(dots_bytecode(), quos("foo", "bar"))
})
