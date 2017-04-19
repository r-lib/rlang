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
