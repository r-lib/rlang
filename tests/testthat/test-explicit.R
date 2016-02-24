context("explicit_promise")

test_that("explicit promise makes a formula", {
  f1 <- explicit_promise(1 + 2 + 3)
  f2 <- ~ 1 + 2 + 3

  expect_equal(f1, f2)
})

test_that("explicit promise works several levels deep", {
  f <- function(x) g(x)
  g <- function(y) h(y)
  h <- function(z) explicit_promise(z)

  f1 <- f(1 + 2 + 3)
  f2 <- ~ 1 + 2 + 3

  expect_equal(f1, f2)
})

test_that("explicit dots makes a list of formulas", {
  fs <- explicit_dots(x = 1 + 2, y = 2 + 3)
  f1 <- ~ 1 + 2
  f2 <- ~ 2 + 3

  expect_equal(fs$x, f1)
  expect_equal(fs$y, f2)
})
