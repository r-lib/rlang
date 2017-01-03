context("f_interp")

test_that("protected against bad inputs", {
  f <- ~ x + 1
  attr(f, ".Environment") <- 10
  expect_error(f_interp(f), "must be an environment")
})


# UQ ----------------------------------------------------------------------

test_that("evaluates contents of uq()", {
  expect_equal(f_interp(~ uq(1 + 2)), ~ 3)
})


test_that("unquoted formulas are interpolated first", {
  f <- function(n) {
    ~ x + uq(n)
  }
  n <- 100

  expect_equal(f_interp(~ uq(f(10))), ~ x + 10)
})


# uqs ---------------------------------------------------------------------

test_that("contents of uqs() must be a vector", {
  expr <- ~ 1 + uqs(environment())
  expect_error(f_interp(expr), "`x` must be a vector")
})

test_that("values of uqs() spliced into expression", {
  expr <- ~ f(a, uqs(list(quote(b), quote(c))), d)
  expect_identical(f_interp(expr), ~ f(a, b, c, d))
})

test_that("names within uqs() are preseved", {
  expr <- ~ f(uqs(list(a = quote(b))))
  expect_identical(f_interp(expr), ~ f(a = b))
})


# uqf ---------------------------------------------------------------------

test_that("requires formula", {
  expect_error(f_interp(~ uqf(10)), "must be a formula")
})

test_that("interpolates formula", {
  expect_equal(f_interp(~ uqf(x ~ y)), ~ (x ~ y))
})
