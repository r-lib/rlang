context("interp")

test_that("protected against bad inputs", {
  f <- ~ x + 1
  attr(f, ".Environment") <- 10
  expect_error(interp(f), "must be an environment")
})


# UQ ----------------------------------------------------------------------

test_that("evaluates contents of UQ()", {
  expect_equal(interp(~ UQ(1 + 2)), ~ 3)
})


test_that("unquoted formulas are interpolated first", {
  f <- function(n) {
    ~ x + UQ(n)
  }
  n <- 100

  expect_equal(interp(~ UQ(f(10))), ~ x + 10)
})


# UQS ---------------------------------------------------------------------

test_that("contents of UQS() must be a vector or language object", {
  expr <- ~ 1 + UQS(environment())
  expect_error(interp(expr), "`x` must be a vector")
})

test_that("values of UQS() spliced into expression", {
  expr <- ~ f(a, UQS(list(quote(b), quote(c))), d)
  expect_identical(interp(expr), ~ f(a, b, c, d))
})

test_that("names within UQS() are preseved", {
  expr <- ~ f(UQS(list(a = quote(b))))
  expect_identical(interp(expr), ~ f(a = b))
})

test_that("UQS() handles language objects", {
  expect_identical(interp(~list(UQS(quote(foo)))), ~list(foo))
  expect_identical(interp(~list(UQS(quote({ foo })))), ~list(foo))
})


# UQF ---------------------------------------------------------------------

test_that("requires formula", {
  expect_error(interp(~ UQF(10)), "must be a formula")
})

test_that("interpolates formula", {
  expect_equal(interp(~ UQF(x ~ y)), ~ (x ~ y))
})
