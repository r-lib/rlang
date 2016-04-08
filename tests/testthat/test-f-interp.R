context("f_interp")

test_that("interp produces single string for character inputs", {
  x <- interp("aaaaaaaaaaaaaa + bbbbbbbbbbbbbbb + ccccccccccccccccc + dddddddddddddddd + eeeeeeeeeeeeeee")
  expect_is(x, "character")
  expect_equal(length(x), 1)
})


test_that("can interpolate from environment", {
  env <- new.env(parent = emptyenv())
  env$a <- 10

  out <- interp(~ f(a), .values = env)
  expect_identical(out, ~f(10))
})

test_that("unquoted formulas are interpolated first", {
  f <- function(n) {
    ~ x + uq(n)
  }
  n <- 100

  expect_equal(f_interp(~ uq(f(10))), ~ x + 10)
})
