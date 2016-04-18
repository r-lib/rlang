context("f_unwrap")

test_that("f_unwrap substitutes values", {
  n <- 100
  f1 <- f_unwrap(~ x + n)
  f2 <- new_formula(quote(x + 100), parent.env(environment()))

  expect_identical(f1, f2)
})

test_that("f_unwrap substitutes even in globalenv", {
  .GlobalEnv$`__1` <- 1
  expect_equal(f_rhs(f_unwrap(new_formula(quote(`__1`), globalenv()))), 1)
})

test_that("doesn't go past empty env", {
  f <- new_formula(quote(x == y), emptyenv())
  expect_equal(f_unwrap(f), f)
})
