context("funwrap")

test_that("funwrap substitutes values", {
  n <- 100
  f1 <- funwrap(~ x + n)
  f2 <- make_formula(quote(x + 100), parent.env(environment()))

  expect_identical(f1, f2)
})

test_that("funwrap substitutes even in globalenv", {
  .GlobalEnv$`__1` <- 1
  expect_equal(rhs(funwrap(make_formula(quote(`__1`), globalenv()))), 1)
})

test_that("doesn't go past empty env", {
  f <- make_formula(quote(x == y), emptyenv())
  expect_equal(funwrap(f), f)
})
