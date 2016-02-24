context("rhs")

test_that("throws errors for bad inputs", {
  expect_error(rhs(1), "not a formula")
  expect_error(rhs(y ~ x), "not a one-sided formula")
})


test_that("extracts call, name, or scalar", {
  expect_identical(rhs(~ x), quote(x))
  expect_identical(rhs(~ f()), quote(f()))
  expect_identical(rhs(~ 1L), 1L)
})
