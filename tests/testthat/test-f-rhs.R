context("f_rhs")

test_that("throws errors for bad inputs", {
  expect_error(f_rhs(1), "not a formula")
  expect_error(f_rhs(`~`()), "Invalid formula")
  expect_error(f_rhs(`~`(1, 2, 3)), "Invalid formula")
})

test_that("extracts call, name, or scalar", {
  expect_identical(f_rhs(~ x), quote(x))
  expect_identical(f_rhs(~ f()), quote(f()))
  expect_identical(f_rhs(~ 1L), 1L)
})
