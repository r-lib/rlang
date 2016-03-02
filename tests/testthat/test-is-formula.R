context("is_formula")

test_that("is.formula works", {
  expect_true(is_formula(~10))
  expect_false(is_formula(10))
})
