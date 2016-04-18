context("new_formula")

test_that("expr must be valid type", {
  expect_error(new_formula(list()), "not a valid language object")
})

test_that("equivalent to ~", {
  f1 <- ~abc
  f2 <- new_formula(quote(abc))

  expect_identical(f1, f2)
})
