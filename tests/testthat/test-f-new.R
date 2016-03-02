context("f_new")

test_that("expr must be valid type", {
  expect_error(f_new(list()), "not a valid language object")
})

test_that("equivalent to ~", {
  f1 <- ~abc
  f2 <- f_new(quote(abc))

  expect_identical(f1, f2)
})
