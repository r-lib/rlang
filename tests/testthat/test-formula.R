context("formula")

# Creation ----------------------------------------------------------------

test_that("env must be an environment", {
  expect_error(f_new(quote(a), env = list()), "must be an environment")
})

test_that("equivalent to ~", {
  f1 <- ~abc
  f2 <- f_new(quote(abc))

  expect_identical(f1, f2)
})

test_that("is_formula works", {
  expect_true(is_formula(~10))
  expect_false(is_formula(10))
})

# Getters -----------------------------------------------------------------

test_that("throws errors for bad inputs", {
  expect_error(f_rhs(1), "not a formula")
  expect_error(f_rhs(`~`()), "Invalid formula")
  expect_error(f_rhs(`~`(1, 2, 3)), "Invalid formula")

  expect_error(f_lhs(1), "not a formula")
  expect_error(f_lhs(`~`()), "Invalid formula")
  expect_error(f_lhs(`~`(1, 2, 3)), "Invalid formula")

  expect_error(f_env(1), "not a formula")
})

test_that("extracts call, name, or scalar", {
  expect_identical(f_rhs(~ x), quote(x))
  expect_identical(f_rhs(~ f()), quote(f()))
  expect_identical(f_rhs(~ 1L), 1L)
})

# Setters -----------------------------------------------------------------

test_that("can replace RHS of one-sided formula", {
  f <- ~ x1
  f_rhs(f) <- quote(x2)

  expect_equal(f, ~ x2)
})

test_that("can replace both sides of two-sided formula", {
  f <- x1 ~ y1
  f_lhs(f) <- quote(x2)
  f_rhs(f) <- quote(y2)

  expect_equal(f, x2 ~ y2)
})

test_that("can remove lhs of two-sided formula", {
  f <- x ~ y
  f_lhs(f) <- NULL

  expect_equal(f, ~ y)
})

test_that("can modify environment", {
  f <- x ~ y
  env <- new.env()
  f_env(f) <- env

  expect_equal(f_env(f), env)
})
