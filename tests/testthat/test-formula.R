context("formula")

# Creation ----------------------------------------------------------------

test_that("env must be an environment", {
  expect_error(new_quosure(quote(a), env = list()), "must be an environment")
})

test_that("equivalent to ~", {
  f1 <- ~abc
  f2 <- new_quosure(quote(abc))

  expect_identical(set_attrs(f1, class = c("quosure", "formula")), f2)
})

test_that("is_formula works", {
  expect_true(is_formula(~10))
  expect_false(is_formula(10))
})

test_that("as_quosure() uses correct env", {
  fn <- function(expr, env = caller_env()) {
    f <- as_quosure(expr, env)
    list(env = get_env(), f = g(f))
  }
  g <- function(expr, env = caller_env()) {
    as_quosure(expr, env)
  }
  f_env <- child_env()
  f <- new_quosure(quote(expr), f_env)

  out_expr_default <- fn(quote(expr))
  out_f_default <- fn(f)
  expect_identical(f_env(out_expr_default$f), get_env())
  expect_identical(f_env(out_f_default$f), f_env)

  user_env <- child_env()
  out_expr <- fn(quote(expr), user_env)
  out_f <- fn(f, user_env)
  expect_identical(f_env(out_expr$f), user_env)
  expect_identical(out_f$f, f)
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


# Utils --------------------------------------------------------------

test_that("f_unwrap() substitutes values", {
  n <- 100
  f1 <- f_unwrap(~ x + n)
  f2 <- set_env(~x + 100, env_parent(get_env()))

  expect_identical(f1, f2)
})

test_that("f_unwrap() substitutes even in globalenv", {
  .GlobalEnv$`__1` <- 1
  expect_equal(f_rhs(f_unwrap(new_quosure(quote(`__1`), env = globalenv()))), 1)
})

test_that("f_unwrap() doesn't go past empty env", {
  f <- new_quosure(quote(x == y), env = empty_env())
  expect_equal(f_unwrap(f), f)
})
