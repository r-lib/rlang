context("f_eval")

test_that("first argument must be a function", {
  expect_error(f_eval(10), "`f` is not a formula")
})

test_that("f_eval uses formula's environment", {
  x <- 10
  f <- local({
    y <- 100
    ~ x + y
  })

  expect_equal(f_eval(f), 110)
})

test_that("data needs to be a list", {
  expect_error(f_eval(~ x, 10), "Do not know how to find data")
})

test_that("looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(f_eval(~ x, data), 100)
})

test_that("pronouns resolve ambiguity looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(f_eval(~ .data$x, data), 100)
  expect_equal(f_eval(~ .env$x, data), 10)
})

test_that("pronouns complain about missing values", {
  expect_error(f_eval(~ .data$x, list()), "Variable 'x' not found in data")
  expect_error(f_eval(~ .env$`__`, list()), "Object '__' not found in environment")
})

test_that("f_eval does quasiquoting", {
  x <- 10
  expect_equal(f_eval(~ uq(quote(x))), 10)
})


test_that("unquoted formulas look in their own env", {
  f <- function() {
    n <- 100
    ~ n
  }

  n <- 10
  expect_equal(f_eval(~ uq(f())), 10)
})

test_that("unquoted formulas can use data", {
  f1 <- function() {
    z <- 100
    ~ x + z
  }
  f2 <- function() {
    z <- 100
    ~ .data$x + .env$z
  }

  z <- 10
  expect_equal(f_eval(~ uq(f1()), data = list(x = 1)), 101)
  expect_equal(f_eval(~ uq(f2()), data = list(x = 1)), 101)
})

test_that("f_eval_lhs uses lhs", {
  f <- 1 ~ 2

  expect_equal(f_eval_lhs(f), 1)
})


# find_data ---------------------------------------------------------------

test_that("find data works for NULL, lists, and data frames", {
  expect_equal(find_data(NULL), list())
  expect_equal(find_data(list(x = 1)), list(x = 1))
  expect_equal(find_data(mtcars), mtcars)
})
