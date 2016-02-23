context("feval")

test_that("feval uses formula's environment", {
  x <- 10
  f <- local({
    y <- 100
    ~ x + y
  })

  expect_equal(feval(f), 110)
})

test_that("data needs to be a list", {
  expect_error(feval(~ x, 10), "must be NULL, a list, or a data frame")
})

test_that("looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(feval(~ x, data), 100)
})

test_that("pronouns resolve ambiguity looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(feval(~ .data$x, data), 100)
  expect_equal(feval(~ .env$x, data), 10)
})

test_that("pronouns complain about missing values", {
  expect_error(feval(~ .data$x, list()), "object 'x' not found")
  expect_error(feval(~ .env$`__`, list()), "object '__' not found")
})

test_that("feval does quasiquoting", {
  x <- 10
  expect_equal(feval(~ ((quote(x)))), 10)
})
