context("f_eval") # --------------------------------------------------

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

test_that("data must be a dictionary", {
  expect_error(f_eval(~ x, 10), "Data source must be a dictionary")
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
  expect_error(f_eval(~ .data$x, list()), "Object 'x' not found in pronoun")
  expect_error(f_eval(~ .data$x, data.frame()), "Variable 'x' not found in data")
  expect_error(f_eval(~ .env$`__`, list()), "Object '__' not found in environment")
})

test_that("f_eval does quasiquoting", {
  x <- 10
  expect_equal(f_eval(~ UQ(quote(x))), 10)
})


test_that("unquoted formulas look in their own env", {
  f <- function() {
    n <- 100
    ~ n
  }

  n <- 10
  expect_equal(f_eval(~ UQ(f())), 10)
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
  expect_equal(f_eval(~ UQ(f1()), data = list(x = 1)), 101)
  expect_equal(f_eval(~ UQ(f2()), data = list(x = 1)), 101)
})

test_that("f_eval_lhs uses lhs", {
  expect_equal(f_eval_lhs(1 ~ 2), 1)
})


context("data_source") # ---------------------------------------------

test_that("NULL return unchanged", {
  expect_identical(data_source(NULL), NULL)
})

test_that("can't access non-existent list members", {
  x1 <- list(y = 1)
  x2 <- data_source(x1)

  expect_equal(x2$y, 1)
  expect_error(x2$z, "Object 'z' not found in pronoun")
  expect_error(x2[["z"]], "Object 'z' not found in pronoun")
})

test_that("can't access non-existent environment components", {
  x1 <- list2env(list(y = 1))
  x2 <- data_source(x1)

  expect_equal(x2$y, 1)
  expect_error(x2$z, "Object 'z' not found in environment")
  expect_error(x2[["z"]], "Object 'z' not found in environment")
})

test_that("can't use non-character vectors", {
  x <- data_source(list(y = 1))

  expect_error(x[[1]], "subset with a string")
  expect_error(x[[c("a", "b")]], "subset with a string")
})

test_that("data_source doesn't taint env class", {
  x1 <- list2env(list(y = 1))
  x2 <- data_source(x1)

  expect_equal(class(x1), "environment")
  expect_equal(class(x2), c("data_source", "environment"))
})
