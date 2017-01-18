context("f_eval") # --------------------------------------------------

test_that("accepts expressions", {
  expect_identical(f_eval(10), 10)
  expect_identical(f_eval(quote(letters)), letters)
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
  expect_equal(f_eval(f_quote(UQ(quote(x)))), 10)
})


test_that("unquoted formulas look in their own env", {
  f <- function() {
    n <- 100
    ~ n
  }

  n <- 10
  expect_equal(f_eval(f_quote(UQ(f()))), 100)
})

test_that("unquoted formulas can use data", {
  f1 <- function() {
    z <- 100
    f_quote(x + z)
  }
  f2 <- function() {
    z <- 100
    f_quote(.data$x + .env$z)
  }

  z <- 10
  expect_equal(f_eval(f_quote(UQ(f1())), data = list(x = 1)), 101)
  expect_equal(f_eval(f_quote(UQ(f2())), data = list(x = 1)), 101)
})

test_that("f_eval_lhs uses lhs", {
  expect_equal(f_eval_lhs(1 ~ 2), 1)
})

test_that("guarded formulas are not evaluated", {
  f <- local(~x)
  expect_identical(f_eval(f_quote(UQF(f))), f)

  f <- a ~ b
  fn <- function() ~UQF(f)
  expect_identical(f_eval(f_quote(!!fn())), f)
  expect_identical(f_eval(f_quote(UQF(f))), f)
})

test_that("fpromises are not evaluated if not forced", {
  fn <- function(arg, force) {
    if (force) arg else "bar"
  }

  f1 <- f_quote(fn(!! ~stop("forced!"), force = FALSE))
  f2 <- f_quote(fn(!! local(~stop("forced!")), force = FALSE))
  expect_identical(f_eval(f1), "bar")
  expect_identical(f_eval(f2), "bar")

  f_forced1 <- f_quote(fn(!! ~stop("forced!"), force = TRUE))
  f_forced2 <- f_quote(fn(!! local(~stop("forced!")), force = TRUE))
  expect_error(f_eval(f_forced1), "forced!")
  expect_error(f_eval(f_forced2), "forced!")
})

test_that("can unquote captured arguments", {
  var <- ~cyl
  fn <- function(arg) f_eval(arg_capture(arg), mtcars)
  expect_identical(fn(var), ~cyl)
  expect_identical(fn(!!var), mtcars$cyl)
})

test_that("fpromises are evaluated recursively", {
  foo <- "bar"
  expect_identical(f_eval(f_quote(foo)), "bar")
  expect_identical(f_eval(f_quote(~~foo)), "bar")
})

test_that("fpromises have lazy semantics", {
  fn <- function(arg) "unforced"
  expect_identical(f_eval(f_quote(fn(~stop()))), "unforced")
})

test_that("can unquote hygienically within captured arg", {
  fn <- function(df, arg) f_eval(arg_capture(arg), df)

  foo <- "bar"; var <- ~foo
  expect_identical(fn(mtcars, list(var, !!var)), list(~foo, "bar"))

  var <- ~cyl
  expect_identical(fn(mtcars, (!!var) > 4), mtcars$cyl > 4)
  expect_identical(fn(mtcars, list(var, !!var)), list(~cyl, mtcars$cyl))
  expect_identical(fn(mtcars, list(~var, !!var)), list(~cyl, mtcars$cyl))
  expect_identical(fn(mtcars, list(~~var, !!~var, !!~~var)), list(~cyl, ~cyl, ~cyl))
})

test_that("can unquote for old-style NSE functions", {
  var <- ~foo
  fn <- function(x) substitute(x)
  expect_identical(f_quote(fn(!!f_rhs(var))), ~fn(foo))
  expect_identical(f_eval(f_quote(fn(!!f_rhs(var)))), quote(foo))
})

test_that("formulas with empty environments are scoped in surrounding formula", {
  var <- local(~letters)
  f <- f_new(var, env = env_new(env()))
  expect_identical(f_eval(f), letters)

  expect_identical(f_eval(~~letters), letters)
})

test_that("all fpromises in the call are evaluated", {
  foobar <- function(x) paste0("foo", x)
  x <- f_new(call("foobar", local({ bar <- "bar"; ~bar })))
  f <- f_new(call("identity", x))
  expect_identical(f_eval(f), "foobar")
})

test_that("two-sided formulas are not treated as fpromises", {
  expect_identical(f_eval(f_new(a ~ b)), a ~ b)
})

test_that("scope info is propagated in quoted formulas", {
  expect_identical(f_eval(~ (a ~ b)), a ~ b)
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
