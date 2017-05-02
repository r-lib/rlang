context("eval_tidy") # --------------------------------------------------

test_that("accepts expressions", {
  expect_identical(eval_tidy(10), 10)
  expect_identical(eval_tidy(quote(letters)), letters)
})

test_that("eval_tidy uses formula's environment", {
  x <- 10
  f <- local({
    y <- 100
    quo(x + y)
  })

  expect_equal(eval_tidy(f), 110)
})

test_that("data must be a dictionary", {
  expect_error(eval_tidy(NULL, list(x = 10, x = 11)), "Data source must be a dictionary")
})

test_that("looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(eval_tidy(quo(x), data), 100)
})

test_that("pronouns resolve ambiguity looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(eval_tidy(quo(.data$x), data), 100)
  expect_equal(eval_tidy(quo(.env$x), data), 10)
})

test_that("pronouns complain about missing values", {
  expect_error(eval_tidy(quo(.data$x), list()), "Object `x` not found in data")
  expect_error(eval_tidy(quo(.data$x), data.frame()), "Column `x` not found in data")
})

test_that("eval_tidy does quasiquoting", {
  x <- 10
  expect_equal(eval_tidy(quo(UQ(quote(x)))), 10)
})


test_that("unquoted formulas look in their own env", {
  f <- function() {
    n <- 100
    quo(n)
  }

  n <- 10
  expect_equal(eval_tidy(quo(UQ(f()))), 100)
})

test_that("unquoted formulas can use data", {
  f1 <- function() {
    z <- 100
    x <- 2
    quo(x + z)
  }
  f2 <- function() {
    z <- 100
    quo(.data$x + .env$z)
  }

  z <- 10
  expect_identical(eval_tidy(f2(), list(x = 1)), 101)
  expect_identical(eval_tidy(quo(!! f1()), data = list(x = 1)), 101)
  expect_identical(eval_tidy(quo(!! f2()), data = list(x = 1)), 101)
})

test_that("bare formulas are not evaluated", {
  f <- local(~x)
  expect_identical(eval_tidy(quo(!! f)), f)

  f <- a ~ b
  expect_identical(eval_tidy(quo(!! f)), f)
})

test_that("quosures are not evaluated if not forced", {
  fn <- function(arg, force) {
    if (force) arg else "bar"
  }

  f1 <- quo(fn(!! quo(stop("forced!")), force = FALSE))
  f2 <- quo(fn(!! local(quo(stop("forced!"))), force = FALSE))
  expect_identical(eval_tidy(f1), "bar")
  expect_identical(eval_tidy(f2), "bar")

  f_forced1 <- quo(fn(!! quo(stop("forced!")), force = TRUE))
  f_forced2 <- quo(fn(!! local(quo(stop("forced!"))), force = TRUE))
  expect_error(eval_tidy(f_forced1), "forced!")
  expect_error(eval_tidy(f_forced2), "forced!")
})

test_that("can unquote captured arguments", {
  var <- quo(cyl)
  fn <- function(arg) eval_tidy(enquo(arg), mtcars)
  expect_identical(fn(var), quo(cyl))
  expect_identical(fn(!!var), mtcars$cyl)
})

test_that("quosures are evaluated recursively", {
  foo <- "bar"
  expect_identical(eval_tidy(quo(foo)), "bar")
  expect_identical(eval_tidy(quo(!!quo(!! quo(foo)))), "bar")
})

test_that("quosures have lazy semantics", {
  fn <- function(arg) "unforced"
  expect_identical(eval_tidy(quo(fn(~stop()))), "unforced")
})

test_that("can unquote hygienically within captured arg", {
  fn <- function(df, arg) eval_tidy(enquo(arg), df)

  foo <- "bar"; var <- quo(foo)
  expect_identical(fn(mtcars, list(var, !!var)), list(quo(foo), "bar"))

  var <- quo(cyl)
  expect_identical(fn(mtcars, (!!var) > 4), mtcars$cyl > 4)
  expect_identical(fn(mtcars, list(var, !!var)), list(quo(cyl), mtcars$cyl))
  expect_equal(fn(mtcars, list(~var, !!var)), list(~var, mtcars$cyl))
  expect_equal(fn(mtcars, list(~~var, !!quo(var), !!quo(quo(var)))), list(~~var, quo(cyl), quo(var)))
})

test_that("can unquote for old-style NSE functions", {
  var <- quo(foo)
  fn <- function(x) substitute(x)
  expect_identical(quo(fn(!!f_rhs(var))), quo(fn(foo)))
  expect_identical(eval_tidy(quo(fn(!!f_rhs(var)))), quote(foo))
})

test_that("all quosures in the call are evaluated", {
  foobar <- function(x) paste0("foo", x)
  x <- new_quosure(call("foobar", local({ bar <- "bar"; quo(bar) })))
  f <- new_quosure(call("identity", x))
  expect_identical(eval_tidy(f), "foobar")
})

test_that("two-sided formulas are not treated as quosures", {
  expect_identical(eval_tidy(new_quosure(a ~ b)), a ~ b)
})

test_that("formulas are evaluated in evaluation environment", {
  f <- eval_tidy(quo(foo ~ bar), list(foo = "bar"))
  expect_false(identical(f_env(f), get_env()))
})

test_that("evaluation env is cleaned up", {
  f <- local(quo(function() list(f = ~letters, env = environment())))
  fn <- eval_tidy(f)
  out <- fn()
  expect_identical(out$f, with_env(env = out$env, ~letters))
})

test_that("inner formulas are rechained to evaluation env", {
  env <- child_env(NULL)
  f1 <- quo(env$eval_env1 <- get_env())
  f2 <- quo({
    !! f1
    env$eval_env2 <- get_env()
  })

  eval_tidy(f2, mtcars)
  expect_identical(env$eval_env1, env$eval_env2)
  expect_true(env_inherits(env$eval_env2, get_env(f2)))
})

test_that("dyn scope is chained to lexical env", {
  foo <- "bar"
  overscope <- child_env(NULL)
  expect_identical(eval_tidy_(quo(foo), overscope), "bar")
})

test_that("whole scope is purged", {
  outside <- child_env(NULL, important = TRUE)
  top <- child_env(outside, foo = "bar", hunoz = 1)
  mid <- child_env(top, bar = "baz", hunoz = 2)
  bottom <- child_env(mid, !!! list(.top_env = top, .env = 1, `~` = 2))

  overscope_clean(bottom)

  expect_identical(names(bottom), character(0))
  expect_identical(names(mid), character(0))
  expect_identical(names(top), character(0))
  expect_identical(names(outside), "important")
})

test_that("empty quosure self-evaluates", {
  quo <- quo(is_missing(!! quo()))
  expect_true(eval_tidy(quo))
})

test_that("cannot replace elements of pronouns", {
  expect_error(eval_tidy(quo(.data$foo <- "bar")), "read-only dictionary")
})

test_that("formulas are not evaluated as quosures", {
  expect_identical(eval_tidy(~letters), ~letters)
})

test_that("can supply environment as data", {
  `_x` <- "foo"
  expect_identical(eval_tidy(quo(`_x`), environment()), "foo")
  expect_error(eval_tidy(quo(`_y`), environment()), "not found")
})
