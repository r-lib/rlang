context("eval_tidy") # --------------------------------------------------

test_that("accepts expressions", {
  expect_identical(eval_tidy(10), 10)
  expect_identical(eval_tidy(quote(letters)), letters)
})

test_that("eval_tidy uses formula's environment", {
  x <- 10
  f <- local({
    y <- 100
    ~ x + y
  })

  expect_equal(eval_tidy(f), 110)
})

test_that("data must be a dictionary", {
  expect_error(eval_tidy(~ x, list(x = 10, x = 11)), "Data source must be a dictionary")
})

test_that("looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(eval_tidy(~ x, data), 100)
})

test_that("pronouns resolve ambiguity looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(eval_tidy(~ .data$x, data), 100)
  expect_equal(eval_tidy(~ .env$x, data), 10)
})

test_that("pronouns complain about missing values", {
  expect_error(eval_tidy(~ .data$x, list()), "Object `x` not found in data")
  expect_error(eval_tidy(~ .data$x, data.frame()), "Variable `x` not found in data")
})

test_that("eval_tidy does quasiquoting", {
  x <- 10
  expect_equal(eval_tidy(quo(UQ(quote(x)))), 10)
})


test_that("unquoted formulas look in their own env", {
  f <- function() {
    n <- 100
    ~ n
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

test_that("guarded formulas are not evaluated", {
  f <- local(~x)
  expect_identical(eval_tidy(quo(UQF(f))), f)

  f <- a ~ b
  fn <- function() ~UQF(f)
  expect_identical(eval_tidy(quo(!!fn())), f)
  expect_identical(eval_tidy(quo(UQF(f))), f)
})

test_that("quosures are not evaluated if not forced", {
  fn <- function(arg, force) {
    if (force) arg else "bar"
  }

  f1 <- quo(fn(!! ~stop("forced!"), force = FALSE))
  f2 <- quo(fn(!! local(~stop("forced!")), force = FALSE))
  expect_identical(eval_tidy(f1), "bar")
  expect_identical(eval_tidy(f2), "bar")

  f_forced1 <- quo(fn(!! ~stop("forced!"), force = TRUE))
  f_forced2 <- quo(fn(!! local(~stop("forced!")), force = TRUE))
  expect_error(eval_tidy(f_forced1), "forced!")
  expect_error(eval_tidy(f_forced2), "forced!")
})

test_that("can unquote captured arguments", {
  var <- ~cyl
  fn <- function(arg) eval_tidy(enquo(arg), mtcars)
  expect_identical(fn(var), ~cyl)
  expect_identical(fn(!!var), mtcars$cyl)
})

test_that("quosures are evaluated recursively", {
  foo <- "bar"
  expect_identical(eval_tidy(quo(foo)), "bar")
  expect_identical(eval_tidy(quo(!!~~foo)), "bar")
})

test_that("quosures have lazy semantics", {
  fn <- function(arg) "unforced"
  expect_identical(eval_tidy(quo(fn(~stop()))), "unforced")
})

test_that("can unquote hygienically within captured arg", {
  fn <- function(df, arg) eval_tidy(enquo(arg), df)

  foo <- "bar"; var <- ~foo
  expect_identical(fn(mtcars, list(var, !!var)), list(~foo, "bar"))

  var <- ~cyl
  expect_identical(fn(mtcars, (!!var) > 4), mtcars$cyl > 4)
  expect_identical(fn(mtcars, list(var, !!var)), list(~cyl, mtcars$cyl))
  expect_equal(fn(mtcars, list(~var, !!var)), list(~var, mtcars$cyl))
  expect_equal(fn(mtcars, list(~~var, !!~var, !!~~var)), list(new_quosure(new_language("_F", quote(var))), ~cyl, ~cyl))
})

test_that("can unquote for old-style NSE functions", {
  var <- quo(foo)
  fn <- function(x) substitute(x)
  expect_identical(quo(fn(!!f_rhs(var))), quo(fn(foo)))
  expect_identical(eval_tidy(quo(fn(!!f_rhs(var)))), quote(foo))
})

test_that("formulas with empty environments are scoped in surrounding formula", {
  var <- local(~letters)
  f <- new_quosure(var, env = child_env(get_env()))
  expect_identical(eval_tidy(f), letters)

  expect_identical(eval_tidy(~~letters), letters)
})

test_that("all quosures in the call are evaluated", {
  foobar <- function(x) paste0("foo", x)
  x <- new_quosure(call("foobar", local({ bar <- "bar"; ~bar })))
  f <- new_quosure(call("identity", x))
  expect_identical(eval_tidy(f), "foobar")
})

test_that("two-sided formulas are not treated as quosures", {
  expect_identical(eval_tidy(new_quosure(a ~ b)), a ~ b)
})

test_that("formulas are evaluated in evaluation environment", {
  f <- eval_tidy(~(foo ~ bar), list(foo = "bar"))
  expect_true(!identical(f_env(f), get_env()))
})

test_that("evaluating a side preserves the other side", {
  expect_identical(eval_tidy_lhs(1 + 2 ~ 1 + 2), 3 ~ 1 + 2)
  expect_identical(eval_tidy_rhs(1 + 2 ~ 1 + 2), 1 + 2 ~ 3)
})

test_that("can evaluate definitions", {
  expect_identical(eval_tidy_lhs(1 + 2 := 1 + 2), 3 := 1 + 2)
  expect_identical(eval_tidy_rhs(1 + 2 := 1 + 2), 1 + 2 := 3)
})

test_that("evaluation env is cleaned up", {
  f <- local(~function() list(f = ~letters, env = environment()))
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
  expect_identical(eval_tidy_(~foo, overscope), "bar")
})

test_that("whole scope is purged", {
  outside <- child_env(NULL, list(important = TRUE))
  top <- child_env(outside, list(foo = "bar", hunoz = 1))
  mid <- child_env(top, list(bar = "baz", hunoz = 2))
  bottom <- child_env(mid, list(
    .top_env = top,
    .env = 1,
    `~` = 2,
    `_F` = 3
  ))

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


context("invoke") # --------------------------------------------------

test_that("invoke() buries arguments", {
  expect_identical(invoke(call_inspect, 1:2), quote(.fn(`1`, `2`)))
  expect_identical(invoke("call_inspect", 1:2), quote(call_inspect(`1`, `2`)))
  expect_identical(invoke(call_inspect, 1:2, .bury = c("foo", "bar")), quote(foo(`bar1`, `bar2`)))
  expect_identical(invoke(call_inspect, 1:2, .bury = FALSE), as.call(list(call_inspect, 1L, 2L)))
})

test_that("invoke() is calls without arguments", {
  expect_identical(invoke("list"), list())
  expect_identical(invoke(list), list())
})
