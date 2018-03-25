context("eval-tidy")

test_that("accepts expressions", {
  expect_identical(eval_tidy(10), 10)
  expect_identical(eval_tidy(quote(letters)), letters)
})

test_that("eval_tidy uses quosure environment", {
  x <- 10
  quo <- local({
    y <- 100
    quo(x + y)
  })
  expect_equal(eval_tidy(quo), 110)
})

test_that("data must be uniquely named", {
  expect_error(eval_tidy(NULL, list(x = 1, x = 2)), "has duplicate columns")

  data <- set_names(data.frame(x = 1, x = 2, y = 3, y = 4), c("x", "x", "y", "y"))
  expect_error(eval_tidy(NULL, data), "has duplicate columns")
})

test_that("can supply unnamed empty data", {
  expect_identical(eval_tidy("foo", list()), "foo")
  expect_identical(eval_tidy("foo", data.frame()), "foo")
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
  expect_error(eval_tidy(quo(.data$x), list()), "Column `x` not found in `.data`")
  expect_error(eval_tidy(quo(.data$x), data.frame()), "Column `x` not found in `.data`")
})

test_that("nested quosures look in their own env", {
  n <- 10
  f <- function() {
    n <- 100
    quo(n)
  }
  quo <- quo(!!f())
  expect_equal(eval_tidy(quo), 100)
})

test_that("nested quosure thunks rechain properly in the non-data mask", {
  bar <- "foo"
  quo <- quo(identity(!!quo(toupper(!!quo(identity(bar))))))
  expect_identical(eval_tidy(quo), "FOO")
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
  expect_identical(quo(fn(!!quo_get_expr(var))), quo(fn(foo)))
  expect_identical(eval_tidy(quo(fn(!!quo_get_expr(var)))), quote(foo))
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
  expect_false(identical(f_env(f), current_env()))
})

test_that("evaluation env is cleaned up", {
  f <- local(quo(function() list(f = ~letters, env = environment())))
  fn <- eval_tidy(f)
  out <- fn()
  expect_identical(out$f, with_env(env = out$env, ~letters))
})

test_that("inner formulas are rechained to evaluation env", {
  env <- child_env(NULL)
  f1 <- quo(env$eval_env1 <- current_env())
  f2 <- quo({
    !! f1
    env$eval_env2 <- current_env()
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

  data_mask_objects <- list(
    .top_env = top,
    .env = 1,
    `~` = 2,
    .__tidyeval_data_mask__. = env()
  )
  bottom <- child_env(mid, !!! data_mask_objects)

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
  expect_error(eval_tidy(quo(.data$foo <- "bar")), "Can't modify the data pronoun")
})

test_that("formulas are not evaluated as quosures", {
  expect_identical(eval_tidy(~letters), ~letters)
})

test_that("can supply environment as data", {
  `_x` <- "foo"
  expect_identical(eval_tidy(quo(`_x`), environment()), "foo")
  expect_error(eval_tidy(quo(`_y`), environment()), "not found")
})

test_that("tilde calls are evaluated in overscope", {
  quo <- quo({
    foo <- "foo"
    ~foo
  })
  f <- eval_tidy(quo)
  expect_true(env_has(f, "foo"))
})

test_that(".env pronoun refers to current quosure (#174)", {
  inner_quo <- local({
    var <- "inner"
    quo(.env$var)
  })

  outer_quo <- local({
    var <- "outer"
    quo(identity(!! inner_quo))
  })

  expect_identical(eval_tidy(outer_quo, list()), "inner")
})

test_that("can call tilde with named arguments (#226)", {
  expect_equal(eval_tidy(quote(`~`(foo = x, bar = y))), x ~ y)
  expect_equal(eval_tidy(quote(`~`(foo = x, bar = y, baz = z))), `~`(foo = x, bar = y, baz = z))
})

test_that("Arguments to formulas are not stripped from their attributes (#227)", {
  quo <- quo(x)

  f <- eval_tidy(quo(~!!quo))
  expect_identical(f_rhs(f), quo)

  f <- eval_tidy(quo(!!quo(x) ~ a))
  expect_identical(f_lhs(f), quo)
})

test_that("tilde thunks are unique", {
  new_tilde_thunk <- function(data_mask, data_mask_top) {
    .Call(rlang_new_tilde_thunk, data_mask, data_mask_top)
  }

  thunk1 <- new_tilde_thunk(1, 2)
  thunk2 <- new_tilde_thunk(1, 2)
  expect_false(is_reference(thunk1, thunk2))

  body1 <- body(thunk1)
  body2 <- body(thunk2)
  expect_false(is_reference(body1, body2))
})

test_that("evaluating an empty quosure fails", {
  expect_error(eval_tidy(quo()), "not found")
})

test_that("can supply a data mask as data", {
  mask <- as_data_mask(list(x = 1L))
  eval_tidy(quo(x <- 2L), mask)
  expect_identical(eval_tidy(quo(x), mask), 2L)
})

test_that("as_data_pronoun() creates pronoun", {
  data <- as_data_pronoun(mtcars)
  expect_is(data, "rlang_data_pronoun")
  expect_error(data$foobar, "Column `foobar` not found in `.data`")
  expect_identical(data[["cyl"]], mtcars$cyl)
})

test_that("pronoun has print() and str() method", {
  data <- as_data_pronoun(mtcars)
  expect_output(print(data), "<pronoun>\n11 objects")
  expect_output(str(data), "32 obs")

  data <- as_data_pronoun(list(a = 1))
  expect_output(print(data), "<pronoun>\n1 object")
})

test_that("data mask can escape", {
  fn <- eval_tidy(quote(function() cyl), mtcars)
  expect_identical(fn(), mtcars$cyl)
})
