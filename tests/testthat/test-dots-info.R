# Basic operations --------------------------------------------------------

test_that("env_dots_exist() detects dots presence", {
  fn <- function(...) env_dots_exist()
  fn_no_dots <- function() env_dots_exist()

  expect_true(fn())
  expect_true(fn(a = 1))
  expect_false(fn_no_dots())
})

test_that("env_dots_length() returns correct count", {
  fn <- function(...) env_dots_length()

  expect_equal(fn(), 0L)
  expect_equal(fn(1), 1L)
  expect_equal(fn(a = 1, b = 2), 2L)
  expect_equal(fn(1, 2, 3, 4, 5), 5L)
  expect_equal(fn(1, , 3), 3L)
})

test_that("env_dots_length() errors without dots", {
  fn <- function() env_dots_length()
  expect_error(fn(), "incorrect context")
})

test_that("env_dots_names() returns names", {
  fn <- function(...) env_dots_names()

  expect_null(fn())
  expect_null(fn(1, 2))
  expect_equal(fn(a = 1, b = 2), c("a", "b"))
  expect_equal(fn(a = 1, 2, c = 3), c("a", "", "c"))
})

test_that("env_dots_names() errors without dots", {
  fn <- function() env_dots_names()
  expect_error(fn(), "incorrect context")
})

test_that("env_dot_get() evaluates and returns dot value", {
  fn <- function(...) env_dot_get(environment(), 1)

  expect_equal(fn(1 + 1), 2)
  expect_equal(fn(10), 10)

  x <- 100
  expect_equal(fn(x), 100)
})

test_that("env_dot_get() can access different positions", {
  fn <- function(...) {
    env <- environment()
    list(env_dot_get(env, 1), env_dot_get(env, 2), env_dot_get(env, 3))
  }
  expect_equal(fn("a", "b", "c"), list("a", "b", "c"))
})

test_that("env_dot_get() returns missing arg for missing dot", {
  fn <- function(...) env_dot_get(environment(), 1)
  expect_true(is_missing(fn(, 2)))
})

test_that("env_dot_get() respects index bounds", {
  fn <- function(...) env_dot_get(environment(), 2)
  fn_no_dots <- function() env_dot_get(environment(), 1)
  fn_empty <- function(...) env_dot_get(environment(), 1)

  expect_error(fn(1), "fewer than")
  expect_error(fn_no_dots(), "incorrect context")
  expect_error(fn_empty(), "fewer than")
})


# Type classification -----------------------------------------------------

test_that("env_dot_type() identifies delayed promises", {
  fn <- function(...) {
    env <- environment()
    env_dot_type(env, 1)
  }

  expect_equal(fn(x), "delayed")
  expect_equal(fn(1 + 1), "delayed")
  expect_equal(fn(42), "delayed")
})

test_that("env_dot_type() detects missing arguments", {
  fn <- function(...) {
    env <- environment()
    n <- env_dots_length()
    vapply(seq_len(n), function(i) env_dot_type(env, i), character(1))
  }

  expect_equal(fn(a, ), c("delayed", "missing"))
  expect_equal(fn(, b, ), c("missing", "delayed", "missing"))
})

test_that("env_dot_type() detects forced promises", {
  fn <- function(...) {
    env <- environment()
    type_before <- env_dot_type(env, 1)
    env_dot_get(env, 1)
    type_after <- env_dot_type(env, 1)
    c(before = type_before, after = type_after)
  }

  result <- fn(1 + 1)
  expect_equal(result[["before"]], "delayed")
  expect_equal(result[["after"]], "forced")
})

test_that("env_dot_type() detects value dots from compiler", {
  fn <- function(...) {
    env <- environment()
    env_dot_type(env, 1)
  }
  wrapper <- compiler::cmpfun(function() fn("hello"))
  expect_equal(wrapper(), "value")
})

test_that("env_dot_type() classifies mixed types correctly", {
  fn <- function(...) {
    env <- environment()
    env_dot_get(env, 2)
    n <- env_dots_length()
    vapply(seq_len(n), function(i) env_dot_type(env, i), character(1))
  }

  # Dot 1 is delayed, dot 2 is forced (we evaluated it), dot 3 is missing
  expect_equal(fn(a, 1 + 1, ), c("delayed", "forced", "missing"))
})


# Delayed accessors -------------------------------------------------------

test_that("env_dot_delayed_expr() returns promise expression", {
  fn <- function(...) {
    env <- environment()
    env_dot_delayed_expr(env, 1)
  }

  expect_equal(fn(x + y), quote(x + y))
  expect_equal(fn(foo), quote(foo))
  expect_equal(fn(42), 42)
})

test_that("env_dot_delayed_env() returns promise environment", {
  fn <- function(...) {
    env <- environment()
    env_dot_delayed_env(env, 1)
  }

  e <- new.env()
  result <- with(e, fn(x + 1))
  expect_identical(result, e)
})

test_that("env_dot_delayed_expr() errors on forced promise", {
  fn <- function(...) {
    env <- environment()
    env_dot_get(env, 1)
    env_dot_delayed_expr(env, 1)
  }
  expect_error(fn(1 + 1), "not a delayed \\.\\.\\.")
})

test_that("env_dot_delayed_env() errors on forced promise", {
  fn <- function(...) {
    env <- environment()
    env_dot_get(env, 1)
    env_dot_delayed_env(env, 1)
  }
  expect_error(fn(1 + 1), "not a delayed \\.\\.\\.")
})

test_that("env_dot_delayed_expr() errors on missing argument", {
  fn <- function(...) {
    env <- environment()
    env_dot_delayed_expr(env, 2)
  }
  expect_error(fn(a, ), "not a delayed \\.\\.\\.")
})

test_that("env_dot_delayed_env() errors on missing argument", {
  fn <- function(...) {
    env <- environment()
    env_dot_delayed_env(env, 2)
  }
  expect_error(fn(a, ), "not a delayed \\.\\.\\.")
})


# Forced accessor ---------------------------------------------------------

test_that("env_dot_forced_expr() returns expression from forced promise", {
  fn <- function(...) {
    env <- environment()
    env_dot_get(env, 1)
    env_dot_forced_expr(env, 1)
  }

  # The exact value of env_dot_forced_expr depends on R internals (JIT state),
  # so just verify it succeeds without error
  expect_no_error(fn(1 + 1))
  expect_no_error(fn(42))
})

test_that("env_dot_forced_expr() errors on delayed promise", {
  fn <- function(...) {
    env <- environment()
    env_dot_forced_expr(env, 1)
  }
  expect_error(fn(1 + 1), "not a forced \\.\\.\\.")
})

test_that("env_dot_forced_expr() errors on missing argument", {
  fn <- function(...) {
    env <- environment()
    env_dot_get(env, 1)
    env_dot_forced_expr(env, 2)
  }
  expect_error(fn(1, ), "not a forced \\.\\.\\.")
})


# Forwarding with `...` (shared promise) ----------------------------------

test_that("`...` forwarding shares the promise object", {
  inner <- function(...) {
    env <- environment()
    list(
      expr = env_dot_delayed_expr(env, 1),
      env = env_dot_delayed_env(env, 1)
    )
  }
  outer <- function(...) inner(...)

  caller_env <- current_env()
  result <- outer(x + y)

  # Shared promise preserves the original expression and environment
  expect_equal(result$expr, quote(x + y))
  expect_identical(result$env, caller_env)
})

test_that("`...` forwarding preserves types", {
  inner <- function(...) {
    env <- environment()
    env_dot_type(env, 1)
  }
  outer <- function(...) inner(...)

  expect_equal(outer(x), "delayed")
})

test_that("`...` forwarding reflects forced state", {
  inner <- function(...) {
    env <- environment()
    env_dot_type(env, 1)
  }
  outer <- function(...) {
    force(..1)
    inner(...)
  }

  expect_equal(outer(1 + 1), "forced")
})

test_that("env_dot_get() works through `...` forwarding", {
  inner <- function(...) env_dot_get(environment(), 1)
  outer <- function(...) inner(...)

  x <- 42
  expect_equal(outer(x), 42)
  expect_equal(outer(1 + 1), 2)
})

test_that("env_dots_length() reflects forwarded count", {
  inner <- function(...) env_dots_length()
  outer <- function(...) inner(...)

  expect_equal(outer(a, b, c), 3L)
})

test_that("env_dots_names() preserved through `...` forwarding", {
  inner <- function(...) env_dots_names()
  outer <- function(...) inner(...)

  expect_equal(outer(a = 1, b = 2), c("a", "b"))
  expect_equal(outer(1, b = 2, 3), c("", "b", ""))
  expect_null(outer(1, 2, 3))
})

test_that("`...` forwarding across multiple levels", {
  level3 <- function(...) {
    env <- environment()
    list(
      type = env_dot_type(env, 1),
      expr = env_dot_delayed_expr(env, 1),
      env = env_dot_delayed_env(env, 1)
    )
  }
  level2 <- function(...) level3(...)
  level1 <- function(...) level2(...)

  caller_env <- current_env()
  result <- level1(x + y)

  expect_equal(result$type, "delayed")
  expect_equal(result$expr, quote(x + y))
  expect_identical(result$env, caller_env)
})


# Forwarding with `..N` (new promise) ------------------------------------

test_that("`..N` forwarding creates a new promise layer", {
  inner <- function(...) {
    env <- environment()
    list(
      expr = env_dot_delayed_expr(env, 1),
      env = env_dot_delayed_env(env, 1)
    )
  }
  outer <- function(...) inner(..1)

  result <- outer(x + y)

  # New promise wrapping `..1`, so expr is `..1` and env is outer's frame
  expect_equal(result$expr, quote(..1))
  expect_false(identical(result$env, current_env()))
})

test_that("`..N` forwarding can select specific dots", {
  inner <- function(...) {
    env <- environment()
    list(
      n = env_dots_length(),
      expr1 = env_dot_delayed_expr(env, 1),
      expr2 = env_dot_delayed_expr(env, 2)
    )
  }
  outer <- function(...) inner(..2, ..1)

  result <- outer(a, b)
  expect_equal(result$n, 2L)
  expect_equal(result$expr1, quote(..2))
  expect_equal(result$expr2, quote(..1))
})

test_that("env_dot_get() works through `..N` forwarding", {
  inner <- function(...) env_dot_get(environment(), 1)
  outer <- function(...) inner(..1)

  x <- 42
  expect_equal(outer(x), 42)
  expect_equal(outer(1 + 1), 2)
})

test_that("`..N` forwarding with forced outer dot shows delayed for new promise", {
  inner <- function(...) {
    env <- environment()
    env_dot_type(env, 1)
  }
  outer <- function(...) {
    force(..1)
    inner(..1)
  }

  # The new promise wrapping `..1` is itself delayed,
  # even though the underlying `..1` in outer is forced
  expect_equal(outer(1 + 1), "delayed")
})

test_that("`..N` forwarding across multiple levels", {
  level3 <- function(...) {
    env <- environment()
    env_dot_type(env, 1)
  }
  level2 <- function(...) level3(..1)
  level1 <- function(...) level2(..1)

  expect_equal(level1(x), "delayed")
})


# Compiler value dots -----------------------------------------------------

test_that("compiler-unwrapped literals create value dots", {
  fn <- function(...) {
    env <- environment()
    env_dot_type(env, 1)
  }
  wrapper <- compiler::cmpfun(function() fn("hello"))
  expect_equal(wrapper(), "value")

  wrapper2 <- compiler::cmpfun(function() fn(42L))
  expect_equal(wrapper2(), "value")
})

test_that("env_dot_get() works with value dots", {
  fn <- function(...) env_dot_get(environment(), 1)
  wrapper <- compiler::cmpfun(function() fn("hello"))
  expect_equal(wrapper(), "hello")
})

test_that("delayed accessors error on value dots", {
  fn_expr <- function(...) {
    env <- environment()
    env_dot_delayed_expr(env, 1)
  }
  fn_env <- function(...) {
    env <- environment()
    env_dot_delayed_env(env, 1)
  }

  wrapper_expr <- compiler::cmpfun(function() fn_expr("hello"))
  wrapper_env <- compiler::cmpfun(function() fn_env("hello"))

  expect_error(wrapper_expr(), "not a delayed \\.\\.\\.")
  expect_error(wrapper_env(), "not a delayed \\.\\.\\.")
})


# Input validation --------------------------------------------------------

test_that("index validation works", {
  expect_error(env_dot_type(environment(), 0), "larger than or equal to 1")
  expect_error(env_dot_type(environment(), -1), "larger than or equal to 1")
  expect_error(env_dot_type(environment(), "a"), "number")
})

test_that("environment validation works", {
  expect_error(env_dots_exist(NULL), "environment")
  expect_error(env_dots_length(1), "environment")
  expect_error(env_dots_names(list()), "environment")
})


# Frame-only lookup -------------------------------------------------------

test_that("env_dots_exist() does not reach into parent envs", {
  fn <- function(...) local(env_dots_exist())
  fn_no_dots <- function() local(env_dots_exist())

  expect_false(fn())
  expect_false(fn(1))
  expect_false(fn_no_dots())
})

test_that("env_dots_exist() only returns TRUE for DOTSXP values", {
  e <- new.env(parent = emptyenv())

  e$... <- 1
  expect_false(env_dots_exist(e))

  e$... <- list(1, 2)
  expect_false(env_dots_exist(e))

  e$... <- NULL
  expect_false(env_dots_exist(e))
})

test_that("env_dots_length() does not reach into parent envs", {
  fn <- function(...) local(env_dots_length())
  expect_error(fn(1, 2, 3), "incorrect context")
})

test_that("env_dots_names() does not reach into parent envs", {
  fn <- function(...) local(env_dots_names())
  expect_error(fn(a = 1, b = 2), "incorrect context")
})

test_that("env_dot_get() does not reach into parent envs", {
  fn <- function(...) local(env_dot_get(environment(), 1))
  expect_error(fn(42), "incorrect context")
})

test_that("env_dot_type() does not reach into parent envs", {
  fn <- function(...) local(env_dot_type(environment(), 1))
  expect_error(fn(x), "incorrect context")
})

test_that("env_dot_delayed_expr() does not reach into parent envs", {
  fn <- function(...) local(env_dot_delayed_expr(environment(), 1))
  expect_error(fn(x + y), "incorrect context")
})

test_that("env_dot_delayed_env() does not reach into parent envs", {
  fn <- function(...) local(env_dot_delayed_env(environment(), 1))
  expect_error(fn(x + 1), "incorrect context")
})


# Promise chain unwrapping ------------------------------------------------
# `...` expansion via `promiseArgs()` wraps each dot element with
# `mkPROMISE(CAR(h), rho)`, creating a promise chain where PRCODE of
# the outer promise is the original inner PROMSXP.

test_that("env_dot_type() unwraps chains to detect forced state", {
  inner <- function(...) env_dot_type(environment(), 1)
  outer <- function(...) { force(..1); inner(...) }
  expect_equal(outer(1 + 1), "forced")
})

test_that("env_dot_type() unwraps chains to detect delayed state", {
  inner <- function(...) env_dot_type(environment(), 1)
  outer <- function(...) inner(...)
  expect_equal(outer(1 + 1), "delayed")
})

test_that("env_dot_delayed_expr() unwraps chains to the outermost expression", {
  inner <- function(...) env_dot_delayed_expr(environment(), 1)
  outer <- function(...) inner(...)

  caller_env <- current_env()
  result <- outer(x + y)

  expect_equal(result, quote(x + y))
})

test_that("env_dot_delayed_env() unwraps chains to the outermost environment", {
  inner <- function(...) env_dot_delayed_env(environment(), 1)
  outer <- function(...) inner(...)

  caller_env <- current_env()
  result <- outer(x + y)

  expect_identical(result, caller_env)
})

test_that("env_dot_forced_expr() unwraps chains to detect forced promise", {
  inner <- function(...) {
    env <- environment()
    env_dot_get(env, 1)
    env_dot_forced_expr(env, 1)
  }
  outer <- function(...) inner(...)
  expect_no_error(outer(1 + 1))
})

test_that("env_dot_delayed_expr() errors on forced chain", {
  inner <- function(...) env_dot_delayed_expr(environment(), 1)
  outer <- function(...) { force(..1); inner(...) }
  expect_error(outer(1 + 1), "not a delayed \\.\\.\\.")
})

test_that("env_dot_delayed_env() errors on forced chain", {
  inner <- function(...) env_dot_delayed_env(environment(), 1)
  outer <- function(...) { force(..1); inner(...) }
  expect_error(outer(1 + 1), "not a delayed \\.\\.\\.")
})

test_that("deeper `...` chains unwrap correctly", {
  inner <- function(...) env_dot_type(environment(), 1)
  mid <- function(...) inner(...)
  outer <- function(...) { force(..1); mid(...) }
  expect_equal(outer(1 + 1), "forced")
})
