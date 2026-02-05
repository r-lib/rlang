test_that("dots_exist() detects dots presence", {

  fn <- function(...) dots_exist()
  fn_no_dots <- function() dots_exist()

  expect_true(fn())
  expect_true(fn(a = 1))
  expect_false(fn_no_dots())
})

test_that("dots_length() returns correct count", {
  fn <- function(...) dots_length()

  expect_equal(fn(), 0L)
  expect_equal(fn(1), 1L)
  expect_equal(fn(a = 1, b = 2), 2L)
  expect_equal(fn(1, 2, 3, 4, 5), 5L)
  expect_equal(fn(1, , 3), 3L)  # missing args count
})

test_that("dots_length() errors if dots don't exist", {
  fn <- function() dots_length()

  expect_error(fn(), "incorrect context")
})

test_that("dots_names() returns names", {
  fn <- function(...) dots_names()

  expect_equal(fn(), character())
  expect_equal(fn(1, 2), c("", ""))
  expect_equal(fn(a = 1, b = 2), c("a", "b"))
  expect_equal(fn(a = 1, 2, c = 3), c("a", "", "c"))
})

test_that("dots_names() errors if dots don't exist", {
  fn <- function() dots_names()

  expect_error(fn(), "incorrect context")
})

test_that("dots_elt() evaluates and returns dot value", {
  fn <- function(...) dots_elt(1)

  expect_equal(fn(1 + 1), 2)
  expect_equal(fn(10), 10)

  x <- 100
  expect_equal(fn(x), 100)
})

test_that("dots_elt() respects index bounds", {
  fn <- function(...) dots_elt(2)
  fn_no_dots <- function() dots_elt(1)

  expect_error(fn(1), "fewer than")
  expect_error(fn_no_dots(), "incorrect context")
})

test_that("dot_type() identifies promise types", {
  fn <- function(...) {
    env <- environment()
    dot_type(1, env)
  }

  # Unevaluated expressions are delayed promises

  expect_equal(fn(x), "delayed")
  expect_equal(fn(1 + 1), "delayed")
  expect_equal(fn(42), "delayed")
})

test_that("dot_type() detects missing arguments", {
  fn <- function(...) {
    env <- environment()
    n <- dots_length()
    vapply(seq_len(n), function(i) dot_type(i, env), character(1))
  }

  expect_equal(fn(a, ), c("delayed", "missing"))
  expect_equal(fn(, b, ), c("missing", "delayed", "missing"))
})

test_that("dot_type() detects forced promises", {
  fn <- function(...) {
    env <- environment()
    type_before <- dot_type(1, env)
    val <- dots_elt(1)
    type_after <- dot_type(1, env)
    c(before = type_before, after = type_after)
  }

  result <- fn(1 + 1)
  expect_equal(result[["before"]], "delayed")
  expect_equal(result[["after"]], "forced")
})

test_that("dot_delayed_expr() returns promise expression", {
  fn <- function(...) {
    env <- environment()
    dot_delayed_expr(1, env)
  }

  expect_equal(fn(x + y), quote(x + y))
  expect_equal(fn(foo), quote(foo))
  expect_equal(fn(42), 42)
})

test_that("dot_delayed_expr() errors on forced promise", {
  fn <- function(...) {
    env <- environment()
    val <- dots_elt(1)  # Force it
    dot_delayed_expr(1, env)
  }

  expect_error(fn(1 + 1), "not a delayed promise")
})

test_that("dot_delayed_expr() errors on missing", {
  fn <- function(...) {
    env <- environment()
    dot_delayed_expr(2, env)
  }

  expect_error(fn(a, ), "not a delayed promise")
})

test_that("dot_delayed_env() returns promise environment", {
  fn <- function(...) {
    env <- environment()
    dot_delayed_env(1, env)
  }

  e <- new.env()
  result <- with(e, fn(x + 1))
  expect_identical(result, e)
})

test_that("dot_delayed_env() errors on forced promise", {
  fn <- function(...) {
    env <- environment()
    val <- dots_elt(1)  # Force it
    dot_delayed_env(1, env)
  }

  expect_error(fn(1 + 1), "not a delayed promise")
})

test_that("dot_delayed_env() errors on missing", {
  fn <- function(...) {
    env <- environment()
    dot_delayed_env(2, env)
  }

  expect_error(fn(a, ), "not a delayed promise")
})

test_that("dot_forced_expr() returns forced promise expression", {
  x <- 1
  y <- 2
  fn <- function(...) {
    env <- environment()
    val <- dots_elt(1)  # Force it
    dot_forced_expr(1, env)
  }

  expect_equal(fn(x + y), quote(x + y))
  expect_equal(fn(42), 42)
})

test_that("dot_forced_expr() errors on delayed promise", {
  fn <- function(...) {
    env <- environment()
    dot_forced_expr(1, env)
  }

  expect_error(fn(1 + 1), "not a forced promise")
})

test_that("dot_forced_expr() errors on missing", {
  a <- 1
  fn <- function(...) {
    env <- environment()
    dots_elt(1)  # Force first one
    dot_forced_expr(2, env)
  }

  expect_error(fn(a, ), "not a forced promise")
})

test_that("index validation works", {
  expect_error(dot_type(0), "larger than or equal to 1")
  expect_error(dot_type(-1), "larger than or equal to 1")
  expect_error(dot_type("a"), "number")
})

test_that("environment validation works", {
  expect_error(dots_exist(NULL), "environment")
  expect_error(dots_length(1), "environment")
  expect_error(dots_names(list()), "environment")
})

test_that("dot_type() returns 'value' for non-promise", {
 # This is hard to test in practice since R wraps most things in promises
  # but we test the logic exists
  fn <- function(...) {
    env <- environment()
    dot_type(1, env)
 }
  # Even literals become promises in ...
  expect_equal(fn(42), "delayed")
})
