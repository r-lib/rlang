context("arg")

# arg_expr -----------------------------------------------------------------

test_that("doesn't go pass lazy loaded objects", {
  expect_identical(arg_expr(mtcars), quote(mtcars))
})

test_that("follows multiple levels", {
  fn <- function(x) g(x)
  g <- function(y) h(y)
  h <- function(z) arg_expr(z)

  expect_identical(fn(x + y), quote(x + y))
})

test_that("follows through dots", {
  fn <- function(...) g(...)
  g <- function(...) h(...)
  h <- function(x1, x2) arg_info(x2)
  info <- fn(mtcars, letters)
  expect_identical(info$expr, quote(letters))
  expect_identical(info$eval_frame, call_frame())
})

test_that("empty argument are reported", {
  fn <- function(x, y) list(info = arg_info(x), env = environment())
  out <- fn(, )
  info <- out$info
  expect_true(is_missing(info$expr))
  expect_identical(info$eval_frame$env, out$env)
  expect_identical(info$caller_frame$env, environment())

  g <- function(x) list(info = fn(x), env = environment())
  out <- g()
  info <- out$info$info
  expect_true(is_missing(info$expr))
  expect_identical(info$eval_frame$env, out$env)
  expect_identical(info$caller_frame$env, environment())
})


# arg_env -----------------------------------------------------------------

test_that("expression is scoped in calling env", {
  fn <- function(x) arg_env(x)
  g <- function(x) fn(x)

  expect_identical(g(mtcars), environment())
  expect_identical(g(list(mtcars)), environment())
})

test_that("default arguments are scoped in execution env", {
  fn <- function(x = default()) list(info = g(x), env = environment())
  g <- function(x) arg_info(x)
  out <- fn()
  info <- out$info
  fn_env <- out$env

  expect_identical(info$eval_frame$env, fn_env)
  expect_identical(info$caller_frame$env, environment())
  expect_equal(info$expr, quote(default()))
})

test_that("missing arguments are scoped in execution env", {
  fn <- function(x) list(info = g(x), env = environment())
  g <- function(x) arg_info(x)
  out <- fn()
  info <- out$info
  fn_env <- out$env

  expect_identical(info$eval_frame$env, fn_env)
  expect_identical(info$caller_frame$env, environment())
  expect_true(is_missing(info$expr))
})

test_that("arguments are scoped in calling env", {
  fn <- function() list(info = g(foo), env = environment())
  g <- function(x) h(x)
  h <- function(x) arg_info(x)
  out <- fn()
  info <- out$info
  fn_env <- out$env

  expect_identical(info$eval_frame$env, fn_env)
  expect_identical(info$caller_frame$env, fn_env)
  expect_equal(info$expr, quote(foo))
})

test_that("dots_capture() produces correct formulas", {
  fn <- function(x = a + b, ...) {
    list(dots = dots_capture(x = x, y = a + b, ...), env = environment())
  }
  out <- fn(z = a + b)

  expect_identical(out$dots$x, f_new(quote(a + b), env = out$env))
  expect_identical(out$dots$y, f_new(quote(a + b), env = out$env))
  expect_identical(out$dots$z, f_new(quote(a + b), env = environment()))
})

# arg_text ----------------------------------------------------------------

test_that("always returns single string", {
  out <- arg_text({
    a + b
  })
  expect_length(out, 1)
})

test_that("can truncate lines", {
  out <- arg_text({
    a + b
  }, nlines = 2)
  expect_equal(out, "{\n...")
})


# arg_label ---------------------------------------------------------------

test_that("quotes strings", {
  expect_equal(arg_label("a"), '"a"')
  expect_equal(arg_label("\n"), '"\\n"')
})

test_that("backquotes names", {
  expect_equal(arg_label(x), "`x`")
})

test_that("converts atomics to strings", {
  expect_equal(arg_label(0.5), "0.5")
})

test_that("truncates long calls", {
  expect_equal(arg_label({ a + b }), "`{\n    ...\n}`")
})


# arg_missing --------------------------------------------------------

test_that("is_missing() works with symbols", {
  x <- arg_missing()
  expect_true(is_missing(x))
})


test_that("is_missing() works with non-symbols", {
  expect_true(is_missing(arg_missing()))

  l <- list(arg_missing())
  expect_true(is_missing(l[[1]]))
  expect_error(missing(l[[1]]), "invalid use")
})
