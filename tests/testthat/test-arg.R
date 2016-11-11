context("arg")

# arg_expr -----------------------------------------------------------------

test_that("doesn't go pass lazy loaded objects", {
  expect_identical(arg_expr(mtcars), quote(mtcars))
})

test_that("follows multiple levels", {
  f <- function(x) g(x)
  g <- function(y) h(y)
  h <- function(z) arg_expr(z)

  expect_identical(f(x + y), quote(x + y))
})

test_that("follows through dots", {
  f <- function(...) g(...)
  g <- function(...) h(...)
  h <- function(x1, x2) arg_info(x2)
  info <- f(mtcars, letters)

  expect_identical(info$expr, quote(letters))
  expect_identical(info$env, environment())
})

# arg_env -----------------------------------------------------------------

test_that("expression is scoped in calling env", {
  f <- function(x) arg_env(x)
  g <- function(x) f(x)

  expect_identical(g(mtcars), environment())
  expect_identical(g(list(mtcars)), environment())
})

test_that("default arguments are scoped in execution env", {
  f <- function(x = default()) list(g(x), environment())
  g <- function(x) arg_info(x)
  out <- f()
  info <- out[[1]]
  expected_env <- out[[2]]

  expect_identical(info$env, expected_env)
  expect_identical(info$caller_frame$env, environment())
  expect_false(info$missing)
})

test_that("missing arguments are scoped in execution env", {
  f <- function(x) list(g(x), environment())
  g <- function(x) arg_info(x)
  out <- f()
  info <- out[[1]]
  expected_env <- out[[2]]

  expect_identical(info$env, expected_env)
  expect_identical(info$caller_frame$env, environment())
  expect_true(info$missing)
})

test_that("arguments are scoped in calling env", {
  f <- function() list(g(foo), environment())
  g <- function(x) h(x)
  h <- function(x) arg_info(x)
  out <- f()
  info <- out[[1]]
  expected_env <- out[[2]]

  expect_identical(info$env, expected_env)
  expect_identical(info$caller_frame$env, expected_env)
  expect_false(info$missing)
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
