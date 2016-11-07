context("expr")

# expr_find ---------------------------------------------------------------

test_that("doesn't go pass lazy loaded objects", {
  expect_identical(expr_find(mtcars), quote(mtcars))
})

test_that("follows multiple levels", {
  f <- function(x) g(x)
  g <- function(y) h(y)
  h <- function(z) expr_find(z)

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

# expr_env ----------------------------------------------------------------

test_that("expression is scoped in calling env", {
  f <- function(x) expr_env(x)
  g <- function(x) f(x)

  expect_identical(g(mtcars), environment())
  expect_identical(g(list(mtcars)), environment())
})

test_that("missing arguments are scoped in execution env", {
  check_missing <- function(g, missing) {
    f <- function() list(f_out = g(), f_env = environment())
    info <- f()

    f_out <- info$f_out
    out <- f_out$g_out$h_out
    f_env <- info$f_env
    g_env <- f_out$g_env

    # Missing arguments are evaluated in execution env
    expect_identical(out$env, g_env)

    # But the caller env is the environment where the argument would have
    # been evaluated had it not been missing
    expect_identical(out$calling_frame$env, f_env)

    # Missingness of argument is properly returned
    expect_identical(out$missing, missing)

    if (missing) {
      expect_null(out$expr)
    }
  }
  g_default <- function(arg = default()) list(g_out = h(arg), g_env = environment())
  g_no_default <- function(arg) list(g_out = h(arg), g_env = environment())
  h <- function(arg) list(h_out = arg_info(arg), h_env = environment())

  check_missing(g_default, FALSE)
  check_missing(g_no_default, TRUE)
})


# expr_text ---------------------------------------------------------------

test_that("always returns single string", {
  out <- expr_text({
    a + b
  })
  expect_length(out, 1)
})

test_that("can truncate lines", {
  out <- expr_text({
    a + b
  }, nlines = 2)
  expect_equal(out, "{\n...")
})


# expr_label --------------------------------------------------------------

test_that("quotes strings", {
  expect_equal(expr_label("a"), '"a"')
  expect_equal(expr_label("\n"), '"\\n"')
})

test_that("backquotes names", {
  expect_equal(expr_label(x), "`x`")
})

test_that("converts atomics to strings", {
  expect_equal(expr_label(0.5), "0.5")
})

test_that("truncates long calls", {
  expect_equal(expr_label({ a + b }), "`{\n    ...\n}`")
})
