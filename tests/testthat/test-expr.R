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
  f <- function(x = list(mtcars)) list(environment(), expr_env(x))
  g <- function() f()

  envs <- g()
  expect_identical(envs[[1]], envs[[2]])
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
