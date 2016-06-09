context("expr")

# expr_find ---------------------------------------------------------------

test_that("doesn't go pass lazy loaded objects", {
  expect_identical(expr_find(mtcars), quote(mtcars))
})

test_that("follows multiple promises", {
  f <- function(x) g(x)
  g <- function(y) h(y)
  h <- function(z) expr_find(z)

  expect_identical(f(x + y), quote(x + y))
})


# expr_env ----------------------------------------------------------------

test_that("follows multiple promises", {
  f <- function(x) g(x)
  g <- function(y) h(y)
  h <- function(z) expr_env(z)

  expect_identical(h(x + y), environment())
})

test_that("throws error if promise forced", {
  f <- function(x) {
    force(x)
    expr_env(x)
  }
  expect_error(f(10), "already been forced")
})


test_that("or can return default env", {
  env <- new.env(parent = emptyenv())
  f <- function(x) {
    force(x)
    expr_env(x, env)
  }
  expect_identical(f(10), env)
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

