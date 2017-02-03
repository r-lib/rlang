context("tidy capture")

test_that("explicit dots make a list of formulas", {
  fs <- tidy_dots(x = 1 + 2, y = 2 + 3)
  f1 <- ~ 1 + 2
  f2 <- ~ 2 + 3

  expect_identical(fs$x, f1)
  expect_identical(fs$y, f2)
})

test_that("tidy_dots() produces correct formulas", {
  fn <- function(x = a + b, ...) {
    list(dots = tidy_dots(x = x, y = a + b, ...), env = environment())
  }
  out <- fn(z = a + b)

  expect_identical(out$dots$x, with_env(out$env, ~x))
  expect_identical(out$dots$y, with_env(out$env, ~a + b))
  expect_identical(out$dots$z, ~a + b)
})

test_that("dots are interpolated", {
  fn <- function(...) {
    baz <- "baz"
    fn_var <- ~baz
    g(..., toupper(!! fn_var))
  }
  g <- function(...) {
    foo <- "foo"
    g_var <- ~foo
    h(toupper(!! g_var), ...)
  }
  h <- function(...) {
    tidy_dots(...)
  }

  bar <- "bar"
  var <- ~bar
  dots <- fn(toupper(!!var))

  expect_identical(lapply(dots, deparse), list("~toupper(~foo)", "~toupper(~bar)", "~toupper(~baz)"))
  expect_identical(lapply(dots, tidy_eval), list("FOO", "BAR", "BAZ"))
})

test_that("dots capture is stack-consistent", {
  fn <- function(...) {
    g(tidy_dots(...))
  }
  g <- function(dots) {
    h(dots, foo(bar))
  }
  h <- function(dots, ...) {
    dots
  }
  expect_identical(fn(foo(baz)), list(~foo(baz)))
})

test_that("splice is consistently recognised", {
  expect_true(is_splice(quote(!!! list())))
  expect_true(is_splice(quote(UQS(list()))))
  expect_true(is_splice(quote(rlang::UQS(list()))))
  expect_false(is_splice(quote(ns::UQS(list()))))
})

test_that("dots can be spliced in", {
  fn <- function(...) {
    var <- "var"
    list(
      out = g(!!! tidy_dots(...), bar(baz), !!! list(a = var, b = ~foo)),
      env = env()
    )
  }
  g <- function(...) {
    tidy_dots(...)
  }

  out <- fn(foo(bar))
  expected <- list(
    ~foo(bar),
    with_env(out$env, ~bar(baz)),
    a = "var",
    b = with_env(out$env, ~foo)
  )
  expect_identical(out$out, expected)
})
