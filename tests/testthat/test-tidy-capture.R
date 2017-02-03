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
