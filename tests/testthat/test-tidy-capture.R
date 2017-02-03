context("tidy capture")

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
