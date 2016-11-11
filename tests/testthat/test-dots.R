context("dots")

test_that("dots are retrieved from frame", {
  fn <- function(f) f()
  expect_identical(fn(frame_dots), list())
  expect_identical(fn(frame_dots_lsp), NULL)

  fn <- function(f, ...) f()
  g <- function(f, ...) fn(f, ...)
  expect_identical(g(frame_dots, a = 1, foo = bar), list(a = 1, foo = quote(bar)))
  expect_identical(g(frame_dots_lsp, a = 1, foo = bar), pairlist(a = 1, foo = quote(bar)))
})

test_that("dots are retrieved from arguments", {
  fn <- function(f, ...) f(...)
  expect_identical(fn(arg_dots), list())
  expect_identical(fn(arg_dots_lsp), NULL)

  g <- function(f, ...) fn(f, ...)
  expect_identical(g(arg_dots, a = 1, foo = bar), list(a = 1, foo = quote(bar)))
  expect_identical(g(arg_dots_lsp, a = 1, foo = bar), pairlist(a = 1, foo = quote(bar)))
})
