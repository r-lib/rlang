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

test_that("dots_info() inspects dots", {
  fn <- function(...) dots_info(...)
  g <-  function(...) fn(...)
  h <- function() list(
    info = g(foo = bar, "foo"),
    env = environment()
  )
  out <- h()
  info <- out$info

  expect_identical(info$foo$expr, quote(bar))
  expect_identical(info[[2]]$expr, "foo")
  expect_identical(info$foo$eval_frame$env, out$env)
  expect_identical(info[[2]]$eval_frame$env, out$env)
})

test_that("unmatched dots return arg_missing()", {
  # Only occurs with partial stack climbing. Necessary for lazyeval
  # compatibility
  fn <- function(...) {
    dots <- arg_dots(...)
    stack <- call_stack(2)
    dots_info_(dots, stack)
  }
  out <- fn(, )
  expect_equal(out[[1]]$expr, arg_missing())
  expect_equal(out[[2]]$expr, arg_missing())
})
