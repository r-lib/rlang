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
  expect_identical(fn(dots), list())
  expect_identical(fn(dots_lsp), NULL)

  g <- function(f, ...) fn(f, ...)
  expect_identical(g(dots, a = 1, foo = bar), list(a = 1, foo = quote(bar)))
  expect_identical(g(dots_lsp, a = 1, foo = bar), pairlist(a = 1, foo = quote(bar)))
})

test_that("dots_inspect() inspects dots", {
  fn <- function(...) dots_inspect(...)
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
    dots <- dots(...)
    stack <- call_stack(2)
    dots_inspect_(dots, stack)
  }
  out <- fn(, )
  expect_equal(out[[1]]$expr, arg_missing())
  expect_equal(out[[2]]$expr, arg_missing())
})

test_that("empty dots return list()", {
  fn <- function(...) dots_inspect(...)
  expect_equal(fn(), list())
})

test_that("explicit dots make a list of formulas", {
  fs <- dots_capture(x = 1 + 2, y = 2 + 3)
  f1 <- ~ 1 + 2
  f2 <- ~ 2 + 3

  expect_identical(fs$x, f1)
  expect_identical(fs$y, f2)
})

test_that("dots_capture() produces correct formulas", {
  fn <- function(x = a + b, ...) {
    list(dots = dots_capture(x = x, y = a + b, ...), env = environment())
  }
  out <- fn(z = a + b)

  expect_identical(out$dots$x, with_env(out$env, ~x))
  expect_identical(out$dots$y, with_env(out$env, ~a + b))
  expect_identical(out$dots$z, ~a + b)
})
