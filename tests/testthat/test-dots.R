context("dots")

test_that("dots are retrieved from arguments", {
  fn <- function(f, ...) f(...)
  expect_identical(fn(dots_exprs), list())

  g <- function(f, ...) fn(f, ...)
  expect_identical(g(dots_exprs, a = 1, foo = bar), list(a = 1, foo = quote(bar)))
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
    dots <- dots_exprs(...)
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

test_that("dots_exprs() captures empty arguments", {
  expect_identical(dots_exprs(, ), set_names(list(arg_missing(), arg_missing()), c("", "")))
})
