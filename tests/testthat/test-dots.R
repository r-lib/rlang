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
  expect_identical(info$foo$ctxt_frame$env, out$env)
  expect_identical(info[[2]]$ctxt_frame$env, out$env)
})

test_that("unmatched dots return missing_arg()", {
  # Only occurs with partial stack climbing. Necessary for lazyeval
  # compatibility
  fn <- function(...) {
    dots <- dots_exprs(...)
    stack <- call_stack(2)
    dots_inspect_(dots, stack)
  }
  out <- fn(, )
  expect_equal(out[[1]]$expr, missing_arg())
  expect_equal(out[[2]]$expr, missing_arg())
})

test_that("empty dots return list()", {
  fn <- function(...) dots_inspect(...)
  expect_equal(fn(), list())
})

test_that("dots_exprs() captures empty arguments", {
  expect_identical(dots_exprs(, ), set_names(list(missing_arg(), missing_arg()), c("", "")))
})
