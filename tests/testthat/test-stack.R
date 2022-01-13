test_that("can return from frame", {
  fn <- function() {
    val <- g()
    paste(val, "to fn()")
  }
  g <- function(env) {
    h(environment())
    stop("g!\n")
  }
  h <- function(env) {
    return_from(env, "returned from h()")
    stop("h!\n")
  }

  expect_equal(fn(), "returned from h() to fn()")
})

test_that("can return to frame", {
  fn <- function() {
    val <- identity(g(environment()))
    paste(val, "to fn()")
  }
  g <- function(env) {
    h(env)
    stop("g!\n")
  }
  h <- function(env) {
    return_to(env, "returned from h()")
    stop("h!\n")
  }

  expect_equal(fn(), "returned from h() to fn()")
})

test_that("current_env() and current_fn() return current frame props", {
  fn <- function() {
    list(
      rlang = list(identity(current_env()), current_fn()),
      base = list(environment(), sys.function())
    )
  }
  out <- fn()
  expect_identical(out$rlang[[1]], out$base[[1]])
  expect_identical(out$rlang[[2]], out$base[[2]])
})

test_that("current_fn() and caller_fn() work", {
  f <- function(n) g(n)
  g <- function(n) h(n)
  h <- function(n) caller_fn(n)
  expect_equal(f(1), g)
  expect_equal(f(2), f)

  # Need to break the chain of callers to get `NULL`.
  # Otherwise we get the `eval()` frame from testthat
  expect_null(eval_bare(quote(f(3)), env()))

  f <- function() current_fn()
  expect_equal(f(), f)
})
