context("stack")

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

test_that("detects frame environment", {
  expect_true(identity(is_frame_env(sys.frame(-1))))
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
