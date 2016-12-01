context("environments")

test_that("env_new() has correct parent", {
  env <- env_new(emptyenv())
  expect_false(env_sees(env, "list"))

  fn <- function() list(new = env_new(), env = environment())
  out <- fn()
  expect_identical(parent.env(out$new), out$env)
})

test_that("env_next() reports correct parent", {
  env <- env_new(
    env_new(env_empty(), list(obj = "b")),
    list(obj = "a")
  )

  expect_identical(env_next(env, 1)$obj, "b")
  expect_identical(env_next(env, 2), env_empty())
  expect_error(env_next(env, 3), "not enough environments")
})

test_that("env_tail() climbs env chain", {
  expect_identical(env_tail(env_global()), env_base())
})

test_that("promises are created", {
  env <- env_new()

  env_assign_lazily(env, "foo", bar <- "bar")
  expect_false(env_has(env(), "bar"))

  force(env$foo)
  expect_true(env_has(env(), "bar"))

  f <- ~stop("forced")
  env_assign_lazily_(env, "stop", f)
  expect_error(env$stop, "forced")
})
