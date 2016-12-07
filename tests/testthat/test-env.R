context("environments")

test_that("env() returns current frame by default", {
  fn <- function() expect_identical(env(), environment())
  fn()
})

test_that("env_parent() returns enclosure frame by default", {
  enclos_env <- env_new(env_package("rlang"))
  fn <- with_env(enclos_env, function() env_parent())
  expect_identical(fn(), enclos_env)
})

test_that("env_new() has correct parent", {
  env <- env_new(env_empty())
  expect_false(env_has(env, "list", inherit = TRUE))

  fn <- function() list(new = env_new(env()), env = environment())
  out <- fn()
  expect_identical(env_parent(out$new), out$env)
})

test_that("env_parent() reports correct parent", {
  env <- env_new(
    env_new(env_empty(), list(obj = "b")),
    list(obj = "a")
  )

  expect_identical(env_parent(env, 1)$obj, "b")
  expect_identical(env_parent(env, 2), env_empty())
  expect_error(env_parent(env, 3), "Not enough environments")
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

test_that("lazies are evaluated in correct environment", {
  env <- env_new(env_base())

  env_assign_lazily(env, "test_captured", test_captured <- letters)
  env_assign_lazily_(env, "test_expr", quote(test_expr <- LETTERS))
  env_assign_lazily_(env, "test_formula", ~ (test_formula <- mtcars))
  expect_false(any(env_has(env(), c("test_captured", "test_expr", "test_formula"))))

  force(env$test_captured)
  force(env$test_expr)
  force(env$test_formula)
  expect_true(all(env_has(env(), c("test_captured", "test_expr", "test_formula"))))

  expect_equal(test_captured, letters)
  expect_equal(test_expr, LETTERS)
  expect_equal(test_formula, mtcars)
})

test_that("formula env is overridden by eval_env", {
  env <- env_new(env_base())
  env_assign_lazily_(env, "within_env", ~ (new_within_env <- "new"), env)
  force(env$within_env)

  expect_false(env_has(env(), "new_within_env"))
  expect_true(env_has(env, "new_within_env"))
  expect_equal(env$new_within_env, "new")
})

test_that("with_env() evaluates within correct environment", {
  fn <- function() {
    g(env())
    "normal return"
  }
  g <- function(env) {
    with_env(env, return("early return"))
  }
  expect_equal(fn(), "early return")
})

test_that("env_namespace() returns current namespace", {
  expect_identical(with_env(env_namespace("rlang"), env_namespace()), env(rlang::env))
})

test_that("env_imports() returns imports env", {
  expect_identical(with_env(env_namespace("rlang"), env_imports()), env_parent(env(rlang::env)))
})
