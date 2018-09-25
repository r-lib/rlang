context("env-special")

test_that("ns_env() returns current namespace", {
  expect_identical(with_env(ns_env("rlang"), ns_env()), get_env(rlang::get_env))
})

test_that("ns_imports_env() returns imports env", {
  expect_identical(with_env(ns_env("rlang"), ns_imports_env()), env_parent(get_env(rlang::get_env)))
})

test_that("ns_env_name() returns namespace name", {
  expect_identical(with_env(ns_env("base"), ns_env_name()), "base")
  expect_identical(ns_env_name(rlang::get_env), "rlang")
})

test_that("scoped_envs() includes global and empty envs", {
  envs <- scoped_envs()
  expect_identical(envs[[1]], global_env())
  expect_identical(envs[[length(envs)]], empty_env())
})

test_that("scoped_envs() returns named environments", {
  expect_identical(names(scoped_envs()), scoped_names())
})

test_that("scoped_env() deals with empty environment", {
  expect_identical(scoped_env("NULL"), empty_env())
})

test_that("is_namespace() recognises namespaces", {
  expect_false(is_namespace(env()))
  expect_true(is_namespace(get_env(is_namespace)))
})

test_that("env_name() returns proper environment name", {
  expect_identical(env_name(global_env()), "global")
  expect_identical(env_name(empty_env()), "empty")
  expect_identical(env_name(base_env()), "base")

  expect_identical(env_name(pkg_env("rlang")), "package:rlang")
  expect_identical(env_name(ns_imports_env("rlang")), "imports:rlang")
  expect_identical(env_name(ns_env("rlang")), "namespace:rlang")

  env <- structure(env(), name = "foobar")
  expect_identical(env_label(env), "foobar")
})

test_that("env_label() returns memory address for anonymous envs", {
  env <- env()
  expect_identical(env_label(env), sexp_address(env))
})

