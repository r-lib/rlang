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

test_that("search_envs() includes the global and base env", {
  envs <- search_envs()
  expect_identical(envs[[1]], global_env())
  expect_identical(envs[[length(envs)]], base_env())
})

test_that("search_envs() returns named environments", {
  expect_identical(names(search_envs()), c("global", search()[-1]))
})

test_that("search_envs() returns an rlang_envs object", {
  expect_is(search_envs(), "rlang_envs")
})

test_that("is_namespace() recognises namespaces", {
  expect_false(is_namespace(env()))
  expect_true(is_namespace(get_env(is_namespace)))
})

test_that("env_name() returns proper environment name", {
  expect_identical(env_name(global_env()), "global")
  expect_identical(env_name(empty_env()), "empty")
  expect_identical(env_name(base_env()), "package:base")

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

test_that("is_attached() detects environments on the search path", {
  expect_false(is_attached("utils"))
  expect_true(is_attached("package:utils"))

  expect_true(is_attached(base_env()))
  expect_true(is_attached(global_env()))
  expect_false(is_attached(ns_env("base")))
})
