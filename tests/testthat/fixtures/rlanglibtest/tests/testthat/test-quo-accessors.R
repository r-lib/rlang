context("quo_accessors")

test_that("r_quo_get_expr() gets expression", {
  r_quo_get_expr <- function(quo) {
    .Call(rlanglibtest_r_quo_get_expr, quo)
  }
  r_quo_set_expr <- function(quo, expr) {
    .Call(rlanglibtest_r_quo_set_expr, quo, expr)
  }
  r_quo_get_env <- function(quo) {
    .Call(rlanglibtest_r_quo_get_env, quo)
  }
  r_quo_set_env <- function(quo, env) {
    .Call(rlanglibtest_r_quo_set_env, quo, env)
  }
  quo <- rlang::quo(foo)

  expect_identical(r_quo_get_expr(quo), rlang::quo_get_expr(quo))
  expect_identical(r_quo_get_env(quo), rlang::quo_get_env(quo))
  expect_identical(r_quo_set_expr(quo, NULL), rlang::quo_set_expr(quo, NULL))
  expect_identical(r_quo_set_env(quo, rlang::empty_env()), rlang::quo_set_env(quo, rlang::empty_env()))
})
