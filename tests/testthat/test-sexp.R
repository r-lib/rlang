context("sexp")

test_that("poke_type() changes object type", {
  x <- new_node(quote(foo), NULL)
  out <- withVisible(poke_type(x, "language"))
  expect_false(out$visible)
  expect_identical(out$value, x)
  expect_identical(typeof(x), "language")
})

test_that("can access promise properties", {
  fn <- function(...) {
    list(node_car(get("...")))
  }
  prom <- fn(foo)
  expect_identical(promise_expr(prom[[1]]), quote(foo))
  expect_identical(promise_env(prom[[1]]), current_env())
})

test_that("can pluck promise and its properties from env", {
  fn <- function(x) {
    list(
      promise_expr("x"),
      promise_env("x")
    )
  }
  expect_identical(fn(foo), list(quote(foo), current_env()))
})

test_that("can pluck promise value", {
  fn <- function(x) promise_value("x")
  expect_identical(fn(foo), sym("R_UnboundValue"))

  fn <- function(x) { force(x); promise_value("x") }
  foo <- "foo"
  expect_identical(fn(foo), "foo")
})
