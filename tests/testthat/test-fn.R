context("function")

test_that("new_function equivalent to regular function", {
  f1 <- function(x = a + b, y) {
    x + y
  }
  attr(f1, "srcref") <- NULL

  f2 <- new_function(alist(x = a + b, y =), quote({x + y}))

  expect_equal(f1, f2)
})

test_that("prim_name() extracts names", {
  expect_equal(prim_name(c), "c")
  expect_equal(prim_name(prim_eval), "eval")
})

test_that("as_closure() returns closure", {
  expect_identical(typeof(as_closure(list)), "closure")
  expect_identical(typeof(as_closure("list")), "closure")
})

test_that("as_closure() handles primitive functions", {
  expect_identical(as_closure(`-`)(e2 = 10, e1 = 5), -5)
  expect_identical(as_closure(`c`)(1, 3, 5), c(1, 3, 5))
  expect_identical(as_closure(is.null)(1), FALSE)
  expect_identical(as_closure(is.null)(NULL), TRUE)
})

test_that("lambda shortcut handles positional arguments", {
  expect_identical(as_function(~ ..1 + ..3)(1, 2, 3), 4)
})

test_that("as_function() handles strings", {
  expect_identical(as_function("mean"), mean)

  env <- env(fn = function() NULL)
  expect_identical(as_function("fn", env), env$fn)
})

test_that("fn_fmls_syms() unnames `...`", {
  expect_identical(fn_fmls_syms(lapply), list(X = quote(X), FUN = quote(FUN), quote(...)))
})
