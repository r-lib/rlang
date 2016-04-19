context("call")

# Creation ----------------------------------------------------------------

test_that("character vector must be length 1", {
  expect_error(call_new(letters), "must be length 1")
})

test_that("args can be specified individually or as list", {
  out <- call_new("f", a = 1, .args = list(b = 2))
  expect_equal(out, quote(f(a = 1, b = 2)))
})

# Standardisation ---------------------------------------------------------

test_that("can standardise base function", {
  out <- call_standardise(quote(matrix(nro = 3, 1:9)))
  expect_equal(out, quote(matrix(data = 1:9, nrow = 3)))
})

test_that("can standardise local function", {
  foo <- function(bar, baz) {}
  out <- call_standardise(quote(foo(baz = 1, 4)))
  expect_equal(out, quote(foo(bar = 4, baz = 1)))
})

# Modification ------------------------------------------------------------

test_that("all args must be named", {
  call <- quote(matrix(1:10))
  expect_error(call_modify(call, list(1)), "must be named")
})

test_that("new args inserted at end", {
  call <- quote(matrix(1:10))
  out <- call_modify(call, list(nrow = 3))
  expect_equal(out, quote(matrix(data = 1:10, nrow = 3)))
})

test_that("new args replace old", {
  call <- quote(matrix(1:10))
  out <- call_modify(call, list(data = 3))
  expect_equal(out, quote(matrix(data = 3)))
})
