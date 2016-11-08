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

test_that("no partial matching", {
  out <- call_standardise(quote(matrix(nro = 3, 1:9)))
  expect_equal(out, quote(matrix(nro = 3, data = 1:9)))
})

test_that("args are standardised", {
  f <- function(x, y) NULL
  expect_equal(call_standardise(quote(f(3))), quote(f(x = 3)))
  expect_equal(call_standardise(quote(f(3, 3))), quote(f(x = 3, y = 3)))
  expect_equal(call_standardise(quote(f(y = 3))), quote(f(y = 3)))
})

test_that("arg validation is delegated to R", {
  f <- function(x, y) list(NULL)
  env <- environment()
  expect_error(call_validate_args(quote(f(x = NULL, x = NULL)), env, f), "matched by multiple")
  expect_error(call_validate_args(quote(f(NULL, NULL, NULL)), env, f), "unused argument")
})

test_that("names of dotted arguments are enumerated", {
  g <- function(dots, ...) f(dots = dots, ...)
  f <- function(dots, ...) call_standardise(enum_dots = dots)

  f_default <- f(FALSE, foo, foo = bar, , "foobar")
  f_enum <- f(TRUE, foo, foo = bar, , "foobar")
  expect_equal(f_default, quote(f(dots = FALSE, foo, foo = bar, , "foobar")))
  expect_equal(f_enum, quote(f(dots = TRUE, ..1 = foo, ..2 = bar, ..3 = , ..4 = "foobar")))

  g_default <- g(FALSE, foo, foo = bar, , "foobar")
  g_enum <- g(TRUE, foo, foo = bar, , "foobar")
  expect_equal(g_default, quote(f(dots = dots, ..1, foo = ..2, ..3, ..4)))
  expect_equal(g_enum, quote(f(dots = dots, ..1 = ..1, ..2 = ..2, ..3 = ..3, ..4 = ..4)))
})

test_that("call is not modified in place", {
  f <- function(...) g(...)
  g <- function(...) call_stack()[1:2]
  stack <- f(foo)
  call_standardise(stack[[1]]$expr, stack[[2]]$env, enum_dots = TRUE)
  expect_equal(stack[[1]]$expr, quote(g(...)))
})

test_that("can standardise without specifying `call`", {
  f <- function(...) call_standardise()
  expect_identical(f(arg), quote(f(arg)))
})

test_that("can standardise formulas", {
  f <- matrix
  expect_equal(call_standardise(~f(x, y, z)), quote(f(data = x, nrow = y, ncol = z)))
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

test_that("can modify without supplying `call`", {
  f <- function() call_modify(new_args = list(bool = FALSE))
  expect_identical(f(), quote(f(bool = FALSE)))
})

# Utils --------------------------------------------------------------

test_that("call_fn_name() handles namespaced calls", {
  expect_equal(call_fn_name(quote(foo::bar())), "bar")
  expect_equal(call_fn_name(quote(foo$bar())), "bar")
  expect_equal(call_fn_name(quote(foo::bar@baz())), "baz")
})
