context("vec-coerce")

test_that("no method dispatch", {
  as.logical.foo <- function(x) "wrong"
  expect_identical(as_integer(structure(TRUE, class = "foo")), 1L)

  as.list.foo <- function(x) "wrong"
  expect_identical(as_list(structure(1:10, class = "foo")), as.list(1:10))
})

test_that("input is left intact", {
  x <- structure(TRUE, class = "foo")
  y <- as_integer(x)
  expect_identical(x, structure(TRUE, class = "foo"))
})

test_that("as_list() zaps attributes", {
  expect_identical(as_list(structure(list(), class = "foo")), list())
})

test_that("as_list() only coerces vector or dictionary types", {
  expect_identical(as_list(1:3), list(1L, 2L, 3L))
  expect_error(as_list(quote(symbol)), "a symbol to a list")
})

test_that("as_list() bypasses environment method and leaves input intact", {
  as.list.foo <- function(x) "wrong"
  x <- structure(child_env(NULL), class = "foo")
  y <- as_list(x)

  expect_is(x, "foo")
  expect_identical(y, set_names(list(), character(0)))
})

test_that("as_integer() and as_logical() require integerish input", {
  expect_error(as_integer(1.5), "a fractional double vector to an integer vector")
  expect_error(as_logical(1.5), "a fractional double vector to a logical vector")
})

test_that("names are preserved", {
  nms <- as.character(1:3)
  x <- set_names(1:3, nms)
  expect_identical(names(as_double(x)), nms)
  expect_identical(names(as_list(x)), nms)
})

test_that("can convert strings (#138)", {
  expect_identical(as_character("a"), "a")
  expect_identical(as_list("a"), list("a"))
})
