test_that("ensym() fails with calls", {
  capture_sym <- function(arg) ensym(arg)
  expect_identical(capture_sym(foo), quote(foo))
  expect_snapshot({
    err(capture_sym(foo(bar)))
  })
})

test_that("ensym() supports strings and symbols", {
  capture_sym <- function(arg) ensym(arg)
  expect_identical(capture_sym("foo"), quote(foo))
  expect_identical(capture_sym(!!"foo"), quote(foo))
  expect_identical(capture_sym(!!sym("foo")), quote(foo))
})

test_that("empty string is treated as the missing argument", {
  expect_identical(sym(""), missing_arg())
})

test_that("syms() supports symbols as well", {
  expect_identical(syms(list(quote(a), "b")), list(quote(a), quote(b)))
})

test_that("is_symbol() matches `name`", {
  expect_true(is_symbol(sym("foo")))
  expect_true(is_symbol(sym("foo"), "foo"))
  expect_false(is_symbol(sym("foo"), "bar"))
})

test_that("is_symbol() matches any name in a vector", {
  expect_false(is_symbol(quote(C), letters))
  expect_true(is_symbol(quote(c), letters))
})

test_that("must supply strings to sym()", {
  expect_snapshot({
    err(sym(letters))
    err(sym(1:2))
  })
})

test_that("data_sym() and data_syms() work", {
  expect_equal(
    data_sym("foo"),
    quote(.data$foo)
  )
  expect_equal(
    data_syms(c("foo", "bar")),
    alist(
      .data$foo,
      .data$bar
    )
  )
})
