context("lang-expr")

# expr_text() --------------------------------------------------------

test_that("always returns single string", {
  out <- expr_text(quote({
    a + b
  }))
  expect_length(out, 1)
})

test_that("can truncate lines", {
  out <- expr_text(quote({
    a + b
  }), nlines = 2)
  expect_equal(out, "{\n...")
})


# expr_label() -------------------------------------------------------

test_that("quotes strings", {
  expect_equal(expr_label("a"), '"a"')
  expect_equal(expr_label("\n"), '"\\n"')
})

test_that("backquotes names", {
  expect_equal(expr_label(quote(x)), "`x`")
})

test_that("converts atomics to strings", {
  expect_equal(expr_label(0.5), "0.5")
})

test_that("truncates long calls", {
  expect_equal(expr_label(quote({ a + b })), "`{\n    ...\n}`")
})


# expr_name() --------------------------------------------------------

test_that("name symbols, calls, and scalars", {
  expect_identical(expr_name(quote(foo)), "foo")
  expect_identical(expr_name(quote(foo(bar))), "foo(bar)")
  expect_identical(expr_name(1L), "1")
  expect_identical(expr_name("foo"), "foo")
  expect_identical(expr_name(function() NULL), "function () ...")
  expect_error(expr_name(1:2), "must quote a symbol, scalar, or call")
})
