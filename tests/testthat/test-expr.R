context("expr")

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

test_that("expr_label() truncates blocks", {
  expect_identical(expr_label(quote({ a + b })), "`{ ... }`")
  expect_identical(expr_label(expr(function() { a; b })), "`function() ...`")
})

test_that("expr_label() truncates long calls", {
  long_call <- quote(foo())
  long_arg <- quote(longlonglonglonglonglonglonglonglonglonglonglong)
  long_call[c(2, 3, 4)] <- list(long_arg, long_arg, long_arg)
  expect_identical(expr_label(long_call), "`foo(...)`")
})


# expr_name() --------------------------------------------------------

test_that("expr_name() with symbols, calls, and literals", {
  expect_identical(expr_name(quote(foo)), "foo")
  expect_identical(expr_name(quote(foo(bar))), "foo(bar)")
  expect_identical(expr_name(1L), "1")
  expect_identical(expr_name("foo"), "foo")
  expect_identical(expr_name(function() NULL), "function () ...")
  expect_identical(expr_name(expr(function() { a; b })), "function() ...")
  expect_identical(expr_name(NULL), "NULL")
  expect_error(expr_name(1:2), "must quote")
  expect_error(expr_name(env()), "must quote")
})

# --------------------------------------------------------------------

test_that("get_expr() supports closures", {
  skip("Disabled because causes dplyr to fail")
  expect_identical(get_expr(identity), quote(x))
})

test_that("set_expr() supports closures", {
  fn <- function(x) x
  expect_equal(set_expr(fn, quote(y)), function(x) y)
})

test_that("expressions are deparsed and printed", {
  expect_output(expr_print(1:2), "<int: 1L, 2L>")
  expect_identical(expr_deparse(1:2), "<int: 1L, 2L>")
})

test_that("imaginary numbers with real part are not syntactic", {
  expect_true(is_syntactic_literal(0i))
  expect_true(is_syntactic_literal(na_cpl))
  expect_false(is_syntactic_literal(1 + 1i))
})
