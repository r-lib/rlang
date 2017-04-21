context("quosure")

test_that("quosures are spliced", {
  q <- quo(foo(!! quo(bar), !! quo(baz(!! quo(baz), 3))))
  expect_identical(quo_text(q), "foo(bar, baz(baz, 3))")

  q <- expr_interp(~foo::bar(!! function(x) ...))
  expect_identical(quo_text(q), "foo::bar(function (x) \n...)")

  q <- quo(!! quo(!! quo(foo(!! quo(!! quo(bar(!! quo(!! quo(!! quo(baz))))))))))
  expect_identical(quo_text(q), "foo(bar(baz))")
})

test_that("formulas are not spliced", {
  expect_identical(quo_text(quo(~foo(~bar))), "~foo(~bar)")
})

test_that("splicing does not affect original quosure", {
  f <- ~foo(~bar)
  quo_text(f)
  expect_identical(f, ~foo(~bar))
})

test_that("as_quosure() doesn't convert functions", {
  expect_identical(as_quosure(base::mean), set_env(quo(!! base::mean), empty_env()))
})

test_that("as_quosure() coerces formulas", {
  expect_identical(as_quosure(~foo), quo(foo))
})

test_that("quo_expr() warns", {
  expect_warning(regex = NA, quo_expr(quo(foo), warn = TRUE))
  expect_warning(quo_expr(quo(list(!! quo(foo))), warn = TRUE), "inner quosure")
})
