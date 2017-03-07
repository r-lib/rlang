context("quosure")

test_that("quosures are spliced", {
  f <- ~foo(~bar, ~baz(~baz, 3))
  expect_identical(quo_text(f), "foo(bar, baz(baz, 3))")

  f <- tidy_interp(~foo::bar(!! function(x) ...))
  expect_identical(quo_text(f), "foo::bar(function (x) \n...)")
})

test_that("splicing does not affect original quosure", {
  f <- ~foo(~bar)
  quo_text(f)
  expect_identical(f, ~foo(~bar))
})

