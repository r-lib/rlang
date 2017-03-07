context("quosure")

test_that("quosures are spliced", {
  q <- ~foo(~bar, ~baz(~baz, 3))
  expect_identical(quo_text(q), "foo(bar, baz(baz, 3))")

  q <- tidy_interp(~foo::bar(!! function(x) ...))
  expect_identical(quo_text(q), "foo::bar(function (x) \n...)")

  q <- ~~~foo(~~bar(~~~baz))
  expect_identical(quo_text(q), "foo(bar(baz))")
})

test_that("splicing does not affect original quosure", {
  f <- ~foo(~bar)
  quo_text(f)
  expect_identical(f, ~foo(~bar))
})

