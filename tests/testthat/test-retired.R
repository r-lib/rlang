context("retired")

test_that("lang_tail() works properly", {
  expect_identical(
    pairlist(sym("a")),
    lang_tail(expr(foo(a)))
  )
})

test_that("lang_head() works properly", {
  expect_identical(
    lang_head(expr(foo(a))),
    expr(foo)
  )
})
