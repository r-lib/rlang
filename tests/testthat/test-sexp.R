context("sexp")

test_that("mut_type() changes SEXPTYPE", {
  x <- node(quote(foo), NULL)
  out <- withVisible(mut_type(x, "language"))
  expect_false(out$visible)
  expect_identical(out$value, x)
  expect_identical(typeof(x), "language")
})
