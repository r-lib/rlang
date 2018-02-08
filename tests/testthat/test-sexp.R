context("sexp")

test_that("poke_type() changes object type", {
  x <- new_node(quote(foo), NULL)
  out <- withVisible(poke_type(x, "language"))
  expect_false(out$visible)
  expect_identical(out$value, x)
  expect_identical(typeof(x), "language")
})
