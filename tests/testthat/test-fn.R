context("functions")

test_that("prim_name() extracts names", {
  expect_equal(prim_name(c), "c")
  expect_equal(prim_name(prim_eval), "eval")
})
