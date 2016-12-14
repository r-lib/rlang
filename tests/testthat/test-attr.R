context("attributes")

test_that("names2() takes care of missing values", {
  x <- set_names(1:3, c("a", NA, "b"))
  expect_identical(names2(x), c("a", "", "b"))
})
