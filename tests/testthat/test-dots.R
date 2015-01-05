
context("lazy_dots")

test_that("lazy_dots works with no args", {

  l1 <- lazy_dots()
  l2 <- lazy_dots(.follow_symbols = TRUE)

  expect_equal(l1, structure(list(), class = "lazy_dots"))
  expect_equal(l2, structure(list(), class = "lazy_dots"))

})
