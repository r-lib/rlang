
context("lazy_dots")

test_that("lazy_dots works with no args", {
  l1 <- lazy_dots()
  l2 <- lazy_dots(.follow_symbols = TRUE)

  expect_equal(l1, structure(list(), class = "lazy_dots"))
  expect_equal(l2, structure(list(), class = "lazy_dots"))

})


test_that(".ignore_empty drops empty arguments", {
  l1 <- lazy_dots(, 1,)
  l2 <- lazy_dots(, 1, , .ignore_empty = TRUE)

  expect_equal(length(l1), 3)
  expect_equal(length(l2), 1)
  expect_equal(l2[[1]]$expr, 1)
})
