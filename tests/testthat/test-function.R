context("function")

test_that("function_new equivalent to regular function", {
  f1 <- function(x = a + b, y) {
    x + y
  }
  attr(f1, "srcref") <- NULL

  f2 <- function_new(alist(x = a + b, y =), quote({x + y}))

  expect_equal(f1, f2)
})
