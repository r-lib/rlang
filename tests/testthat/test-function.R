context("function")

test_that("new_function equivalent to regular function", {
  f1 <- function(x = a + b, y) {
    x + y
  }
  attr(f1, "srcref") <- NULL

  f2 <- new_function(alist(x = a + b, y =), quote({x + y}))

  expect_equal(f1, f2)
})
