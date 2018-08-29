context("test-exec")

test_that("supports tidy dots", {
  expect_equal(exec(list, x = 1), list(x = 1))

  args <- list(x = 1)
  expect_equal(exec(list, !!!args), list(x = 1))
  expect_equal(exec(list, !!!args, y = 2), list(x = 1, y = 2))
})

test_that("does not inline expressions", {
  expect_equal(exec(list, x = expr(x), y = expr(y)), exprs(x = x, y = y))
})
