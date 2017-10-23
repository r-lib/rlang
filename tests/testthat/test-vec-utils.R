context("vec-utils")

test_that("vector is modified", {
  x <- c(1, b = 2, c = 3, 4)
  out <- modify(x, 5, b = 20, splice(list(6, c = "30")))
  expect_equal(out, list(1, b = 20, c = "30", 4, 5, 6))
})

test_that("are_na() requires vector input but not is_na()", {
  expect_error(are_na(base::eval), "must be a vector")
  expect_false(is_na(base::eval))
})
