context("vector")

test_that("splice() produces correctly named lists", {
  inputs <- list(arg1 = "a", arg2 = "b")

  out1 <- splice(inputs, arg3 = c("c1", "c2"))
  out2 <- splice(inputs, arg = list(arg3 = 1, arg4 = 2))

  expect_equal(names(out1), c("arg1", "arg2", "arg3"))
  expect_equal(names(out2), c("arg1", "arg2", "arg3", "arg4"))
})

test_that("vector is modified", {
  x <- c(1, b = 2, c = 3, 4)
  out <- modify(x, 5, b = 20, .elts = list(6, c = "30"))
  expect_equal(out, list(1, b = 20, c = "30", 4, 5, 6))
})

test_that("are_na() requires vector input but not is_na()", {
  expect_error(are_na(base::eval), "must be a vector")
  expect_false(is_na(base::eval))
})
