context("f_list")

test_that("regular elements are left as is", {
  expect_equal(f_list(x = 1:10), list(x = 1:10))
  expect_equal(f_list(x = ~x), list(x = ~x))
})

test_that("output always has names", {
  out <- f_list(1, 2, 3)
  expect_equal(names(out), c("", "", ""))
})

test_that("names taken from LHS of formula", {
  out <- f_list("x" ~ y)
  expect_equal(out, list(x = ~y))
})

test_that("LHS evaluated in formula environment", {
  f <- function(x) {
    paste0(x, 1) ~ y
  }

  expect_equal(f_list(f("y")), list(y1 = ~ y))
})
