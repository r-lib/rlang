context("f_list")

test_that("input must be a list", {
  expect_error(as_f_list(1), "must be a list")
})

test_that("LHS must evaluate to a string", {
  expect_error(f_list(1 ~ x), "must evaluate to a string or name")
  expect_error(f_list(letters ~ x), "must evaluate to a single string")
  expect_error(f_list(x ~ x ~ z), "must be a single-sided formula")
})

test_that("regular elements are left as is", {
  expect_equal(f_list(x = 1:10), list(x = 1:10))
  expect_equal(f_list(x = ~x), list(x = ~x))
})

test_that("output is actually a formula", {
  out <- f_list(x = ~x)[[1]]
  expect_s3_class(out, "formula")
  expect_identical(attr(out, ".Environment"), environment())
})

test_that("output always has names", {
  out <- f_list(1, 2, 3)
  expect_equal(names(out), c("", "", ""))
})

test_that("names taken from LHS of formula", {
  out1 <- f_list("x" ~ y)
  out2 <- f_list(quote(x) ~ y)

  var <- ~x
  out3 <- f_list(var ~ y); out3

  expect_equal(out1, list(x = ~y))
  expect_equal(out2, list(x = ~y))
  expect_equal(out3, list(x = ~y))
})

test_that("null LHS leaves names unchanged", {
  expect_equal(f_list(x = NULL ~ y), list(x = ~y))
})

test_that("LHS evaluated in formula environment", {
  f <- function(x) {
    paste0(x, 1) ~ y
  }

  expect_equal(f_list(f("y")), list(y1 = ~ y))
})
