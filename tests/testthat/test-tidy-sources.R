context("tidy sources")

test_that("can't access non-existent list members", {
  x1 <- list(y = 1)
  x2 <- data_source(x1)

  expect_equal(x2$y, 1)
  expect_error(x2$z, "Object 'z' not found in pronoun")
  expect_error(x2[["z"]], "Object 'z' not found in pronoun")
})

test_that("can't access non-existent environment components", {
  x1 <- list2env(list(y = 1))
  x2 <- data_source(x1)

  expect_equal(x2$y, 1)
  expect_error(x2$z, "Object 'z' not found in environment")
  expect_error(x2[["z"]], "Object 'z' not found in environment")
})

test_that("can't use non-character vectors", {
  x <- data_source(list(y = 1))

  expect_error(x[[1]], "subset with a string")
  expect_error(x[[c("a", "b")]], "subset with a string")
})

test_that("subsetting .data pronoun fails when not supplied", {
  f <- quo(.data$foo)
  expect_error(eval_tidy(f), "not found in pronoun")
})

test_that("names() and length() methods", {
  x <- data_source(mtcars)
  expect_identical(names(x), names(mtcars))
  expect_identical(length(x), length(mtcars))
})
