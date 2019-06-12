context("attributes")

test_that("names2() takes care of missing values", {
  x <- set_names(1:3, c("a", NA, "b"))
  expect_identical(names2(x), c("a", "", "b"))
})

test_that("names2() fails for environments", {
  expect_error(names2(env()), "Use `env_names()` for environments.", fixed = TRUE)
})

test_that("inputs must be valid", {
  expect_error(set_names(environment()), "must be a vector")
  expect_error(set_names(1:10, letters[1:4]), "same length")
})

test_that("can supply vector or ...", {
  expect_named(set_names(1:2, c("a", "b")), c("a", "b"))
  expect_named(set_names(1:2, "a", "b"), c("a", "b"))
  expect_named(set_names(1:2, 1, 2), c("1", "2"))
})

test_that("can supply function/formula to rename", {
  x <- c(a = 1, b = 2)
  expect_named(set_names(x, toupper), c("A", "B"))
  expect_named(set_names(x, ~ toupper(.)), c("A", "B"))
  expect_named(set_names(x, paste, "foo"), c("a foo", "b foo"))
})

test_that("set_names() zaps names", {
  expect_null(names(set_names(mtcars, NULL)))
})

test_that("set_names() coerces to character", {
  expect_identical(set_names(1L, TRUE), c(`TRUE` = 1L))
  expect_identical(set_names(1:2, "a", TRUE), c(a = 1L, `TRUE` = 2L))
})

test_that("has_name() works with pairlists", {
  expect_true(has_name(fn_fmls(`[.data.frame`), "drop"))
})

test_that("set_names() first names the vector before applying a function (#688)", {
  exp <- set_names(letters, toupper(letters))
  expect_identical(set_names(set_names(letters), toupper), exp)
  expect_identical(set_names(letters, toupper), exp)
})
