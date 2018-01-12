context("dictionary")

test_that("can't access non-existent list members", {
  x1 <- list(y = 1)
  x2 <- as_dictionary(x1)

  expect_equal(x2$y, 1)
  expect_error(x2$z, "Object `z` not found in `.data`")
  expect_error(x2[["z"]], "Object `z` not found in `.data`")
})

test_that("can't access non-existent environment components", {
  x1 <- list2env(list(y = 1))
  x2 <- as_dictionary(x1)

  expect_equal(x2$y, 1)
  expect_error(x2$z, "Object `z` not found in environment")
  expect_error(x2[["z"]], "Object `z` not found in environment")
})

test_that("can't use non-character vectors", {
  x <- as_dictionary(list(y = 1))

  expect_error(x[[1]], "with a string")
  expect_error(x[[c("a", "b")]], "with a string")
})

test_that("subsetting .data pronoun fails when not supplied", {
  f <- quo(.data$foo)
  expect_error(eval_tidy(f), "not found in `.data`")
})

test_that("names() and length() methods", {
  x <- as_dictionary(mtcars)
  expect_identical(names(x), names(mtcars))
  expect_identical(length(x), length(mtcars))
})

test_that("can replace elements of dictionaries", {
  expect_src <- function(dict, expected) {
    src <- .subset2(dict, "src")
    expect_identical(src, expected)
  }

  x <- as_dictionary(list(foo = "bar"))

  x$foo <- "baz"
  expect_src(x, list(foo = "baz"))

  x[["bar"]] <- "bam"
  expect_src(x, list(foo = "baz", bar = "bam"))

  expect_error(x[[3]] <- NULL, "with a string")
})

test_that("cannot replace elements of read-only dictionaries", {
  x <- as_dictionary(list(foo = "bar"), read_only = TRUE)
  expect_error(x$foo <- "baz", "Can't modify")
  expect_error(x[["foo"]] <- "baz", "Can't modify")
})
