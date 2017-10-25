context("s3")

test_that("inherits from all classes", {
  x <- structure(list(), class = c("foo", "bar", "baz"))

  expect_true(inherits_all(x, c("foo")))
  expect_true(inherits_all(x, c("foo", "baz")))
  expect_true(inherits_all(x, c("foo", "bar", "baz")))

  expect_false(inherits_all(x, c("fooz")))
  expect_false(inherits_all(x, c("foo", "barz", "baz")))
  expect_false(inherits_all(x, c("fooz", "bar", "baz")))

  expect_error(inherits_all(x, chr()), "empty")
})

test_that("inherits from any class", {
  x <- structure(list(), class = "bar")

  expect_true(inherits_any(x, c("bar", "foo")))
  expect_true(inherits_any(x, c("foo", "bar")))
  expect_true(inherits_any(x, c("foo", "bar", "baz")))

  expect_false(inherits_any(x, c("foo", "baz")))

  expect_error(inherits_any(x, chr()), "empty")
})

test_that("can box and unbox a value", {
  box <- box(letters, "foo")
  expect_true(is_box(box))
  expect_true(is_box(box), "foo")
  expect_false(is_box(box, "bar"))
  expect_identical(unbox(box), letters)

  box <- box(NULL, c("foo", "bar", "baz"))
  expect_true(is_box(box, c("foo", "baz")))
  expect_false(is_box(box, c("baz", "foo")))
})
