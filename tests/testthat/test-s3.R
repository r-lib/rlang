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

test_that("inherits only from class", {
  x <- structure(list(), class = c("foo", "bar", "baz"))
  expect_false(inherits_only(x, c("foo", "baz")))
  expect_true(inherits_only(x, c("foo", "bar", "baz")))
})

test_that("can box and unbox a value", {
  box <- new_box(letters, "foo")
  expect_true(is_box(box))
  expect_true(is_box(box), "foo")
  expect_false(is_box(box, "bar"))
  expect_identical(unbox(box), letters)

  box <- new_box(NULL, c("foo", "bar", "baz"))
  expect_true(is_box(box, c("foo", "baz")))
  expect_false(is_box(box, c("baz", "foo")))
})

test_that("as_box() ensures boxed value", {
  box <- as_box(NULL)
  expect_true(inherits_only(box, "rlang_box"))

  boxbox <- as_box(box)
  expect_true(inherits_only(box, "rlang_box"))
  expect_null(unbox(box))

  some_box <- as_box(NULL, "some_box")
  some_boxbox <- as_box(some_box, "other_box")
  expect_true(inherits_only(some_boxbox, c("other_box", "rlang_box")))
  expect_true(inherits_only(unbox(some_boxbox), c("some_box", "rlang_box")))
  expect_null(unbox(unbox(some_boxbox)))
})

test_that("as_box_if() ensures boxed value if predicate returns TRUE", {
  box <- as_box_if(NULL, is_null, "null_box")
  expect_true(inherits_only(box, c("null_box", "rlang_box")))

  boxbox <- as_box_if(box, is_null, "null_box")
  expect_true(inherits_only(box, c("null_box", "rlang_box")))
  expect_null(unbox(boxbox))

  expect_null(as_box_if(NULL, is_vector, "null_box"))
})

test_that("unboxing a non-boxed value is an error", {
  expect_error(unbox(NULL), "must be a box")
})
