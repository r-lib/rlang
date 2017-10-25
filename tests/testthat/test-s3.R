context("s3")

test_that("can box and unbox a value", {
  box <- box(letters, "foo")
  expect_true(is_box(box))
  expect_true(is_box(box), "foo")
  expect_false(is_box(box, "bar"))
  expect_identical(unbox(box), letters)
})
