context("vec-utils")

test_that("seq2() creates increasing sequences", {
  expect_identical(seq2(2, 3), 2:3)
  expect_identical(seq2(3, 2), int())
})

test_that("seq2_along() creates increasing sequences", {
  expect_identical(seq2_along(3, 1:2), int())
  expect_identical(seq2_along(-1, 1:2), -1:2)
})

test_that("seq2() fails with non-scalar inputs", {
  expect_error(seq2(int(), 1), "must be length one")
  expect_error(seq2(1, int()), "must be length one")
})
