context("interp")

test_that("interp produces single string for character inputs", {
  x <- interp("aaaaaaaaaaaaaa + bbbbbbbbbbbbbbb + ccccccccccccccccc + dddddddddddddddd + eeeeeeeeeeeeeee")
  expect_is(x, "character")
  expect_equal(length(x), 1)
})

