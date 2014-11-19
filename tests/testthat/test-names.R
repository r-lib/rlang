context("names")

test_that("auto_name does not truncate symbols (#19)", {
  long_name <- quote(AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA)
  dots <- as.lazy_dots(long_name)

  expect_equal(auto_names(dots), as.character(long_name))
})
