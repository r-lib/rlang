context("versioning")

test_that("components are checked", {
  ver <- as_version("0.0.0.0")
  expect_error(ver_check(ver), "must have 3 components, not 4")

  ver <- as_version("0.100.0")
  expect_error(ver_check(ver), "more than 2 digits")

  ver <- as_version("0.99.0")
  expect_error(ver_check(ver), NA)

  ver <- as_version("0.0.1")
  expect_error(ver_check(ver), "can't be a minor update")
})
})
