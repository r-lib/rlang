context("versioning")

test_that("components are checked", {
  expect_error(ver_check(ver("0.0.0.0"), n_components = 3), "must have 3 components, not 4")
  expect_error(ver_check(ver("0.100.0"), max_digits = 2), "more than 2 digits")
  expect_error(ver_check(ver("0.99.0"), max_digits = 2), NA)
  expect_error(ver_check(ver("0.0.1"), minor = FALSE), "can't be a minor update")
  expect_error(ver_check(ver("0.1.0"), minor = TRUE), "must be a minor update")
  expect_error(ver_check(ver("0.0.1"), minor = NULL), NA)
})

test_that("cycle must have at least one version", {
  expect_error(new_cycle(""), "`cycle` can't be empty")
  expect_error(new_cycle(c("", "", ""), "`cycle` can't be empty"))
})

test_that("cycles are filled with empty versions", {
  expect_identical(set_names(new_cycle("0.1.0"), NULL), list(ver("0.0.0"), ver("0.0.0"), ver("0.1.0")))
  expect_identical(set_names(new_cycle(c("0.1.0", "0.2.0")), NULL), list(ver("0.0.0"), ver("0.1.0"), ver("0.2.0")))
})

test_that("cycle must be a character vector with 1 to 3 components", {
  expect_error(new_cycle(c("0.0.0", "0.1.0", "0.2.0", "0.4.0")), "must have 1, 2, or 3 components")
  expect_error(new_cycle(chr()), "must have 1, 2, or 3 components")
  expect_error(new_cycle(1L), "can't parse version")
})

test_that("cycle must be monotonic", {
  expect_error(new_cycle(c("0.1.0", "0.1.0")), "must be monotonically increasing")
  expect_error(new_cycle(c("0.2.0", "0.1.0")), "must be monotonically increasing")
})

test_that("alias is created from cycle", {
  expect_named(new_cycle(c("0.1.0")), c("0.0.0", "0.0.0", "0.1.0"))
  expect_named(new_cycle(c("0.1.0", `0.1.9000` = "0.2.0")), c("0.0.0", "0.1.0", "0.2.0"))
  expect_error(new_cycle(c("0.1.0", `0.0.9000` = "0.2.0")), "must be monotonically increasing")
})
