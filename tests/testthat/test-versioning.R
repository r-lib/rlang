context("versioning")

test_that("versions are vectors", {
  ver <- ver("1.0.0")
  expect_length(ver, 3)
  expect_identical(ver[[3]], 0L)
  ver[[3]] <- 9L
  expect_identical(ver[[3]], 9L)
})

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

test_that("cycles are completed", {
  expect_identical(new_cycle("0.1.0"), new_cycle(c("0.1.0", "0.2.0", "0.3.0")))
  expect_identical(new_cycle(c("0.1.0", "0.2.0")), new_cycle(c("0.1.0", "0.2.0", "0.3.0")))
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

test_that("versions are trimmed", {
  ver <- ver("0.1.0")

  expect_identical(ver_trim(ver, 4), ver)
  expect_identical(ver_trim(ver, 3), ver)
  expect_identical(ver_trim(ver, 2), ver("0.1"))
})

test_that("correct deprecation message is built", {
  cycle <- c("1.0.0", "2.0.0", "3.0.0")
  expect_identical(deprecated_function_msg("pkg::foo()", "1.0.0", 1, "bar()"), "`pkg::foo()` is soft-deprecated as of version 1.0.0, please use `bar()` instead")
  expect_identical(deprecated_function_msg("pkg::foo()", "2.0.0", 2), "`pkg::foo()` is deprecated as of version 2.0.0")
  expect_identical(deprecated_function_msg("pkg::foo()", "3.0.0", 3), "`pkg::foo()` is defunct as of version 3.0.0")
})

test_that("can't deprecate function multiple times", {
  foo <- function() "returned"
  foo <- deprecate(foo, "0.1.0")
  expect_error(deprecate(foo, "0.1.0"), "Function `foo` is already deprecated")
})

test_that("deprecated function does not signal if not in deprecation cycle", {
  foo <- function() "returned"
  foo <- set_env(foo, ns_env("rlang"))

  future_ver <- as.character(future_rlang_ver())
  foo <- deprecate(foo, c(future_ver, "", ""))

  expect_true(is_deprecated(foo))
  expect_condition(foo(), NA)
})

test_that("deprecated function signals soft-deprecation", {
  foo <- function() "returned"
  foo <- set_env(foo, ns_env("rlang"))

  past_ver <- as.character(past_rlang_ver())
  foo <- deprecate(foo, c(past_ver, "", ""))

  msg <- sprintf("deprecated as of version %s", past_ver)
  expect_condition(foo(), "deprecated", msg)
})

test_that("deprecated function signals deprecation", {
  foo <- function() "returned"
  foo <- set_env(foo, ns_env("rlang"))

  past_ver <- as.character(past_rlang_ver())
  foo <- deprecate(foo, c("", past_ver, ""))

  msg <- sprintf("deprecated as of version %s", past_ver)
  expect_warning(foo(), msg)
})

test_that("deprecated function signals itself defunct", {
  foo <- function() "returned"
  foo <- set_env(foo, ns_env("rlang"))

  past_ver <- as.character(past_rlang_ver())
  foo <- deprecate(foo, c("", "", past_ver))

  expect_error(foo(), "defunct as of version 0.1.0")
})
