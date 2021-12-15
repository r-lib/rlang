# check_installed() fails if packages are not installed

    Code
      (expect_error(check_installed("_foo")))
    Output
      <error/rlang_error>
      Error in `foo()`: The package `_foo` is required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"))))
    Output
      <error/rlang_error>
      Error in `foo()`: The packages `_foo` and `_bar` are required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"), "to proceed.")))
    Output
      <error/rlang_error>
      Error in `foo()`: The packages `_foo` and `_bar` are required to proceed.

# is_installed() checks minimal versions

    Code
      (expect_error(is_installed(c("rlang", "testthat"), version = "0.1"),
      "the same length"))
    Output
      <error/rlang_error>
      Error in `is_installed()`: `version` must be `NULL` or a vector of versions the same length as `pkg`.

# check_installed() checks minimal versions

    Code
      (expect_error(check_installed(c("rlang", "testthat"), version = "0.1")))
    Output
      <error/rlang_error>
      Error in `check_installed()`: `version` must be `NULL` or a vector of versions the same length as `pkg`.
    Code
      (expect_error(check_installed("_foo", version = "1.0")))
    Output
      <error/rlang_error>
      Error in `foo()`: The package `_foo` (>= 1.0) is required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"), version = c("1.0", NA))))
    Output
      <error/rlang_error>
      Error in `foo()`: The packages `_foo` (>= 1.0) and `_bar` are required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"), version = c(NA, "2.0"))))
    Output
      <error/rlang_error>
      Error in `foo()`: The packages `_foo` and `_bar` (>= 2.0) are required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"), "to proceed.", version = c(
        "1.0", "2.0"))))
    Output
      <error/rlang_error>
      Error in `foo()`: The packages `_foo` (>= 1.0) and `_bar` (>= 2.0) are required to proceed.

# `pkg` is type-checked

    Code
      (expect_error(is_installed(1)))
    Output
      <error/rlang_error>
      Error in `is_installed()`: `pkg` must be a package name or a vector of package names.
    Code
      (expect_error(is_installed(na_chr)))
    Output
      <error/rlang_error>
      Error in `is_installed()`: `pkg` must be a package name or a vector of package names.
    Code
      (expect_error(check_installed(c("foo", ""))))
    Output
      <error/rlang_error>
      Error in `check_installed()`: `pkg` must be a package name or a vector of package names.
    Code
      (expect_error(check_installed(c("foo", "bar"), version = c("1", ""))))
    Output
      <error/rlang_error>
      Error in `check_installed()`: `version` must be `NULL` or a vector of versions the same length as `pkg`.

