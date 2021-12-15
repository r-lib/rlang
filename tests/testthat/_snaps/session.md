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

# pkg_version_info() parses info

    Code
      (expect_error(pkg_version_info("foo 1.0"), "valid"))
    Output
      <error/rlang_error>
      Error in `caller()`: Must supply valid package names.
      x Problematic names:
      * "foo 1.0"
    Code
      (expect_error(pkg_version_info("foo (1.0)"), "parse"))
    Output
      <error/rlang_error>
      Error in `caller()`: Can't parse version in `pkg`.
    Code
      (expect_error(pkg_version_info("foo (>= 1.0)", "1.0"), "both"))
    Output
      <error/rlang_error>
      Error in `caller()`: Can't supply version in both `pkg` and `version`.
      x Redundant versions:
      * "foo (>= 1.0)"
    Code
      (expect_error(pkg_version_info(c("foo (!= 1.0)"))))
    Output
      <error/rlang_error>
      Error in `caller()`: `op` must be one of ">", ">=", "<", or "<=".

# pkg_version_info() supports `op`

    Code
      err(pkg_version_info(c("foo", "bar", "baz"), NULL, c(NA, NA, ">=")))
    Output
      <error/rlang_error>
      Error in `caller()`: `version` must be supplied when `op` is supplied.
    Code
      err(pkg_version_info(c("foo", "bar", "baz"), c("1", "2", NA), c(NA, NA, ">=")))
    Output
      <error/rlang_error>
      Error in `caller()`: `version` must be supplied when `op` is supplied.
    Code
      err(pkg_version_info(c("foo", "bar (>= 2.0)"), c(NA, "2.0"), c(NA, ">=")))
    Output
      <error/rlang_error>
      Error in `caller()`: Can't supply version in both `pkg` and `version`.
      x Redundant versions:
      * "bar (>= 2.0)"
    Code
      err(pkg_version_info("foo", "1.0", "!="))
    Output
      <error/rlang_error>
      Error in `caller()`: `op` must be one of ">", ">=", "<", or "<=".

